%include mystyle.fmt

\subsubsection{Network.XMPP}

В модуле @Network.XMPP@ описывается структура данных @Stanza@, представляющая строфы XMPP.
Так же данный модуль предоставляет способ аутентификации на сервере и структуру данных,
позволяющую отправлять строфы на сервер.
Данные, поступающие от сервера, преобразуются в поток строф.

\begin{code}
module Network.XMPP
( Stanza (..)
, Subscribe (..)
, Connection (..)
, login
) where
\end{code}

Используется сторонняя библиотека gsasl\cite{gsasl}.

\begin{code}
import System.IO
import Network
import Network.Parser
import Network.XMPPTypes
import Network.XMPPMapping
import Network.Protocol.SASL.GNU
import Control.Applicative
import Control.Monad.State.Lazy
import Data.Maybe
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
\end{code}

Тип, представляющий сообщения для управления подпиской (запросить разрешение на подписку, подтвердить запрос, отклонить запрос).

\begin{code}
data Subscribe
    = Request
        { from :: Maybe JID
        , to :: Maybe JID
        }
    | Confirm
        { from :: Maybe JID
        , to :: Maybe JID
        }
    | Refuse
        { from :: Maybe JID
        , to :: Maybe JID
        }
\end{code}

Строфы, с которыми может манипулировать (получать и отправлять) внешняя программа, использующая данную библиотеку.

\begin{code}
data Stanza
    = Msg Message
    | Roster [JID]
    | Sub Subscribe
    | EndStream

instance BShow Stanza where
    bShow (Msg m) = bShow m
    bShow (Roster ls) = bShow . show $ map showB ls
    bShow (Sub(Request f t)) = "<presence type='subscribe'"
        +++ (bShowAttr "from" f) +++ (bShowAttr "to" t) +++ "/>"
    bShow (Sub(Confirm f t)) = "<presence type='subscribed'"
        +++ (bShowAttr "from" f) +++ (bShowAttr "to" t) +++ "/>"
    bShow (Sub(Refuse f t)) = "<presence type='unsubscribed'"
        +++ (bShowAttr "from" f) +++ (bShowAttr "to" t)
        +++ "/><iq type='set'><query xmlns='jabber:iq:roster'><item"
        +++ (bShowAttr "jid" t) +++ " subscription='remove'/></query></iq>"
    bShow EndStream = bShow "</stream:stream>"

instance Show Stanza where
    show s = showB . bShow $ s
\end{code}

Структура данных, через которую внешняя программа может отправлять строфы серверу.

\begin{code}
data Connection = Connection    { send :: Stanza -> IO () 
                                }
\end{code}

Информация о подключении.

\begin{code}
type Name = BString

type Server = BString

type Password = BString

data Info
    = Info
        { server :: Server
        , username :: Name
        , password :: Password
        }
\end{code}

Структура данных, описывющая этапы авторизации.

\begin{code}
data XMPPEvent
    = None
    | Open
    | Auth
    | OpenAuth
    | Bind
    | Session
    | Presence
    | RosterD
    | Done
    deriving Eq
\end{code}

Процесс авторизации реализован при помощи State-монады.
В качестве внутреннего состояния используется структура @ProtocolState@.
В ней записаны информация о соединении,
последний пройденный этап авторизации,
входной поток сообщений, соединение для отправления сообщений и список контактов.
Переход из одного состояния в другое определяется текущим этапом авторизации и пришедшим сообщением.

\begin{code}
data ProtocolState
    = ProtocolState
        { info :: Info
        , done :: XMPPEvent
        , fromServer :: [InnerStanza]
        , connection :: BString -> IO ()
        , roster :: [JID]
        }

type XMPPState m a = StateT ProtocolState m a
\end{code}

\paragraph{Вспомогательные функции для работы с @XMPPState@.}

\emph{Вернуть} {\textit sample} очередное входящее сообщение и удалить его из входного потока.

\begin{code}
pollMsg :: (Monad m) => XMPPState m InnerStanza
pollMsg = StateT $ \s -> case s of
    (ProtocolState _ _ [] _ _) -> undefined
    (ProtocolState i d (x:xs) c m) -> do
        return $ (x, ProtocolState i d xs c m)
\end{code}

Установить пройденный этап авторизации.

\begin{code}
setDone :: (Monad m) => XMPPEvent -> XMPPState m ()
setDone d = modify $ \(ProtocolState i _ f c m) -> ProtocolState i d f c m
\end{code}

Отправить сообщение.

\begin{code}
sendToServer :: (MonadIO m, BShow a) => a -> XMPPState m ()
sendToServer msg = do
    c <- gets connection
    liftIO $ c (bShow msg)
\end{code}

Обновить список контактов.

\begin{code}
setRoster :: (Monad m) => [JID] -> XMPPState m ()
setRoster ls = StateT $ \(ProtocolState i d f c _) -> return $ ((), ProtocolState i d f c ls)
\end{code}

Аутентификация на сервере при помощи SASL.

Получить имя механизма аутентификации.

\begin{code}
mechName :: Mechanism -> BString
mechName (Mechanism name) = name
\end{code}

Завершить процесс аутентификации.

\begin{code}
sessionFinish :: XMPPState Session Bool
sessionFinish = do
    msg <- pollMsg
    case msg of
        IAuth (ISuccess _) -> return True
        otherwise -> return False
\end{code}

Цикл аутентификации.

\begin{code}
sessionLoop :: XMPPState Session Bool
sessionLoop = do
    msg <- pollMsg
    case msg of
        (IAuth (IChallenge ch)) -> do
            (text, pr) <- lift . step64 $ ch
            let out = "<response xmlns='urn:ietf:params:xml:ns:xmpp-sasl'>"
                    +++ text +++ "</response>"
            sendToServer out
            case pr of
                Complete -> sessionFinish
                NeedsMore -> sessionLoop
        (IAuth (ISuccess ch)) -> do
            (text, pr) <- lift . step64 $ ch
            case pr of
                Complete -> return True
                NeedsMore -> return False
        otherwise -> return False
\end{code}

Установить параметры сессии и запустить цикл аутентификации.

\begin{code}
saslSession :: XMPPState Session Bool
saslSession = do
    m <- mechName <$> lift mechanismName
    u <- gets $ username . info
    p <- gets $ password . info
    s <- gets $ server . info
    lift . setProperty PropertyAuthID $ u
    lift . setProperty PropertyPassword $ p
    lift . setProperty PropertyHostname $ s
    lift . setProperty PropertyService $ bShow "xmpp"
    (text, pr) <- lift . step64 $ B.empty
    let out = "<auth xmlns='urn:ietf:params:xml:ns:xmpp-sasl' mechanism='"
            +++ m +++ "'>" +++ text +++ "</auth>"
    sendToServer out
    case pr of
        Complete -> sessionFinish
        NeedsMore -> sessionLoop
\end{code}

Найти строфу во входном потоке, информирующую об ошибке, и перевести протокол в новое состояние.

\begin{code}
findFailure :: ProtocolState -> ProtocolState
findFailure (ProtocolState i d f c m) = ProtocolState i d (tail . dropWhile (/= Failure) $ f) c m
\end{code}

Запусить клиентскую сессию процесса аутентификации.

\begin{code}
saslMap :: ProtocolState -> Mechanism -> Session (Bool, ProtocolState) -> SASL (Bool, ProtocolState)
saslMap start m session = do
    res <- runClient m session
    case res of
        Left err -> return (False, findFailure start)
        Right ans -> return ans
\end{code}

Определить подходящий механизм и запустить процесс аутентификации.

\begin{code}
saslAuth :: [Mechanism] -> XMPPState SASL Bool
saslAuth ms = do
    suggested <- lift . clientSuggestMechanism $ ms
    s <- get
    case suggested of
        Just m -> do
            ans <- mapStateT (saslMap s m) saslSession
            if ans
                then
                    return True
                else
                    saslAuth $ filter (/= m) ms
        Nothing -> return False
\end{code}

Преобразование структуры данных, представляющих строфы для внутренней обработки, в строфы для обработки внешним приложением.

\begin{code}
toStanzaElement :: InnerStanza -> Maybe Stanza
toStanzaElement (IMsg msg) = Just $ Msg msg
toStanzaElement (Iq (IRoster ls)) = Just $ Roster ls
toStanzaElement CloseStream = Just EndStream
toStanzaElement (IRequest jid) = Just . Sub $ Request (Just jid) Nothing
toStanzaElement (IConfirm jid) = Just . Sub $ Confirm (Just jid) Nothing
toStanzaElement (IRefuse jid) = Just . Sub $ Refuse (Just jid) Nothing
toStanzaElement _ = Nothing

toStanza :: [InnerStanza] -> [Stanza]
toStanza = map fromJust . filter isJust . map toStanzaElement
\end{code}

Вспомогательные определения.

\begin{code}
portNum :: PortNumber
portNum = 5222

hSend :: (BShow a) => Handle -> a -> IO ()
hSend h bs = do
    B.hPut h (bShow bs)
    hFlush h

startStream :: BString -> BString
startStream server = "<?xml version='1.0'?><stream:stream to='"
    +++ server
    +++ "' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams' version='1.0'>"

bindStanza :: String
bindStanza = "<iq type='set'><bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'>"
    ++ "<resource>test</resource></bind></iq>"

sessionStanza :: String
sessionStanza = "<iq type='set'><session xmlns='urn:ietf:params:xml:ns:xmpp-session'/></iq>"

presenceStanza :: String
presenceStanza = "<presence><show/></presence>"

rosterRequest :: String
rosterRequest = "<iq type='get'><query xmlns='jabber:iq:roster'/></iq>"
\end{code}

Основная функция модуля. Позволяет авторзоваться на сервере.
Возвращает поток сообщений от сервера, начальный список контактов и структуру данных @Connection@.

\begin{code}
login :: Server -> Name -> Password -> IO ([Stanza], [JID], Connection)
login server name password = do
    h <- connectTo (showB server) (PortNumber portNum)
    hSetBuffering h NoBuffering
    hSetBinaryMode h True
    input <- (toXMPP . parseSAXStream [bShow "stream:stream"]) <$> L.hGetContents h
    let authenticate :: XMPPState IO ([Stanza], [JID])
        authenticate = do
            d <- gets done
            msg <- pollMsg
            case (d, msg) of
                (None, OpenStream) -> setDone Open
                (Open, StreamFeatures _ _ ms) -> do
                    mapStateT runSASL $ saslAuth ms
                    sendToServer $ startStream server
                    setDone Auth
                (Auth, OpenStream) -> setDone OpenAuth
                (OpenAuth, StreamFeatures _ _ _) -> do
                    sendToServer bindStanza
                    setDone Bind
                (Bind, Iq IBind) -> do
                    sendToServer sessionStanza
                    setDone Session
                (Session, Iq ISession) -> do
                    sendToServer presenceStanza
                    setDone Presence
                (Presence, IPresence) -> do
                    sendToServer rosterRequest
                    setDone RosterD
                (RosterD, (Iq (IRoster ls))) -> do
                    setRoster ls
                    setDone Done
                otherwise -> liftIO . putStrLn . show $ msg
            lv <- gets done
            if lv == Done
                then do
                    stream <- gets fromServer
                    rstr <- gets roster
                    return (toStanza $ stream, rstr)
                else
                    authenticate
    hSend h $ startStream server
    (stream, ls) <- evalStateT authenticate
        $ ProtocolState (Info server name password) None input (hSend h) []
    return (stream, ls, Connection (hSend h))
\end{code}
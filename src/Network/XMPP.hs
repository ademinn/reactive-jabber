module Network.XMPP
( Stanza (..)
, Subscribe (..)
, Connection (..)
, login
) where

import System.IO
import Network
import Network.Parser
import Network.XMPPTypes
import Network.XMPPMapping
import Network.Protocol.SASL.GNU
--import Reactive.Banana
--import Reactive.Banana.Combinators
--import Reactive.Banana.Switch
--import Reactive.Banana.Frameworks
import Text.XML.Expat.SAX
import Control.Applicative
import Control.Monad.State.Lazy
import Control.Concurrent
import qualified Control.Exception as E
import Data.Maybe
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC


time :: Int
time = 1000

portNum :: PortNumber
portNum = 5222

data Stanza
    = Msg Message
    | Roster [JID]
    | Sub Subscribe
    | EndStream

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

showJust :: (Show a) => Maybe a -> String
showJust Nothing = ""
showJust (Just x) = show x

showAttr :: String -> Maybe JID -> String
showAttr _ Nothing = ""
showAttr s (Just x) = " " ++ s ++ "='" ++ (showB x) ++ "'"

instance Show Stanza where
    show (Msg (Message f t b)) = "<message" ++ f' ++ t' ++ "><body>" ++ (showB b) ++ "</body></message>"
        where
            f' = case f of
                    Nothing -> ""
                    Just s -> " from='" ++ (showB s) ++ "'"
            t' = case t of
                    Nothing -> ""
                    Just s -> " to='" ++ (showB s) ++ "'"
    show (Roster ls) = show $ map showB ls
    show (Sub(Request f t)) = "<presence type='subscribe'" ++ (showAttr "from" f) ++ (showAttr "to" t) ++ "/>"
    show (Sub(Confirm f t)) = "<presence type='subscribed'" ++ (showAttr "from" f) ++ (showAttr "to" t) ++ "/>"
    show (Sub(Refuse f t)) = "<presence type='unsubscribed'" ++ (showAttr "from" f) ++ (showAttr "to" t)
        ++ "/><iq type='set'><query xmlns='jabber:iq:roster'><item" ++ (showAttr "jid" t) ++ " subscription='remove'/></query></iq>"
    show EndStream = "</stream:stream>"

data Connection = Connection    { send :: Stanza -> IO () 
                                }

nullConnection :: Connection
nullConnection = Connection (\_ -> return ())

data Auth = Challenge | Success

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

type Name = BString

type Server = BString

type Password = BString

data Info
    = Info
        { server :: Server
        , username :: Name
        , password :: Password
        }

data ProtocolState
    = ProtocolState
        { info :: Info
        , done :: XMPPEvent
        , fromServer :: [InnerStanza]
        , connection :: BString -> IO ()
        , roster :: [JID]
        }

type XMPPState m a = StateT ProtocolState m a

peekMsg :: (Monad m) => XMPPState m InnerStanza
peekMsg = StateT $ \s -> case s of
    (ProtocolState _ _ [] _ _) -> undefined
    (ProtocolState i d (x:xs) c m) -> return $ (x, ProtocolState i d (x:xs) c m)

pollMsg :: (Monad m, MonadIO m) => XMPPState m InnerStanza
pollMsg = StateT $ \s -> case s of
    (ProtocolState _ _ [] _ _) -> undefined
    (ProtocolState i d (x:xs) c m) -> do
        liftIO $ putStrLn $ show x
        return $ (x, ProtocolState i d xs c m)

setDone :: (Monad m) => XMPPEvent -> XMPPState m ()
setDone d = modify $ \(ProtocolState i _ f c m) -> ProtocolState i d f c m
--changeWaiting :: (Monad m) => XMPPEvent -> XMPPEvent -> XMPPState m ()
--changeWaiting new old = StateT $ \(ProtocolState _ _ f c m) -> return $ ((), ProtocolState new old f c m)

--processState :: XMPPState IO ()
--processState = do
--    msg <- pollMsg
--    s <- get
--    case (s, msg) of
--        (ProtocolState Open None f c m, OpenStream) -> changeWaiting Features Open
--    return ()

--processState :: ProtocolState -> IO ((), ProtocolState)
--processState (ProtocolState Open None (OpenStream:xs) c m) = return $ ((), ProtocolState Features Open xs c m)
--processState (ProtocolState Features Open ((StreamFeatures _ _ ms):xs) c m) = do
    
--processState (ProtocolState )



--processStateM :: XMPPState IO ()
--processStateM = StateT processState
--foo :: XMPPState IO ()
--foo = StateT $ \(w d c s m) -> case

--getUsername :: XMPPState m Name
--getUsername = gets 

mechName :: Mechanism -> BString
mechName (Mechanism name) = name

sendToServer :: (MonadIO m) => BString -> XMPPState m ()
sendToServer msg = do
    c <- gets connection
    liftIO $ c msg

sessionFinish :: XMPPState Session Bool
sessionFinish = do
    msg <- pollMsg
    case msg of
        IAuth (ISuccess _) -> return True
        otherwise -> return False

sessionLoop :: XMPPState Session Bool
sessionLoop = do
    msg <- pollMsg
    case msg of
        (IAuth (IChallenge ch)) -> do
            (text, pr) <- lift . step64 $ ch
--            liftIO . putStrLn . show $ pr
            let out = (C.pack "<response xmlns='urn:ietf:params:xml:ns:xmpp-sasl'>") `B.append` text `B.append` (C.pack "</response>")
            sendToServer out
            case pr of
                Complete -> sessionFinish
                NeedsMore -> sessionLoop
        (IAuth (ISuccess ch)) -> do
            (text, pr) <- lift . step64 $ ch
--            let out = (C.pack "<response xmlns='urn:ietf:params:xml:ns:xmpp-sasl'>") `B.append` text `B.append` (C.pack "</response>")
--            sendToServer out
--            liftIO . putStrLn . show $ pr
--            let out = (C.pack "<response xmlns='urn:ietf:params:xml:ns:xmpp-sasl'>") `B.append` text `B.append` (C.pack "</response>")
--            sendToServer out
            case pr of
                Complete -> return True
                NeedsMore -> return False
        otherwise -> return False

saslSession :: XMPPState Session Bool
saslSession = do
    m <- mechName <$> lift mechanismName
    u <- gets $ username . info
    p <- gets $ password . info
    s <- gets $ server . info
    lift . setProperty PropertyAuthID $ u
    lift . setProperty PropertyPassword $ p
    lift . setProperty PropertyHostname $ s
    lift . setProperty PropertyService $ C.pack "xmpp"
    (text, pr) <- lift . step64 $ C.pack ""
    let out = (C.pack "<auth xmlns='urn:ietf:params:xml:ns:xmpp-sasl' mechanism='") `B.append` m `B.append` (C.pack "'>") `B.append` text `B.append` (C.pack "</auth>")
    sendToServer out
    case pr of
        Complete -> sessionFinish
        NeedsMore -> sessionLoop

findFailure :: ProtocolState -> ProtocolState
findFailure (ProtocolState i d f c m) = ProtocolState i d (tail . dropWhile (/= Failure) $ f) c m

saslMap :: ProtocolState -> Mechanism -> Session (Bool, ProtocolState) -> SASL (Bool, ProtocolState)
saslMap start m session = do
    res <- runClient m session
    case res of
        Left err -> return (False, findFailure start)
        Right ans -> return ans

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

toStanzaElement :: InnerStanza -> Maybe Stanza
toStanzaElement (IMsg msg) = Just $ Msg msg
toStanzaElement (Iq (Q (RosterList ls))) = Just $ Roster ls
toStanzaElement CloseStream = Just EndStream
toStanzaElement (IRequest jid) = Just . Sub $ Request (Just jid) Nothing
toStanzaElement (IConfirm jid) = Just . Sub $ Confirm (Just jid) Nothing
toStanzaElement (IRefuse jid) = Just . Sub $ Refuse (Just jid) Nothing
toStanzaElement _ = Nothing

toStanza :: [InnerStanza] -> [Stanza]
toStanza = map fromJust . filter isJust . map toStanzaElement

setRoster :: (Monad m) => [JID] -> XMPPState m ()
setRoster ls = StateT $ \(ProtocolState i d f c _) -> return $ ((), ProtocolState i d f c ls)

hSend :: Handle -> BString -> IO ()
hSend h bs = do
    C.putStrLn bs
    B.hPut h bs
    hFlush h

handleError :: IOError -> IO ([Stanza], [JID], Connection)
handleError _ = do
    putStrLn "Error handled"
    return ([], [], nullConnection)

login :: Server -> Name -> Password -> (Connection -> IO ()) -> IO ([Stanza], [JID], Connection)
login server name password callback = do
    login' server name password
    --E.catch (login' server name password) handleError

startStream :: String -> String
startStream server = "<?xml version='1.0'?><stream:stream to='" ++ server ++ "' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams' version='1.0'>"

login' :: Server -> Name -> Password -> IO ([Stanza], [JID], Connection) --XMPPState IO ()
login' server name password = do
    h <- connectTo (showB server) (PortNumber portNum)
    putStrLn $ startStream (showB server)
    hSetBuffering h NoBuffering
    hSetBinaryMode h True
    input1 <- L.hGetContents h
    --let input2 = parse (ParseOptions Nothing Nothing) input1
    let input3 = parseSAXStream [C.pack "stream:stream"] input1
    let input = toXMPP input3
    forkIO $ do
        LC.putStrLn input1
        --LC.putStrLn input1
--        sequence $ map (putStrLn . show) input
        return ()
    --input <- toXMPP . parseSAXStream [C.pack "stream:stream"] . parse (ParseOptions Nothing Nothing) <$> L.hGetContents h
    let authenticate :: XMPPState IO ([Stanza], [JID])
        authenticate = do
            d <- gets done
            msg <- pollMsg
            case (d, msg) of
                (None, OpenStream) -> setDone Open
                (Open, StreamFeatures _ _ ms) -> do
                    mapStateT runSASL $ saslAuth ms
                    sendToServer . C.pack $ startStream (showB server)
                    setDone Auth
                (Auth, OpenStream) -> setDone OpenAuth
                (OpenAuth, StreamFeatures _ _ _) -> do
                    sendToServer. C.pack $ "<iq type='set'><bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'><resource>test</resource></bind></iq>"
                    setDone Bind
                (Bind, Iq IBind) -> do
                    sendToServer . C.pack $ "<iq type=\"set\"><session xmlns=\"urn:ietf:params:xml:ns:xmpp-session\" /></iq>"
                    setDone Session
                (Session, Iq ISession) -> do
                    sendToServer . C.pack $ "<presence><show/></presence>"
                    setDone Presence
                (Presence, IPresence) -> do
                    sendToServer . C.pack $ "<iq type = \"get\"><query xmlns=\"jabber:iq:roster\"/></iq>"
                    setDone RosterD
                (RosterD, (Iq (Q (RosterList ls)))) -> do
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
    B.hPut h $ C.pack $ startStream (showB server)
    hFlush h
    (stream, ls) <- evalStateT authenticate $ ProtocolState (Info server name password) None input (hSend h) []
--    forkIO $ do
        --LC.putStrLn input1
--        sequence $ map (putStrLn . show) stream
--        return ()
    return (stream, ls, Connection (hSend h . C.pack . show))

--login :: Frameworks t => Server -> Name -> Password -> IO (Moment t (Behavior t ProtocolState)) --Connection
--login server name password = do
--    h <- connectTo server $ PortNumber portNum
--    hSetBinaryMode h True
--    let sendToServer = B.hPut h
--    (recvFromServerHandler, recvFromServer) <- newAddHandler
--    (changePublicStateHandler, changePublicState) <- newAddHandler
--    (sendToClientHandler, sendToClient) <- newAddHandler
--    sendToServer . C.pack $ "<stream:stream to=\"jabber.ru\" xmlns=\"jabber:client\" xmlns:stream=\"http://etherx.jabber.org/streams\" version=\"1.0\">"
--    let startState = ProtocolState Open None Nothing sendToClientHandler
--    let errorState = ProtocolState None None Nothing sendToClientHandler
--    let networkDescription :: Frameworks t => Moment t ()
--        networkDescription = do
--            recvEvent <- fromAddHandler recvFromServerHandler
--            (changeStateEvent, changeState) <- newEvent
--            let
--                privateState = stepper startState changeStateEvent
--
--                (<%>) f e = f <$> (((,) <$> privateState) <@> e)
--                
--                processEvent :: (ProtocolState, InnerStanza) -> IO ()
--                processEvent (ps, is) = do
--                    x' <- processEvent' ps is
--                    changeState x'
--                    changePublicState x'
--                
--                processEvent' :: ProtocolState -> InnerStanza -> IO ProtocolState
--                processEvent' (ProtocolState Open None x y) OpenStream = return $ ProtocolState Features Open x y
--                processEvent' (ProtocolState Features Open x y) (StreamFeatures _ _ ms) = do
--                    runSASL $ do
--                        m <- clientSuggestMechanism ms
--                        return ()
--                    return errorState
--                processEvent' _ _ = return errorState
--                
--            reactimate $ (processEvent <%> recvEvent)
--    return . fromChanges startState $ changePublicStateHandler
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
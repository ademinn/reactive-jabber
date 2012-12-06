%include polycode.fmt

\subsubsection{Network.XMPPMapping}

Определение интерфейса модуля.

\begin{code}
module Network.XMPPMapping
( TLS (..)
, CompressionMethod (..)
, Compression (..)
, IAuth (..)
, Iq (..)
, InnerStanza (..)
, toXMPP
)where
\end{code}

Импортирование модулей.

\begin{code}
import Network.Parser
import Network.XMPPTypes
import Network.Protocol.SASL.GNU
import Control.Applicative
import Data.Maybe
import qualified Data.ByteString.Char8 as C
\end{code}

\begin{code}
type XMPPNode = Node BString BString
\end{code}

Шифрование потока.

\begin{code}
data TLS = TLS
    deriving (Eq, Show)
\end{code}

Методы сжатия.

\begin{code}
data CompressionMethod
    = Zlib
    deriving (Eq, Show)

data Compression = Compression [CompressionMethod]
    deriving (Eq, Show)
\end{code}

Сообщения процесса аутентификации (SASL).

\begin{code}
data IAuth = IChallenge BString | ISuccess BString
    deriving (Eq, Show)
\end{code}

Iq-строфы.

\begin{code}
data Iq
    = IBind
    | ISession
    | IRoster [JID]
    deriving (Eq, Show)
\end{code}

Строфы, обработка которых происходит внутри модуля Network.XMPP.

\begin{code}
data InnerStanza
    = OpenStream
    | CloseStream
    | StreamFeatures
        { tls :: Maybe TLS
        , compression :: Maybe Compression
        , mechanisms :: [Mechanism]
        }
    | IAuth IAuth
    | Iq Iq
    | IMsg Message
    | IPresence
    | IRequest JID
    | IRefuse JID
    | IConfirm JID
    | Failure
    deriving (Eq, Show)
\end{code}

Проверить, поддерживает ли сервер TLS.

\begin{code}
getTLS :: [XMPPNode] -> Maybe TLS
getTLS xs = case findChild (bShow "starttls") xs of
    Just _ -> Just TLS
    otherwise -> Nothing
\end{code}

Поддерживаемые сервером методы сжатия.

\begin{code}
getCompression :: [XMPPNode] -> Maybe Compression
getCompression _ = Nothing
\end{code}

Поддерживаемые сервером механизмы аутентификации.

\begin{code}
getMechanism :: XMPPNode -> Maybe Mechanism
getMechanism (Element name _ [Text text]) = case (showB name) of
    "mechanism" -> Just . Mechanism $ text
    otherwise -> Nothing
getMechanism _ = Nothing

getMechanisms :: [XMPPNode] -> [Mechanism]
getMechanisms xs = case findChild (bShow "mechanisms") xs of
    Just x -> case x of
        (Element _ _ c) -> map fromJust $ filter isJust $ map getMechanism c
        otherwise -> []
    otherwise -> []
\end{code}

Определить все механизмы, поддерживаемые сервером.

\begin{code}
getStreamFeatures :: [XMPPNode] -> InnerStanza
getStreamFeatures xs = StreamFeatures (getTLS xs) (getCompression xs) (getMechanisms xs)
\end{code}

Определить JID отправителя сообщения.

\begin{code}
getFrom :: XMPPNode -> BString
getFrom (Element _ attrs _) = C.takeWhile (/= '/') . snd . fromJust . findAttribute (bShow "from") $ attrs
\end{code}

Определить содержание сообщения.

\begin{code}
getBody :: XMPPNode -> BString
getBody (Element _ _ cs) = case findChild (bShow "body") cs of
    Just (Element _ _ [Text text]) -> text
    otherwise -> bShow ""
\end{code}

Преобразовать XML элемент, содержащий сообщение, в тип Message.

\begin{code}
getMessage :: XMPPNode -> Message
getMessage node = Message (Just $ getFrom node) Nothing (getBody node)
\end{code}

Найти атрибут jid у XML элемента.

\begin{code}
getJID :: XMPPNode -> JID
getJID (Element _ attrs _) = snd . fromJust . findAttribute (bShow "jid") $ attrs
\end{code}

Получить список контактов.

\begin{code}
getRoster :: [XMPPNode] -> [JID]
getRoster ls = map getJID ls
\end{code}

Преобразовать элемент, содержащий iq-строфу.

\begin{code}
parseIq :: XMPPNode -> Maybe Iq
parseIq (Element _ at [Element name attrs xs]) = case (showB name) of
    "query" ->  case findAttribute (bShow "xmlns") attrs of
        Just (_, t) -> if (showB t) == "jabber:iq:roster"
            then
                case findAttribute (bShow "type") at of
                    Just (_, res) -> case showB res of
                        "result" -> Just . IRoster . getRoster $ xs
                        otherwise -> Nothing
                    Nothing -> Nothing
            else
                Nothing
        Nothing -> Nothing
    "bind" -> Just IBind
    "session" -> Just ISession
    otherwise -> Nothing
parseIq _ = Nothing
\end{code}

Преобразовать XML элемент во внутреннюю строфу.

\begin{code}
elemToXMPP :: XMPPNode -> Maybe InnerStanza
elemToXMPP (OpenTag tag _) = case (showB tag) of
    "stream:stream" -> Just OpenStream
    otherwise -> Nothing
elemToXMPP (CloseTag tag) = case (showB tag) of
    "stream:stream" -> Just CloseStream
    otherwise -> Nothing
elemToXMPP node@(Element name attrs c) = case (showB name) of
    "stream:features" -> Just $ getStreamFeatures c
    "challenge" -> case c of
        [Text text] -> Just . IAuth . IChallenge $ text
        otherwise -> Nothing
    "success" -> case c of
        [Text text] -> Just . IAuth . ISuccess $ text
        otherwise -> Just . IAuth . ISuccess . bShow $ ""
    "message" -> Just . IMsg $ getMessage node
    "iq" -> Iq <$> (parseIq node)
    "presence" -> case findAttribute (bShow "type") attrs of
        Just (_, res) -> case showB res of
            "subscribe" -> IRequest . snd <$> findAttribute (bShow "from") attrs
            "subscribed" -> IConfirm . snd <$> findAttribute (bShow "from") attrs
            "unsubscribed" -> IRefuse . snd <$> findAttribute (bShow "from") attrs
            otherwise -> Nothing
        Nothing -> Just IPresence
    "failure" -> Just Failure
    otherwise -> Nothing
elemToXMPP _ = Nothing
\end{code}

Преобразовать поток XML-элементов в поток внутренних строф.

\begin{code}
toXMPP :: [XMPPNode] -> [InnerStanza]
toXMPP = map fromJust . filter isJust . map elemToXMPP
\end{code}
module Network.XMPPMapping
( TLS (..)
, CompressionMethod (..)
, Compression (..)
, IAuth (..)
, Query (..)
, Iq (..)
, InnerStanza (..)
, toXMPP
)where

import Network.Parser
import Network.XMPPTypes
import Network.Protocol.SASL.GNU
import Control.Applicative
import Data.Maybe
import qualified Data.ByteString.Char8 as C

type XMPPNode = Node BString BString

data TLS = TLS
    deriving (Eq, Show)

data CompressionMethod
    = Zlib
    deriving (Eq, Show)

data Compression = Compression [CompressionMethod]
    deriving (Eq, Show)

data IAuth = IChallenge BString | ISuccess BString
    deriving (Eq, Show)

data Query
    = RosterList [JID]
    deriving (Eq, Show)

data Iq
    = IBind
    | ISession
    | Q Query
    | Unknown1
    | Unknown2
    | Unknown3
    | Unknown4
    deriving (Eq, Show)

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
    | Failure
    deriving (Eq, Show)

--instance Eq InnerStanza where
    
--data StreamFeatures
--    = TLS
--    | 

--login :: String -> String -> String -> String -> IO (Connection, [String])
--login username password hostname server = do
--    h <- connectTo server $ PortNumber portNum

getTLS :: [XMPPNode] -> Maybe TLS
getTLS xs = case findChild (C.pack "starttls") xs of
    Just _ -> Just TLS
    otherwise -> Nothing

getCompression :: [XMPPNode] -> Maybe Compression
getCompression _ = Nothing

getMechanism :: XMPPNode -> Maybe Mechanism
getMechanism (Element name _ [Text text]) = case (showB name) of
    "mechanism" -> Just . Mechanism $ text
    otherwise -> Nothing
getMechanism _ = Nothing

getMechanisms :: [XMPPNode] -> [Mechanism]
getMechanisms xs = case findChild (C.pack "mechanisms") xs of
    Just x -> case x of
        (Element _ _ c) -> map fromJust $ filter isJust $ map getMechanism c
        otherwise -> []
    otherwise -> []

getStreamFeatures :: [XMPPNode] -> InnerStanza
getStreamFeatures xs = StreamFeatures (getTLS xs) (getCompression xs) (getMechanisms xs)

getFrom :: XMPPNode -> BString
getFrom (Element _ attrs _) = C.takeWhile (/= '/') . snd . fromJust . findAttribute (C.pack "from") $ attrs

getBody :: XMPPNode -> BString
getBody (Element _ _ cs) = case findChild (C.pack "body") cs of
    Just (Element _ _ [Text text]) -> text
    otherwise -> C.pack ""

getMessage :: XMPPNode -> Message
getMessage node = Message (Just $ getFrom node) Nothing (getBody node)

getJID :: XMPPNode -> JID
getJID (Element _ attrs _) = snd . fromJust . findAttribute (C.pack "jid") $ attrs

getRoster :: [XMPPNode] -> Query
getRoster ls = RosterList $ map getJID ls

parseIq :: XMPPNode -> Maybe Iq
parseIq (Element _ _ [Element name attrs xs]) = case (showB name) of
    "query" ->  case findAttribute (C.pack "xmlns") attrs of
        Just (_, t) -> if (showB t) == "jabber:iq:roster"
            then
                Just . Q . getRoster $ xs
            else
                Just Unknown1
        Nothing -> Just Unknown2
    "bind" -> Just IBind
    "session" -> Just ISession
    otherwise -> Just Unknown3
parseIq _ = Just Unknown4
--case findChild (C.pack "query") cs of
--    Just (Element _ attrs xs) -> case findAttribute (C.pack "xmlns") attrs of
--        Just (_, t) -> if (showB t) == "jabber:iq:roster"
--            then
--                Just . Q . getRoster $ xs
--            else
--                Nothing
--    otherwise -> Nothing

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
        otherwise -> Just . IAuth . ISuccess . C.pack $ ""
    "message" -> Just . IMsg $ getMessage node
    "iq" -> Iq <$> (parseIq node)
    "presence" -> Just IPresence
    "failure" -> Just Failure
    otherwise -> Nothing
elemToXMPP _ = Nothing

toXMPP :: [XMPPNode] -> [InnerStanza]
toXMPP = map fromJust . filter isJust . map elemToXMPP
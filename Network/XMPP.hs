module Network.XMPP where

import Network
import Network.Parser
import Network.Protocol.SASL.GNU
import Reactive.Banana
import Reactive.Banana.Combinators
import Reactive.Banana.Switch
import Reactive.Banana.Frameworks
import Data.Maybe
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

time :: Int
time = 1000

portNum :: PortNumber
portNum = 5222

type LString = L.ByteString

type BString = B.ByteString

toStrict :: LString -> BString
toStrict = B.concat . L.toChunks

type JID = BString

type XMPPNode = Node LString LString

data Message
    = Message
        { from :: Maybe BString
        , to :: Maybe BString
        , body :: BString
        }

data Stanza
    = Msg Message
    | EndStream

data TLS = TLS

data CompressionMethod
    = Zlib

data Compression = Compression [CompressionMethod]

data Authentication = Challange BString | Success

data Query
    = RosterList [JID]

data Iq
    = Bind
    | Session
    | Query

data InnerStanza
    = OpenStream
    | CloseStream
    | StreamFeatures
        { tls :: Maybe TLS
        , compression :: Maybe Compression
        , mechanisms :: [Mechanism]
        }
    | Auth Authentication
    | Iq Iq
    | IMsg Message

--data StreamFeatures
--    = TLS
--    | 
instance Show Stanza where
    show (Msg (Message f t b)) = "<message" ++ (show f') ++ (show t') ++ "><body>" ++ (show b) ++ "</body></message>"
        where
            f' = case f of
                    Nothing -> ""
                    Just s -> " from=\"" ++ (show s) ++ "\""
            t' = case t of
                    Nothing -> ""
                    Just s -> " to=\"" ++ (show s) ++ "\""
    show EndStream = "</stream:stream>"

data Connection = Connection    { username :: BString
                                , server :: BString
                                , send :: Stanza -> IO () 
                                , receive :: AddHandler Stanza
                                }

--login :: String -> String -> String -> String -> IO (Connection, [String])
--login username password hostname server = do
--    h <- connectTo server $ PortNumber portNum

getTLS :: [XMPPNode] -> Maybe TLS
getTLS xs = case findChild (read "starttls") xs of
    Just _ -> Just TLS
    otherwise -> Nothing

getCompression :: [XMPPNode] -> Maybe Compression
getCompression _ = Nothing

getMechanism :: XMPPNode -> Maybe Mechanism
getMechanism (Element name _ [Text text]) = case (show name) of
    "mechanism" -> Just . Mechanism . toStrict $ text
    otherwise -> Nothing
getMechanism _ = Nothing

getMechanisms :: [XMPPNode] -> [Mechanism]
getMechanisms xs = case findChild (read "mechanisms") xs of
    Just x -> case x of
        (Element _ _ c) -> map fromJust $ filter isJust $ map getMechanism c
        otherwise -> []
    otherwise -> []

getStreamFeatures :: [XMPPNode] -> InnerStanza
getStreamFeatures xs = StreamFeatures (getTLS xs) (getCompression xs) (getMechanisms xs)

elemToXMPP :: XMPPNode -> Maybe InnerStanza
elemToXMPP (OpenTag tag _) = case (show tag) of
    "stream:stream" -> Just OpenStream
    otherwise -> Nothing
elemToXMPP (CloseTag tag) = case (show tag) of
    "stream:stream" -> Just CloseStream
    otherwise -> Nothing
elemToXMPP (Element name attrs c) = case (show name) of
    "stream:features" -> Just $ getStreamFeatures c
    "challenge" -> case c of
        [Text text] -> Just . Auth . Challange . toStrict $ text
        otherwise -> Nothing
    "success" -> Just . Auth . Success
    otherwise -> Nothing
elemToXMPP _ = Nothing
--toXMPP :: [Node tag text] -> [InnerStanza]
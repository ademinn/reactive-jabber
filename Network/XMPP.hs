module Network.XMPP where

import Network.Parser
import Reactive.Banana
import Reactive.Banana.Combinators
import Reactive.Banana.Switch
import Reactive.Banana.Frameworks
import qualified Data.ByteString.Lazy as B

time :: Int
time = 1000

portNum :: PortNumber
portNum = 5222

type BString = B.ByteString

data Stanza = Message   { from :: Maybe BString
                        , to :: Maybe BString
                        , body :: BString
                        }
            | CloseStream
data InnerStanza =

instance Show Stanza where
    show (Message f t b) = "<message" ++ (f') ++ (t') ++ "><body>" ++ b ++ "</body></message>"
        where
            f' = case f of
                    Nothing -> ""
                    Just s -> " from=\"" ++ s ++ "\""
            t' = case t of
                    Nothing -> ""
                    Just s -> " to=\"" ++ s ++ "\""
    show CloseStream = "</stream:stream>"

data Connection = Connection    { username :: BString
                                , server :: BString
                                , send :: Stanza -> IO () 
                                , receive :: AddHandler Stanza
                                }

--login :: String -> String -> String -> String -> IO (Connection, [String])
--login username password hostname server = do
--    h <- connectTo server $ PortNumber portNum

elemToXMPP :: Node BString BString -> Maybe InnerStanza
elemToXMPP
--toXMPP :: [Node tag text] -> [InnerStanza]
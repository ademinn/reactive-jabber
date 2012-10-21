module Network.XMPPTypes
( BString
, LString
, JID
, Message (..)
, toStrict
, showB
) where

import Data.List
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L

type LString = L.ByteString

type BString = B.ByteString

showB :: BString -> String
showB = init . tail . show

toStrict :: LString -> BString
toStrict = B.concat . L.toChunks

type JID = BString

data Message
    = Message
        { from :: Maybe BString
        , to :: Maybe BString
        , body :: BString
        }
    deriving (Eq, Show)
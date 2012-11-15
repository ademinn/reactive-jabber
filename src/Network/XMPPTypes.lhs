%include polycode.fmt

Определение интерфейса модуля.

\begin{code}
module Network.XMPPTypes
( BString
, LString
, BShow (..)
, JID
, Message (..)
, toStrict
, showB
, (+++)
) where
\end{code}

Импортирование модулей.

\begin{code}
import Data.List
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
\end{code}

Вспомогательные определения для работы с ленивыми и строгими ByteString.

\begin{code}
type LString = L.ByteString

type BString = B.ByteString

class BShow a where
    bShow :: a -> BString

instance BShow BString where
    bShow = id

instance BShow String where
    bShow = C.pack

instance (BShow a) => BShow (Maybe a) where
    bShow Nothing = B.empty
    bShow (Just x) = bShow x

showB :: (BShow a) => a -> String
showB = C.unpack . bShow

toStrict :: LString -> BString
toStrict = B.concat . L.toChunks

(+++) :: (BShow a, BShow b) => a -> b -> BString
(+++) a b = (bShow a) `B.append` (bShow b)
\end{code}

Тип Message представляет XMPP-сообщение.

\begin{code}
type JID = BString

data Message
    = Message
        { from :: Maybe BString
        , to :: Maybe BString
        , body :: BString
        }
    deriving (Eq, Show)

instance BShow Message where
    bShow (Message f t b) = "<message" +++ f' +++ t' +++ "><body>" +++ b +++ "</body></message>"
        where
            f' = bShow' "from" f
            t' = bShow' "to" t
            bShow' tag val = case val of
                Nothing -> B.empty
                Just v -> tag +++ "='" +++ v +++ "'"
    
--    show (Msg (Message f t b)) = "<message" ++ f' ++ t' ++ "><body>" ++ (showB b) ++ "</body></message>"
--        where
--            f' = case f of
--                    Nothing -> ""
--                    Just s -> " from='" ++ (showB s) ++ "'"
--            t' = case t of
--                    Nothing -> ""
--                    Just s -> " to='" ++ (showB s) ++ "'"

\end{code}
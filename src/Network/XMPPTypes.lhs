%include mystyle.fmt

\subsubsection{Network.XMPPTypes}

Модуль @Network.XMPPTypes@ содержит описание типов данных,
использующихся внутри библиотеки, а также функций для работы с этими типами данных.


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
, bShowAttr
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
\end{code}

Преобразование ленивых ByteString в строгие.

\begin{code}
toStrict :: LString -> BString
toStrict = B.concat . L.toChunks
\end{code}

Конкатенация двух @BShow@-структур.

\begin{code}
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

bShowAttr :: String -> Maybe BString -> BString
bShowAttr _ Nothing = B.empty
bShowAttr a (Just b) = (" " ++ a ++ "='") +++ b +++ "'"

instance BShow Message where
    bShow (Message f t b) = "<message"
        +++ (bShowAttr "from" f)
        +++ (bShowAttr "to" t)
        +++ "><body>" +++ b
        +++ "</body></message>"
\end{code}
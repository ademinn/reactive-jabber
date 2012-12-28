%include mystyle.fmt

\subsubsection{Network.Parser}

Модуль @Network.Parser@ используется для преобразования потока SAXEvent'ов в поток элементов XML.
В данном модуле определяется структура данных @Node@, представляющая элемент XML,
функции для поиска детей и значений атрибутов в @Node@, а также функция, преобразующая
поток @SAXEvent@ в поток @Node@.

\begin{code}
module Network.Parser
( Node (..)
, parseSAXStream
, findChild
, findAttribute
) where
\end{code}

Используется сторонняя библиотека Hexpat~\cite{hexpat}.

\begin{code}
import Text.XML.Expat.SAX
import Control.Monad.State.Lazy
import Control.Applicative
import qualified Data.ByteString.Lazy as L
\end{code}

Тип Node представляет элемент XML-потока.

\begin{code}
data Node tag text
    = Element
        { eName :: tag
        , eAttributes :: [(tag, text)]
        , eChildren :: [Node tag text]
        }
    | Text text
    | OpenTag tag [(tag, text)]
    | CloseTag tag
    | End

instance (Show tag, Show text) => Show (Node tag text) where
    show (Element n a c) = (show n) ++ ": " ++ (show a) ++ "| " ++ (show c)
    show (Text text) = show text
    show (OpenTag n a) = (show n) ++ ": " ++ (show a)
    show (CloseTag n) = show n
    show End = "End"
\end{code}

Вспомогательная функция для работы со списками.

\begin{code}
headSafe :: [a] -> Maybe a
headSafe [] = Nothing
headSafe (x:_) = Just x
\end{code}

Далее следуют функции для работы с XML-элементами.

Получить тэг элемента.
\begin{code}
getTag :: Node tag text -> Maybe tag
getTag (Element tag _ _) = Just tag
getTag (OpenTag tag _) = Just tag
getTag (CloseTag tag) = Just tag
getTag (Text _) = Nothing
getTag End = Nothing
\end{code}

Проверить, что элемент имеет заданное имя.

\begin{code}
cmpTagNode :: (Eq tag) => tag -> Node tag text -> Bool
cmpTagNode t1 node = case (getTag node) of
    Just t2 -> t1 == t2
    Nothing -> False
\end{code}

Найти вложенный элемент с заданным именем.

\begin{code}
findChild :: (Eq tag) => tag -> [Node tag text] -> Maybe (Node tag text)
findChild t xs = headSafe $ dropWhile (not . cmpTagNode t) xs
\end{code}

Найти атрибут с заданным именем.

\begin{code}
findAttribute :: (Eq tag) => tag -> [(tag, text)] -> Maybe (tag, text)
findAttribute t xs = headSafe $ dropWhile ((t /=) . fst) xs
\end{code}

Для преобразования потока SAXEvent'ов в поток @Node@ используется State-монада.
Состоянием является пара из входного потока и потока SAXEvent'ов с их положением.
Это сделано для возможности устранения ошибок во входном потоке.

\begin{code}
type ParseStateM tag text a = State (L.ByteString, [(SAXEvent tag text, XMLParseLocation)]) a
\end{code}

Получить следующий элемент и удалить его из потока.

\begin{code}
pollNext :: ParseStateM tag text (Maybe (SAXEvent tag text))
pollNext = state $ \(src, s) -> case s of
                                    [] -> (Nothing, (src, []))
                                    (x, XMLParseLocation _ _ _ c):xs -> (Just x, (L.drop c src, xs))
\end{code}

Получить следующий элемент, не удаляя его из потока.

\begin{code}
peekNext :: ParseStateM tag text (Maybe (SAXEvent tag text))
peekNext = state $ \(src, s) -> case s of
                            [] -> (Nothing, (src, []))
                            (x, _):xs -> (Just x, (src, s))
\end{code}

Преобразовать входной поток в поток @Node@.
Элементы, встречающиеся в передаваемом списке, стоит представлять в виде @OpenTag@ / @CloseTag@, а их детей помещать в основной поток.
Это сделано для более удобной обработки элементов stream:stream, внутри которых происходит все общение клиента и сервера.

\begin{code}
parseSAXStream :: (Eq tag, GenericXMLString text, GenericXMLString tag)
    => [tag]
    -> L.ByteString
    -> [Node tag text]
parseSAXStream ls stream = evalState (parseState ls) (stream, loc)
    where loc = parseLocations (ParseOptions Nothing Nothing) stream

\end{code}

Разборщик SAXEvent'ов. Так как XMPP-поток не полностью соответсвует XML-спецификации (перед открытием внутреннего потока посылается XML-заголовок),
то при получении FailDocument элемент отбрасывается, а в состояние State-монады помещается новый поток SAXEvent'ов, который создается из остатка входного потока.

\begin{code}
parseState :: (Eq tag, GenericXMLString text, GenericXMLString tag)
    => [tag]
    -> ParseStateM tag text [Node tag text]
parseState ls = do
    el <- parseElement ls
    case el of
        End -> return [el]
        otherwise -> (el:) <$> parseState ls
\end{code}

Отбросить текущий элемент. Оставшийся входной поток заново разобрать при помощи Hexpat.

\begin{code}
backupState :: (GenericXMLString text, GenericXMLString tag)
    => ParseStateM tag text ()
backupState = state $ \(src, _) -> ((), (src, parseLocations (ParseOptions Nothing Nothing) src))
\end{code}

Получить все дочерние элементы текущего элемента.

\begin{code}
getChildren :: (Eq tag, GenericXMLString text, GenericXMLString tag)
    => tag
    -> [tag]
    -> ParseStateM tag text [Node tag text]
getChildren p ls = do
    el <- peekNext
    case el of
        Just x -> do
            case x of
                EndElement p -> do
                    pollNext
                    return []
                otherwise -> (:) <$> parseElement ls <*> getChildren p ls
\end{code}

Разобрать текущий элемент.
Если элемент содержится в передаваемом списке, то необходимо добавить в выходной поток @OpenTag@, соответствующий текущему элементу,
затем добавить в выходной поток все его дочерние элементы, после чего добавить @CloseTag@.
Если элемент представляет из себя текстовые данные, то необходимо добавить соответвующий элемент в выходной поток.
Если найдена ошибка, то надо пропустить этот элемент и продолжить разбор.

\begin{code}
parseElement :: (Eq tag, GenericXMLString text, GenericXMLString tag)
    => [tag]
    -> ParseStateM tag text (Node tag text)
parseElement ls = do
    el <- pollNext
    case el of
        Nothing -> return End
        Just x -> do
            case x of
                StartElement name attrs -> if name `elem` ls
                    then do
                        return $ OpenTag name attrs
                    else do
                        ch <- getChildren name ls
                        return $ Element name attrs ch
                EndElement name -> case name `elem` ls of
                    True -> do
                        return $ CloseTag name
                CharacterData t -> do
                    return $ Text t
                FailDocument _ -> do
                    backupState
                    parseElement ls
                otherwise -> parseElement ls
\end{code}
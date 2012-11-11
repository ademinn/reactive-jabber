module Network.Parser
( Node (..)
, parseSAXStream
, findChild
, findAttribute
) where

--import qualified Data.ByteString.Lazy as B
import Text.XML.Expat.SAX
import Control.Monad.State.Lazy
import Control.Applicative
import qualified Data.ByteString.Lazy as L

type LString = L.ByteString

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

type ParseStateM tag text a = State (LString, [(SAXEvent tag text, XMLParseLocation)]) a --[Node tag text]

headSafe :: [a] -> Maybe a
headSafe [] = Nothing
headSafe (x:_) = Just x

getTag :: Node tag text -> Maybe tag
getTag (Element tag _ _) = Just tag
getTag (OpenTag tag _) = Just tag
getTag (CloseTag tag) = Just tag
getTag (Text _) = Nothing
getTag End = Nothing

cmpTagNode :: (Eq tag) => tag -> Node tag text -> Bool
cmpTagNode t1 node = case (getTag node) of
    Just t2 -> t1 == t2
    Nothing -> False

findChild :: (Eq tag) => tag -> [Node tag text] -> Maybe (Node tag text)
findChild t xs = headSafe $ dropWhile (not . cmpTagNode t) xs

findAttribute :: (Eq tag) => tag -> [(tag, text)] -> Maybe (tag, text)
findAttribute t xs = headSafe $ dropWhile ((t /=) . fst) xs

pollNext :: ParseStateM tag text (Maybe (SAXEvent tag text))
pollNext = state $ \(src, s) -> case s of
                                    [] -> (Nothing, (src, []))
                                    (x, XMLParseLocation _ _ _ c):xs -> (Just x, (L.drop c src, xs))

peekNext :: ParseStateM tag text (Maybe (SAXEvent tag text))
peekNext = state $ \(src, s) -> case s of
                            [] -> (Nothing, (src, []))
                            (x, _):xs -> (Just x, (src, s))

--parseXMLByteString :: (Eq tag) => [tag] -> LString -> [Node tag text]
--parseXMLByteString


parseSAXStream :: (Eq tag, GenericXMLString text, GenericXMLString tag) => [tag] -> LString -> [Node tag text]
parseSAXStream ls stream = evalState (parseState ls) (stream, parseLocations (ParseOptions Nothing Nothing) stream)

parseState :: (Eq tag, GenericXMLString text, GenericXMLString tag) => [tag] -> ParseStateM tag text [Node tag text]
parseState ls = do
    el <- parseElement ls
    case el of
        End -> return [el]
        otherwise -> (el:) <$> parseState ls

getChildren :: (Eq tag, GenericXMLString text, GenericXMLString tag) => tag -> [tag] -> ParseStateM tag text [Node tag text]
getChildren p ls = do
    el <- peekNext
    case el of
        Just x -> do
            case x of
                EndElement p -> do
                    pollNext
                    return []
                otherwise -> (:) <$> parseElement ls <*> getChildren p ls

backupState :: (GenericXMLString text, GenericXMLString tag) => ParseStateM tag text ()
backupState = state $ \(src, _) -> ((), (src, parseLocations (ParseOptions Nothing Nothing) src))

parseElement :: (Eq tag, GenericXMLString text, GenericXMLString tag) => [tag] -> ParseStateM tag text (Node tag text)
parseElement ls = do
    el <- pollNext
    case el of
        Nothing -> return End
        Just x -> do
            case x of
                StartElement name attrs -> if name `elem` ls
                    then do
                        return $ OpenTag name attrs --Element name attrs []
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

--    s <- get
--    case s of
--        [] -> return End
--        x:xs -> do
--            case x of
--                 StartElement n a -> if n `elem` ls then return Element n a [] else do
                    
--            let xName = 
--            if x `elem` ls
--                    then
                        


--parse' :: [SAXEvent String String] -> [String]
--parse' = evalState parse''

--parse'' :: State [SAXEvent String String] [String]
--parse'' = do
--    el <- parseTag
--    (:) <$> return el <*> parse''

--parseTag :: State [SAXEvent String String] String
--parseTag = state $ \s -> case (s !! 0) of
                            --(StartElement "a" []) -> ("ok", tail s)
                            --x -> (show x, tail s)
--                            (StartElement "a" []) -> parseA s
--                            x -> (show x, tail s)
                            --[] -> ("end", [])

--parseA :: [SAXEvent String String] -> (String, [SAXEvent String String])
--parseA ((StartElement "a" []):(CharacterData d):(EndElement "a"):els) = (d, els)
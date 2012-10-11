module Network.Parser
( Node (..)
, parseSAXStream
) where

--import qualified Data.ByteString.Lazy as B
import Text.XML.Expat.SAX
import Control.Monad.State.Lazy
import Control.Applicative

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

type ParseStateM tag text a = State [SAXEvent tag text] a --[Node tag text]

pollNext :: ParseStateM tag text (Maybe (SAXEvent tag text))
pollNext = state $ \s -> case s of
                            [] -> (Nothing, [])
                            x:xs -> (Just x, xs)

peekNext :: ParseStateM tag text (Maybe (SAXEvent tag text))
peekNext = state $ \s -> case s of
                            [] -> (Nothing, [])
                            x:xs -> (Just x, xs)

parseSAXStream :: (Eq tag) => [tag] -> [SAXEvent tag text] -> [Node tag text]
parseSAXStream ls events = evalState (parseState ls) events

parseState :: (Eq tag) => [tag] -> ParseStateM tag text [Node tag text]
parseState ls = do
    el <- parseElement ls
    case el of
        End -> return [el]
        otherwise -> (el:) <$> parseState ls

getChildren :: (Eq tag) => tag -> [tag] -> ParseStateM tag text [Node tag text]
getChildren p ls = do
    el <- peekNext
    case el of
        Just x -> do
            case x of
                EndElement p -> do
                    pollNext
                    return []
                otherwise -> (:) <$> parseElement ls <*> getChildren p ls

parseElement :: (Eq tag) => [tag] -> ParseStateM tag text (Node tag text)
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
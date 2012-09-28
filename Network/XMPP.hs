module Network.XMPP
( Stanza (Message, CloseStream)
, Connection
, login
, from
, to
, body
, username
, server
, send
, receive
) where

import Text.XML.HaXml.Types
import Text.XML.HaXml.SAX
import Network
import Network.Protocol.SASL.GNU
import System.IO
import Data.List (isInfixOf)
import Data.ByteString.UTF8
import Control.Monad.IO.Class

time :: Int
time = 1000

portNum :: PortNumber
portNum = 5222

data Stanza = Message   { from :: Maybe String
                        , to :: Maybe String
                        , body :: String
                        }
            | CloseStream

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

data Connection = Connection    { username :: String
                                , server :: String
                                , send :: Stanza -> IO () 
                                , receive :: IO Stanza
                                }

hSendStanza :: Handle -> Stanza -> IO ()
hSendStanza h st = hSend h $ show st

hReceiveStanza :: Handle -> IO Stanza
hReceiveStanza h = do
    input <- hGetInput h
    let (els, _) = saxParse "" input
    ans <- parseMsgStream els
    case ans of
        Just st -> do
            case st of
                CloseStream -> hClose h
                otherwise -> return ()
            return st
        Nothing -> hReceiveStanza h

getAttr :: String -> [Attribute] -> String
getAttr at (x:xs) = case x of
    (N at, AttValue [Left s]) -> s
    otherwise -> getAttr at xs

parseBody :: [SaxElement] -> String -> IO (Maybe Stanza)
parseBody (x:xs) from = do
    case x of
        (SaxCharData s) -> do
            let from' = takeWhile (/= '/') from
            return $ Just $ Message (Just from') Nothing s
        (SaxElementClose "body") -> return Nothing
        otherwise -> parseBody xs from

parseMsg :: [SaxElement] -> String -> IO (Maybe Stanza)
parseMsg (x:xs) from = do
    case x of
        (SaxElementOpen "body" _) -> parseBody xs from
        (SaxElementClose "message") -> return Nothing
        otherwise -> parseMsg xs from

parseMsgStream :: [SaxElement] -> IO (Maybe Stanza)
parseMsgStream [] = return Nothing
parseMsgStream (x:xs) = do
    case x of
        (SaxElementOpen "message" attrs) -> parseMsg xs (getAttr "from" attrs)
        (SaxElementClose "stream:stream") -> return $ Just CloseStream
        otherwise -> parseMsgStream xs

data Result = Success | Failure

hWaitForever :: Handle -> IO ()
hWaitForever h = do
    flag <- hWaitForInput h time
    if flag then return () else hWaitForever h

hGetInput' :: Handle -> String -> IO String
hGetInput' h acc = do
    flag <- hReady h
    if flag
        then do
            c <- hGetChar h
            hGetInput' h $ acc ++ [c]
        else
            return acc

hGetInput :: Handle -> IO String
hGetInput h = do
    hWaitForever h
    s <- hGetInput' h ""
    putStrLn $ "<- " ++ s
    return s

hSend :: Handle -> String -> IO ()
hSend h s = do
    putStrLn $ "-> " ++ s
    hPutStr h s
    hFlush h

getHead' :: Handle -> String -> IO String
getHead' h acc = do
    s <- hGetInput h
    let acc' = acc ++ s
    if (isInfixOf "</stream:features>" s) then return acc' else getHead' h acc'

getHead :: Handle -> IO String
getHead h = getHead' h ""

getMech :: [SaxElement] -> [String]
getMech els = case els of
    (SaxElementOpen "mechanism" _ : SaxCharData d : SaxElementClose "mechanism" : els') -> d : getMech els'
    (_ : els') -> getMech els'
    [] -> []

finishLogin :: Handle -> Session Result
finishLogin h = do
    s <- liftIO $ hGetInput h
    if (s == "<success xmlns='urn:ietf:params:xml:ns:xmpp-sasl'/>") then return Success else return Failure

parseLogin :: String -> Maybe String
parseLogin s = let (els, _) = saxParse "" s in
    case els of
        ([SaxElementOpen "challenge" _ , SaxCharData d , SaxElementClose "challenge"]) -> Just d
        otherwise -> Nothing

loopLogin :: Handle -> Session Result
loopLogin h = do
    ans <- liftIO $ hGetInput h
    let ans' = parseLogin ans
    case ans' of
        Just s -> do
            (text, pr) <- step64 $ fromString s
            let out = "<response xmlns='urn:ietf:params:xml:ns:xmpp-sasl'>" ++ (toString text) ++ "</response>"
            liftIO $ hSend h out
            case pr of
                Complete -> finishLogin h
                NeedsMore -> loopLogin h
        Nothing -> return Failure

containsJID :: [Attribute] -> Bool
containsJID [] = False
containsJID (x:xs) = case x of
    (N "jid", _) -> True
    otherwise -> containsJID xs

isJIDItem :: SaxElement -> Bool
isJIDItem el = case el of
    (SaxElementOpen "item" attrs) -> containsJID attrs
    (SaxElementTag "item" attrs) -> containsJID attrs
    otherwise -> False

getJID' :: [Attribute] -> String
getJID' (x:xs) = case x of
    (N "jid", AttValue [Left s]) -> s
    otherwise -> getJID' xs

getJID :: SaxElement -> String
getJID el = case el of
    (SaxElementOpen "item" attrs) -> getJID' attrs
    (SaxElementTag "item" attrs) -> getJID' attrs

getJIDs :: [SaxElement] -> [String]
getJIDs l = map getJID $ filter isJIDItem l

getRoster' :: Handle -> String -> IO String
getRoster' h acc = do
    s <- hGetInput h
    let acc' = acc ++ s
    if (isInfixOf "<query xmlns='jabber:iq:roster'>" acc') then return acc' else getRoster' h acc'

getRoster :: Handle -> IO [String]
getRoster h = do
    hSend h "<iq type = \"get\"><query xmlns=\"jabber:iq:roster\"/></iq>"
    s <- getRoster' h ""
    let (ans, _) = saxParse "" s
    return $ getJIDs ans

logout :: Handle -> IO ()
logout h = hSend h "</stream:stream>"

login :: String -> String -> String -> String -> IO (Connection, [String])
login username password hostname server = do
    putStrLn $ "username: " ++ username ++ "\nserver: " ++ server
    h <- connectTo server $ PortNumber portNum
    hSend h "<stream:stream to=\"jabber.ru\" xmlns=\"jabber:client\" xmlns:stream=\"http://etherx.jabber.org/streams\" version=\"1.0\">"
    hd <- getHead h
    let (els, _) = saxParse "" hd
        mechs = filter (/= "SCRAM-SHA-1") (getMech els)
    runSASL $ do
        mmech <- clientSuggestMechanism $ map (Mechanism . fromString) mechs
        case mmech of
            Just m -> runClient m $ do
                let mechName = toString m' where Mechanism m' = m
                setProperty PropertyAuthID $ fromString username
                setProperty PropertyPassword $ fromString password
                setProperty PropertyService $ fromString "xmpp"
                setProperty PropertyHostname $ fromString hostname
                (text, pr) <- step64 $ fromString ""
                let out = "<auth xmlns='urn:ietf:params:xml:ns:xmpp-sasl' mechanism='" ++ mechName ++ "'>" ++ (toString text) ++ "</auth>"
                liftIO $ hSend h out
                case pr of
                    Complete -> finishLogin h
                    NeedsMore -> loopLogin h
    hSend h "<stream:stream to=\"jabber.ru\" xmlns=\"jabber:client\" xmlns:stream=\"http://etherx.jabber.org/streams\" version=\"1.0\">"
    hGetInput h
    hSend h "<iq type='set' id='bund_2'><bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'><resource>test</resource></bind></iq>"
    hGetInput h
    hSend h "<iq type=\"set\" id=\"9747\"><session xmlns=\"urn:ietf:params:xml:ns:xmpp-session\" /></iq>"
    hGetInput h
    roster <- getRoster h
    hSend h "<presence><show/></presence>"
    return (Connection username server (hSendStanza h) (hReceiveStanza h), roster)
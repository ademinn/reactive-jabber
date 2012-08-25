import System.IO
import Network
import Network.Protocol.SASL.GNU
import Text.XML.HaXml.SAX
import Data.List
import Data.ByteString.UTF8
import Control.Monad.IO.Class

time :: Int
time = 1000

portNum :: PortNumber
portNum = 5222

data Result = Success | Failure

hWaitForever :: Handle -> IO ()
hWaitForever h = do
  flag <- hWaitForInput h time
  if flag then return () else hWaitForever h

hGetInput' :: Handle -> String -> IO String
hGetInput' h acc = do
  flag <- hReady h
  if flag then do
    c <- hGetChar h
    hGetInput' h $ acc ++ [c]
  else return acc

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
  if (isInfixOf "</stream::features>" s) then return acc' else getHead' h acc'
  
getHead :: Handle -> IO String
getHead h = do
  getHead' h ""
  
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
  

login :: String -> String -> String -> String -> IO Handle
login username password hostname server = do
  h <- connectTo server $ PortNumber portNum
  hSend h "<stream:stream to=\"jabber.ru\" xmlns=\"jabber:client\" xmlns:stream=\"http://etherx.jabber.org/streams\""
  hd <- getHead h
  let (els, _) = saxParse "" hd
      mechs = filter (/= "SCRAM-SHA-1") (getMech els)
  runSASL $ do
    mmech <- clientSuggestMechanism $ map (Mechanism . fromString) mechs
    case mmech of
      --Nothing -> return "err"
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
        --return "ok"
    --return "ok"  
  hSend h "<stream:stream to=\"jabber.ru\" xmlns=\"jabber:client\" xmlns:stream=\"http://etherx.jabber.org/streams\""
  hGetInput h
  hSend h "<iq type='set' id='bund_2'><bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'><resource>test</resource></bind></iq>"
  hGetInput h
  hSend h "<iq type=\"set\" id=\"9747\"><session xmlns=\"urn:ietf:params:xml:ns:xmpp-session\" /></iq>"
  hGetInput h
  return h
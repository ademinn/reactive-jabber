import System.IO
import Control.Monad (forever)
import Network
import Network.Protocol.SASL.GNU
import Text.XML.HaXml.SAX
import Data.List
import Data.ByteString.UTF8
import Control.Monad.IO.Class
import Reactive.Banana
import Reactive.Banana.WX
import Graphics.UI.WX hiding (Event)
import Graphics.UI.WX.Controls
import Graphics.UI.WX.Classes
import Control.Concurrent
import Graphics.UI.WXCore.WxcDefs
import Graphics.UI.WX.Dialogs
import Graphics.UI.WXCore.WxcClassesMZ

--type MyList = Composite (SingleListBox ()) ()

data Stanza = Message
              { to :: String ,
                from :: String,
                msg :: String
              }
            | Roster
              { list :: [String]
              }
            | IQ
              { msg :: String
              }
              
              
data Chat a = Chat 
              { chatPanel :: Panel a,
                history :: TextCtrl a,
                inputMsg :: TextCtrl a
              }

instance Widget (Chat a) where
  widget w = widget . chatPanel $ w --column 5 $ [widget . history $ w, widget . inputMsg $ w]

chat :: Window a -> IO (Chat ())
chat wnd = do
  p <- panel wnd []
  hist <- textCtrl p []
  inMsg <- textCtrl p []
  set p [ layout := fill . column 5 $ [widget hist, widget inMsg]]
  return $ Chat p hist inMsg

main = start $ do
  f <- frame [ text := "reactive-jabber" ]
  listBox <- singleListBox f []
  set f [ layout := fill . widget $ listBox ]
  d <- dialog f [ text := "Login" ]
  usernameEntry <- entry d []
  passwordEntry <- entry d []
  realmEntry <- entry d []
  serverEntry <- entry d []
  ok <- button d  [text := "Ok" ]
  set d [ layout := margin 5 $
          grid 5 5 [[label "Username", widget usernameEntry],
                    [label "Password", widget passwordEntry],
                    [label "Realm", widget realmEntry],
                    [label "Server", widget serverEntry],
                    [widget ok]]]
  
  (inputStream, fireInput) <- newAddHandler
  itemAppend listBox "temp"
  ch <- frame [ text := "chat" ]
  nb <- notebook ch []
  t1 <- chat nb --panel nb [ text := "tab 1" ]
  notebookAddPage nb (chatPanel t1) "Tab" True (-1)
  --t2 <- panel nb [ text := "tab 2" ]
  --set ch [ layout := fill $ tabs nb [tab "tab1" (container t1 $ column 5 $ [label $ "label"])] ]
--  result <- showModal d (\stop -> set ok [ on command := stop (Just ())])
--  if result == Just () then do 
--    username <- get usernameEntry text
--    password <- get passwordEntry text
--    realm <- get realmEntry text
--    server <- get serverEntry text
--    h <- login username password realm server
--    return ()
--  else return ()
--  forkIO $ do
--    forever $ (hGetInput h) >>= fireInput
  return ()

--createChat :: String -> IO Layout


setChat :: Notebook a -> String -> IO Int
setChat nb name = do
  names <- chatList nb
  let mindex = elemIndex name names
  case mindex of
    Just index -> notebookSetSelection nb index
    Nothing -> return (-1)
      
    
  
chatList :: Notebook a -> IO [String]
chatList nb = do
  cnt <- notebookGetPageCount nb
  chatList' nb cnt
  
chatList' :: Notebook a -> Int -> IO [String]
chatList' nb cnt = do
  let cnt' = cnt - 1
  if (cnt' >= 0) then do
    s <- notebookGetPageText nb cnt'
    s' <- chatList' nb cnt'
    return $ s' ++ [s]
  else return []
  
recvMsgLoop :: Handle -> (Stanza -> IO ()) -> ([SaxElement], String) -> IO ()
recvMsgLoop h fire (els, s) = do
  input <- hGetInput h
  let (els', ms') = saxParse "" $ s ++ input
      s' = case ms' of
        Just js -> js
        Nothing -> ""
  
  return ()

--parseMsgStream :: [SaxElement] -> ([Stanza], [SaxElement])
--parseMsgStream

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
  if (isInfixOf "</stream:features>" s) then return acc' else getHead' h acc'
  
getHead :: Handle -> IO String
getHead h = getHead' h ""
--  s1 <- hGetInput h
--  putStrLn s1
--  s2 <- hGetInput h
--  putStrLn s2
--  return $ s1 ++ s2
  
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
  hSend h "<stream:stream to=\"jabber.ru\" xmlns=\"jabber:client\" xmlns:stream=\"http://etherx.jabber.org/streams\" version=\"1.0\">"
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
  hSend h "<stream:stream to=\"jabber.ru\" xmlns=\"jabber:client\" xmlns:stream=\"http://etherx.jabber.org/streams\" version=\"1.0\">"
  hGetInput h
  hSend h "<iq type='set' id='bund_2'><bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'><resource>test</resource></bind></iq>"
  hGetInput h
  hSend h "<iq type=\"set\" id=\"9747\"><session xmlns=\"urn:ietf:params:xml:ns:xmpp-session\" /></iq>"
  hGetInput h
  hSend h "</stream:stream>"
  return h
  
--main = do
  
--  login "" "" "jabber.ru" "jabber.ru"
--  return ()
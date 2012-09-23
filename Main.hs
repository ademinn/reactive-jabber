import Reactive.Banana
import Reactive.Banana.Combinators
import Reactive.Banana.Switch
import Reactive.Banana.Frameworks
import System.IO
import System.Glib.Types
import Text.XML.HaXml.SAX
import Text.XML.HaXml.Types
import Network
import Network.Protocol.SASL.GNU
import Data.List hiding (union)
import Data.Maybe (listToMaybe)
import Data.Traversable (sequenceA)
import Data.ByteString.UTF8
import qualified Data.Map as Map
import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Control.Monad.IO.Class
import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as Model


--type MyList = Composite (SingleListBox ()) ()
--  passwordEntry <- textCtrlEx d wxTE_PASSWORD []

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


data Chat = Chat
            {   userName :: String,
                chatPanel :: VPaned,
                addMsg :: String -> IO ()
            }

type ChatRequest = (String, Maybe String)

chatNew :: String -> ((String, String) -> IO ()) -> IO Chat
chatNew s fire = do
    outputWdgt <- textViewNew
    tb <- textViewGetBuffer outputWdgt
    textViewSetEditable outputWdgt False
    inputWdgt <- textViewNew
    itb <- textViewGetBuffer inputWdgt
    inputWdgt `on` keyPressEvent $ do
        s' <- eventKeyName
        if s' == "Return"
            then do
                stIt <- liftIO $ startBuf itb
                enIt <- liftIO $ endBuf itb
                out <- liftIO $ textBufferGetText itb stIt enIt False
                --liftIO $ putStrLn out
                liftIO $ fire (s, out)
                liftIO $ textBufferSetText itb ""
                return True
            else return False
    mainWdgt <- vPanedNew
    panedAdd1 mainWdgt outputWdgt
    panedAdd2 mainWdgt inputWdgt
    return $ Chat s mainWdgt (appendText tb)

startBuf :: TextBuffer -> IO TextIter
startBuf tb = textBufferGetIterAtOffset tb 0

endBuf :: TextBuffer -> IO TextIter
endBuf tb = textBufferGetIterAtOffset tb (-1)

appendText :: TextBuffer -> String -> IO ()
appendText tb msg = do
    --line <- textBufferGetLineCount tb
    ti <- endBuf tb
    textBufferInsert tb ti (msg ++ "\n")

instance GObjectClass Chat where
    toGObject = toGObject . chatPanel
    unsafeCastGObject = undefined --Chat undefined undefined --(unsafeCastGObject o)

instance ObjectClass Chat
instance WidgetClass Chat

main :: IO ()
main = do
    hSetEncoding stdout utf8
    initGUI
    loginDialog <- dialogNew
    upper <- dialogGetUpper loginDialog
    t <- tableNew 3 2 False
    loginLab <- labelNew $ Just "login"
    loginEntry <- entryNew
    tableAttachDefaults t loginLab 0 1 0 1
    tableAttachDefaults t loginEntry 1 2 0 1
    passwordLab <- labelNew $ Just "password"
    passwordEntry <- entryNew
    entrySetVisibility passwordEntry False
    tableAttachDefaults t passwordLab 0 1 1 2
    tableAttachDefaults t passwordEntry 1 2 1 2
    serverLab <- labelNew $ Just "server"
    serverEntry <- entryNew
    tableAttachDefaults t serverLab 0 1 2 3
    tableAttachDefaults t serverEntry 1 2 2 3
    boxPackStart upper t PackGrow 5
    widgetShowAll upper
    dialogAddButton loginDialog "Cancel" ResponseCancel
    dialogAddButton loginDialog "Ok" ResponseOk
    loginDialog `on` response $ \res -> do
        putStrLn . show $ res
        case res of
            ResponseOk -> do
                loginText <- entryGetText loginEntry
                passwordText <- entryGetText passwordEntry
                serverText <- entryGetText serverEntry
                widgetDestroy loginDialog
                (h, roster) <- login loginText passwordText serverText serverText
                mainLoop loginText h roster
            otherwise -> do
                widgetDestroy loginDialog
                mainQuit
    widgetShowAll loginDialog
    mainGUI
    return ()

quitChat :: Handle -> IO ()
quitChat h = do
    logout h
    hClose h
    mainQuit

mainLoop :: String -> Handle -> [String] -> IO ()
mainLoop name h roster = do
    window <- windowNew
    chWindow <- windowNew
    chats <- notebookNew
    set chWindow    [ containerChild := chats
                    , windowDefaultWidth := 200
                    , windowDefaultHeight := 100
                    ]
    (handler, fireHandler) <- newAddHandler
    (msgHandler, fireMsgHandler) <- newAddHandler
    (sendHandler, fireSendHandler) <- newAddHandler
    treeview <- listTreeView name roster fireHandler
    set window  [ windowDefaultWidth := 100
                , windowDefaultHeight := 200
                , containerChild := treeview
                ]
    --onDestroy window mainQuit
    window `on` deleteEvent $ do
        liftIO $ widgetDestroy chWindow
        liftIO $ putStrLn "exit"
        liftIO $ quitChat h
        return False
    
    chWindow `on` deleteEvent $ do
        liftIO $ widgetHideAll chWindow
        return True

    let networkDescription :: Frameworks t => Moment t ()
        networkDescription = do
            (eAdd, fireAdd) <- newEvent
            eHandler <- fromAddHandler handler
            eMsg <- fromAddHandler msgHandler
            eSend <- fromAddHandler sendHandler
            let
                sendTo :: (String, String) -> IO ()
                sendTo (to, msg) = do
                    sendMsg h to msg
                    fireMsgHandler (to, Just (name ++ ": " ++ msg))
                eMap = accumE Map.empty $ insertSafe <$> eAdd
                bMap = stepper Map.empty eMap
                addChat :: (ChatRequest, Map.Map String Chat) -> IO ()
                addChat ((from, msg), m) = do
                    chat <- case Map.lookup from m of
                                Just v -> return v
                                Nothing -> do
                                    c <- chatNew from fireSendHandler
                                    fireAdd (from, c)
                                    return c
                    case msg of
                        Just s -> do
                            addMsg chat s
                        Nothing -> do
                            ret <- notebookGetNPages chats
                            vis <- get chats widgetVisible
                            if or [(ret == 0), (not vis)]
                                then widgetShowAll chWindow
                                else return ()
                            ind <- notebookPageNum chats chat
                            case ind of
                                Just i -> notebookSetCurrentPage chats i
                                Nothing -> do
                                    i <- notebookAppendPage chats chat from
                                    widgetShowAll chat
                                    notebookSetCurrentPage chats i
                    --return ()
            reactimate $ (putStrLn <$> eHandler) `union` (sendTo <$> eSend) `union` (addChat <$> ((rTuple <$> bMap) <@> (((,Nothing) <$> eHandler) `union` eMsg)))
            --return ()
    network <- compile networkDescription
    actuate network
    widgetShowAll window
    forkIO $ do
        recvMsgLoop h fireMsgHandler
--    mainGUI
    return ()

sendMsg :: Handle -> String -> String -> IO ()
sendMsg h to msg = do
    hSend h $ "<message to='" ++ to ++ "'><body>" ++ msg ++ "</body></message>"

tmpPrint :: ChatRequest -> IO ()
tmpPrint (s, ms) = case ms of
    Nothing -> return ()
    Just msg -> putStrLn $ s ++ ": " ++ msg

rTuple :: a -> b -> (b, a)
rTuple x y = (y, x)

insertSafe :: Ord k => (k, a) -> Map.Map k a -> Map.Map k a
insertSafe (k, v) m = if k `Map.member` m then m else Map.insert k v m

insertSafe' :: Ord k => k -> Map.Map k k -> Map.Map k k
insertSafe' k m = insertSafe (k, k) m

listTreeView :: String -> [String] -> (String -> IO ()) -> IO TreeView
listTreeView title sourceList fireAction = do
    list <- listStoreNew sourceList
    treeview <- Model.treeViewNewWithModel list
    Model.treeViewSetHeadersVisible treeview True
    col <- Model.treeViewColumnNew
    Model.treeViewColumnSetTitle col title
    renderer <- Model.cellRendererTextNew
    Model.cellLayoutPackStart col renderer False
    Model.cellLayoutSetAttributes col renderer list
            $ \ind -> [Model.cellText := ind]
    Model.treeViewAppendColumn treeview col
    treeview `on` buttonPressEvent $ do
        click <- eventClick
        if click == DoubleClick
            then do
                liftIO $ onDoubleClick list treeview fireAction
                return ()
            else
                return ()
        return False
    return treeview

onDoubleClick :: ListStore String -> TreeView -> (String -> IO()) -> IO ()
onDoubleClick list treeview fireAction = do
    tree <- Model.treeViewGetSelection treeview
    sel <- Model.treeSelectionGetSelectedRows tree
    let s = head  (head sel)
    v <- Model.listStoreGetValue list s
    fireAction v
    --putStrLn $ "selected " ++ v

recvMsgLoop :: Handle -> (ChatRequest -> IO ()) -> IO ()
recvMsgLoop h fire = recvMsgLoop' h fire ""

recvMsgLoop' :: Handle -> (ChatRequest -> IO ()) -> String -> IO ()
recvMsgLoop' h fire s = do
    b <- hIsClosed h
    if b
        then
            return ()
        else do
            input <- hGetInput h
            let (els', ms') = saxParse "" $ s ++ input
                s' = case ms' of
                    Just js -> js
                    Nothing -> ""
            parseMsgStream els' fire
            recvMsgLoop' h fire s'

getAttr :: String -> [Attribute] -> String
getAttr at (x:xs) = case x of
    (N at, AttValue [Left s]) -> s
    otherwise -> getAttr at xs

parseBody :: [SaxElement] -> String -> (ChatRequest -> IO ()) -> IO ()
parseBody (x:xs) from fire = do
    case x of
        (SaxCharData s) -> do
            let from' = takeWhile (/= '/') from
            fire $ (from', Just (from' ++ ": " ++ s))
        (SaxElementClose "body") -> return ()
        otherwise -> parseBody xs from fire

parseMsg :: [SaxElement] -> String -> (ChatRequest -> IO ()) -> IO ()
parseMsg (x:xs) from fire = do
    case x of
        (SaxElementOpen "body" _) -> parseBody xs from fire
        (SaxElementClose "message") -> return ()
        otherwise -> parseMsg xs from fire

parseMsgStream :: [SaxElement] -> (ChatRequest -> IO ()) -> IO ()
parseMsgStream [] _ = return ()
parseMsgStream (x:xs) fire = do
    case x of
        (SaxElementOpen "message" attrs) -> parseMsg xs (getAttr "from" attrs) fire
        otherwise -> return ()
    parseMsgStream xs fire

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
    

getRoster :: Handle -> IO [String]
getRoster h = do
    hSend h "<iq type = \"get\"><query xmlns=\"jabber:iq:roster\"/></iq>"
    s <- hGetInput h
    let (ans, _) = saxParse "" s
    return $ getJIDs ans

logout :: Handle -> IO ()
logout h = hSend h "</stream:stream>"

login :: String -> String -> String -> String -> IO (Handle, [String])
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
    roster <- getRoster h
    hSend h "<presence><show/></presence>"
--    hSend h "</stream:stream>"
    return (h, roster)

--main = do

--  login "" "" "jabber.ru" "jabber.ru"
--  return ()

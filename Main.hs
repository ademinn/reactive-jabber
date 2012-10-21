import Reactive.Banana
import Reactive.Banana.Combinators
import Reactive.Banana.Switch
import Reactive.Banana.Frameworks
import System.IO
import System.Glib.Types
import qualified Data.Map as Map
import Control.Concurrent
import Control.Monad.IO.Class
import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as Model
import Network.XMPP
import Network.XMPPTypes
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

data Chat = Chat    { userName :: String
                    , chatPanel :: VPaned
                    , addMsg :: BString -> IO ()
                    }

data MsgT = MsgT  { chatName :: String
                , sender :: String
                , msgText :: BString
                }

instance Show MsgT where
    show (MsgT _ s t) = s ++ ": " ++ (showB t)

type ChatRequest = (String, Maybe String)

chatNew :: String -> String -> (MsgT -> IO ()) -> IO Chat
chatNew s name fire = do
    outputWdgt <- textViewNew
    tb <- textViewGetBuffer outputWdgt
    textViewSetEditable outputWdgt False
    inputWdgt <- textViewNew
    itb <- textViewGetBuffer inputWdgt
    inputWdgt `on` keyPressEvent $ do
        s' <- eventKeyName
        if s' == "Return"
            then do
                liftIO $ do
                    stIt <- startBuf itb
                    enIt <- endBuf itb
                    out <- textBufferGetByteString itb stIt enIt False
                    fire $ MsgT s name out
                    textBufferSetText itb ""
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

appendText :: TextBuffer -> BString -> IO ()
appendText tb msg = do
    ti <- endBuf tb
    textBufferInsertByteString tb ti (msg `B.append` (C.pack "\n"))

instance GObjectClass Chat where
    toGObject = toGObject . chatPanel
    unsafeCastGObject = undefined

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
                putStrLn $ tail $ show $ ((C.pack loginText) :: BString)
                putStrLn $ loginText ++ passwordText ++ serverText
                (stream, roster, con) <- login (C.pack serverText) (C.pack loginText) (C.pack passwordText) (\_ -> return ()) --serverText
                widgetDestroy loginDialog
                mainLoop (loginText) stream (map showB roster) con
            otherwise -> do
                widgetDestroy loginDialog
                mainQuit
    widgetShowAll loginDialog
    mainGUI
    return ()

quitChat :: Connection -> IO ()
quitChat con = do
    send con EndStream
    mainQuit

mainLoop :: String -> [Stanza] -> [String] -> Connection -> IO ()
mainLoop name stream roster con = do
    --let name = username con
    window <- windowNew
    chWindow <- windowNew
    chats <- notebookNew
    set chWindow    [ containerChild := chats
                    , windowDefaultWidth := 200
                    , windowDefaultHeight := 100
                    ]
    (inMsg, fireInMsg) <- newAddHandler
    (doubleClick, fireDoubleClick) <- newAddHandler
    treeview <- listTreeView name roster fireDoubleClick
    set window  [ windowDefaultWidth := 100
                , windowDefaultHeight := 200
                , containerChild := treeview
                ]
    window `on` deleteEvent $ do
        liftIO $ do
            widgetDestroy chWindow
            putStrLn "exit"
            quitChat con
        return False
    
    chWindow `on` deleteEvent $ do
        liftIO $ widgetHideAll chWindow
        return True

    let networkDescription :: Frameworks t => Moment t ()
        networkDescription = do
            (eAddChat, fireAddChat) <- newEvent
            (eAddChat', fireAddChat') <- newEvent
            (eShowChat', fireShowChat') <- newEvent
            (ePrintMsg, firePrintMsg) <- newEvent
            (eOutMsg, fireOutMsg) <- newEvent
            eInMsg <- fromAddHandler inMsg
            eShowChat <- fromAddHandler doubleClick
            let
                eChatMap = accumE Map.empty $ insertSafe <$> eAddChat'
                bChatMap = stepper Map.empty eChatMap
                
                (<%>) f e = f <$> ((rTuple <$> bChatMap) <@> e)
                
                addChat :: (String, Map.Map String Chat) -> IO ()
                addChat (to, m) = do
                    case Map.lookup to m of
                        Just _ -> return ()
                        Nothing -> do
                            c <- chatNew to name fireOutMsg
                            fireAddChat' (to, c)
                
                inMsgProc :: Stanza -> IO ()
                inMsgProc stanza = do
                    case stanza of
                        Msg (Message (Just f) _ b) -> do
                            fireAddChat (showB f)
                            firePrintMsg $ MsgT (showB f) (showB f) b
                        otherwise -> return ()
                
                outMsgProc :: (MsgT, Map.Map String Chat) -> IO ()
                outMsgProc (msg, m) = do
                    let to = chatName msg
                        c = m Map.! to
                    send con (Msg $ Message Nothing (Just (C.pack to)) (msgText msg))
                    addMsg c $ showBMsg msg
                
                printMsg :: (MsgT, Map.Map String Chat) -> IO ()
                printMsg (msg, m) = do
                    let to = chatName msg
                        c = m Map.! to
                    addMsg c $ showBMsg msg
                
                showChat :: String -> IO ()
                showChat name = do
                    fireAddChat name
                    fireShowChat' name
                    
                showChat' :: (String, Map.Map String Chat) -> IO ()
                showChat' (name, m) = do
                    let chat = m Map.! name
                    ret <- notebookGetNPages chats
                    vis <- get chats widgetVisible
                    if or [(ret == 0), (not vis)]
                        then widgetShowAll chWindow
                        else return ()
                    ind <- notebookPageNum chats chat
                    case ind of
                        Just i -> notebookSetCurrentPage chats i
                        Nothing -> do
                            i <- notebookAppendPage chats chat name
                            widgetShowAll chat
                            notebookSetCurrentPage chats i
            reactimate $ (addChat <%> eAddChat) `union` (showChat <$> eShowChat) `union` (showChat' <%> eShowChat') `union` (printMsg <%> ePrintMsg) `union` (outMsgProc <%> eOutMsg) `union` (inMsgProc <$> eInMsg)
    network <- compile networkDescription
    actuate network
    forkIO $ do
        sequence $ map (postGUIAsync . fireInMsg) $ stream
        return ()
--        recvMsgLoop stream fireInMsg
    widgetShowAll window
        --recvMsgLoop (receive con) fireInMsg
    return ()

showBMsg :: MsgT -> BString
showBMsg (MsgT _ f t) = (C.pack (f ++ ": ")) `B.append` t

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

recvMsgLoop :: [Stanza] -> (Stanza -> IO ()) -> IO ()
recvMsgLoop [] _ = return ()
recvMsgLoop (x:xs) fire = do
--    threadsEnter
    threadDelay 1000
    postGUIAsync . fire $ x
--    threadsLeave
    recvMsgLoop xs fire
--recvMsgLoop :: IO Stanza -> (Stanza -> IO ()) -> IO ()
--recvMsgLoop recv fire = do
--    stanza <- recv
--    case stanza of
--        EndStream -> return ()
--        otherwise -> do
--            fire stanza
--            recvMsgLoop recv fire
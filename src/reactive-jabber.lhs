%include polycode.fmt

\subsubsection{reactive-jabber}

Импортирование модулей.

\begin{code}
import Reactive.Banana
import Reactive.Banana.Combinators
import Reactive.Banana.Switch
import Reactive.Banana.Frameworks
import System.IO
import System.Glib.Types
import qualified Data.List as List
import qualified Data.Map as Map
import Control.Concurrent
import Control.Monad.IO.Class
import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as Model
import Network.XMPP
import Network.XMPPTypes
\end{code}

Функции для работы с TextBuffer.

\begin{code}
startBuf :: TextBuffer -> IO TextIter
startBuf tb = textBufferGetIterAtOffset tb 0

endBuf :: TextBuffer -> IO TextIter
endBuf tb = textBufferGetIterAtOffset tb (-1)

appendText :: TextBuffer -> BString -> IO ()
appendText tb msg = do
    ti <- endBuf tb
    textBufferInsertByteString tb ti (msg +++ "\n")
\end{code}

Структура данных для представления сообщений.
В отличие от Message используется не для приема и отправки сообщений, а для их отображения.
Вместо полей from и to имеет поля chatName и sender,
что позволяет одинково обрабатывать как входящие, так и исходящие сообщения.

\begin{code}
data MsgT
    = MsgT
        { chatName :: String
        , sender :: String
        , msgText :: BString
        }

instance BShow MsgT where
    bShow (MsgT _ f t) = (f ++ ": ") +++ t
\end{code}

Структура данных, представляющая собой виджет чата.

\begin{code}
data Chat
    = Chat
        { userName :: String
        , chatPanel :: VPaned
        , addMsg :: BString -> IO ()
        }

instance GObjectClass Chat where
    toGObject = toGObject . chatPanel
    unsafeCastGObject = undefined

instance ObjectClass Chat
instance WidgetClass Chat

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
    return $ Chat s mainWdgt (postGUIAsync . appendText tb)
\end{code}

Точка входа в программу.
Запускает диалоговое окно и получает данные, необходимые для авторизации.
Производит авторизацию и запускает основной цикл обработки событий.

\begin{code}
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
                (stream, roster, con) <- login (bShow serverText) (bShow loginText) (bShow passwordText)
                widgetDestroy loginDialog
                mainLoop (loginText ++ "@" ++ serverText) stream (map showB roster) con
            otherwise -> do
                widgetDestroy loginDialog
                mainQuit
    widgetShowAll loginDialog
    mainGUI
    return ()
\end{code}

Фунция для завершения работы приложения.

\begin{code}
quitChat :: Connection -> IO ()
quitChat con = do
    send con EndStream
    mainQuit
\end{code}

Основная функция. В ней создается пользовательский интерфейс, и описывается реактивная сеть, обеспечивающая работу программы.

\begin{code}
mainLoop :: String -> [Stanza] -> [String] -> Connection -> IO ()
mainLoop name stream roster con = do
\end{code}

Создание обработчиков событий: получение сообщения и двойной клик на элементе списка контактов.

\begin{code}
    (inMsg, fireInMsg) <- newAddHandler
    (doubleClick, fireDoubleClick) <- newAddHandler
\end{code}

Создание интерфейса.

\begin{code}
    window <- windowNew
    chWindow <- windowNew
    chats <- notebookNew
    set chWindow    [ containerChild := chats
                    , windowDefaultWidth := 200
                    , windowDefaultHeight := 100
                    ]
    vBox <- vBoxNew False 5
    (treeview, list) <- listTreeView name roster
    let getSel = getSelected list treeview
        addContact jid = do
            ls <- listStoreToList list
            if jid `notElem` ls
                then do
                    listStoreAppend list jid
                    return ()
                else
                    return ()
        removeContact jid = do
            ls <- listStoreToList list
            case List.elemIndex jid ls of
                Just i -> do
                    listStoreRemove list i
                    send con . Sub  $ Refuse (Just $ bShow name) (Just $ bShow jid)
                Nothing -> return ()

    treeview `on` buttonPressEvent $ do
        click <- eventClick
        if click == DoubleClick
            then do
                liftIO $ do
                    s <- getSel
                    fireDoubleClick s
                return ()
            else
                return ()
        return False
    boxPackStart vBox treeview PackGrow 0

    addDialog <- dialogNew
    addUpper <- dialogGetUpper addDialog
    addBox <- hBoxNew False 5
    addLabel <- labelNew $ Just "JID"
    boxPackStart addBox addLabel PackNatural 0
    addEntry <- entryNew
    boxPackStart addBox addEntry PackGrow 0
    boxPackStart addUpper addBox PackGrow 5
    dialogAddButton addDialog "Ok" ResponseOk
    dialogAddButton addDialog "Cancel" ResponseCancel
    widgetShowAll addUpper
    addDialog `on` deleteEvent $ do
        liftIO $ do
            dialogResponse addDialog ResponseCancel
            widgetHideAll addDialog
        return True

    addDialog `on` response $ \_ -> do
        widgetHideAll addDialog

    addButton <- buttonNewWithLabel "Add contact"
    addButton `on` buttonActivated $ do
        widgetShowAll addDialog
        res <- dialogRun addDialog
        if res == ResponseOk
            then do
                jid <- entryGetText addEntry
                send con $ Sub (Request (Just $ bShow name) (Just $ bShow jid))
            else
                return ()

    boxPackStart vBox addButton PackNatural 0

    removeDialog <- dialogNew
    removeUpper <- dialogGetUpper removeDialog
    removeLabel <- labelNew $ Just "Nothing"
    boxPackStart removeUpper removeLabel PackGrow 5
    windowSetTransientFor removeDialog window
    windowSetModal removeDialog True
    dialogAddButton removeDialog "Yes" ResponseYes
    dialogAddButton removeDialog "No" ResponseNo
    widgetShowAll removeUpper

    removeDialog `on` deleteEvent $ do
        liftIO $ do
            dialogResponse removeDialog ResponseNo
            widgetHideAll removeDialog
        return True

    removeDialog `on` response $ \_ -> do
        widgetHideAll removeDialog

    removeButton <- buttonNewWithLabel "Remove contact"
    removeButton `on` buttonActivated $ do
        jid <- getSel
        labelSetText removeLabel $ "Remove " ++ jid ++ " from roster?"
        widgetShowAll removeDialog
        res <- dialogRun removeDialog
        case res of
            ResponseYes -> removeContact jid
            otherwise -> return ()

    boxPackStart vBox removeButton PackNatural 0

    set window  [ windowDefaultWidth := 100
                , windowDefaultHeight := 200
                , containerChild := vBox
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

    requestDialog <- dialogNew
    requestUpper <- dialogGetUpper requestDialog
    requestLabel <- labelNew $ Just "Nobody"
    boxPackStart requestUpper requestLabel PackGrow 5
    windowSetTransientFor requestDialog window
    windowSetModal requestDialog True
    dialogAddButton requestDialog "Yes" ResponseYes
    dialogAddButton requestDialog "No" ResponseNo
    widgetShowAll requestUpper

    requestDialog `on` deleteEvent $ do
        liftIO $ do
            dialogResponse requestDialog ResponseNo
            widgetHideAll requestDialog
        return True

    requestDialog `on` response $ \_ -> do
        widgetHideAll requestDialog
\end{code}

\begin{code}
    let showRequest :: JID -> IO ResponseId
        showRequest jid = do
            labelSetText requestLabel $ "Authorize " ++ (showB jid) ++ "?"
            widgetShowAll requestDialog
            dialogRun requestDialog
\end{code}

Описание сети событий.

\begin{code}
    let networkDescription :: Frameworks t => Moment t ()
        networkDescription = do
\end{code}

Создание внутренних событий и преобразование внешних сообщений в события.

\begin{code}
            (eAddChat, fireAddChat) <- newEvent
            (eAddChat', fireAddChat') <- newEvent
            (eShowChat', fireShowChat') <- newEvent
            (ePrintMsg, firePrintMsg) <- newEvent
            (eOutMsg, fireOutMsg) <- newEvent
            eInMsg <- fromAddHandler inMsg
            eShowChat <- fromAddHandler doubleClick
\end{code}

Далее следует описание обработчиков событий.

\begin{code}
            let
\end{code}



\begin{code}
                eChatMap = accumE Map.empty $ insertSafe <$> eAddChat'
                bChatMap = stepper Map.empty eChatMap

                (<%>) f e = f <$> (flip (,) <$> bChatMap <@> e)
\end{code}



\begin{code}
                addChat :: (String, Map.Map String Chat) -> IO ()
                addChat (to, m) = do
                    case Map.lookup to m of
                        Just _ -> return ()
                        Nothing -> do
                            c <- chatNew to name fireOutMsg
                            fireAddChat' (to, c)
\end{code}


\begin{code}
                inMsgProc :: Stanza -> IO ()
                inMsgProc stanza = do
                    case stanza of
                        Msg (Message (Just f) _ b) -> do
                            fireAddChat (showB f)
                            firePrintMsg $ MsgT (showB f) (showB f) b
                        Sub (Request (Just jid) _) -> do
                            res <- postGUISync $ showRequest jid
                            case res of
                                ResponseYes -> send con . Sub $ Confirm (Just $ bShow name) (Just jid)
                                otherwise -> send con . Sub $ Refuse (Just $ bShow name) (Just jid)
                            return ()
                        Sub (Confirm (Just jid) _) -> do
                            addContact $ showB jid
                        Sub (Refuse (Just jid) _) -> do
                            removeContact $ showB jid
                        otherwise -> return ()
\end{code}


\begin{code}
                outMsgProc :: (MsgT, Map.Map String Chat) -> IO ()
                outMsgProc (msg, m) = do
                    let to = chatName msg
                        c = m Map.! to
                    send con . Msg $ Message Nothing (Just $ bShow to) (msgText msg)
                    addMsg c $ bShow msg
\end{code}


\begin{code}
                printMsg :: (MsgT, Map.Map String Chat) -> IO ()
                printMsg (msg, m) = do
                    let to = chatName msg
                        c = m Map.! to
                    addMsg c $ bShow msg
\end{code}


\begin{code}
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
\end{code}


\begin{code}
            reactimate $ (addChat <%> eAddChat) `union` (showChat <$> eShowChat)
                `union` (showChat' <%> eShowChat') `union` (printMsg <%> ePrintMsg)
                `union` (outMsgProc <%> eOutMsg) `union` (inMsgProc <$> eInMsg)
\end{code}


\begin{code}
    network <- compile networkDescription
    widgetShowAll window
    actuate network
    forkIO $ do
        sequence $ map fireInMsg stream
        return ()
    return ()
\end{code}

\begin{code}
insertSafe :: Ord k => (k, a) -> Map.Map k a -> Map.Map k a
insertSafe (k, v) m = if k `Map.member` m then m else Map.insert k v m

listTreeView :: String -> [String] -> IO (TreeView, ListStore String)
listTreeView title sourceList = do
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
    return (treeview, list)

getSelected :: ListStore a -> TreeView -> IO a
getSelected list treeView = do
    tree <- Model.treeViewGetSelection treeView
    sel <- Model.treeSelectionGetSelectedRows tree
    let s = head  (head sel)
    Model.listStoreGetValue list s
\end{code}
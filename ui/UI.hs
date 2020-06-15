module UI (app,initState,loadEditors) where

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.FileBrowser as FB
import           Brick.Widgets.List as L
import qualified Control.Exception as E
import           Control.Monad (zipWithM)
import           Control.Monad.IO.Class
import qualified Data.List.Zipper as Z
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Graphics.Vty (defAttr)
import qualified Graphics.Vty as V
import           System.FilePath ((-<.>))
import qualified Control.Exception as E ()

import qualified Data.Forest as F
import qualified Data.Sentence as S
import qualified Data.Sentences as S
import           Lens.Micro
import           UI.AnnotationEditor
import qualified UI.MiniBuffer as MB

data Name = MB | FB deriving (Eq,Ord,Show)

type FileBrowserWithCallBack =
  (FB.FileBrowser Name, [FileInfo] -> UIState -> IO UIState)

-------------------------------------------------------------------------------
-- UIState and Lenses

data UIState = UIState {
  uiFilePath    :: FilePath,
  uiEditors     :: Z.Zipper Editor,
  uiMinibuffer  :: Maybe (MB.MiniBuffer Name (UIState -> UIState)),
  uiFileBrowser :: Maybe FileBrowserWithCallBack
}

filePathL :: Lens' UIState FilePath
filePathL = lens uiFilePath (\e fp -> e{uiFilePath = fp})

editorsL :: Lens' UIState (Z.Zipper Editor)
editorsL = lens uiEditors (\e z -> e{uiEditors = z})

minibufferL
  :: Lens' UIState (Maybe (MB.MiniBuffer Name (UIState -> UIState)))
minibufferL = lens uiMinibuffer (\e mb -> e{uiMinibuffer = mb})

fileBrowserWithCallBackL
  :: Lens' UIState (Maybe FileBrowserWithCallBack)
fileBrowserWithCallBackL =
  lens uiFileBrowser (\e fbcb -> e{uiFileBrowser = fbcb})

fileBrowserL :: Traversal' UIState (FileBrowser Name)
fileBrowserL = fileBrowserWithCallBackL._Just._1

fileBrowserCallBackL
  :: Traversal' UIState ([FileInfo] -> UIState -> IO UIState)
fileBrowserCallBackL = fileBrowserWithCallBackL._Just._2

cursorL :: Lens' (Z.Zipper a) (Maybe a)
cursorL = lens Z.safeCursor modify where
  modify z Nothing  = Z.delete z
  modify z (Just x) = Z.replace x z

currentEditorL :: Lens' UIState (Maybe Editor)
currentEditorL = editorsL.cursorL

-------------------------------------------------------------------------------
-- State loading

initState :: FilePath -> UIState
initState filePath = UIState filePath Z.empty Nothing Nothing

loadEditors :: FilePath -> IO UIState
loadEditors filePath = do
  let displayIOException :: E.IOException -> String
      displayIOException = E.displayException
  mSentences <- E.try (S.readFile filePath)
  case mSentences of
    Left e -> do
      let mb = MB.message ("Error: " ++ displayIOException e) >> MB.abort
      return (initState filePath & minibufferL .~ Just mb)
    Right sentences -> do
      let editors = fmap (makeEditor F.emptyForest) sentences
          uiState = initState filePath & editorsL .~ editors
          filePathForest = filePath -<.> "fst"
      eForests <- E.try (loadForests filePathForest (Z.toList sentences))
                  <&> _Left %~ ("Error: " ++) . displayIOException
      case eForests >>= maybe (Left "Warning: invalid forest file.") Right of
        Left e -> do
          let mb = MB.message e >> MB.abort
          return (uiState & minibufferL .~ Just mb)
        Right forests -> do
          let setForest f e = e & forestL .~ f
              mb = MB.message ("Loaded " ++ filePath) >> MB.abort
          return $
            uiState
            & editorsL    %~ S.zipperWith setForest (Z.fromList forests)
            & minibufferL .~ Just mb

safeWordNr :: Int -> S.Sentence -> MB.MiniBuffer Name S.Word
safeWordNr n sentence
  | Just x <- S.wordNr n sentence  = return x
  | otherwise = do
      MB.message "Invalid word number. Hit Enter to continue."
      MB.abort

saveForests :: FilePath -> [F.Forest] -> IO ()
saveForests filePath = T.writeFile filePath . F.serialiseForests

loadForests :: FilePath -> [S.Sentence] -> IO (Maybe [F.Forest])
loadForests filePath sentences =
  T.readFile filePath >>= return . F.deserialiseForests sentences


-------------------------------------------------------------------------------
-- Widgets

sentenceWidget :: T.Text -> Widget n
sentenceWidget text = txtWrap (T.map explicitNewline text) where
  explicitNewline '\n' = '\8617'
  explicitNewline c = c

sentencesWidget :: UIState -> Widget Name
sentencesWidget (UIState _ _ _ (Just (fb,_))) =
  FB.renderFileBrowser True fb
  <=>
  case FB.fileBrowserException fb of
    Nothing -> str "Press ESC to cancel."
    Just e -> strWrap $ "Error (Press ESC to cancel):" ++ E.displayException e
sentencesWidget (UIState _ es mb Nothing) =
  hBorder
  <=>
  sentenceWidget (S.initText (fmap editorSentence es))
  <=>
  padTopBottom 1 (maybe emptyWidget editorWidget $ Z.safeCursor es)
  <=>
  sentenceWidget (S.tailText (fmap editorSentence es))
  <=>
  hBorder
  <=>
  maybe (str "R)oot C)hild E)rase S)ave L)oad Q)uit") MB.miniBufferWidget mb

-------------------------------------------------------------------------------
-- Event handling

handleEvent :: UIState -> BrickEvent Name () -> EventM Name (Next UIState)
handleEvent uiState (AppEvent ()) =
  continue uiState

handleEvent uiState (VtyEvent (V.EvKey V.KEsc []))
  | Nothing <- uiState^?fileBrowserL =
      halt uiState
  | Just fb <- uiState^?fileBrowserL, not (FB.fileBrowserIsSearching fb) =
      continue $ uiState
        & fileBrowserWithCallBackL .~ Nothing
        & minibufferL .~ Just (MB.message "File browser cancelled." >> MB.abort)
handleEvent uiState (VtyEvent vtyEvent)
  | Just (fb,callback) <- uiState^.fileBrowserWithCallBackL = do
      newFB <- FB.handleFileBrowserEvent vtyEvent fb
      let selection = fileBrowserSelection newFB
          newUIState = uiState & fileBrowserL .~ newFB
      case vtyEvent of
        V.EvKey V.KEnter [] | not (null selection) ->
          liftIO (callback selection newUIState) >>= continue
        _ -> do
          continue newUIState

handleEvent uiState (VtyEvent (V.EvKey V.KUp [])) =
  continue (uiState & editorsL %~ Z.left)

handleEvent uiState (VtyEvent (V.EvKey V.KDown []))
  | Z.endp (uiState^.editorsL) = continue uiState
  | otherwise = continue (uiState & editorsL %~ Z.right)

handleEvent uiState (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt uiState

handleEvent uiState (VtyEvent (V.EvKey (V.KChar 'r') []))
  | Nothing <- uiState^.minibufferL,
    Just e <- uiState^.editorsL.to Z.safeCursor =
      let mb = do
            n <- MB.promptNatural MB "root (C-g to cancel): "
            w <- safeWordNr n (editorSentence e)
            return $ (minibufferL .~ Nothing)
              . (currentEditorL._Just.forestL %~ F.addRoot w)
      in continue (uiState & minibufferL .~ Just mb)

handleEvent uiState (VtyEvent (V.EvKey (V.KChar 'c') []))
  | Nothing <- uiState^.minibufferL,
    Just e <- uiState^.editorsL.to Z.safeCursor =
      let mb = do
            cn <- MB.promptNatural MB "child (C-g to cancel): "
            c <- safeWordNr cn (editorSentence e)      
            rn <- MB.promptNatural MB $
              "child  " ++ show cn ++ " of (C-g to cancel): "
            r <- safeWordNr rn (editorSentence e)
            return $ (minibufferL .~ Nothing)
              . (currentEditorL._Just.forestL %~ F.addChild c r)

      in continue (uiState & minibufferL .~ Just mb)

handleEvent uiState (VtyEvent (V.EvKey (V.KChar 'e') []))
  | Nothing <- uiState^.minibufferL,
    Just e <- uiState^.editorsL.to Z.safeCursor =
      let mb = do
            n <- MB.promptNatural MB "node (C-g to cancel): "
            c <- safeWordNr n (editorSentence e)
            return $ (minibufferL .~ Nothing)
              . (currentEditorL._Just.forestL %~ F.clear c)
      in continue (uiState & minibufferL .~ Just mb)
--        
handleEvent uiState (VtyEvent (V.EvKey (V.KChar 's') []))
  | Nothing <- uiState^.minibufferL = do
      let filePath = (uiState^.filePathL) -<.> "fst"
      result <- liftIO $ E.try $ saveForests filePath $
        uiState^.editorsL.to Z.toList^..each.forestL
        -- a list containing the forest of each editor in uiState^.editorsL
      let mb = case result of
            Left e -> do
              MB.message ("Error: " ++ E.displayException (e :: E.IOException))
              MB.abort
            Right () -> do
              MB.message ("Saved to " ++ filePath ++ ".")
              MB.abort
      continue (uiState & minibufferL .~ Just mb)
  
handleEvent uiState (VtyEvent (V.EvKey (V.KChar 'l') []))
  | Nothing <- uiState^.minibufferL = do
      fb <- liftIO (FB.newFileBrowser selectNonDirectories FB Nothing)
      let callback :: [FileInfo] -> UIState -> IO UIState
          callback fileInfos callBackUIState
            | [] <- fileInfos =
                let mb = MB.message "No files were selected." >> MB.abort
                in return $ callBackUIState
                   & minibufferL .~ Just mb
                   & fileBrowserWithCallBackL .~ Nothing
                   
            | (fileInfo:_) <- fileInfos = do
                let filePath = FB.fileInfoFilePath fileInfo
                loadEditors filePath
      continue (uiState & fileBrowserWithCallBackL .~ Just (fb,callback))

handleEvent uiState evt | Just mb <- uiState^.minibufferL = do
  newMB <- MB.handleMiniBufferEvent mb evt
  case newMB of
    MB.Abort -> continue (uiState & minibufferL .~ Nothing)
    MB.Return f -> continue (f uiState)
    _ -> continue (uiState & minibufferL .~ Just newMB)

handleEvent uiState _evt = continue uiState


fileBrowserAttrs :: [(AttrName,V.Attr)]
fileBrowserAttrs = 
    [ (L.listSelectedFocusedAttr, V.black `on` V.white)
    , (FB.fileBrowserCurrentDirectoryAttr, fg V.green)
    , (FB.fileBrowserSelectionInfoAttr, V.black `on` V.white)
    , (FB.fileBrowserDirectoryAttr, fg V.white)
    , (FB.fileBrowserBlockDeviceAttr, fg V.red)
    , (FB.fileBrowserCharacterDeviceAttr, fg V.red)
    , (FB.fileBrowserNamedPipeAttr, fg V.red)
    , (FB.fileBrowserSymbolicLinkAttr, fg V.cyan)
    , (FB.fileBrowserUnixSocketAttr, fg V.red)
    , (FB.fileBrowserSelectedAttr, fg V.white)
    , (attrName "error", fg V.red)
    ]

app :: App UIState () Name
app = App {
  appDraw = pure . sentencesWidget,
  appChooseCursor = neverShowCursor,
  appHandleEvent = handleEvent,
  appStartEvent = return,
  appAttrMap = const $ attrMap Graphics.Vty.defAttr $
    [(focusAttr,V.white `on` V.black)]
    ++
    fileBrowserAttrs
}

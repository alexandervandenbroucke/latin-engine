module UI (app,initState,loadEditors) where

import           Brick
import           Brick.Widgets.Border
import qualified Control.Exception as E
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
import qualified Data.Paragraph as P
import           Lens.Micro
import           UI.AnnotationEditor
import qualified UI.MiniBuffer as MB

data Name = MB | FB deriving (Eq,Ord,Show)

-------------------------------------------------------------------------------
-- UIState and Lenses

data UIState = UIState {
  uiFilePath    :: FilePath,
  uiEditors     :: Z.Zipper Editor,
  uiMinibuffer  :: Maybe (MB.MiniBuffer Name (UIState -> UIState))
}

filePathL :: Lens' UIState FilePath
filePathL = lens uiFilePath (\e fp -> e{uiFilePath = fp})

editorsL :: Lens' UIState (Z.Zipper Editor)
editorsL = lens uiEditors (\e z -> e{uiEditors = z})

minibufferL
  :: Lens' UIState (Maybe (MB.MiniBuffer Name (UIState -> UIState)))
minibufferL = lens uiMinibuffer (\e mb -> e{uiMinibuffer = mb})

cursorL :: Lens' (Z.Zipper a) (Maybe a)
cursorL = lens Z.safeCursor modify where
  modify z Nothing  = Z.delete z
  modify z (Just x) = Z.replace x z

currentEditorL :: Lens' UIState (Maybe Editor)
currentEditorL = editorsL.cursorL

-------------------------------------------------------------------------------
-- State loading

initState :: FilePath -> UIState
initState filePath = UIState filePath Z.empty Nothing

loadEditors :: FilePath -> IO UIState
loadEditors filePath = do
  let displayIOException :: E.IOException -> String
      displayIOException = E.displayException
  mSentences <- E.try (P.readFile filePath)
  case mSentences of
    Left e -> do
      let mb = MB.message ("Error: " ++ displayIOException e) >> MB.abort
      return (initState filePath & minibufferL .~ Just mb)
    Right sentences -> do
      let editors = fmap makeEmptyEditor sentences
          uiState = initState filePath & editorsL .~ editors
          filePathForest = filePath -<.> "fst"
      eForests <- E.try (loadForests filePathForest)
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
            & editorsL    %~ P.zipperWith setForest (Z.fromList forests)
            & minibufferL .~ Just mb

safeWordNr :: Int -> S.Sentence -> MB.MiniBuffer Name S.Word
safeWordNr n sentence
  | Just x <- S.wordNr n sentence  = return x
  | otherwise = do
      MB.message "Invalid word number. Hit Enter to continue."
      MB.abort

saveForests :: FilePath -> [F.Forest] -> IO ()
saveForests filePath = T.writeFile filePath . F.serialiseForests

loadForests :: FilePath -> IO (Maybe [F.Forest])
loadForests filePath =
  T.readFile filePath >>= return . F.deserialiseForests


-------------------------------------------------------------------------------
-- Widgets

sentenceWidget :: T.Text -> Widget n
sentenceWidget text = txtWrap (T.map explicitNewline text) where
  explicitNewline '\n' = '\8617'
  explicitNewline c = c

sentencesWidget :: UIState -> Widget Name
sentencesWidget (UIState _ es mb) =
  hBorder
  <=>
  sentenceWidget (P.initText (fmap editorSentence es))
  <=>
  padTopBottom 1 (maybe emptyWidget editorWidget $ Z.safeCursor es)
  <=>
  sentenceWidget (P.tailText (fmap editorSentence es))
  <=>
  hBorder
  <=>
  maybe (str "R)oot C)hild E)rase S)ave Q)uit") MB.miniBufferWidget mb

-------------------------------------------------------------------------------
-- Event handling

handleEvent :: UIState -> BrickEvent Name () -> EventM Name (Next UIState)
handleEvent uiState (AppEvent ()) =
  continue uiState

handleEvent uiState (VtyEvent (V.EvKey V.KEsc [])) = halt uiState

handleEvent uiState (VtyEvent (V.EvKey V.KUp [])) =
  continue (uiState & editorsL %~ Z.left)

handleEvent uiState (VtyEvent (V.EvKey V.KDown []))
  | Z.endp (uiState^.editorsL) = continue uiState
  | otherwise = continue (uiState & editorsL %~ Z.right)

handleEvent uiState (VtyEvent (V.EvKey (V.KChar c) []))
  | 'q' <- c
  = halt uiState
  | 'r' <- c,
    Nothing <- uiState^.minibufferL,
    Just e <- uiState^.editorsL.to Z.safeCursor
  = let mb = do
            root <- MB.promptNatural MB "root (C-g to cancel): "
            _ <- safeWordNr root (editorSentence e)
            return $ \s ->
              s & minibufferL .~ Nothing
                & currentEditorL._Just.forestL %~ F.setRoot root
    in continue (uiState & minibufferL .~ Just mb)
  | 'c' <- c,
    Nothing <- uiState^.minibufferL,
    Just e <- uiState^.editorsL.to Z.safeCursor
  = let mb = do
          child <- MB.promptNatural MB "child (C-g to cancel): "
          _ <- safeWordNr child (editorSentence e)
          parent <- MB.promptNatural MB $
            "child  " ++ show c ++ " of (C-g to cancel): "
          _ <- safeWordNr parent (editorSentence e)
          return $ \s ->
            s & minibufferL .~ Nothing
              & currentEditorL._Just.forestL %~ F.addChild child parent
    in continue (uiState & minibufferL .~ Just mb)
  | 'e' <- c,
    Nothing <- uiState^.minibufferL,
    Just e <- uiState^.editorsL.to Z.safeCursor
  = let mb = do
          n <- MB.promptNatural MB "node (C-g to cancel): "
          _ <- safeWordNr n (editorSentence e)
          return $ \s ->
            s & minibufferL .~ Nothing
              & currentEditorL._Just.forestL %~ F.clear n
    in continue (uiState & minibufferL .~ Just mb)
  | 's' <- c,
    Nothing <- uiState^.minibufferL
  = let filePath = (uiState^.filePathL) -<.> "fst"
        mb (Left e) = do
          MB.message ("Error: " ++ E.displayException (e :: E.IOException))
          MB.abort
        mb (Right ()) = do
          MB.message ("Saved to " ++ filePath ++ ".")
          MB.abort
    in do
      result <- liftIO $ E.try $ saveForests filePath $
        uiState^.editorsL.to Z.toList^..each.forestL
        -- a list containing the forest of each editor in uiState^.editorsL
      continue (uiState & minibufferL .~ Just (mb result))

handleEvent uiState evt | Just mb <- uiState^.minibufferL = do
  newMB <- MB.handleMiniBufferEvent mb evt
  case newMB of
    MB.Abort -> continue (uiState & minibufferL .~ Nothing)
    MB.Return f -> continue (f uiState)
    _ -> continue (uiState & minibufferL .~ Just newMB)

handleEvent uiState _evt = continue uiState



app :: App UIState () Name
app = App {
  appDraw = pure . sentencesWidget,
  appChooseCursor = neverShowCursor,
  appHandleEvent = handleEvent,
  appStartEvent = return,
  appAttrMap = const $ attrMap Graphics.Vty.defAttr []
}

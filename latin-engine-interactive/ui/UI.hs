{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module UI (app,initState,loadEditors) where

import           Brick
import           Brick.Widgets.Border
import qualified Control.Exception as E
import qualified Control.Exception as E ()
import           Control.Monad.Except
import qualified Control.Monad.Writer as W
import qualified Data.List.Zipper as Z
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Graphics.Vty as Vty
import           Prelude hiding (init,tail)
import           System.FilePath ((-<.>))

import qualified Data.Forest as F
import qualified Data.Sentence as S
import qualified Data.Paragraph as P
import           Lens.Micro
import qualified UI.SentenceEditor as SE
import qualified UI.MiniBuffer as MB
import qualified UI.IDIndexedEditor as ID

data Name = MB | PAR | ANN deriving (Eq,Ord,Show)

-------------------------------------------------------------------------------
-- UIState and Lenses

type Editors = (SE.Editor, ID.Editor [T.Text])

data UIState = UIState {
  uiFilePath   :: FilePath,
  uiEditors    :: Z.Zipper Editors,
  uiMinibuffer :: MB.MiniBuffer Name (UIState -> UIState),
  uiFocused    :: Name
}

filePathL :: Lens' UIState FilePath
filePathL = lens uiFilePath (\e fp -> e{uiFilePath = fp})

editorsL :: Lens' UIState (Z.Zipper Editors)
editorsL = lens uiEditors (\e z -> e{uiEditors = z})

editorL :: Lens' UIState (Maybe Editors)
editorL = editorsL.safeCursorL

safeCursorL :: Lens' (Z.Zipper a) (Maybe a)
safeCursorL = lens Z.safeCursor setter where
  setter z Nothing  = Z.delete z
  setter z (Just x) = Z.insert x (Z.delete z)

sentenceL :: Traversal' UIState SE.Editor
sentenceL = editorL._Just._1

annotationL :: Traversal' UIState (ID.Editor [T.Text])
annotationL = editorL._Just._2

minibufferL
  :: Lens' UIState (MB.MiniBuffer Name (UIState -> UIState))
minibufferL = lens uiMinibuffer (\e mb -> e{uiMinibuffer = mb})

focusedL :: Lens' UIState Name
focusedL = lens uiFocused (\e name -> e{uiFocused = name})

-------------------------------------------------------------------------------
-- State loading

displayIOError :: E.IOException -> String
displayIOError e = "Error: " ++ E.displayException e

initState :: FilePath -> UIState
initState filePath = UIState filePath Z.empty MB.abort PAR where

loadParagraph :: (MonadIO m, MonadError String m) => FilePath -> m [SE.Editor]
loadParagraph filePath = do
  paragraph <- liftIO (E.try $ P.readFile filePath) >>=
    either (throwError . displayIOError) return
  return $ map SE.makeEmptyEditor paragraph

loadForests :: (MonadIO m, MonadError String m) => FilePath -> m [F.Forest]
loadForests filePath = do
  fileData <- liftIO (E.try $ T.readFile filePath) >>=
    either (throwError . displayIOError) return
  maybe (throwError $ "Warning: invalid forest file: " ++ filePath) return $
    F.deserialiseForests fileData

loadAnnotations
  :: (MonadIO m, MonadError String m) => FilePath -> m [ID.Editor [T.Text]]
loadAnnotations filePath = do
  annotations <- liftIO (E.try $ T.readFile filePath) >>=
    either (throwError . displayIOError) return
  maybe (throwError $ "Warning: invalid annotation file: " ++ filePath) return $
    mapM ID.deserialise (T.lines annotations)

loadEditors :: FilePath -> IO UIState
loadEditors filePath = do
  let ExceptT go = W.runWriterT $ do
        paragraph <- loadParagraph filePath
        forests <- loadForests (filePath -<.> "fst") `catchError` \e -> do
          W.tell [e] >> return (repeat F.emptyForest)
        annotations <- loadAnnotations (filePath -<.> "ann") `catchError` \e -> 
          W.tell [e] >> return (repeat ID.empty)
        let makeEditors se f ann = (se & SE.forestL .~ f, ann)
        initState filePath
          & editorsL .~ Z.fromList (zipWith3 makeEditors paragraph forests annotations)
          & return
  eUIState <- go
  return $ case eUIState of
    Left e ->
      let mb  = MB.message e >> MB.abort
      in initState filePath & minibufferL .~ mb
    Right (uiState,warnings) ->
      let minibuffer
            | null warnings = MB.message ("Loaded " ++ filePath) >> MB.abort
            | otherwise = mapM MB.message warnings >> MB.abort
      in uiState & minibufferL .~ minibuffer


saveEditors :: FilePath -> UIState -> IO ()
saveEditors filePath uiState  = do
  let saveForests = T.writeFile (filePath -<.> "fst") . F.serialiseForests
      saveAnnotations =
        T.writeFile (filePath -<.> "ann") . T.unlines . map ID.serialise
  saveForests (uiState^.editorsL.to Z.toList^..each._1.SE.forestL)
  saveAnnotations (uiState^.editorsL.to Z.toList^..each._2)


-- | Return all elements in the zipper to the left of the cursor
init :: Z.Zipper a -> Z.Zipper a
init (Z.Zip [] _) = Z.Zip [] []
init (Z.Zip (x:xs) _) = Z.Zip xs [x]

-- | Return all elements in the zipper to the right of the cursor
tail :: Z.Zipper a -> Z.Zipper a
tail (Z.Zip _ []) = Z.Zip [] []
tail (Z.Zip _ (_:xs)) = Z.Zip [] xs


-------------------------------------------------------------------------------
-- Widgets

sentenceWidget :: T.Text -> Widget n
sentenceWidget text = txtWrap (T.map explicitNewline text) where
  explicitNewline '\n' = '\8617'
  explicitNewline c = c

paragraphWidget :: UIState -> Widget Name
paragraphWidget uiState =
  let attr
        | uiState^.focusedL == PAR = focusedBorderAttr
        | otherwise = unFocusedBorderAttr
      parBorder = withAttr attr (hBorderWithLabel (str "[Paragraph]"))
      inits = sentenceWidget $ P.toText $
        uiState^.editorsL.to (Z.toList . init)^..each._1.SE.sentenceL
      sentence = padTopBottom 1 $
        maybe emptyWidget SE.editorWidget $ uiState^?sentenceL
      tails = sentenceWidget $ P.toText $
        uiState^.editorsL.to (Z.toList . tail)^..each._1.SE.sentenceL
  in parBorder <=> inits <=> sentence <=> tails

annotationWidget :: UIState -> Widget Name
annotationWidget uiState =
  let attr
        | uiState^.focusedL == ANN = focusedBorderAttr
        | otherwise = unFocusedBorderAttr
      annBorder = withAttr attr (hBorderWithLabel (str "[Annotation]"))
      headers = [T.pack "ID", T.pack "Word", T.pack "Annotation"]
      widget = case uiState^?annotationL of
        Nothing -> ID.editorWidgetUnfocused T.unpack ID.empty
        Just editor
          | uiState^.focusedL == ANN ->
              ID.editorWidgetMultiAttr ID.focusedAttr headers editor
          | otherwise ->
              ID.editorWidgetMultiAttr ID.unFocusedAttr headers editor
  in annBorder <=> padBottom Max widget

allWidgets :: UIState -> Widget Name
allWidgets uiState =
  paragraphWidget uiState
  <=>
  annotationWidget uiState
  <=>
  hBorder
  <=>
  (if MB.hasAborted (uiState^.minibufferL) then
     str "R)oot C)hild E)rase S)ave F)ocus A)nnotate U)nannotate Q)uit"
    else
     MB.miniBufferWidget (uiState^.minibufferL))

-------------------------------------------------------------------------------
-- Event handling

safeWordNr :: Int -> S.Sentence -> MB.MiniBuffer Name S.Word
safeWordNr n sentence
  | Just x <- S.wordNr n sentence  = return x
  | otherwise = do
      MB.message "Invalid word number. Hit Enter to continue."
      MB.abort

updateMiniBuffer
  :: UIState -> MB.MiniBuffer Name (UIState -> UIState) -> UIState
updateMiniBuffer uiState (MB.Return f)  = f uiState
updateMiniBuffer uiState mb = uiState & minibufferL .~ mb

handleEvent :: UIState -> BrickEvent Name () -> EventM Name (Next UIState)
handleEvent uiState (AppEvent ()) =
  continue uiState

handleEvent uiState (VtyEvent (Vty.EvKey Vty.KEsc [])) = halt uiState

handleEvent uiState (VtyEvent (Vty.EvKey Vty.KUp []))
  | PAR <- uiState^.focusedL = continue (uiState & editorsL %~ Z.left)
  | ANN <- uiState^.focusedL = continue (uiState & annotationL %~ ID.prev)
  | MB  <- uiState^.focusedL = continue uiState

handleEvent uiState (VtyEvent (Vty.EvKey Vty.KDown []))
  | PAR <- uiState^.focusedL, not (Z.endp (uiState^.editorsL)) =
      continue (uiState & editorsL %~ Z.right)
  | ANN <- uiState^.focusedL = continue (uiState & annotationL %~ ID.next)
  | otherwise = continue uiState

handleEvent uiState evt
  | mb <- uiState^.minibufferL, not (MB.hasAborted mb) = do
      MB.handleMiniBufferEvent mb evt >>= continue . updateMiniBuffer uiState

handleEvent uiState (VtyEvent (Vty.EvKey (Vty.KChar c) []))
  | 'q' <- c
  = halt uiState
  | 'f' <- c
  = let swap MB  = MB
        swap PAR = if uiState^?annotationL == Nothing then PAR else ANN
        swap ANN = PAR
    in continue (uiState & focusedL %~ swap)
  | 's' <- c
  = let mb (Left e) = do
          MB.message ("Error: " ++ E.displayException (e :: E.IOException))
          MB.abort
        mb (Right ()) = do
          MB.message ("Saved to " ++ uiState^.filePathL ++ ".")
          MB.abort
    in do
      result <- liftIO $ E.try $ saveEditors (uiState^.filePathL) uiState
      continue (uiState & minibufferL .~ mb result)
  | Just editors <- uiState^.editorL
  = continue $ updateMiniBuffer uiState $ do
      editors' <- handleEditorEvent editors uiState c
      return (\s -> s & editorL .~ Just editors' & minibufferL .~ MB.abort)

handleEvent uiState _evt = continue uiState

handleEditorEvent
  :: Editors
  -> UIState
  -> Char
  -> MB.MiniBuffer Name Editors
handleEditorEvent editors uiState c
  | 'r' <- c = do
      root <- MB.promptNatural MB "root (C-g to cancel): "
      _ <- safeWordNr root (editors^._1.SE.sentenceL)
      return (editors & _1.SE.forestL %~ F.setRoot root)
  | 'c' <- c = do
      child <- MB.promptNatural MB "child (C-g to cancel): "
      _ <- safeWordNr child (editors^._1.SE.sentenceL)
      parent <- MB.promptNatural MB $
        "child  " ++ show child ++ " of (C-g to cancel): "
      _ <- safeWordNr parent (editors^._1.SE.sentenceL)
      return (editors & _1.SE.forestL %~ F.addChild child parent)
  | 'e' <- c = do
      n <- MB.promptNatural MB "erase (C-g to cancel): "
      _ <- safeWordNr n (editors^._1.SE.sentenceL)
      return (editors & _1.SE.forestL %~ F.clear n)
  | 'a' <- c = do
      n <- MB.promptNatural MB "annotate (C-g to cancel): "
      w <- safeWordNr n (editors^._1.SE.sentenceL)
      annotation <- MB.promptString MB $
        "annotate " ++ show n ++ " (C-g to cancel): "
      case annotation of
        "" -> MB.message "Error: empty annotation." >> MB.abort
        _  -> return $
          editors & _2.ID.valueL n .~ Just [S.wordText w,T.pack annotation]
  | 'u' <- c, ANN <- uiState^.focusedL = do
      return (editors & _2.ID.valueL (editors^._2.ID.focusL) .~ Nothing)
  | 'u' <- c = do
      n <- MB.promptNatural MB "unannotate (C-g to cancel): "
      _ <- safeWordNr n (editors^._1.SE.sentenceL)
      case editors^._2.ID.valueL n of
        Nothing -> MB.message "Error: no such annotation." >> MB.abort
        Just{} -> return (editors & _2.ID.valueL n .~ Nothing)
  | otherwise = MB.abort

focusedBorderAttr :: AttrName
focusedBorderAttr = "focused-border"

unFocusedBorderAttr :: AttrName
unFocusedBorderAttr = "unfocused-border"

app :: App UIState () Name
app = App {
  appDraw = pure . allWidgets,
  appChooseCursor = const (showCursorNamed MB),
  appHandleEvent = handleEvent,
  appStartEvent = return,
  appAttrMap =
      let gray = Vty.rgbColor (20 :: Int) 20 20
          attrs = [
            (ID.focusedAttr <> ID.lineAttr,
              Vty.defAttr `Vty.withStyle` Vty.standout),
            (ID.unFocusedAttr,
              fg gray),
            (ID.unFocusedAttr <> ID.lineAttr,
              gray `on` Vty.white),
            (focusedBorderAttr,
              Vty.defAttr),
            (unFocusedBorderAttr,
              fg gray)]
  in const $ attrMap Vty.defAttr attrs
}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module UI (app,initState,loadEditors) where

import           Brick
import           Brick.Widgets.Border
import qualified Control.Exception as E
import qualified Control.Exception as E ()
import           Control.Monad.Except
import qualified Control.Monad.Writer as W
import qualified Data.List.Zipper as Z
import qualified Data.Text as T
import qualified Graphics.Vty as Vty
import           Prelude hiding (init,tail)
import           System.FilePath ((-<.>))

import qualified Data.Forest as F
import qualified Data.Forest.Serialise as SerialiseF
import qualified Data.Paragraph as P
import qualified Data.Sentence as S
import           Data.Sentence.Serialise as SerialiseS
import           Lens.Micro
import qualified UI.DeterminationEditor as DE
import qualified UI.IDIndexedEditor as ID
import qualified UI.MiniBuffer as MB
import qualified UI.SentenceEditor as SE

data Name = MB | PAR | ANN | DET S.Word deriving (Eq,Ord,Show)

-------------------------------------------------------------------------------
-- UIState and Lenses

type Editors = (SE.Editor, ID.Editor [T.Text])

data UIState = UIState {
  uiFilePath   :: FilePath,
  uiEditors    :: Z.Zipper Editors,
  uiMinibuffer :: MB.MiniBuffer Name UIState,
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

senL :: Lens' Editors SE.Editor
senL = _1

annL :: Lens' Editors (ID.Editor [T.Text])
annL = _2

sentenceL :: Traversal' UIState SE.Editor
sentenceL = editorL._Just.senL

annotationL :: Traversal' UIState (ID.Editor [T.Text])
annotationL = editorL._Just.annL

minibufferL :: Lens' UIState (MB.MiniBuffer Name UIState)
minibufferL = lens uiMinibuffer (\e mb -> e{uiMinibuffer = mb})

focusedL :: Lens' UIState Name
focusedL = lens uiFocused (\e name -> e{uiFocused = name})

-------------------------------------------------------------------------------
-- State loading & saving

displayIOError :: E.IOException -> String
displayIOError e = "Error: " ++ E.displayException e

-- | Empty ui state: no files loaded.
emptyState :: FilePath -> UIState
emptyState filePath = UIState filePath Z.empty MB.abort PAR

-- | Create a ui state by loading a file.
initState :: FilePath -> IO UIState
initState filePath = do
  let uiState = emptyState filePath
  eResult <- runExceptT (loadEditors filePath)
  return $ case eResult of
    Left e ->
      uiState & minibufferL .~ (MB.message e >> MB.abort)
    Right (editors, warnings) ->
      let minibuffer
            | null warnings
            = MB.message ("Loaded " ++ filePath) >> MB.abort
            | otherwise
            = mapM MB.message warnings >> MB.abort
      in uiState & editorsL .~ Z.fromList editors & minibufferL .~ minibuffer

loadParagraph :: (MonadIO m, MonadError String m) => FilePath -> m [SE.Editor]
loadParagraph filePath =
  liftIO (E.try $ P.readFile filePath) >>=
  either (throwError . displayIOError) (return . map SE.makeEmptyEditor)

loadForests :: (MonadIO m, MonadError String m) => FilePath -> m [F.Forest]
loadForests filePath =
  liftIO (E.try $ SerialiseF.readForests filePath) >>=
  either (throwError . displayIOError) return      >>=
  maybe (throwError $ "Warning: invalid forest file: " ++ filePath) return

loadAnnotations
  :: (MonadIO m, MonadError String m)
  => P.Paragraph -> FilePath -> m [ID.Editor [T.Text]]
loadAnnotations paragraph filePath = do
  annotations <-
    liftIO (E.try $ SerialiseS.readFile filePath) >>=
    either (throwError . displayIOError) return   >>=
    maybe  (throwError $ "Warning: malformed file: " ++ filePath) return
  let withWords sentence editor =
        let addWord n _ = case S.wordNr n sentence of
              Nothing -> throwError $
                "Warning: invalid annotation file: " ++ filePath
              Just w -> return (S.wordText w)
        in editor
           & fmap (map return)    -- Editor [m Text]
           & ID.addColumn addWord -- Editor [m Text]
           & fmap sequence        -- Editor (m [Text])
           & sequence             -- m (Editor [Text])
           -- This complicated sequencing is necessary to thread exceptions
           -- through the ID.Editor structure. Perhaps it would be easier to
           -- simply pre-process all the keys in the Editor, and throw
           -- exceptions early, before updating the editor structure.
           -- Though, this way the module UI does not depend directly on
           -- IntMap.
  zipWithM withWords paragraph annotations

-- | Load editors from file.
--
-- Throws an exception if the source file cannot be read.
--
-- The result is a list of editors and a (potentially empty) list of warnings.
-- Warnings can be generated if the forest or annotation file is missing.
loadEditors
  :: (MonadError String m, MonadIO m)
  => FilePath -> m ([Editors],[String])
loadEditors filePath = W.runWriterT $ do
    sentences <- loadParagraph filePath
    let paragraph = sentences^..each.SE.sentenceL

    forests <- loadForests (filePath -<.> "fst.json")
      `catchError` \e -> W.tell [e] >> return (repeat F.emptyForest)

    annotations <- loadAnnotations paragraph (filePath -<.> "ann.json")
      `catchError` \e -> W.tell [e] >> return (repeat ID.empty)

    let editors = zipWith3 makeEditors sentences forests annotations where
          makeEditors se f ann = (se & SE.forestL .~ f, ann)

    return editors


saveEditors :: FilePath -> [Editors] -> IO ()
saveEditors filePath editors = do
  SerialiseF.writeForests (filePath -<.> "fst.json") $
    editors^..each.senL.SE.forestL

  SerialiseS.writeFile (filePath -<.> "ann.json") $
    editors^..each.annL.to ID.dropColumn

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

paragraphWidget :: UIState -> Widget n
paragraphWidget uiState =
  let attr
        | uiState^.focusedL == PAR
        = focusedBorderAttr
        | otherwise
        = unFocusedBorderAttr
      parBorder = withAttr attr (hBorderWithLabel (str "[Paragraph]"))
      inits = withAttr unFocusedParagraphAttr $ sentenceWidget $ P.toText $
        uiState^.editorsL.to (Z.toList . init)^..each.senL.SE.sentenceL
      widget = padTopBottom 1 $
        maybe emptyWidget SE.editorWidget (uiState^?sentenceL)
      sentence
        | uiState^.focusedL == PAR
        = widget
        | otherwise
        = withAttr unFocusedParagraphAttr widget
      tails = withAttr unFocusedParagraphAttr $ sentenceWidget $ P.toText $
        uiState^.editorsL.to (Z.toList . tail)^..each.senL.SE.sentenceL

  in parBorder <=> inits <=> sentence <=> tails

determinationWidget :: S.Word -> T.Text -> Widget n
determinationWidget word hint =
  let detBorder = withAttr focusedBorderAttr $ hBorderWithLabel $
        hBox [str "[", txt (S.wordText word),str "]"]
      widget = DE.determinationWidget word hint
  in detBorder <=> padBottom Max widget

annotationWidget :: UIState -> Widget n
annotationWidget uiState =
  let attr
        | uiState^.focusedL == ANN
        = focusedBorderAttr
        | otherwise
        = unFocusedBorderAttr
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

lowerPane :: UIState -> Widget n
lowerPane uiState
  | DET word <- uiState^.focusedL
  = let hint = maybe "" id $
          uiState^?annotationL.ID.valueL (S.wordId word)._Just._last
    in determinationWidget word hint
  | otherwise
  = annotationWidget uiState

allWidgets :: UIState -> Widget Name
allWidgets uiState =
  paragraphWidget uiState
  <=>
  lowerPane uiState
  <=>
  hBorder
  <=>
  (if MB.isDone (uiState^.minibufferL) then
     str "R)oot C)hild E)rase S)ave F)ocus A)nnotate U)nannotate D)etermine Q)uit"
    else
     MB.miniBufferWidget (uiState^.minibufferL))

-------------------------------------------------------------------------------
-- Event handling

safeWordNr :: Int -> S.Sentence -> MB.MiniBuffer Name S.Word
safeWordNr n sentence
  | Just x <- S.wordNr n sentence
  = return x
  | otherwise
  = do
      MB.message "Invalid word number. Hit Enter to continue."
      MB.abort

-- | Set the minibuffer state
updateMiniBuffer
  :: UIState -> MB.MiniBuffer Name UIState -> UIState
updateMiniBuffer _ (MB.Return uiState)  = uiState
updateMiniBuffer uiState mb = uiState & minibufferL .~ mb

-- | Display the annotation prompt
handleAnnotation
  :: S.WordId -> Editors -> MB.MiniBuffer Name Editors
handleAnnotation n editors = do
  w <- safeWordNr n (editors^.senL.SE.sentenceL)
  let wStr = T.unpack (S.wordText w)
  let msg = "annotate " ++ wStr ++ " [" ++ show n ++ "] (C-g to cancel): "
  let promptForAnnotation
        | Just [_,ann] <- editors^.annL.ID.valueL n
        = MB.promptPrimitive MB (const True) msg (T.unpack ann)
        | otherwise
        = MB.promptString MB msg
  annotation <- promptForAnnotation
  case annotation of
    "" -> MB.message "Error: empty annotation." >> MB.abort
    _  -> return $
          editors & annL.ID.valueL n ?~ [S.wordText w,T.pack annotation]


-- | Handle events
handleEvent :: UIState -> BrickEvent Name () -> EventM Name (Next UIState)

handleEvent uiState event = case event of
  -- * 'ESC' key always halts
  VtyEvent (Vty.EvKey Vty.KEsc []) -> halt uiState

  -- * This handler forwards key events to the minibuffer.
  _ | mb <- uiState^.minibufferL, not (MB.isDone mb) ->
        MB.handleMiniBufferEvent mb event
        >>= continue . updateMiniBuffer uiState

  -- * 'q' key halts
  VtyEvent (Vty.EvKey (Vty.KChar 'q') []) ->
    halt uiState

  -- * Other key events do not halt, and are delegated to another function.
  VtyEvent (Vty.EvKey key modifiers) ->
    handleKeyEvent key modifiers uiState >>= continue

  -- * Default catch-all clause
  _ -> continue uiState


-- | Handle keypress events that cannot halt.
handleKeyEvent
  :: MonadIO m => Vty.Key -> [Vty.Modifier] -> UIState -> m UIState
handleKeyEvent key modifiers uiState
  -- * Focus previous element
  | Vty.KUp <- key, [] <- modifiers
  = return $ case uiState^.focusedL of
      PAR -> uiState & editorsL %~ Z.left
      ANN -> uiState & annotationL %~ ID.prev
      MB  -> uiState
      DET{} -> uiState -- for now

  -- * Focus next element
  | Vty.KDown <- key, [] <- modifiers
  = return $ case uiState^.focusedL of
      PAR | not (Z.endp (uiState^.editorsL)) ->
            uiState & editorsL %~ Z.right
      ANN ->
        uiState & annotationL %~ ID.next
      DET{} ->
        uiState
      _ ->
        uiState

  -- * Forward Char key event
  | Vty.KChar c <- key
  = handleCharEvent c uiState

  -- * Catch all clause
  | otherwise
  = return uiState

-- | Handle keypress events that correspond to a character key.
handleCharEvent :: MonadIO m => Char -> UIState -> m UIState
handleCharEvent c uiState
  -- * Swap focus
  | 'f' <- c
  = let swap (DET w) = (DET w)
        swap MB  = MB
        swap PAR = if uiState^?annotationL == Nothing then PAR else ANN
        swap ANN = PAR
    in return (uiState & focusedL %~ swap)

  -- * Save editor state
  | 's' <- c
  = let mb (Left e) = do
          MB.message ("Error: " ++ E.displayException (e :: E.IOException))
          MB.abort
        mb (Right ()) = do
          MB.message ("Saved to " ++ uiState^.filePathL ++ ".")
          MB.abort
    in do
      result <- liftIO $ E.try $ saveEditors (uiState^.filePathL) $
        uiState^.editorsL.to Z.toList
      return (uiState & minibufferL .~ mb result)

  -- * Handle determination prompt
  | 'd' <- c, Just editors <- uiState^.editorL
  = return $ updateMiniBuffer uiState $ do
      wordId <- MB.promptNatural MB "determine (C-g to cancel): "
      word <- safeWordNr wordId (editors^.senL.SE.sentenceL)
      let focus = uiState^.focusedL
      let mb = do
            MB.message "Press RETURN to continue."
            -- reset focus
            return (uiState & focusedL .~ focus)
      return (uiState & focusedL .~ DET word & minibufferL .~ mb)

  -- * Handle editor events
  | Just editors <- uiState^.editorL
  = return $ updateMiniBuffer uiState $ do
      editors' <- handleEditorEvent editors (uiState^.focusedL) c
      return (uiState & editorL ?~ editors')

  -- * catch all case
  | otherwise = return uiState

-- | Handle commands to the editor.
--
-- These events can only affect the editor state, through a minibuffer.
handleEditorEvent :: Editors -> Name -> Char -> MB.MiniBuffer Name Editors
handleEditorEvent editors focused c
  -- * set root
  | 'r' <- c
  = do
      root <- MB.promptNatural MB "root (C-g to cancel): "
      _ <- safeWordNr root (editors^.senL.SE.sentenceL)
      return (editors & senL.SE.forestL %~ F.setRoot root)

  -- * set child
  | 'c' <- c
  = do
      child <- MB.promptNatural MB "child (C-g to cancel): "
      _ <- safeWordNr child (editors^.senL.SE.sentenceL)
      parent <- MB.promptNatural MB $
        "child  " ++ show child ++ " of (C-g to cancel): "
      _ <- safeWordNr parent (editors^.senL.SE.sentenceL)
      return (editors & senL.SE.forestL %~ F.addChild child parent)

  -- * erase (unset status)
  | 'e' <- c
  = do
      n <- MB.promptNatural MB "erase (C-g to cancel): "
      _ <- safeWordNr n (editors^.senL.SE.sentenceL)
      return (editors & senL.SE.forestL %~ F.clear n)

  -- * add annotation (annotation pane focused)
  | 'a' <- c, ANN <- focused,
    n <- editors^.annL.ID.focusL,
    Just{} <- S.wordNr n (editors^.senL.SE.sentenceL)
  = handleAnnotation n editors

  -- * add annotation (annotation pane not focused)
  | 'a' <- c
  = do
      n <- MB.promptNatural MB "annotate (C-g to cancel): "
      handleAnnotation n editors

  -- * remove annotation (annotation pane focused)
  | 'u' <- c, ANN <- focused
  = return (editors & annL.ID.valueL (editors^.annL.ID.focusL) .~ Nothing)

  -- * remove annotation (annotation pane not focused)
  | 'u' <- c
  = do
      n <- MB.promptNatural MB "unannotate (C-g to cancel): "
      _ <- safeWordNr n (editors^.senL.SE.sentenceL)
      case editors^.annL.ID.valueL n of
        Nothing -> MB.message "Error: no such annotation." >> MB.abort
        Just{} -> return (editors & annL.ID.valueL n .~ Nothing)

  -- * catch all case
  | otherwise
  = MB.abort

focusedBorderAttr :: AttrName
focusedBorderAttr = "focused-border"

unFocusedBorderAttr :: AttrName
unFocusedBorderAttr = "unfocused-border"

unFocusedParagraphAttr :: AttrName
unFocusedParagraphAttr = "unfocused-paragraph"

app :: App UIState () Name
app = App {
  appDraw = pure . allWidgets,
  appChooseCursor = const (showCursorNamed MB),
  appHandleEvent = handleEvent,
  appStartEvent = return,
  appAttrMap =
      let gray = Vty.rgbColor (20 :: Int) 20 20
          attrs = [
            (unFocusedParagraphAttr,
              fg gray),
            (ID.focusedAttr <> ID.lineAttr,
              Vty.defAttr `Vty.withStyle` Vty.standout),
            (ID.unFocusedAttr,
              fg gray),
            (ID.unFocusedAttr <> ID.lineAttr,
              gray `on` Vty.white),
            (focusedBorderAttr,
              Vty.defAttr),
            (unFocusedBorderAttr,
              fg gray),
            (DE.stemAttr,
              fg Vty.red),
            (DE.inflectedAttr,
             fg Vty.green)]

  in const $ attrMap Vty.defAttr attrs
}

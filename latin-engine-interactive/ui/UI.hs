{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module UI (app,initState,loadEditors) where

import           Brick
import           Brick.Widgets.Border
import qualified Control.Exception as E
import           Control.Monad.Except
import qualified Control.Monad.Writer as W
import qualified Data.List.Zipper as Z
import qualified Data.Text as T
import qualified Graphics.Vty as Vty
import           Prelude hiding (init,tail)
import           System.FilePath ((-<.>))

import qualified Data.Forest as F
import qualified Data.Forest.Serialise as SerialiseF
import           Data.Maybe (fromMaybe, isNothing)
import qualified Data.Paragraph as P
import qualified Data.Sentence as S
import qualified Data.Sentence.Serialise as SerialiseS
import           Control.Lens hiding (zoom)
import qualified UI.DeterminationEditor as DE
import qualified UI.IDIndexedEditor as ID
import qualified UI.MiniBuffer as MB
import qualified UI.SentenceEditor as SE
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)

data Name = MB | PAR | ANN | DET S.Word deriving (Eq,Ord,Show)

-------------------------------------------------------------------------------
-- UIState and Lenses

type Editors = (SE.Editor, ID.Editor [T.Text])

data UIState = UIState {
  uiFilePath   :: FilePath,
  uiEditors    :: Z.Zipper Editors,
  uiMinibuffer :: MB.MiniBuffer Name (MaybeT (EventM Name UIState)) (),
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

minibufferL
  :: Lens' UIState (MB.MiniBuffer Name (MaybeT (EventM Name UIState)) ())
minibufferL = lens uiMinibuffer (\e mb -> e{uiMinibuffer = mb})

focusedL :: Lens' UIState Name
focusedL = lens uiFocused (\e name -> e{uiFocused = name})

-------------------------------------------------------------------------------
-- State loading & saving

displayIOError :: E.IOException -> String
displayIOError e = "Error: " ++ E.displayException e

-- | Empty ui state: no files loaded.
emptyState :: FilePath -> UIState
emptyState filePath = UIState filePath Z.empty (pure ()) PAR

-- | Create a ui state by loading a file.
initState :: FilePath -> IO UIState
initState filePath = do
  let uiState = emptyState filePath
  eResult <- runExceptT (loadEditors filePath)
  return $ case eResult of
    Left e ->
      uiState & minibufferL .~ MB.message e
    Right (editors, warnings) ->
      let minibuffer
            | null warnings
            = MB.message msg
            | otherwise
            = mapM_ MB.message warnings
          n = length editors
          fileMsg = "Loaded " ++ filePath ++ " "
          sentenceMsg
            | n == 1    = "[1 sentence]"
            | otherwise = "[" ++ show n ++ " sentences]"
          msg = fileMsg ++ " " ++ sentenceMsg
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
           & fmap (map pure)      -- Editor [m Text]
           & ID.addColumn addWord -- Editor [m Text]
           & traverse sequenceA   -- m (Editor [Text])
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
      `catchError` \e -> W.tell [e] >> return []

    annotations <- loadAnnotations paragraph (filePath -<.> "ann.json")
      `catchError` \e -> W.tell [e] >> return []

    -- Assume missing forests and annotations are empty.
    let forests' = forests ++ repeat F.emptyForest
        annotations' = annotations ++ repeat ID.empty

    let editors = zipWith3 makeEditors sentences forests' annotations' where
          makeEditors se f ann = (se & SE.forestL .~ f, ann)

    return editors

-- | Save editors to a file.
saveEditors :: FilePath -> [Editors] -> IO ()
saveEditors filePath editors = do
  SerialiseF.writeForests (filePath -<.> "fst.json") $
    editors^..each.senL.SE.forestL

  SerialiseS.writeFile (filePath -<.> "ann.json") $
    editors^..each.annL.to ID.dropColumn


-------------------------------------------------------------------------------
-- Widgets

sentenceWidget :: T.Text -> Widget n
sentenceWidget text = txtWrap (T.map explicitNewline text) where
  explicitNewline '\n' = '\8617'
  explicitNewline c = c

paragraphWidget :: Name -> Z.Zipper Editors -> Widget n
paragraphWidget focus editors =
  let attr
        | focus == PAR
        = focusedBorderAttr
        | otherwise
        = unFocusedBorderAttr
      parBorder = withAttr attr (hBorderWithLabel (str "[Paragraph]"))
      inits = withAttr unFocusedParagraphAttr $ sentenceWidget $ P.toText $
        init editors^.to Z.toList^..each.senL.SE.sentenceL
      widget = padTopBottom 1 $
        maybe emptyWidget SE.editorWidget (editors^?safeCursorL._Just.senL)
      sentence
        | focus == PAR
        = widget
        | otherwise
        = withAttr unFocusedParagraphAttr widget
      tails = withAttr unFocusedParagraphAttr $ sentenceWidget $ P.toText $
        tail editors^.to Z.toList^..each.senL.SE.sentenceL

  in parBorder <=> inits <=> sentence <=> tails

determinationWidget :: S.Word -> T.Text -> Widget n
determinationWidget word hint =
  let detBorder = withAttr focusedBorderAttr $ hBorderWithLabel $
        hBox [str "[", txt (S.wordText word),str "]"]
      widget = DE.determinationWidget word hint
  in detBorder <=> padBottom Max widget

annotationWidget :: Name -> Maybe (ID.Editor [T.Text]) -> Widget n
annotationWidget focus annotation =
  let attr
        | focus == ANN
        = focusedBorderAttr
        | otherwise
        = unFocusedBorderAttr
      annBorder = withAttr attr (hBorderWithLabel (str "[Annotation]"))
      headers = [T.pack "ID", T.pack "Word", T.pack "Annotation"]
      widget = case annotation of
        Nothing -> ID.editorWidgetUnfocused T.unpack ID.empty
        Just editor
          | focus == ANN ->
              ID.editorWidgetMultiAttr ID.focusedAttr headers editor
          | otherwise ->
              ID.editorWidgetMultiAttr ID.unFocusedAttr headers editor
  in annBorder <=> padBottom Max widget

lowerPane :: Name -> Maybe (ID.Editor [T.Text]) -> Widget n
lowerPane focus annotation
  | DET word <- focus
  = let hint = fromMaybe "" $
          annotation^?_Just.ID.valueL (S.wordId word)._Just._last
    in determinationWidget word hint
  | otherwise
  = annotationWidget focus annotation

allWidgets :: UIState -> Widget Name
allWidgets uiState =
  paragraphWidget (uiState^.focusedL) (uiState^.editorsL)
  <=>
  lowerPane (uiState^.focusedL) (uiState^?annotationL)
  <=>
  hBorder
  <=>
  (if MB.isPure (uiState^.minibufferL) then
     str "R)oot C)hild E)rase S)ave F)ocus A)nnotate U)nannotate D)etermine Q)uit"
    else
     MB.miniBufferWidget (uiState^.minibufferL))

-------------------------------------------------------------------------------
-- Event handling

-- | Display the annotation prompt
annotationPrompt
  :: S.WordId
  -> MB.MiniBuffer Name (MaybeT (EventM Name Editors)) ()
annotationPrompt n = do
  sentence <- zoom (senL.SE.sentenceL) get
  w <- safeWordNr n sentence
  let wStr = T.unpack (S.wordText w)

  let promptForAnnotation msg = get >>= \case
        Just [_,ann] -> MB.promptPrimitive MB (const True) id msg (T.unpack ann)
        _otherwise   -> MB.promptString MB msg

  annotation <- zoom (annL.ID.valueL n) $ promptForAnnotation $
    "annotate " ++ wStr ++ " [" ++ show n ++ "] (C-g to cancel): "

  case annotation of
    "" -> MB.message "Error: empty annotation."
    _  ->  annL.ID.valueL n .= Just [S.wordText w,T.pack annotation]

-- | Handle events
handleEvent :: BrickEvent Name () -> EventM Name UIState ()
handleEvent event = do
  uiState <- get
  case event of
    -- * 'ESC' key always halts
    VtyEvent (Vty.EvKey Vty.KEsc []) -> halt

    -- * This handler forwards key events to the minibuffer.
    _ | mb <- uiState^.minibufferL, not (MB.isPure mb) ->
          Brick.zoom minibufferL $
            MB.handleMiniBufferEvent (put $ pure ()) event

    -- * 'q' key halts
    VtyEvent (Vty.EvKey (Vty.KChar 'q') []) ->
      halt

    -- * Other key events do not halt, and are delegated to another function.
    VtyEvent (Vty.EvKey key modifiers) -> do
      minibufferL .= handleKeyEvent key modifiers

    -- * Default catch-all clause
    _ -> pure ()

  -- Make sure to reset the selected word if the minibuffer is empty.
  mb <- runMaybeT $ MB.run =<< gets (view minibufferL)
  case mb of
     Just (Left mb) -> minibufferL .= mb
     _otherwise     -> do
       sentenceL.SE.selectedL .= Nothing
       minibufferL .= pure ()

-- | Handle keypress events that cannot halt.
handleKeyEvent
  :: Vty.Key
  -> [Vty.Modifier]
  -> MB.MiniBuffer Name (MaybeT (EventM Name UIState)) ()
handleKeyEvent key modifiers
  | Vty.KUp     <- key, [] <- modifiers = focusPrevious
  | Vty.KDown   <- key, [] <- modifiers = focusNext
  | Vty.KChar c <- key, [] <- modifiers = handleCharEvent c
  | otherwise                           = pure ()
  where
    focusPrevious = modify $ \uiState ->
      case uiState^.focusedL of
        PAR   -> uiState & editorsL %~ Z.left
        ANN   -> uiState & annotationL %~ ID.prev
        MB    -> uiState
        DET{} -> uiState -- for now

    focusNext = modify $ \uiState ->
      case uiState^.focusedL of
        PAR | not (Z.endp (uiState^.editorsL)) ->
                uiState & editorsL %~ Z.right
        ANN ->
          uiState & annotationL %~ ID.next
        DET{} ->
          uiState
        _ ->
          uiState

-- | Handle keypress events that correspond to a character key.
handleCharEvent
  :: Char -> MB.MiniBuffer Name (MaybeT (EventM Name UIState)) ()
handleCharEvent c
  | 'f' <- c  = swapFocus
  | 's' <- c  = saveEditorState
  | 'd' <- c  = determine
  | otherwise = editorEvent
  where
    swapFocus = do
      annotation <- gets (preview annotationL)
      focusedL %= \case
        DET w -> DET w
        MB    -> MB
        PAR
          | isNothing annotation -> PAR
          | otherwise            -> ANN
        ANN -> PAR

    saveEditorState = do
      uiState <- get
      result <- liftIO $ E.try $ saveEditors (uiState^.filePathL) $
        uiState^.editorsL.to Z.toList
      case result of
        Left e ->
          MB.message ("Error: " ++ E.displayException (e :: E.IOException))
        Right () ->
          MB.message ("Saved to " ++ uiState^.filePathL ++ ".")

    determine = do
      uiState <- get
      -- Find out which word we are determining
      word <- if
        | ANN           <- uiState^.focusedL
        , Just focus    <- uiState^?annotationL.ID.focusL
        , Just sentence <- uiState^?sentenceL.SE.sentenceL
        , Just word     <- S.wordNr focus sentence
        -> pure word

        | Just editors <- uiState^.editorL
        -> promptWord (editors^.senL.SE.sentenceL) "determine"

        | otherwise
        -> fail "No such word"

      -- If we found a word, focus on determining it
      focusedL .= DET word
      sentenceL.SE.selectedL ?= word
      MB.message "Press RETURN to continue."
      focusedL .= uiState^.focusedL     -- reset focus
      sentenceL.SE.selectedL .= Nothing -- reset selected word

    editorEvent = do
      focus <- zoom focusedL get
      handleEditorEvent focus c
      pure ()

-- | Handle commands to the editor.
--
-- These events can only affect the editor state, through a minibuffer.
handleEditorEvent
  :: Name
  -> Char
  -> MB.MiniBuffer Name (MaybeT (EventM Name UIState)) ()
handleEditorEvent focused c
  -- * set root
  | 'r' <- c
  = do
      root <- S.wordId <$> promptWord' "root"
      sentenceL.SE.forestL %= F.setRoot root

  -- * set child
  | 'c' <- c
  = do
      child@(S.wordId -> childId) <- promptWord' "child"
      -- highlight the selected word
      sentenceL.SE.selectedL .= Just child
      parentId <- S.wordId <$> promptWord' ("child  " ++ show childId ++ " of")

      sentenceL.SE.forestL %= F.addChild childId parentId
      -- remove highlight
      sentenceL.SE.selectedL .= Nothing

  -- * erase (unset status)
  | 'e' <- c
  = do
      wordId <- S.wordId <$> promptWord' "erase"
      sentenceL.SE.forestL %= F.clear wordId

  -- * add annotation (annotation pane focused)
  | 'a' <- c, ANN <- focused
  = zoom (editorL._Just) $ do
      focus <- zoom (annL.ID.focusL) get
      annotationPrompt focus

  -- * add annotation (annotation pane not focused)
  | 'a' <- c
  = do
      word@(S.wordId -> wordId) <- promptWord' "annotate"
      sentenceL.SE.selectedL .= Just word
      zoom (editorL._Just) $ annotationPrompt wordId
      sentenceL.SE.selectedL .= Nothing

  -- * remove annotation (annotation pane focused)
  | 'u' <- c, ANN <- focused
  = zoom (editorL._Just.annL) $ do
      focus <- zoom ID.focusL get
      ID.valueL focus .= Nothing

  -- * remove annotation (annotation pane not focused)
  | 'u' <- c
  = do
      wordId <- S.wordId <$> promptWord' "unannotate"
      annotation <- zoom (editorL._Just.annL.ID.valueL wordId) get
      case annotation of
        Nothing -> MB.message "Error: no such annotation."
        Just{} ->  editorL._Just.annL.ID.valueL wordId .= Nothing

  -- * catch all case
  | otherwise
  = pure ()
  where
    promptWord' prompt = do
      sentence <- gets (preview $ sentenceL.SE.sentenceL)
      case sentence of
        Nothing -> fail ""
        Just s  -> promptWord s prompt

-------------------------------------------------------------------------------
-- App instance & Utilities

focusedBorderAttr :: AttrName
focusedBorderAttr = attrName "focused-border"

unFocusedBorderAttr :: AttrName
unFocusedBorderAttr = attrName "unfocused-border"

unFocusedParagraphAttr :: AttrName
unFocusedParagraphAttr = attrName "unfocused-paragraph"

-- | Return all elements in the zipper to the left of the cursor
init :: Z.Zipper a -> Z.Zipper a
init (Z.Zip [] _) = Z.Zip [] []
init (Z.Zip (x:xs) _) = Z.Zip xs [x]

-- | Return all elements in the zipper to the right of the cursor
tail :: Z.Zipper a -> Z.Zipper a
tail (Z.Zip _ []) = Z.Zip [] []
tail (Z.Zip _ (_:xs)) = Z.Zip [] xs

-- | Prompt for a word number within a sentence.
promptWord
  :: MonadFail m
  => S.Sentence
  -> String
  -> MB.MiniBuffer Name m S.Word
promptWord sentence prompt = do
  wordId <- MB.promptNatural MB (prompt <> "(C-g to cancel): ")
  safeWordNr wordId sentence

-- | Look up a word in a sentence, or set a minibuffer error message if the
-- word id is out of bounds.
safeWordNr :: MonadFail m => Int -> S.Sentence -> MB.MiniBuffer Name m S.Word
safeWordNr n sentence
  | Just x <- S.wordNr n sentence = pure x
  | otherwise = do
      MB.message "Invalid word number. Hit Enter to continue."
      fail "aborted"

app :: App UIState () Name
app = App {
  appDraw = pure . allWidgets,
  appChooseCursor = const (showCursorNamed MB),
  appHandleEvent = handleEvent,
  appStartEvent = pure (),
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
             fg Vty.green),
            (SE.selectedAttr,
              gray `on` Vty.white)]

  in const $ attrMap Vty.defAttr attrs
}

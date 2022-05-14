{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE ViewPatterns #-}

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
import           Control.Lens
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

-- | Set the selected word of the current editor
setSelectedWord :: S.Word -> UIState -> UIState
setSelectedWord word = editorL.mapped.senL.SE.selectedL ?~ word

-- | Set the selected word of the current editor to the word selected
-- in the given editor.
resetSelectedWord :: Editors -> UIState -> UIState
resetSelectedWord editor =
  editorL.mapped.senL.SE.selectedL .~ editor^.senL.SE.selectedL

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
            = MB.message msg >> MB.abort
            | otherwise
            = mapM_ MB.message warnings >> MB.abort
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
  (if MB.isDone (uiState^.minibufferL) then
     str "R)oot C)hild E)rase S)ave F)ocus A)nnotate U)nannotate D)etermine Q)uit"
    else
     MB.miniBufferWidget (uiState^.minibufferL))

-------------------------------------------------------------------------------
-- Event handling

-- | Step the minibuffer state
updateMiniBuffer :: UIState -> UIState
updateMiniBuffer uiState = case uiState^.minibufferL of
  MB.Return uiState -> updateMiniBuffer uiState
  MB.Done -> uiState & sentenceL.SE.selectedL .~ Nothing
  _ -> uiState

-- | Display the annotation prompt
annotationPrompt
  :: S.WordId -> Editors -> MB.MiniBuffer Name Editors
annotationPrompt n editors = do
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
  _ | mb <- uiState^.minibufferL, not (MB.isDone mb) -> do
        mb' <- MB.handleMiniBufferEvent mb event
        continue $ uiState & minibufferL .~ mb' & updateMiniBuffer

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
  | Vty.KChar c <- key, [] <- modifiers
  = handleCharEvent c uiState

  -- * Catch all clause
  | otherwise
  = return uiState

-- | Handle keypress events that correspond to a character key.
handleCharEvent :: MonadIO m => Char -> UIState -> m UIState
handleCharEvent c uiState
  -- * Swap focus
  | 'f' <- c
  = let swap (DET w) = DET w
        swap MB  = MB
        swap PAR = if isNothing (uiState^?annotationL) then PAR else ANN
        swap ANN = PAR
    in return (uiState & focusedL %~ swap)

  -- * Save editor state
  | 's' <- c
  = let
    in do
      result <- liftIO $ E.try $ saveEditors (uiState^.filePathL) $
        uiState^.editorsL.to Z.toList
      return $ uiState
        & minibufferL .~ do
          case result of
            Left e ->
              MB.message ("Error: " ++ E.displayException (e :: E.IOException))
            Right () ->
              MB.message ("Saved to " ++ uiState^.filePathL ++ ".")
          MB.abort

  -- * Handle determination prompt
  | 'd' <- c, Just editors <- uiState^.editorL
  = return $ updateMiniBuffer $ flip (set minibufferL) uiState $ do
      word <- promptWord (editors^.senL.SE.sentenceL) "determine"

      return $ uiState
        & focusedL .~ DET word
        & setSelectedWord word

        & minibufferL .~ do
            MB.message "Press RETURN to continue."
            return $ uiState
              & focusedL .~ uiState^.focusedL -- reset focus
              & resetSelectedWord editors     -- reset selected sentence

  -- * Handle editor events
  | Just{} <- uiState^.editorL
  = pure $ runEditorUpdates (handleEditorEvent (uiState^.focusedL) c) uiState

  -- * catch all case
  | otherwise = return uiState

data EditorUpdate a
  = Done a
  | Update (Editors -> (Editors, MB.MiniBuffer Name (EditorUpdate a)))
  deriving Functor

instance Applicative EditorUpdate where
  pure = return
  (<*>) = ap

instance Monad EditorUpdate where
  return = Done
  Done x >>= f = f x
  Update u >>= f = Update $ \e -> let (e',mb) = u e in  (e', fmap (>>= f) mb)

runEditorUpdates :: EditorUpdate a -> UIState -> UIState
runEditorUpdates (Done _) state  = state
runEditorUpdates (Update u) state
  | Just e <- state^.editorL =
      let (e',mb) =  u e
          s = state & editorL ?~ e'
      in s
         & minibufferL .~ fmap (`runEditorUpdates` s) mb
         & updateMiniBuffer
  | otherwise = state

update :: Editors -> EditorUpdate ()
update e = Update $ const (e, pure (pure ()))

minibuffer :: MB.MiniBuffer Name a -> EditorUpdate a
minibuffer mb = Update (, Done <$> mb)

askEditors :: EditorUpdate Editors
askEditors = Update $ \e -> (e, pure $ Done e)

-- | Handle commands to the editor.
--
-- These events can only affect the editor state, through a minibuffer.
handleEditorEvent :: Name -> Char -> EditorUpdate ()
handleEditorEvent focused c
  -- * set root
  | 'r' <- c
  = do
      root <- S.wordId <$> promptWord' "root"
      askEditors >>= update . (senL.SE.forestL %~ F.setRoot root)

  -- * set child
  | 'c' <- c
  = do
      child@(S.wordId -> childId) <- promptWord' "child"
      -- highlight the selected word
      askEditors >>= update . (senL.SE.selectedL ?~ child)
      parentId <- S.wordId <$> promptWord' ("child  " ++ show childId ++ " of")
      askEditors >>= update
        . (senL.SE.forestL %~ F.addChild childId parentId)
        -- remove highlight
        . (senL.SE.selectedL .~ Nothing)

  -- * erase (unset status)
  | 'e' <- c
  = do
      wordId <- S.wordId <$> promptWord' "erase"
      askEditors >>= update . (senL.SE.forestL %~ F.clear wordId)

  -- * add annotation (annotation pane focused)
  | 'a' <- c, ANN <- focused
  = do
      editors <- askEditors
      minibuffer (annotationPrompt (editors^.annL.ID.focusL) editors) >>= update
  -- * add annotation (annotation pane not focused)
  | 'a' <- c
  = do
      word@(S.wordId -> wordId) <- promptWord' "annotate"
      askEditors >>= update  . (senL.SE.selectedL ?~ word)
      askEditors >>= minibuffer . annotationPrompt wordId >>= update
      askEditors >>= update  . (senL.SE.selectedL .~ Nothing)

  -- * remove annotation (annotation pane focused)
  | 'u' <- c, ANN <- focused
  = do
      editors <- askEditors
      update (editors & annL.ID.valueL (editors^.annL.ID.focusL) .~ Nothing)

  -- * remove annotation (annotation pane not focused)
  | 'u' <- c
  = do
      wordId <- S.wordId <$> promptWord' "unannotate"
      editors <- askEditors
      case editors^.annL.ID.valueL wordId of
        Nothing -> minibuffer $ MB.message "Error: no such annotation." >> MB.abort
        Just{} ->  update $ editors & annL.ID.valueL wordId .~ Nothing

  -- * catch all case
  | otherwise
  = minibuffer MB.abort
  where promptWord' prompt = do
          editors <- askEditors
          minibuffer $ promptWord (editors^.senL.SE.sentenceL) prompt

-------------------------------------------------------------------------------
-- App instance & Utilities

focusedBorderAttr :: AttrName
focusedBorderAttr = "focused-border"

unFocusedBorderAttr :: AttrName
unFocusedBorderAttr = "unfocused-border"

unFocusedParagraphAttr :: AttrName
unFocusedParagraphAttr = "unfocused-paragraph"

-- | Return all elements in the zipper to the left of the cursor
init :: Z.Zipper a -> Z.Zipper a
init (Z.Zip [] _) = Z.Zip [] []
init (Z.Zip (x:xs) _) = Z.Zip xs [x]

-- | Return all elements in the zipper to the right of the cursor
tail :: Z.Zipper a -> Z.Zipper a
tail (Z.Zip _ []) = Z.Zip [] []
tail (Z.Zip _ (_:xs)) = Z.Zip [] xs

-- | Prompt for a word number within a sentence.
promptWord :: S.Sentence -> String -> MB.MiniBuffer Name S.Word
promptWord sentence prompt = do
  wordId <- MB.promptNatural MB (prompt <> "(C-g to cancel): ")
  safeWordNr wordId sentence

-- | Look up a word in a sentence, or set a minibuffer error message if the
-- word id is out of bounds.
safeWordNr :: Int -> S.Sentence -> MB.MiniBuffer Name S.Word
safeWordNr n sentence
  | Just x <- S.wordNr n sentence
  = return x
  | otherwise
  = MB.message "Invalid word number. Hit Enter to continue." >> MB.abort

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
             fg Vty.green),
            (SE.selectedAttr,
              gray `on` Vty.white)]

  in const $ attrMap Vty.defAttr attrs
}

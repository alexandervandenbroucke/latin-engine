{- |

Module:      UI.AnnotationEditor
Description: A widget for editing forests of parse trees.
Maintainer:  alexander.vandenbroucke@gmail.com

An editor for things that are indexed by 'WordId' and shown in a column.

Looking like this:

> ID Word   Annotation 
> 3  Gallia NOM F S < Gallia, -ae; subject
> 6  Divisa P ind perf 3 S < dividere, -eo; main verb

Operations: request focused value, focus next/prev, delete value, add a value
-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.IDIndexedEditor where

import           Brick
import           Brick.AttrMap (AttrName)
import qualified Data.IntMap as M
import           Data.List (transpose)
import           Data.Sentence (WordId)
import qualified Data.Text as T
import           Lens.Micro
import           Text.Read (readMaybe)

-- | A 'S.WordId' indexed map of things
type Annotations a = M.IntMap a

-- | Editor data type, contains the focused word id and annotations.
data Editor a = Editor {
  _focused :: WordId,
  _values  :: Annotations a
} deriving (Eq,Ord,Show)


-- | An empty editor.
empty :: Editor a
empty = Editor 0 M.empty

-- | A lens to the 'Editor''s focus
focusL :: Lens' (Editor a) WordId
focusL = lens _focused (\e wordId -> e{_focused = wordId})

-- | A lens to the 'Editor''s values
valuesL :: Lens' (Editor a) (Annotations a)
valuesL = lens _values (\e m -> e{_values = m})

-- | A lens to a map's values.
lookupL :: Int -> Lens' (M.IntMap a) (Maybe a)
lookupL n = lens (M.lookup n) setter where
  setter m Nothing  = M.delete n m
  setter m (Just x) = M.insert n x m

-- | A lens to the value associated with a particular word.
valueL :: WordId -> Lens' (Editor a) (Maybe a)
valueL wordId = valuesL.lookupL wordId

-- | The prefix attribute of this widget
editAttr :: AttrName
editAttr = "id-indexed-editor"

-- | The attribute prefix when the editor has focus.
focusedAttr :: AttrName
focusedAttr = editAttr <> "focused"

-- | The attribute prefix when the editor does not have focus.
unFocusedAttr :: AttrName
unFocusedAttr = editAttr <> "unfocused"

-- | The attribute of the selected line
lineAttr :: AttrName
lineAttr = "line"

-- | Move the focused value to the next 'WordId' that has an annotation.
next :: Editor a -> Editor a
next editor = case M.lookupGT (editor^.focusL) (editor^.valuesL) of
  Nothing         -> editor
  Just (wordId,_) -> editor & focusL .~ wordId

-- | Move the focused value to the previous 'WordId' that has an annotation.
prev :: Editor a -> Editor a
prev editor = case M.lookupLT (editor^.focusL) (editor^.valuesL) of
  Nothing         -> editor
  Just (wordId,_) -> editor & focusL .~ wordId

-- | A widget for a focused 'Editor' that has annotations that can be show
-- as a 'String'.
--
-- It's useful to have function argument instead of 'Show', because show
-- is often not desirable, e.g., for @Text@ values.
editorWidget
  :: (a -> String) -- ^ The string representation of @a@
  -> Editor a      -- ^ the editor
  -> Widget n
editorWidget = editorWidgetAttr focusedAttr

-- | Like 'editorWidget' but unfocused.
editorWidgetUnfocused :: (a -> String) -> Editor a -> Widget n
editorWidgetUnfocused = editorWidgetAttr unFocusedAttr

-- | A widget for an 'Editor' that has annotations that can be shown as a
-- 'String', using a specific 'AttrName' for styling.
--
-- e.g., use 'focusedAttr' or 'unFocusedAttr'.
editorWidgetAttr :: AttrName -> (a -> String) -> Editor a -> Widget n
editorWidgetAttr attr _ editor
  | editor^.valuesL.to null = withAttr attr (str "No Annotations")
editorWidgetAttr attr shew editor =
  let maxWidth = maximum $
        map length ("ID":(editor^.valuesL.to M.toList^..each._1.to show))
      padToMaxWidth = pad maxWidth
      pad n s = s ++ replicate (n - length s) ' '
      line (n,v) = padRight Max $ str $ padToMaxWidth (show n) ++ " " ++ shew v
      lineWidget (n,v)
        | n == editor^.focusL = withAttr (attr <> lineAttr) (line (n,v))
        | otherwise           = withAttr attr (line (n,v))
      header = padRight Max $ str $ padToMaxWidth "ID" ++ " Annotation"
  in vBox (header:map lineWidget (editor^.valuesL.to M.toAscList))

-- | A widget for an 'Editor' that has several columns of annotations of
-- 'T.Text' values.
--
-- The 'Editor' is shown in a table format and with the focused annotation
-- is highlighted based on the 'lineAttr' of the specified attribute.
editorWidgetMultiAttr
  :: AttrName -- ^ Attribute to determine styling.
  -> [T.Text] -- ^ Table headers
  -> Editor [T.Text] -- ^ The editor
  -> Widget n
editorWidgetMultiAttr attr headers editor
  | editor^.valuesL.to null = withAttr attr (str "No Annotations")
  | otherwise = vBox (headerRow : zipWith row ids idRows) where
      (ids,rows) = unzip (editor^.valuesL.to M.toAscList)
      idcolumn = map (T.pack . show) ids
      idRows = zipWith (:) idcolumn rows
      columns = transpose idRows
      widths = map (maximum . map T.length) (zipWith (:) headers columns)
      headerRow = hBox (zipWith cell widths headers)
      row n cells
        | n == editor^.focusL =
            withAttr (attr <> lineAttr) $ padRight Max $
            hBox $ zipWith cell widths cells
        | otherwise =
            withAttr attr $ padRight Max $ hBox $ zipWith cell widths cells
      cell n = padRight (Pad 1) . str . pad n . T.unpack
      pad n s = s ++ replicate (n - length s) ' '

-- | Serialise and 'Editor'
serialise :: Show a => Editor a -> T.Text
serialise editor = T.pack $ show $ editor^.valuesL

-- | Deserialise and 'Editor'
deserialise :: Read a => T.Text -> Maybe (Editor a)
deserialise text = setv <$> readMaybe (T.unpack text) where
  setv v = empty & valuesL .~ v

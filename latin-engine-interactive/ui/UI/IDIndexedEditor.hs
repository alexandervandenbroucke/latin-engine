{- |

Module:      UI.AnnotationEditor
Description: A widget for editing forests of parse trees.
Maintainer:  alexander.vandenbroucke@gmail.com

An editor for things that are indexed by 'WordId'

Looking like this:

> ID |
> ---+------------------------
> 1  | ACC m S < dux
> 50 | A ind. pres. 3S < amare

would be nice to have alternating colours

Maybe support multiple columns?

ID | Declension/Conjugatin | 
1  | II
operations: focus on a value, focus next/prev, delete focused value, add a
value
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

type Annotations a = M.IntMap a

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

mapWithWordId :: (WordId -> a -> b) -> Annotations a -> Annotations b
mapWithWordId f = M.mapWithKey f

traverseWithKey
  :: Applicative f
  => (WordId -> a -> f b)
  -> Annotations a
  -> f (Annotations b)
traverseWithKey = M.traverseWithKey

editAttr :: AttrName
editAttr = "id-indexed-editor"

focusedAttr :: AttrName
focusedAttr = editAttr <> "focused"

unFocusedAttr :: AttrName
unFocusedAttr = editAttr <> "unfocused"

lineAttr :: AttrName
lineAttr = "line"

next :: Editor a -> Editor a
next editor = case M.lookupGT (editor^.focusL) (editor^.valuesL) of
  Nothing         -> editor
  Just (wordId,_) -> editor & focusL .~ wordId

prev :: Editor a -> Editor a
prev editor = case M.lookupLT (editor^.focusL) (editor^.valuesL) of
  Nothing         -> editor
  Just (wordId,_) -> editor & focusL .~ wordId

editorWidget :: (a -> String) -> Editor a -> Widget n
editorWidget = editorWidgetAttr focusedAttr

editorWidgetUnfocused :: (a -> String) -> Editor a -> Widget n
editorWidgetUnfocused = editorWidgetAttr unFocusedAttr

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

editorWidgetMultiAttr :: AttrName -> [T.Text] -> Editor [T.Text] -> Widget n
editorWidgetMultiAttr attr headers editor
  | editor^.valuesL.to null = withAttr attr (str "No Annotations")
  | otherwise = vBox (headerRow : zipWith row ids idRows) where
      (ids,rows) = unzip (editor^.valuesL.to M.toAscList)
      idcolumn = map (T.pack . show) ids
      idRows = zipWith (:) idcolumn rows
      columns = transpose idRows
      widths = map (maximum . map T.length) columns
      headerRow = hBox (zipWith cell widths headers)
      row n cells
        | n == editor^.focusL =
            withAttr (attr <> lineAttr) $ padRight Max $
            hBox $ zipWith cell widths cells
        | otherwise =
            withAttr attr $ padRight Max $ hBox $ zipWith cell widths cells
      cell n = padRight (Pad 1) . str . pad n . T.unpack
      pad n s = s ++ replicate (n - length s) ' '

serialise :: Show a => Editor a -> T.Text
serialise editor = T.pack $ show $ editor^.valuesL

deserialise :: Read a => T.Text -> Maybe (Editor a)
deserialise text = setv <$> readMaybe (T.unpack text) where
  setv v = empty & valuesL .~ v

{- |

Module:      UI.SentenceEditor
Description: A widget for editing forests of parse trees of a sentence.
Maintainer:  alexander.vandenbroucke@gmail.com

-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.SentenceEditor (
  -- * Editor State
  Editor(..),
  sentenceL, forestL, selectedL,
  makeEditor,
  selectedAttr,
  -- * Editor Widget
  makeEmptyEditor,
  editorWidget
) where

import           Brick
import qualified Data.Forest as F
import qualified Data.Sentence as S
import qualified Data.Text as T
import           Control.Lens

-- | An editor is a 'S.Sentence' and its corresponding parse tree 'F.Forest'.
data Editor = Editor {
  editorSentence :: S.Sentence,
  editorForest   :: F.Forest,
  editorSelected :: Maybe S.Word
} deriving Show

-- | Lens to the 'Editor''s 'S.Sentence'
sentenceL :: Lens' Editor S.Sentence
sentenceL = lens editorSentence (\e s -> e{editorSentence = s})

-- | Lens to the 'Editor''s 'F.Forest'
forestL :: Lens' Editor F.Forest
forestL = lens editorForest (\e f -> e{editorForest = f})

-- | Lens to the 'Editor''s selected word.
selectedL :: Lens' Editor (Maybe S.Word)
selectedL = lens editorSelected  (\e s -> e{editorSelected = s})

-- | Make an editor.
makeEditor :: S.Sentence -> F.Forest -> Editor
makeEditor editorSentence editorForest = Editor{..} where
  editorSelected = Nothing

-- | Combine make an editor from a sentence and assume the forest is empty.
makeEmptyEditor :: S.Sentence -> Editor
makeEmptyEditor sentence = makeEditor sentence F.emptyForest

-- | Look up the status of a 'S.Word' in the 'F.Forest' and convert it to
-- 'T.Text'.
statusText :: S.Word -> F.Forest -> T.Text
statusText word forest | n <- S.wordId word = case forest `F.statusOf` n of
  F.Root    -> T.pack (show n ++ "R")
  F.Child r -> T.pack (show n ++ "C" ++ show r)
  F.Clear   -> T.pack (show n)

selectedAttr :: AttrName
selectedAttr = attrName "sentence-editor" <> attrName "selected-word"


-- | Render an 'Editor'.
editorWidget :: Editor -> Widget n
editorWidget (Editor sentence forest selected) =
  cropToContext $ padRight Max $ Widget Greedy Fixed $ do
  ctx <- getContext

  let len (text,status,_) =  max (textWidth text) (textWidth status) + 1
      pairs :: [(T.Text,T.Text,Widget n -> Widget n)]
      pairs =
        [ (text,statusText word forest,attr)
        | word@(S.Word _ text) <- S.toWords sentence
        , let attr
                | Just word == selected = withAttr selectedAttr
                | otherwise = id]
      width = ctx^.availWidthL
      splitted = S.splitLines width len pairs
      
      renderLine :: [(T.Text,T.Text,Widget n -> Widget n)] -> Widget n
      renderLine line = padRight Max $ hBox (map renderPair line) where
        renderPair (text,status,attr) =
          padRight (Pad 1) $ attr $ txt text <=> txt status

  render (vBox $ map renderLine splitted)

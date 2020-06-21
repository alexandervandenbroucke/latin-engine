{- |

Module:      UI.Sentence
Description: A widget for editing forests of parse trees of a sentence.
Maintainer:  alexander.vandenbroucke@gmail.com

-}

module UI.SentenceEditor (
  -- * Editor State
  Editor(..),
  sentenceL, forestL,
  makeEditor,
  -- * Editor Widget
  makeEmptyEditor,
  editorWidget
) where

import           Brick
import qualified Data.Forest as F
import qualified Data.Sentence as S
import qualified Data.Text as T
import           Lens.Micro

-- | An editor is a 'S.Sentence' and its corresponding parse tree 'F.Forest'.
data Editor = Editor {
  editorSentence :: S.Sentence,
  editorForest   :: F.Forest
} deriving Show

-- | Lens to the 'Editor''s 'S.Sentence'
sentenceL :: Lens' Editor S.Sentence
sentenceL = lens editorSentence (\e s -> e{editorSentence = s})

-- | Lens to the 'Editor''s 'F.Forest'
forestL :: Lens' Editor F.Forest
forestL = lens editorForest (\e f -> e{editorForest = f})

-- | Make an editor.
makeEditor :: S.Sentence -> F.Forest -> Editor
makeEditor sentence forest = Editor sentence forest

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

-- | Render an 'Editor'.
editorWidget :: Editor -> Widget n
editorWidget (Editor sentence forest) =
  cropToContext $ padRight Max $ Widget Greedy Fixed $ do
  ctx <- getContext

  let len (text,status) =  max (textWidth text) (textWidth status) + 1
      pairs :: [(T.Text,T.Text)]
      pairs =
        [(text,statusText word forest)
        | word@(S.Word _ text) <- S.words sentence ]
      width = ctx^.availWidthL
      splitted = S.splitLines width len pairs
      renderPair (text,status) = padRight (Pad 1) (txt text <=> txt status)
      renderLine :: [(T.Text,T.Text)] -> Widget n
      renderLine line = padRight Max $ hBox (map renderPair line)
  render (vBox $ map renderLine splitted)

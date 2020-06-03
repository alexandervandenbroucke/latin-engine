-- This module divides things up into sentences (i.e. chuncks of text,
-- separated by a period. This is appropriate for proze. Alternatively,
-- for poetry you could use stanzas (i.e. chunks of text separated by lines).
--
-- Sentences have decorations related to the grammatical structure (i.e. verbs,
-- subjects, clause markings). Stanzas should have annotations related to the
-- structure of the meter (e.g. stresses, caesura, ...).
module Data.Sentence (
  Word(..),
  Sentence(..),
  makeSentence,
  Data.Sentence.words, wordCount, sentenceText, wordNr
)
where

import qualified Data.Array as A
import qualified Data.Text as T
import           Prelude hiding (Word)

data Word = Word {
  wordId   :: Int,
  wordText :: T.Text
} deriving (Show,Read,Eq,Ord)

newtype Sentence = Sentence {
  unSentence :: A.Array Int Word
} deriving Show


makeSentence :: T.Text -> Sentence
makeSentence text = Sentence (A.listArray (1,n) ws) where
  ws = zipWith Word [1..] (T.words text)
  n = length ws

wordCount :: Sentence -> Int
wordCount = snd . A.bounds . unSentence

wordNr :: Int -> Sentence -> Maybe Word
wordNr i (Sentence arr)
  | lo <= i && i <= hi = Just (arr A.! i)
  | otherwise = Nothing
  where (lo,hi) = A.bounds arr

words :: Sentence -> [Word]
words = A.elems . unSentence

sentenceText :: Sentence -> T.Text
sentenceText (Sentence ws) =
  T.unwords [txt | Word _ txt <- A.elems ws] <> T.pack "."

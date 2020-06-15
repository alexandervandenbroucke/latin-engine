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
  Data.Sentence.null,
  makeSentence,
  words, wordCount, sentenceText, wordNr,
  splitLines, splitSentence
)
where

import qualified Data.Array as A
import qualified Data.Text as T
import           Prelude hiding (Word,words)

data Word = Word {
  wordId   :: Int,
  wordText :: T.Text
} deriving (Show,Read,Eq,Ord)

newtype Sentence = Sentence {
  unSentence :: A.Array Int Word
} deriving Show

null :: Sentence -> Bool
null s = wordCount s == 0

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

-- | Split a list of things into lines.
--
-- Given a maximum width @w@, list @xs@ of @a@, and a function @len@, that
-- gives the width of an @a@, split @xs@ into lines such that no line exceeds
-- the maximum width.
splitLines :: Int -> (a -> Int) -> [a] -> [[a]]
splitLines width len = go 0 [] where
  go _ line [] = [reverse line]
  go n line (x:xs)
    | n + len x > width = reverse line : go 0 [] (x:xs)
    | otherwise = go newLength newLine xs where
        newLine = x:line
        newLength = n + len x

-- | Split a sentence into lines of at most 80 columns.
splitSentence :: Sentence -> [[Word]]
splitSentence = splitLines 80 (T.length . wordText) . words

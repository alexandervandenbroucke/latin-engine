{- |

Module:      Data.Sentence
Description: A data type for sequences of words
Maintainer:  alexander.vandenbroucke@gmail.com

This module divides things up into sentences (i.e. chuncks of text,
containing words separated by whitespace, and terminated by a period).
Words are identified by their sequence number in a sentence (the 'WordId').

Termination by a period is appropriate for proze. Alternatively, for poetry you
could use stanzas (i.e. chunks of text separated by lines).

-}

module Data.Sentence (
  WordId,
  Word(..),
  Sentence(..),
  Data.Sentence.null,
  makeSentence, fromWords,
  toWords, wordCount, sentenceText, wordNr,
  splitLines, splitSentence
)
where

import qualified Data.Array as A
import qualified Data.Text as T
import           Prelude hiding (Word,words)

-- | Sequence number identifying a word
type WordId = Int

-- | A word.
--
-- Words are pieces of 'T.Text', identified by a 'WordId' and should not
-- contain any whitespace.
data Word = Word {
  wordId   :: WordId,
  wordText :: T.Text
} deriving (Show,Read,Eq,Ord)

-- | A sentence is an array of 'Word's.
newtype Sentence = Sentence {
  unSentence :: A.Array Int Word
} deriving Show

-- | Return true if the Sentence contains no words.
null :: Sentence -> Bool
null s = wordCount s == 0

-- | Create a sentence from a 'T.Text', by splitting the text into words.
makeSentence :: T.Text -> Sentence
makeSentence text = Sentence (A.listArray (1,n) ws) where
  ws = zipWith Word [1..] (T.words (removeTrailingFullStops text))
  n = length ws

-- | Create a sentence from a list of words.
fromWords :: [T.Text] -> Sentence
fromWords ws = Sentence (A.listArray (1,n) words) where
  words = zipWith Word [1..] ws
  n = length ws

removeTrailingFullStops :: T.Text -> T.Text
removeTrailingFullStops = go where
  go sentence = maybe sentence go (T.stripSuffix (T.pack ".") sentence)

-- | The number of words in this sentence.
wordCount :: Sentence -> Int
wordCount = snd . A.bounds . unSentence

-- | Look up a word in a sentence by its 'WordId'
wordNr :: WordId -> Sentence -> Maybe Word
wordNr i (Sentence arr)
  | lo <= i && i <= hi = Just (arr A.! i)
  | otherwise = Nothing
  where (lo,hi) = A.bounds arr

-- | Return a list of all the words in the sentence, ordered ascendingly by
-- their 'WordId'.
toWords :: Sentence -> [Word]
toWords = A.elems . unSentence

-- | Turn a sentence into a 'T.Text' by concatenating all words with spaces in
-- between.
--
-- The following property should hold for all sentences @s@
--
-- prop> makeSentence . sentenceText = id
--
-- But the reverse propety does not necessarily hold, because full stops and
-- spaces may be deleted.
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
splitSentence = splitLines 80 (T.length . wordText) . toWords

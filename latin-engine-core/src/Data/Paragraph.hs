{- |

Module:      Data.Paragraph
Description: A data type for sequences of sentences
Maintainer:  alexander.vandenbroucke@gmail.com

A module for parsing a paragraph into individual sentences.

A 'Paragraph' is a 'Z.Zipper' of 'S.Sentence's.
Sentences are separated by whitespace. This is important, since sentences can
contain periods (e.g., in initials), so a period does not necessarily signal
the end of a sentence, a period followed by whitespace does.
-}

module Data.Paragraph (
  Paragraph,
  fromText, toText, Data.Paragraph.readFile
)
where

import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Data.Sentence as S
import Data.Maybe (fromMaybe)

-- | A Paragraph is a 'Z.Zipper' of 'S.Sentence's.
type Paragraph = [S.Sentence]


-- | Parse a 'T.Text'. into a paragraph.
fromText :: T.Text -> Paragraph
fromText =
  filter (not . S.null)
  . map S.makeSentence
  . T.splitOn (T.pack ".")

-- | Remove trailing new lines from the end of a 'T.Text'.
removeTrailingNewline :: T.Text -> T.Text
removeTrailingNewline sentence =
  fromMaybe sentence (T.stripSuffix (T.pack "\n") sentence)


-- | Turn a 'Paragraph' into text, adding the appropriate whitespaces between
-- sentences.
-- Note that is may not be an exact inverse of 'fromText',
-- empty lines and newlines may be removed.
toText :: Paragraph -> T.Text
toText = T.intercalate (T.pack " ") . map S.sentenceText
-- note, with the current representation, this could be quite inefficient,
-- since the text is already there in memory and sentences are guaranteed to
-- be contiguous.

-- | Read and parse a file.
readFile :: FilePath -> IO Paragraph
readFile = fmap fromText . T.readFile

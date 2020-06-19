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
  initText,cursorText,tailText,
  initSync, zipperWith,
  fromText, toText, Data.Paragraph.readFile
)
where

import qualified Data.List.Zipper as Z
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Data.Sentence as S

-- | A Paragraph is a 'Z.Zipper' of 'S.Sentence's.
type Paragraph = Z.Zipper S.Sentence

-- | The text above the cursor.
initText :: Paragraph -> T.Text
initText = toText . initp

-- | The text of the sentence under the cursor,or empty if there is no such
-- sentence.
cursorText :: Paragraph -> T.Text
cursorText = maybe T.empty S.sentenceText . Z.safeCursor

-- | The text below the cursor.
tailText :: Paragraph -> T.Text
tailText = toText . tailp

-- | Parse a 'T.Text'. into a paragraph.
fromText :: T.Text -> Paragraph
fromText =
  Z.fromList
  . filter (not . S.null)
  . map (S.makeSentence . (<> T.pack ".") . removeTrailingNewline)
  . T.splitOn (T.pack ". ")

-- | Remove trailing new lines from the end of a 'T.Text'.
removeTrailingNewline :: T.Text -> T.Text
removeTrailingNewline sentence =
  maybe sentence id (T.stripSuffix (T.pack "\n") sentence)


-- | Turn a 'Paragraph' into text, adding the appropriate whitespaces between
-- sentences.
-- Note that is may not be an exact inverse of 'fromText',
-- empty lines and newlines may be removed.
toText :: Paragraph -> T.Text
toText = T.intercalate (T.pack " ") . map S.sentenceText . Z.toList
-- note, with the current representation, this could be quite inefficient,
-- since the text is already there in memory and sentences are guaranteed to
-- be contiguous.

-- | Read and parse a file.
readFile :: FilePath -> IO Paragraph
readFile = fmap fromText . T.readFile

-- | Return all elements in the zipper to the left of the cursor
initp :: Z.Zipper a -> Z.Zipper a
initp (Z.Zip [] _) = Z.Zip [] []
initp (Z.Zip (x:xs) _) = Z.Zip xs [x]

-- | Return all elements in the zipper to the right of the cursor
tailp :: Z.Zipper a -> Z.Zipper a
tailp (Z.Zip _ []) = Z.Zip [] []
tailp (Z.Zip _ (_:xs)) = Z.Zip [] xs

-- | Move a zipper such that it has the same number of elements to the left
-- of the cursor as another zipper.
initSync :: Z.Zipper a -> Z.Zipper b -> Z.Zipper a
initSync za zb = go (Z.start za) zb where
  go z z'
    | Z.beginp z' = z
    | otherwise = go (Z.right z) (Z.left z')

-- | Like 'zipWith' for lists.
zipperWith :: (a -> b -> c) -> Z.Zipper a -> Z.Zipper b -> Z.Zipper c
zipperWith f (Z.Zip as as') (Z.Zip bs bs') =
  Z.Zip (zipWith f as bs) (zipWith f as' bs')

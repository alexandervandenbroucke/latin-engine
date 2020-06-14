-- | A module for parsing a paragraph into individual sentences.
--
-- A sentence is recognised as a string of characters, not containing a period
-- followed by a space, terminated by a full stop and a space (except the
-- last sentence). The terminating full stop is not included in the sentence.

module Data.Sentences (
  Sentences,  
  initText,cursorText,tailText,
  initSync, zipperWith,
  fromText, toText, Data.Sentences.readFile
)
where

import qualified Data.List.Zipper as Z
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Data.Sentence as S


type Sentences = Z.Zipper S.Sentence

initText :: Sentences -> T.Text
initText = toText . initp

cursorText :: Sentences -> T.Text
cursorText = maybe T.empty S.sentenceText . Z.safeCursor

tailText :: Sentences -> T.Text
tailText = toText . tailp

fromText :: T.Text -> Sentences
fromText =
  Z.fromList
  . filter (not . S.null)
  . map (S.makeSentence . removeTrailingFullStop . removeTrailingNewline)
  . T.splitOn (T.pack ". ")

removeTrailingFullStop :: T.Text -> T.Text
removeTrailingFullStop sentence =
  maybe sentence id (T.stripSuffix (T.pack ".") sentence)

removeTrailingNewline :: T.Text -> T.Text
removeTrailingNewline sentence =
  maybe sentence id (T.stripSuffix (T.pack "\n") sentence)


-- | Note that is may not be an exact inverse of 'fromText', a final period
-- can be added, empty lines may be removed.
toText :: Sentences -> T.Text
toText = T.intercalate (T.pack " ") . map S.sentenceText . Z.toList
-- note, with the current representation, this could be quite inefficient,
-- since the text is already there in memory and sentences are guaranteed to
-- be contiguous.

readFile :: String -> IO Sentences
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

-- | Like 'zipWith' for lists
zipperWith :: (a -> b -> c) -> Z.Zipper a -> Z.Zipper b -> Z.Zipper c
zipperWith f (Z.Zip as as') (Z.Zip bs bs') =
  Z.Zip (zipWith f as bs) (zipWith f as' bs')

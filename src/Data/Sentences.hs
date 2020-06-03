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

import Data.Sentence


type Sentences = Z.Zipper Sentence

initText :: Sentences -> T.Text
initText = toText . initp

cursorText :: Sentences -> T.Text
cursorText = maybe T.empty sentenceText . Z.safeCursor

tailText :: Sentences -> T.Text
tailText = toText . tailp

fromText :: T.Text -> Sentences
fromText = Z.fromList . map makeSentence . T.splitOn (T.pack ".")

-- | Note that is may not be an exact inverse of 'fromText', a final period
-- can be added.
toText :: Sentences -> T.Text
toText = T.concat . map ((<> T.pack ".") . sentenceText) . Z.toList
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

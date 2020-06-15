{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}

module Hack where

import           Control.Monad (foldM)
import qualified Data.IntMap as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Prelude hiding (Word)
import           Text.Read (readMaybe)

type Corpus = [Sentence]
type Sentence = [Word]
data Word = Word Int T.Text deriving (Show,Read)

instance Eq Word where
  Word n1 _ == Word n2 _ = n1 == n2

instance Ord Word where
  Word n1 _ <= Word n2 _ = n1 <= n2

sentences :: T.Text -> Corpus
sentences txt =
  let rawSentences = map T.words (T.splitOn (T.pack ".") txt)
  in map (zipWith Word [1..]) rawSentences

readCorpus :: FilePath -> IO Corpus
readCorpus filePath = fmap sentences (T.readFile filePath)

splitLines :: Int -> T.Text -> [T.Text] -> [T.Text]
splitLines _ line [] = [line]
splitLines n line (txt:txts)
  | n + T.length txt > 80 = line : splitLines 0 (T.pack "") (txt:txts)
  | otherwise = splitLines newLength newLine txts where
      newLine = line <> txt
      newLength = T.length newLine

statusLine :: Word -> Status -> T.Text
statusLine (Word nr _) Clear = T.pack (show nr)
statusLine (Word nr _) Root  = T.pack (show nr ++ "R")
statusLine (Word nr _) (Child root) = T.pack (show nr ++ "C" ++ show root)

printSentence :: Forest -> Sentence -> IO ()
printSentence tree sentence =
  let pad n txt = txt <> T.replicate (n - T.length txt) (T.pack " ")
      padWord word@(Word n txt) = (pad l txt,pad l status) where
        status = statusLine word (statusOf word tree) 
        l = max (T.length status) (T.length txt) + 1
      putLine line = do
        let (txt,nums) = unzip line
        T.putStrLn (T.concat txt)
        T.putStrLn (T.concat nums)
      (words,numbers) = unzip (map padWord sentence)
      wordLines = splitLines 0 (T.pack "") words
      numberLines = splitLines 0 (T.pack "") numbers
      interleave [] ys = ys
      interleave (x:xs) ys = x:interleave ys xs
  in mapM_ T.putStrLn (interleave wordLines numberLines)

wordNr :: Int -> Sentence -> Maybe Word
wordNr n sentence
  | (w:_) <- filter checkNr sentence = Just w
  | otherwise = Nothing
  where checkNr (Word i _) = n == i

-------------------------------------------------------------------------------
-- Forest data type

-- \ Status of a Word
data Status = Root | Child Int | Clear

-- | A forest  of parse trees.
--
-- A word is in tree possible states:
--
-- * Root: the tree is considered a parse tree root, this is indicated by
-- the word being present in its own set of children
--
-- * Child: the word is the child of some other word
--
-- * Clear: the word is not yet included in the forest.
newtype Forest = Forest (M.IntMap (S.Set Word)) deriving (Show,Read,Eq,Ord)

emptyForest :: Forest
emptyForest = Forest M.empty

statusOf :: Word -> Forest -> Status
statusOf word@(Word nr _) (Forest forest)
  | word `S.member` M.findWithDefault S.empty nr forest = Root
  | (k:_) <- M.keys (M.filter (word `S.member`) forest) = Child k
  | otherwise                                           = Clear

addRoot :: Word -> Forest -> Forest
addRoot word@(Word n _) (Forest forest) = Forest $
  -- set root status
  M.insertWith S.union n (S.singleton word) $
  -- remove word as a child in other trees
  M.map (S.delete word) forest

addChild :: Word -> Word -> Forest -> Forest
addChild word@(Word childId _) (Word rootId _) (Forest forest) = Forest $
  M.insertWith S.union rootId (S.singleton word) $
  -- remove from other roots
  M.map (S.delete word) forest
  -- does not check for cycles

roots :: Forest -> S.Set Word
roots (Forest forest) = S.unions $ M.elems $ M.mapWithKey go forest where
  go k s = S.filter (\(Word n _) -> n == k) s

children :: Word -> Forest -> S.Set Word
children word@(Word rootId _) (Forest forest) =
  S.delete word (M.findWithDefault S.empty rootId forest)

-- | Clear has no effect if the word has children, otherwise it clears the
-- root & child status of the word.
clear :: Word -> Forest -> Forest
clear word@(Word n _) forest@(Forest m)
  | S.null (children word forest) =
      -- unchild from other words & remove root status
      Forest (M.map (S.delete word) m)
  | otherwise = forest


descendents :: Word -> Forest -> S.Set Word
descendents root forest = go (S.singleton root) where
  go :: S.Set Word -> S.Set Word
  go s =
    let s' = S.unions [children w forest | w <- S.elems s]
    in if s == s' then s else go s'

serialise :: Forest -> T.Text
serialise (Forest forest) =
  let serialiseNode (n,cs) =
        T.unwords $ map (T.pack . show) (n:[m | Word m _ <- S.toList cs])
  in T.unlines $ map serialiseNode $ M.toList forest

deserialise :: Sentence -> T.Text -> Maybe Forest
deserialise sentence =
  let wordNr' n = wordNr n sentence
      readMaybeText = readMaybe . T.unpack  
      deserialiseLine :: T.Text -> Maybe (Int,S.Set Word)
      deserialiseLine (mapM readMaybeText . T.words -> Just (root:children)) =
        fmap ((root,) . S.fromList) (mapM wordNr' children)
      deserialiseLine _ = Nothing
  in fmap (Forest . M.fromList) . mapM deserialiseLine . T.lines

-------------------------------------------------------------------------------
-- CLI

data Command
  = AddRoot Int
  | AddChild Int Int
  | ClearStatus Int
  | PrintForest
  | Quit

getCommand :: IO Command
getCommand = do
  putStr "> "
  str <- getLine
  case words str of
    ["root",readMaybe -> Just n]                      -> return (AddRoot n)
    ["child",readMaybe -> Just c,readMaybe -> Just r] -> return (AddChild c r)
    ["clear",readMaybe -> Just w]                     -> return (ClearStatus w)
    ["tree"]                                          -> return PrintForest
    ["quit"]                                          -> return Quit
    _ -> putStrLn "invalid command" >> getCommand

edit :: Sentence -> Forest -> IO Forest
edit sentence = go where
  wordNr' n = wordNr n sentence
  go tree = do
    printSentence tree sentence
    c <- getCommand
    case c of
      AddRoot (wordNr' -> Just r) -> go (addRoot r tree)
      AddChild (wordNr' -> Just c) (wordNr' -> Just r) -> go (addChild c r tree) 
      ClearStatus (wordNr' -> Just w) -> go (clear w tree)
      PrintForest -> print tree >> go tree
      Quit -> return tree
      _ -> putStrLn "Invalid word number." >> go tree

{-# LANGUAGE ViewPatterns  #-}
{-# LANGUAGE TupleSections #-}

module Data.Forest (
  Status(Root,Child,Clear),
  Forest(..),
  emptyForest,
  statusOf,
  addRoot,
  addChild,
  clear,
  roots,
  children,
  descendents,
  serialise,
  deserialise)
where

import           Control.Monad ((>=>))
import qualified Data.IntMap as M
import           Data.Sentence
import qualified Data.Set as S
import qualified Data.Text as T
import           Prelude hiding (Word)
import           Text.Read (readMaybe)

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
addChild word (Word rootId _) (Forest forest) = Forest $
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
clear word forest@(Forest m)
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
        (n:[m | Word m _ <- S.toList cs])
  in T.pack $ show $ map serialiseNode $ M.toList forest

deserialise :: Sentence -> T.Text -> Maybe Forest
deserialise sentence =
  let wordNr' n = wordNr n sentence
      readMaybeText = readMaybe . T.unpack  
      deserialiseLine :: [Int] -> Maybe (Int,S.Set Word)
      deserialiseLine (root:cs) =
        fmap ((root,) . S.fromList) (mapM wordNr' cs)
      deserialiseLine _ = Nothing
  in readMaybeText >=> mapM deserialiseLine >=> return . Forest . M.fromList

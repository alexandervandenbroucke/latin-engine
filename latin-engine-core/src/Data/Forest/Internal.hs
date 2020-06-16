module Data.Forest.Internal
  (
    -- * Data Types
    WordIdSet,
    Forest(..),
    Status(..),
    InternalStatus(..),
    inject,
    -- * Querying
    statusOf,
    roots,
    children,
    descendents,
    -- * Creating and modifying
    emptyForest,
    setRoot,
    addChild,
    unsafeClear,
    clear
  )
where

import qualified Data.IntMap as M
import qualified Data.IntSet as S
import           Data.Sentence (WordId)

data Status = Clear | Root | Child !WordId deriving (Eq,Ord,Show,Read)

data InternalStatus = IRoot | IChild !WordId deriving (Eq,Ord,Show,Read)

type WordIdSet = S.IntSet

-- | Inject 'InternalStatus' into 'Status'.
--
-- Map an internal status onto the corresponding external status.
--
-- More formally, the former can be isomorphically embeded into the latter.
inject :: InternalStatus -> Status
inject IRoot = Root
inject (IChild wordId) = Child wordId

-- | A Forest of parse trees
--
-- The forest is represented bottom-up: children
-- point to their parents.
--
-- Each record is either 'IRoot', a parse-tree root, or 'IChild' the child of
-- some other node.
--
newtype Forest = Forest (M.IntMap InternalStatus) deriving (Show,Read,Eq,Ord)

emptyForest :: Forest
emptyForest = Forest M.empty

-- | The status of a word
-- A word is in tree possible states:
--
-- * Root: the tree is considered a parse tree root, this is indicated by
-- the word being present in its own set of children
--
-- * Child: the word is the child of some other word
--
-- * Clear: the word is not yet included in the forest.
statusOf :: Forest -> WordId -> Status
statusOf (Forest m) wordId = maybe Clear inject (M.lookup wordId m)

setRoot :: WordId -> Forest -> Forest
setRoot wordId (Forest m) = Forest (M.insert wordId IRoot m)

addChild :: WordId -> WordId -> Forest -> Forest
addChild childId parentId (Forest m) =
  Forest (M.insert childId (IChild parentId) m)

unsafeClear :: WordId -> Forest -> Forest
unsafeClear wordId (Forest m) = Forest (M.delete wordId m)

-- | Clear has no effect if the word has children, otherwise it clears the root
-- or child status of the word.
clear :: WordId -> Forest -> Forest
clear wordId forest
  | S.null (children wordId forest) = unsafeClear wordId forest
  | otherwise = forest

roots :: Forest -> WordIdSet
roots (Forest m) = S.fromList $ M.keys $ M.filter (== IRoot) m

children :: WordId -> Forest -> WordIdSet
children wordId (Forest m) =
  S.fromList $ M.keys $ M.filter (== IChild wordId) m

descendents :: WordId -> Forest -> WordIdSet
descendents root forest = go (S.singleton root) where
  go s =
    let s' = S.unions [children w forest | w <- S.elems s]
    in if s == s' then s else go s'

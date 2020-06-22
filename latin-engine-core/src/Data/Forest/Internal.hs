{- |

Module:      Data.Forest.Internal
Description: Internals for forests of parse trees
Maintainer:  alexander.vandenbroucke@gmail.com

Internals of the 'Forest' data type.

Internally, a 'Forest' of parse trees is represented as a map indexed by
'WordId'. This module defines functions to query, create and modify such maps.

-}

{-# LANGUAGE DeriveGeneric #-}

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
    nonClear,
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
import           GHC.Generics

-- | The external status of a word in the parse-tree forest.
--
-- A word is either 'Clear' (no status has been set), a 'Root' of a parse tree
-- in the forest, or a 'Child' of some other word.
data Status = Clear | Root | Child !WordId deriving (Eq,Ord,Show,Read,Generic)

-- | The internal status of a word. This is the data type that is stored in
-- the map. Unlike 'Status', there is no explicit representation of 'Clear'.
-- Cleared words simply do not have an entry in the 'Forest''s map.
data InternalStatus = IRoot | IChild !WordId deriving (Eq,Ord,Show,Read)

-- | A Set of 'WordId's.
type WordIdSet = S.IntSet

-- | Inject 'InternalStatus' into 'Status'.
--
-- Map an internal status onto the corresponding external status.
--
-- More formally, the former can be isomorphically embeded into the latter.
inject :: InternalStatus -> Status
inject IRoot = Root
inject (IChild wordId) = Child wordId

-- | A Forest of parse trees.
newtype Forest = Forest (M.IntMap InternalStatus) deriving (Show,Read,Eq,Ord)

-- The forest is represented bottom-up: children point to their parents.
--
-- Each record in the map is either 'IRoot', a parse-tree root, or
-- 'IChild' the child of some other node.

-- | A forest containing no trees.
emptyForest :: Forest
emptyForest = Forest M.empty

-- | Look up the status of a word.
--
-- A word is in one of tree possible states:
--
-- * 'Root': the tree is considered a parse tree root, this is indicated by
-- the word being present in its own set of children
--
-- * 'Child': the word is the child of some other word
--
-- * 'Clear': the word is not yet included in the forest.
statusOf :: Forest -> WordId -> Status
statusOf (Forest m) wordId = maybe Clear inject (M.lookup wordId m)

-- | Turn the word into a new parse-tree root.
setRoot :: WordId -> Forest -> Forest
setRoot wordId (Forest m) = Forest (M.insert wordId IRoot m)

-- | Add the first word as a child to the second word.
addChild :: WordId -> WordId -> Forest -> Forest
addChild childId parentId (Forest m) =
  Forest (M.insert childId (IChild parentId) m)

-- | Delete a word's entry from the 'Forest' map.
--
-- It does NOT check if the word has children in the forest, and may thus lead
-- to dangling orphans, 'clear' is more likely to be what you want.
unsafeClear :: WordId -> Forest -> Forest
unsafeClear wordId (Forest m) = Forest (M.delete wordId m)

-- | Clear has no effect if the word has children, otherwise it clears the root
-- or child status of the word.
clear :: WordId -> Forest -> Forest
clear wordId forest
  | S.null (children wordId forest) = unsafeClear wordId forest
  | otherwise = forest

-- | Get the roots of all the parse trees in the forest.
roots :: Forest -> WordIdSet
roots (Forest m) = S.fromList $ M.keys $ M.filter (== IRoot) m

-- | Get all the children of a word in the forest.
children :: WordId -> Forest -> WordIdSet
children wordId (Forest m) =
  S.fromList $ M.keys $ M.filter (== IChild wordId) m

-- | Get all transitive children of a word in the forest.
descendents :: WordId -> Forest -> WordIdSet
descendents root forest = go (S.singleton root) where
  go s =
    let s' = S.unions [children w forest | w <- S.elems s]
    in if s == s' then s else go s'

-- | Get all the wordIds that are not clear
nonClear :: Forest -> WordIdSet
nonClear (Forest forest) = S.fromList (M.keys forest)

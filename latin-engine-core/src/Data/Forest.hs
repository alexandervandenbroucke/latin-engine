{- |

Module:      Data.Forest
Description: Forests of parse trees
Maintainer:  alexander.vandenbroucke@gmail.com

This module defines a data structure and functions for working with a
collection of parse trees (i.e., a 'Forest').
Such forests reflect the grammatical structure of a sentence. Sentences can
have multiple parse trees, but allowing multiple parse trees is also useful
for incrementally parsing a sentence, building up several parse trees in a
bottom-up fashion.

This module also provides a functions for serialising and deserialising a
parse tree to a ".fst" file format.

-}
{-# LANGUAGE ViewPatterns  #-}
{-# LANGUAGE TupleSections #-}

module Data.Forest (
  -- * Data Types
  Status(Root,Child,Clear),
  Forest,
  -- * Queries
  statusOf,
  roots,
  children,
  descendents,
  nonClear,
  -- * Creating and Modifying
  emptyForest,
  setRoot,
  addChild,
  clear,
  -- * Serialisation
  serialise,
  deserialise,
  serialiseForests,
  deserialiseForests
  )
where

import           Data.Forest.Internal
import qualified Data.IntMap as M
import qualified Data.Text as T
import           Prelude hiding (Word)
import           Text.Read (readMaybe)

{-# DEPRECATED serialise, deserialise, serialiseForests, deserialiseForests
    ["use latin-engine-json package instead"] #-}

-- | Serialise a 'Forest' to a 'T.Text'.
serialise :: Forest -> T.Text
serialise (Forest forest) = T.pack $ show $ M.toList forest

-- | Deserialise a 'Forest' from a 'T.Text'.
deserialise :: T.Text -> Maybe Forest
deserialise = fmap (Forest . M.fromList) . readMaybe . T.unpack

-- | Serialise a list of forests to a 'T.Text'.
serialiseForests :: [Forest] -> T.Text
serialiseForests = T.pack . show

-- | Deserialise a list of forests to a 'T.Text'.
deserialiseForests :: T.Text -> Maybe [Forest]
deserialiseForests = readMaybe . T.unpack

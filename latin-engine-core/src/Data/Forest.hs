{-# LANGUAGE ViewPatterns  #-}
{-# LANGUAGE TupleSections #-}

module Data.Forest (
  -- * Data Types
  Status(Root,Child,Clear),
  Forest,
  statusOf,
  roots,
  children,
  descendents,
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

import           Control.Monad ((>=>))
import           Data.Forest.Internal
import qualified Data.IntMap as M
import qualified Data.Text as T
import           Prelude hiding (Word)
import           Text.Read (readMaybe)

serialise :: Forest -> T.Text
serialise (Forest forest) = T.pack $ show $ M.toList forest

deserialise :: T.Text -> Maybe Forest
deserialise = fmap (Forest . M.fromList) . readMaybe . T.unpack

serialiseForests :: [Forest] -> T.Text
serialiseForests = T.pack . show . map serialise

deserialiseForests :: T.Text -> Maybe [Forest]
deserialiseForests = readMaybe . T.unpack >=> mapM deserialise

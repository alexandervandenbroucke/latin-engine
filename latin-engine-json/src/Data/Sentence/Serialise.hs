{- |

Module:      Data.Sentence.Serialise
Description: Serialisation of Sentences
Maintainer:  alexander.vandenbroucke@gmail.com

This module provides functions for serialising maps of 'S.WordId's.

-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Sentence.Serialise (
  WordIdMap(..),
  FromWordIdMap(..), ToWordIdMap(..),
  wordIdMapsToFile,
  wordIdMapsFromFile,
  writeFile, readFile,
  FromJSON,ToJSON
)
where

import           Data.Aeson
import qualified Data.IntMap as M
import qualified Data.Sentence as S
import qualified Data.Text as T
import           Prelude hiding (writeFile,readFile)

-- | A Wrapper around maps of 'S.WordId's
newtype WordIdMap a = WordIdMap {
  unWordIdMap :: M.IntMap a
} deriving (ToJSON,FromJSON,Eq,Ord,Show)

-- | Turn a @'WordIdMap' ('Src' a)@ into an @a@.
class FromWordIdMap a where
  type Src a
  fromWordIdMap :: WordIdMap (Src a) -> Maybe a

-- | Turn an @a@ into a @'WordIdMap' ('Dst' a)@ .
class ToWordIdMap a where
  type Dst a
  toWordIdMap :: a -> WordIdMap (Dst a)

instance ToWordIdMap (M.IntMap a) where
  type Dst (M.IntMap a) = a
  toWordIdMap = WordIdMap

instance FromWordIdMap (M.IntMap a) where
  type Src (M.IntMap a) = a
  fromWordIdMap = Just . unWordIdMap


instance ToWordIdMap S.Sentence where
  type Dst S.Sentence = T.Text
  toWordIdMap sentence = WordIdMap $ 
    M.fromList [(S.wordId w, S.wordText w) | w <- S.toWords sentence]

-- | Turn a WordIdMap into a 'S.Sentence'.
--
-- The input WordIdMap's keys should be contiguous and should start at 1. If
-- this is not the case, the function will return 'Nothing'.
instance FromWordIdMap S.Sentence where
  type Src S.Sentence = T.Text
  fromWordIdMap (WordIdMap m) = do
    (1,_) <- M.lookupMin m
    (n,_) <- M.lookupMax m
    texts <- mapM (\k -> M.lookup k m) [1..n]
    return (S.fromWords texts)

wordIdMapsToFile :: ToJSON a => FilePath -> [WordIdMap a] -> IO ()
wordIdMapsToFile = encodeFile

wordIdMapsFromFile :: FromJSON a => FilePath -> IO (Maybe [WordIdMap a])
wordIdMapsFromFile = decodeFileStrict

writeFile :: (ToWordIdMap a, ToJSON (Dst a)) => FilePath -> [a] -> IO ()
writeFile fp = wordIdMapsToFile fp . map toWordIdMap

readFile :: (FromWordIdMap a, FromJSON (Src a)) => FilePath -> IO (Maybe [a])
readFile fp = do
  ws <- wordIdMapsFromFile fp
  return (ws >>= mapM fromWordIdMap)

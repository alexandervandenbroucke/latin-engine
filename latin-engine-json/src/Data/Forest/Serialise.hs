{- |

Module:      Data.Forest.Serialise
Description: Serialisation of Forests of parse trees
Maintainer:  alexander.vandenbroucke@gmail.com

The idea is to provide serialisation independent of the internal structure of
'Forest'.

A 'F.Forest' is serialised to a 'WordIdMap' containing 'SerialisableStatus'es.
See "Data.Sentence.Serialise' for more information about 'WordIdMap's.

A 'F.Status' is serialised as a JSON object containing a @type@ field, which
is one of the strings, @"root"@ and @"child"@. If the type is
@"child"@, an additional field @"parent"@ contains the word id of the parent
node.

-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Forest.Serialise (
  SerialisableForest(..),
  SerialisableStatus(..),
  writeForests, readForests
) where

import           Data.Aeson
import qualified Data.Forest as F
import qualified Data.IntSet as S
import           Data.Sentence.Serialise (
  WordIdMap(..),ToWordIdMap(..),FromWordIdMap(..), writeFile, readFile)
import qualified Data.IntMap as M
import qualified Data.Text as T

newtype SerialisableForest = SerialisableForest {
  unSerialisable :: F.Forest
}

data SerialisableStatus = SRoot | SChild Int deriving (Eq,Ord,Show)

instance ToJSON SerialisableStatus where
  toJSON SRoot  = object [ "type" .= T.pack "root" ]
  toJSON (SChild n) = object [ "type" .= T.pack "child", "parent" .= n ]

instance FromJSON SerialisableStatus where
  parseJSON = withObject "Status" $ \v -> do
    ty <- v .: "type"
    case T.unpack ty of
      "root"  -> return SRoot
      "child" -> SChild <$> v .: "parent"
      str -> fail ("unknown status type: " ++ str)

instance ToWordIdMap SerialisableForest where
  type Dst SerialisableForest = SerialisableStatus
  toWordIdMap (SerialisableForest forest) =
    let ids = S.toList (F.nonClear forest)
        status n = case forest `F.statusOf` n of
          F.Root -> SRoot
          F.Child p -> SChild p
          _ -> error "SerialisableForest: unexpected clear value in F.nonClear"
    in WordIdMap (M.fromList [(n,status n) | n <- ids])
    
instance FromWordIdMap SerialisableForest where
  type Src SerialisableForest = SerialisableStatus
  fromWordIdMap (WordIdMap m) =
    let toForest n SRoot = F.setRoot n F.emptyForest
        toForest n (SChild p) = F.addChild n p F.emptyForest
    in Just (SerialisableForest (M.foldMapWithKey toForest m))

writeForests :: FilePath -> [F.Forest] -> IO ()
writeForests fp = Data.Sentence.Serialise.writeFile fp . map SerialisableForest

readForests :: FilePath -> IO (Maybe [F.Forest])
readForests fp =
  fmap (map unSerialisable) <$> Data.Sentence.Serialise.readFile fp



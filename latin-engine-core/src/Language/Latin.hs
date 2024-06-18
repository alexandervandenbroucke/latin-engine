{- |

Module:      Language.Latin
Description: Grammar definitions for Latin.
Maintainer:  alexander.vandenbroucke@gmail.com

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveLift #-}

module Language.Latin where

import           Prelude hiding (reverse)

import qualified Data.List as L
import           Language.Haskell.TH.Syntax (Lift)
import           Language.Parser

-------------------------------------------------------------------------------
-- Data types for declensions

data Declension = I | II | III | IV | V deriving (Eq,Ord,Show,Lift)

data Case = NOM | ACC | GEN | DAT | ABL | VOC deriving (Eq,Ord,Show,Lift)

data Multiplicity = S | P deriving (Eq,Ord,Show,Lift)

data Gender = M | F | N deriving (Eq,Ord,Show,Lift)

data Determination
  = Determination Declension Case Multiplicity Gender
  deriving (Eq,Ord,Show,Lift)

-- | Specify the case of an expression.
case_ :: Case -> Multiplicity -> Gender -> Parser (Case,Multiplicity,Gender)
case_ c m g = Eps (c,m,g)

-- | Determine a noun.
mkDetermination :: Declension -> (Case,Multiplicity,Gender) -> Determination
mkDetermination d (c,m,g) = Determination d c m g

declIII :: Parser Determination
declIII =
  let cases g = mconcat [
        ""              .> case_ NOM S g,
        "em"            .> case_ ACC S g,
        "is"            .> case_ GEN S g,
        "i"             .> case_ DAT S g,
        "e"             .> case_ ABL S g,
        ""              .> case_ VOC S g,
        "es"            .> case_ NOM P g,
        "es"            .> case_ ACC P g,
        ("um" <> "ium") .> case_ GEN P g,
        "ibus"          .> case_ DAT P g,
        "ibus"          .> case_ ABL P g,
        "es"            .> case_ VOC P g]
      cases_N = mconcat [
        "is"            .> case_ GEN S N,
        "i"             .> case_ DAT S N,
        "e"             .> case_ ABL S N,
        ("a" <> "ia")   .> case_ NOM P N,
        ("a" <> "ia")   .> case_ ACC P N,
        ("um" <> "ium") .> case_ GEN P N,
        "ibus"          .> case_ DAT P N,
        "ibus"          .> case_ ABL P N,
        ("a" <> "ia")   .> case_ VOC P N ]
  in mkDetermination III <$> mconcat [
    cases M,
    cases F,
    case_ NOM S N,
    case_ ACC S N,
    case_ VOC S N,    
    cases_N]

declI :: Parser Determination
declI = mkDetermination I <$> mconcat [
  "a"    .> case_ NOM S F,
  "am"   .> case_ ACC S F,
  "ae"   .> case_ GEN S F,
  "ae"   .> case_ DAT S F,
  "a"    .> case_ ABL S F,
  "a"    .> case_ VOC S F,
  "ae"   .> case_ NOM P F,
  "as"   .> case_ ACC P F,
  "arum" .> case_ GEN P F,
  "is"   .> case_ DAT P F,
  "is"   .> case_ ABL P F,
  "ae"   .> case_ VOC P F]

declII :: Parser Determination
declII = mkDetermination II <$> mconcat [
  -- masculine
  ("er" <> "r" <> "us") .> case_ NOM S M,
  "um"   .> case_ ACC S M,
  "i"    .> case_ GEN S M,
  "o"    .> case_ DAT S M,
  "o"    .> case_ ABL S M,
  "e"    .> case_ VOC S M,
  "i"    .> case_ NOM P M,
  "os"   .> case_ ACC P M,
  "orum" .> case_ GEN P M,
  "is"   .> case_ DAT P M,
  "is"   .> case_ ABL P M,
  "i"    .> case_ VOC P M,
  -- neuter
  "um"   .> case_ NOM S N,
  "um"   .> case_ ACC S N,
  "i"    .> case_ GEN S N,
  "o"    .> case_ DAT S N,
  "o"    .> case_ ABL S N,
  "um"   .> case_ VOC S N,
  "a"    .> case_ NOM P N,
  "a"    .> case_ ACC P N,
  "orum" .> case_ GEN P N,
  "is"   .> case_ DAT P N,
  "is"   .> case_ ABL P N,
  "a"    .> case_ VOC P N]
  
declIV :: Parser Determination
declIV = mkDetermination IV <$> mconcat [
  -- masculine
  "us"   .> case_ NOM S M,
  "um"   .> case_ ACC S M,
  "us"   .> case_ GEN S M,
  "ui"   .> case_ DAT S M,
  "u"    .> case_ ABL S M,
  "us"   .> case_ VOC S M,
  "us"   .> case_ NOM P M,
  "us"   .> case_ ACC P M,
  "uum"  .> case_ GEN P M,
  "ibus" .> case_ DAT P M,
  "ibus" .> case_ ABL P M,
  "us"   .> case_ VOC P M,
  -- neuter
  "u"    .> case_ NOM S N,
  "u"    .> case_ ACC S N,
  "us"   .> case_ GEN S N,
  "u"    .> case_ DAT S N,
  "u"    .> case_ ABL S N,
  "u"    .> case_ VOC S N,
  "ua"   .> case_ NOM P N,
  "ua"   .> case_ ACC P N,
  "uum"  .> case_ GEN P N,
  "ibus" .> case_ DAT P N,
  "ibus" .> case_ ABL P N,
  "ua"   .> case_ VOC P N]

declV :: Parser Determination
declV = mkDetermination V <$> mconcat [
  "es"   .> case_ NOM S F,
  "em"   .> case_ ACC S F,
  "ei"   .> case_ GEN S F,
  "ei"   .> case_ DAT S F,
  "e"    .> case_ ABL S F,
  "es"   .> case_ NOM P F,
  "es"   .> case_ ACC P F,
  "erum" .> case_ GEN P F,
  "ebus" .> case_ DAT P F,
  "ebus" .> case_ ABL P F,
  "es"   .> case_ VOC P F]

allDeclensions :: Parser Determination
allDeclensions = declI <> declII <> declIII <> declIV <> declV

reverseAllDeclensions :: Parser Determination
reverseAllDeclensions = reverse $ declI <> declII <> declIII <> declIV <> declV


prettyParses :: [Parser Determination] -> String -> IO ()
prettyParses ps str = do 
  let prettyDet (Determination d c m g) =
        L.intercalate "," [show c, show m, show g] ++ " ("  ++ show d ++ ")"
  let pretty (pre,post,det) =
        putStrLn (pre ++ "|" ++ post ++ " " ++ prettyDet det)
  mapM_ pretty [(pre,post,det) |
   (n,p) <- zip [0..] ps,
   let (pre,post) = splitAt n str,
   det <- epsilon p]

determine :: Parser Determination -> String -> IO ()
determine p0 str =
  let ps = parses (Language.Parser.reverse p0) (Reverse str)
  in prettyParses ps str

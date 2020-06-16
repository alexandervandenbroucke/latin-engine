{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleContexts #-}

module Diagrams.LatinEngine (
  -- * Colour Maps
  ColourMap, makeColourMap, defaultColourMap,
  -- * Making Diagrams of Sentences
  sentencesDiagram  
)
where 

import qualified Data.Forest as F
import           Data.Monoid (Any)
import qualified Data.Sentence as S
import qualified Data.Text as T
import qualified Diagrams.Backend.Rasterific as R
import qualified Diagrams.Prelude as D
import           Lens.Micro ((^.))

-------------------------------------------------------------------------------
-- ColourMap

type ColourMap = Int -> D.Colour Double

makeColourMap :: [D.Colour Double] -> ColourMap
makeColourMap [] _ = error "makeColourMap: emtpy list of colours."
makeColourMap colours i = colours !! (i `mod` length colours)

defaultColourMap :: ColourMap
defaultColourMap =
  makeColourMap [D.green, D.red, D.blue,D.purple,D.orange]

-------------------------------------------------------------------------------
-- Making Diagrams of Sentences

sentencesDiagram
  :: ColourMap -> [S.Sentence] -> [F.Forest] -> D.QDiagram R.B D.V2 Float Any
sentencesDiagram colourMap sentences forests =
  let diagrams = zipWith  sentenceDiagram' forests sentences
      sentenceDiagram' forest sentence =
        childMarkers colourMap forest sentence $
        sentenceDiagram colourMap 1 1 forest sentence
  in D.scale 15 (D.vsep 1 diagrams)

wordDiagram
  :: ColourMap -> F.Forest -> S.Word -> D.QDiagram R.B D.V2 Float Any
wordDiagram colourMap forest word =
  let  txt = R.texterific (T.unpack (S.wordText word))
       colour = colourMap (S.wordId word)
       marker
         | F.Root <- forest `F.statusOf` S.wordId word =
             D.bgFrame 0.1 colour (D.bgFrame 0.1 D.white txt)
         | otherwise =
             txt
  in marker D.# D.centerX D.# D.named (S.wordId word) D.# D.alignL

childMarkers
  :: ColourMap
  -> F.Forest
  -> S.Sentence
  -> D.QDiagram R.B D.V2 Float Any
  -> D.QDiagram R.B D.V2 Float Any
childMarkers colourMap forest sentence d =
  let statusOf word = (n,forest `F.statusOf` n) where n = S.wordId word
      children = [(w,p) | (w,F.Child p) <- map statusOf (S.words sentence)]
      attach (c,p) = D.withNames [c,p] $ \[cDiagram, pDiagram] ->
        -- the evenness of the root determines if an arc is drawn on top
        -- or below.
        let location d
              | even p    = D.boundaryFrom d D.unit_Y
              | otherwise = D.boundaryFrom d D.unitY
            p1 .^. p2 =
              let h = if even p then -1 else 1 * (1 - 0.7*(10 - r)/10)
                  -- the idea is to make longer arcs higher, needs some work
                  c1 = D.r2 (0,h)
                  c2 = D.r2 (0,h) D.^+^ v
                  v  = p2 D..-. p1
                  r  = min (v^.D._r) 10
              in (D.fromSegments [D.bezier3 c1 c2 v]) `D.place` p1
            arc = location cDiagram .^. location pDiagram
        in (`D.atop` arc D.# D.lc (colourMap p))
  in foldr attach d children

sentenceDiagram
  :: ColourMap
  -> Float
  -> Float
  -> F.Forest
  -> S.Sentence
  -> D.QDiagram R.B D.V2 Float Any
sentenceDiagram colourMap hSep vSep forest =
  D.vsep vSep .
  map (D.hsep hSep . map (wordDiagram colourMap forest)) .
  S.splitSentence

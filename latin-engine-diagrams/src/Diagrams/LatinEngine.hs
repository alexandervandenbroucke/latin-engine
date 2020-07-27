{- |

Module:      Diagrams.LatinEngine
Description: A module for rendering forests of parse trees of sentences
Maintainer:  alexander.vandenbroucke@gmail.com

This module implements routines for rendering a 'S.Sentence' and its
accompanying 'F.Forest' to a 'D.Diagram'.

-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleContexts #-}

module Diagrams.LatinEngine (
  -- * Colour Maps
  ColourMap, makeColourMap, defaultColours,
  -- * Config
  Config(..), coloursL, paragraphSkipL, lineSkipL, wordSkipL, scaleL,
  -- * Making Diagrams of Sentences
  sentencesDiagram
)
where

import           Control.Monad (foldM)
import qualified Data.Forest as F
import qualified Data.IntMap as M
import           Data.Monoid (Any)
import qualified Data.Sentence as S
import qualified Data.IntSet as S
import qualified Data.Text as T
import qualified Diagrams.Backend.Rasterific as R
import qualified Diagrams.Prelude as D
import           Lens.Micro (Lens',lens,(^.))

import qualified Diagrams.LineBreaking as LB

-------------------------------------------------------------------------------
-- ColourMap

-- | A function from ints to colours.
type ColourMap = Int -> D.Colour Double

-- | Make a colour map from a finite list of colours.
-- The colours are wrapped around when reaching the end of the list.
makeColourMap :: [D.Colour Double] -> ColourMap
makeColourMap [] _ = error "makeColourMap: emtpy list of colours."
makeColourMap colours i = colours !! (i `mod` length colours)

-- | A default colour map containing green, red, blue, purple and orange.
defaultColours :: [D.Colour Double]
defaultColours = [D.green, D.red, D.blue,D.purple,D.orange]

-- | A colour map created by graph-colouring the parse forest.
--
-- The function returns 'Nothing' if the parse forest is not actually
-- a forest (e.g. if a cycle or forward edge is found).
--
-- Note: at least two colours must be provided.
--
-- Note: words that are 'F.Clear' will default to 'D.black'.
forestColourMap :: [D.Colour Double] -> F.Forest -> Maybe ColourMap
forestColourMap colours forest = do
  let colour = zip (cycle colours)
      go m (c,r)
        | M.member r m = Nothing
        | otherwise = foldM go (M.insert r c m) cs where
            permissible = filter (/= c) colours
            cs = zip (cycle permissible) (S.toList $ F.children r forest)
  m <- foldM go M.empty (colour (S.toList $ F.roots forest))
  return (\i -> M.findWithDefault D.black i m)

      

-------------------------------------------------------------------------------
-- Making Diagrams of Sentences

-- | Parameters for rendering a sentence and parse tree.
data Config = Config {
  _confColours       :: [D.Colour Double], -- ^ colour map for roots and arcs
  _confParagraphSkip :: Float,             -- ^ space between paragaphs
  _confLineSkip      :: Float,             -- ^ space between sentences
  _confWordSkip      :: Float,             -- ^ space between words
  _confScale         :: Float              -- ^ scaling factor of the final
                                           --   diagram
}

-- | The default config has the 'defaultColourMap', skips 2, 2 and 1, and
-- a scale factor of 15.
instance D.Default Config where
  def = Config defaultColours 2 2 1 15

-- | Lens to the 'Config's 'ColourMap'
coloursL :: Lens' Config [D.Colour Double]
coloursL = lens _confColours (\conf m -> conf{_confColours = m})

-- | Lens to the 'Config's paragraph skip.
paragraphSkipL :: Lens' Config Float
paragraphSkipL =
  lens _confParagraphSkip (\conf skip -> conf{_confParagraphSkip = skip})

-- | Lens to the 'Config's line skip.
lineSkipL :: Lens' Config Float
lineSkipL =
  lens _confLineSkip (\conf skip -> conf{_confLineSkip = skip})

-- | Lens to the 'Config's word skip.
wordSkipL :: Lens' Config Float
wordSkipL =
  lens _confWordSkip (\conf skip -> conf{_confWordSkip = skip})

-- | Lens to the 'Config's scale factor.
scaleL :: Lens' Config Float
scaleL =
  lens _confScale (\conf scale -> conf{_confScale = scale})

-- | Render a set of sentences and corresponding forests.
sentencesDiagram
  :: Config -> [S.Sentence] -> [F.Forest] -> D.QDiagram R.B D.V2 Float Any
sentencesDiagram conf sentences forests =
  let diagrams = zipWith  sentenceDiagram' forests sentences
      colourMap = maybe (const D.black) id . forestColourMap (conf^.coloursL)
      sentenceDiagram' forest sentence =
        childMarkers (colourMap forest) forest sentence $
        sentenceDiagram (colourMap forest) conf forest sentence
  in D.scale (conf^.scaleL) (D.vsep (conf^.paragraphSkipL) diagrams)

-- | Render a single word.
--
-- The diagram of the word is named with its word id.
wordDiagram
  :: ColourMap -> F.Forest -> S.Word -> D.QDiagram R.B D.V2 Float Any
wordDiagram colourMap forest word =
  let  txt = R.texterific (T.unpack (S.wordText word))  D.# D.fc colour
       colour = colourMap (S.wordId word)
       marker
         | F.Root <- forest `F.statusOf` S.wordId word =
             D.bgFrame 0.1 colour (D.bgFrame 0.1 D.white txt)
         | otherwise =
             txt
  in marker D.# D.centerX D.# D.named (S.wordId word) D.# D.alignL

-- | Overlay (underlay really) markers the arcs on the diagram of a sentence.
--
-- This function assumes that the diagram of each word has been named with its
-- id.
childMarkers
  :: ColourMap
  -> F.Forest
  -> S.Sentence
  -> D.QDiagram R.B D.V2 Float Any
  -> D.QDiagram R.B D.V2 Float Any
childMarkers colourMap forest sentence d =
  let statusOf word = (n,forest `F.statusOf` n) where n = S.wordId word
      children = [(w,p) | (w,F.Child p) <- map statusOf (S.toWords sentence)]
      attach (c,p) = D.withNames [c,p] $ \[cDiagram, pDiagram] ->
        -- the evenness of the root determines if an arc is drawn on top
        -- or below.
        let location d
              | even p    = D.boundaryFrom d D.unit_Y
              | otherwise = D.boundaryFrom d D.unitY
            p1 .^. p2 =
              let h = (if even p then -1 else 1) * (1 - 0.7*(10 - r)/10)
                  -- the idea is to make longer arcs higher, needs some work
                  c1 = D.r2 (0,h)
                  c2 = D.r2 (0,h) D.^+^ v
                  v  = p2 D..-. p1
                  r  = min (v^.D._r) 10
              in (D.fromSegments [D.bezier3 c1 c2 v]) `D.place` p1
            arc = location cDiagram .^. location pDiagram
        in (`D.atop` arc D.# D.lcA (colourMap c `D.withOpacity` 0.5))
  in foldr attach d children

-- | A diagram showing the text and root markers of a sentence.
sentenceDiagram
  :: ColourMap
  -> Config
  -> F.Forest
  -> S.Sentence
  -> D.QDiagram R.B D.V2 Float Any
sentenceDiagram colourMap conf forest =
  D.vsep (conf^.lineSkipL) .
  map (D.hsep (conf^.wordSkipL) . map (wordDiagram colourMap forest)) .
  LB.break 80 20 forest

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

import qualified Data.Forest as F
import qualified Data.List.Zipper as Z
import           Data.Monoid (Any)
import qualified Data.Sentence as S
import qualified Data.Sentences as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Diagrams.Backend.Rasterific as R
import qualified Diagrams.Core as D
import qualified Diagrams.Prelude as D
import           System.Environment (getProgName,getArgs)
import           System.FilePath (FilePath, (-<.>))

main :: IO ()
main = do
  progName <- getProgName
  args <- getArgs
  case args of
    [] -> putStrLn ("usage: " ++ progName ++ " filename")
    (filePath:_) -> do
      sentences <- S.readFile filePath
      Just forests <- readForests (filePath -<.> "fst") (Z.toList sentences)
      let diagram = sentencesDiagram (Z.toList sentences) forests
      writeRasterific (filePath -<.> "pdf") diagram

readForests :: FilePath -> [S.Sentence] -> IO (Maybe [F.Forest])
readForests filePath sentences =
  F.deserialiseForests sentences <$> T.readFile filePath

writeRasterific :: FilePath -> D.QDiagram R.B D.V2 Float Any -> IO ()
writeRasterific filePath diagram =
  let width  = D.width diagram
      height = D.height diagram
  in R.renderRasterific (filePath -<.> "pdf") (D.dims2D width height) diagram
  

sentencesDiagram :: [S.Sentence] -> [F.Forest] -> D.QDiagram R.B D.V2 Float Any
sentencesDiagram sentences forests =
  let diagrams = zipWith  sentenceDiagram' forests sentences
      sentenceDiagram' forest sentence =
        childMarkers colourMap forest sentence $
        sentenceDiagram colourMap 1 1 forest sentence
      colourMap = makeColourMap [D.green, D.red, D.blue,D.purple,D.orange]
  in D.scale 15 (D.vsep 1 diagrams)


type ColourMap = Int -> D.Colour Double

makeColourMap :: [D.Colour Double] -> ColourMap
makeColourMap [] _ = error "makeColourMap: emtpy list of colours."
makeColourMap colours i = colours !! (i `mod` length colours)

wordDiagram
  :: ColourMap -> F.Forest -> S.Word -> D.QDiagram R.B D.V2 Float Any
wordDiagram colourMap forest word =
  let status = F.statusOf word forest
      txt = R.texterific (T.unpack (S.wordText word))
      colour = colourMap (S.wordId word)
      marker
        | F.Root <- status = D.bgFrame 0.1 colour (D.bgFrame 0.1 D.white txt)
        | otherwise = txt
  in marker D.# D.centerX D.# D.named (S.wordId word) D.# D.alignL

childMarkers
  :: ColourMap
  -> F.Forest
  -> S.Sentence
  -> D.QDiagram R.B D.V2 Float Any
  -> D.QDiagram R.B D.V2 Float Any
childMarkers colourMap forest sentence d =
  let statuses = [(word,F.statusOf word forest) | word <- S.words sentence]
      children = [(S.wordId w,p) | (w,F.Child p) <- statuses]
      colourMap' = D.lc . colourMap
      attach (c,p) = D.withNames [c,p] $ \[cDiagram, pDiagram] ->
        D.atop ((D.location cDiagram .^. D.location pDiagram) D.# colourMap' p)
      p1 .^. p2 =
        let c1 = D.r2 (0,-1)
            c2 = D.r2 (0,-1) D.^+^ v
            v  = p2 D..-. p1
        in D.fromSegments [D.bezier3 c1 c2 v] `D.place` p1
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

-- Local Variables:
-- dante-target: "tree-render"
-- End:

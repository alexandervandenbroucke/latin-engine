import qualified Diagrams.Backend.Rasterific as R
import           Diagrams.Prelude ((^.))
import qualified Diagrams.Prelude as D
import qualified Options.Applicative as OA
import           System.FilePath ((-<.>))

import qualified Data.Forest.Serialise as F
import qualified Data.Paragraph as P
import qualified Data.Sentence as S
import qualified Diagrams.LatinEngine as LE
import qualified Diagrams.LatinEngine.LineBreaking as LB


main :: IO ()
main = do
  let info = OA.info (renderTask OA.<**> OA.helper)
        (OA.fullDesc
         <> OA.progDesc "Render the TARGET text's parse forests to PDF.")
  
  task <- OA.execParser info
  sentences <- P.readFile (taskSource task)
  forests   <- fmap (maybe [] id) (F.readForests (taskForest task))
  let diagram = LE.sentencesDiagram (taskConfig task) sentences forests
      width   = D.width diagram
      height  = D.height diagram      
  R.renderRasterific (taskOutput task) (D.dims2D width height) diagram

data RenderTask = RenderTask {
  taskSource :: FilePath,
  taskOutput :: FilePath,
  taskForest :: FilePath,
  taskConfig :: LE.Config
}

mkRenderTask
  :: Maybe FilePath -> Maybe FilePath -> LE.Config -> FilePath -> RenderTask
mkRenderTask output forest config source =
  RenderTask
  {
    taskSource = source,
    taskOutput = maybe (source -<.> "pdf") id output,
    taskForest = maybe (source -<.> "fst.json") id forest,
    taskConfig = config
  }

-- | Parse command line options.
renderTask :: OA.Parser RenderTask
renderTask =
  let source = OA.strArgument
        (OA.metavar "TARGET"
         <> OA.help "File containing the text to be rendered")
      output = optionalFilePathOption
        (OA.long "output"
         <> OA.short 'o'
         <> OA.metavar "FILE"
         <> OA.value Nothing
         <> OA.help "File to Write output to" )
      forest = optionalFilePathOption
        (OA.long "forest"
         <> OA.short 'f'
         <> OA.metavar "FORESTFILE"
         <> OA.value Nothing
         <> OA.help "File to read the forests from (in json format)")
      config =
        LE.Config  (D.def^.LE.coloursL)
        <$> parSkip
        <*> lineSkip
        <*> wordSkip
        <*> scale
        <*> lineBreaking
      parSkip = OA.option OA.auto
        (OA.long "paragraph-skip"
         <> OA.short 'p'
         <> OA.metavar "PARAGRAPHSKIP"
         <> OA.value (D.def^.LE.paragraphSkipL)
         <> OA.help "Space between paragraphs")
      lineSkip = OA.option OA.auto
        (OA.long "line-skip"
         <> OA.short 'l'
         <> OA.metavar "LINESKIP"
         <> OA.value (D.def^.LE.lineSkipL)
         <> OA.help "Space between lines")
      wordSkip = OA.option OA.auto
        (OA.long "word-skip"
        <> OA.short 'w'
        <> OA.metavar "WORDSKIP"
        <> OA.value (D.def^.LE.wordSkipL)
        <> OA.help "Space between words")
      scale = OA.option OA.auto
        (OA.long "scale"
        <> OA.short 's'
        <> OA.metavar "SCALE"
        <> OA.value (D.def^.LE.scaleL)
        <> OA.help "Scaling factor of the final output")
      lineBreaking = (intelligent OA.<|> wrap) <*> width <*> tolerance where
        intelligent =
          OA.flag LB.break LB.break
          (OA.long "adaptive"
           <> OA.short 'a'
           <> OA.help "Use adaptive line breaking to avoid breaking subclauses")
        wrap =
          OA.flag' (\w _t _f -> S.splitSentence w)
          (OA.long "word-wrap"
          <> OA.short 'r'
          <> OA.help
            "Use simple line breaking that wraps lines at a certain column width")
        width = OA.option OA.auto
          (OA.long "columns"
           <> OA.short 'c'
           <> OA.help "Desired number of columns per line"
           <> OA.metavar "COLUMNS"
           <> OA.value 80)
        tolerance = OA.option OA.auto
          (OA.long "tolerance"
           <> OA.short 't'
           <> OA.help (concat [
              "Tolerance for the adaptive line breaking.",
              " The algorithm tries to avoid creating lines that differ from",
              " COLUMNS by more or less than TOLERANCE." ])
           <> OA.metavar "TOLERANCE"
           <> OA.value 20)
  in mkRenderTask <$> output <*> forest <*> config <*> source


optionalFilePathOption
  :: OA.Mod OA.OptionFields (Maybe FilePath) -> OA.Parser (Maybe FilePath)  
optionalFilePathOption = OA.option (OA.eitherReader (Right . Just))


-- greeting :: OA.Parser String
-- greeting = f1 OA.<|> f2 where
--   f1 = OA.flag "intelligent" "intelligent"
--     ( OA.long "intelligent" <> OA.short 'i'
--       <> OA.help "Adaptive line breaking: avoids breaking subclauses")
--   f2 = OA.flag' "wrap"
--     ( OA.long "wrap" <> OA.short 'w' <> OA.help "Simple line wrapping")
    
  

-- Local Variables:
-- dante-target: "render"
-- End:

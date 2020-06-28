import qualified Data.Forest.Serialise as F
import qualified Data.Paragraph as P
import qualified Diagrams.Backend.Rasterific as R
import           Diagrams.LatinEngine (sentencesDiagram)
import qualified Diagrams.Prelude as D
import           System.Environment (getProgName,getArgs)
import           System.FilePath ((-<.>))

main :: IO ()
main = do
  progName <- getProgName
  args <- getArgs
  case args of
    [] -> putStrLn ("usage: " ++ progName ++ " filename")
    (filePath:_) -> do
      sentences <- P.readFile filePath
      Just forests <- F.readForests (filePath -<.> "fst.json")
      let diagram =
            sentencesDiagram D.def sentences forests
          width  = D.width diagram
          height = D.height diagram      
      R.renderRasterific (filePath -<.> "pdf") (D.dims2D width height) diagram


-- Local Variables:
-- dante-target: "render"
-- End:

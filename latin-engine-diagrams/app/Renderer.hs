import qualified Data.Forest as F
import qualified Data.List.Zipper as Z
import qualified Data.Sentences as S
import qualified Data.Text.IO as T
import qualified Diagrams.Backend.Rasterific as R
import           Diagrams.LatinEngine (sentencesDiagram)
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
      Just forests <- readForests (filePath -<.> "fst")
      let diagram =
            sentencesDiagram D.def (Z.toList sentences) forests
          width  = D.width diagram
          height = D.height diagram      
      R.renderRasterific (filePath -<.> "pdf") (D.dims2D width height) diagram

readForests :: FilePath -> IO (Maybe [F.Forest])
readForests filePath =
  F.deserialiseForests <$> T.readFile filePath

-- Local Variables:
-- dante-target: "render"
-- End:

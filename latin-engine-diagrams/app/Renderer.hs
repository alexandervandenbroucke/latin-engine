import qualified Data.Forest as F
import qualified Data.List.Zipper as Z
import qualified Data.Sentence as S
import qualified Data.Sentences as S
import qualified Data.Text.IO as T
import qualified Diagrams.Backend.Rasterific as R
import qualified Diagrams.Prelude as D
import           System.Environment (getProgName,getArgs)
import           System.FilePath (FilePath, (-<.>))
import Diagrams.LatinEngine (defaultColourMap,sentencesDiagram)

main :: IO ()
main = do
  progName <- getProgName
  args <- getArgs
  case args of
    [] -> putStrLn ("usage: " ++ progName ++ " filename")
    (filePath:_) -> do
      sentences <- S.readFile filePath
      Just forests <- readForests (filePath -<.> "fst") (Z.toList sentences)
      let diagram =
            sentencesDiagram defaultColourMap (Z.toList sentences) forests
          width  = D.width diagram
          height = D.height diagram      
      R.renderRasterific (filePath -<.> "pdf") (D.dims2D width height) diagram

readForests :: FilePath -> [S.Sentence] -> IO (Maybe [F.Forest])
readForests filePath sentences =
  F.deserialiseForests sentences <$> T.readFile filePath

-- Local Variables:
-- dante-target: "render"
-- End:

import           Brick

import           System.Environment
import           UI

main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName
  case args of
    [] -> putStrLn ("usage: " ++ progName ++ " filename")
    (filePath:_) -> loadEditors filePath >>= defaultMain app >> return ()


-- Local Variables:
-- dante-target: "interactive"
-- End:

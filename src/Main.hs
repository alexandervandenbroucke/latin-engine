import Brick

import UI
import qualified Data.Sentences as S

main :: IO ()
main =
  let file = "Caesar_DBG_1.txt"
  in loadEditors file >>= defaultMain app >> return ()

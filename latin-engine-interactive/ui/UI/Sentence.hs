module UI.Sentence (
  sentenceWidget,
  explicitNewline
)
where

import           Brick
import qualified Data.Text as T

sentenceWidget :: T.Text -> Widget n
sentenceWidget = txtWrap . explicitNewline

explicitNewline :: T.Text -> T.Text
explicitNewline = T.map go where
  go '\n' = '\8617'
  go c = c

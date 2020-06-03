module UI.AnnotationEditor where

import           Brick
import qualified Data.Forest as F
import qualified Data.Sentence as S
import qualified Data.Text as T
import           Lens.Micro

data Editor = Editor {
  editorSentence :: S.Sentence,
  editorForest   :: F.Forest
} deriving Show

sentenceL :: Lens' Editor S.Sentence
sentenceL = lens editorSentence (\e s -> e{editorSentence = s})

forestL :: Lens' Editor F.Forest
forestL = lens editorForest (\e f -> e{editorForest = f})

makeEditor :: F.Forest -> S.Sentence -> Editor
makeEditor forest sentence = Editor sentence forest

makeEditorWidget :: S.Sentence -> Widget n
makeEditorWidget sentence = editorWidget (makeEditor F.emptyForest sentence)

splitLines :: Int -> (a -> Int) -> [a] -> [[a]]
splitLines width len = go 0 [] where
  go _ line [] = [reverse line]
  go n line (x:xs)
    | n + len x > width = reverse line : go 0 [] (x:xs)
    | otherwise = go newLength newLine xs where
        newLine = x:line
        newLength = n + len x

statusText :: S.Word -> F.Forest -> T.Text
statusText word@(S.Word n _) forest = case F.statusOf word forest of
  F.Root    -> T.pack (show n ++ "R")
  F.Child r -> T.pack (show n ++ "C" ++ show r)
  F.Clear   -> T.pack (show n)

editorWidget :: Editor -> Widget n
editorWidget (Editor sentence forest) = cropToContext $ Widget Greedy Greedy $ do
  ctx <- getContext
  
  let len (text,status) =  max (textWidth text) (textWidth status) + 1
      pairs :: [(T.Text,T.Text)]
      pairs =
        [(text,statusText word forest)
        | word@(S.Word _ text) <- S.words sentence ]
      width = ctx^.availWidthL
      splitted = splitLines width len pairs
      renderPair (text,status) = padRight (Pad 1) (txt text <=> txt status)
      renderLine :: [(T.Text,T.Text)] -> Widget n
      renderLine line = padRight Max $ hBox (map renderPair line)
  render (vBox $ map renderLine splitted)
  
wrap :: Int -> [(T.Text,Int)] -> [[(T.Text,Int)]]
wrap width = go 0 where
  go _      [] = [[]]
  go column ((w,n):ws)
    | column + T.length w + 1 <= width =
        let (l:ls) = go (column + T.length w + 1) ws
        in ((w,n):l):ls
    | otherwise =
        [] : go 0 ((w,n):ws)

focusAttr :: AttrName
focusAttr = attrName "focus"

handleEditorEvent :: Editor -> BrickEvent n () -> EventM n (Next Editor)
handleEditorEvent s (AppEvent ()) = continue s
-- handleEditorEvent s (VtyEvent (V.EvKey V.KLeft [])) = continue (focusPrev s)
-- handleEditorEvent s (VtyEvent (V.EvKey V.KRight [])) = continue (focusNext s)
handleEditorEvent s _ = continue s

            
-- Generate events for going past the beginning or end of a sentence?
-- This could allow the UI to cycle to the next sentence.

-- Some sort of careting will be necessary.

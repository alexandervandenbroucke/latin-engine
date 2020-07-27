{- |

Module:      Diagrams.LineBreaking
Description: Algorithm for breaking sentences into lines.
Maintainer:  alexander.vandenbroucke@gmail.com

This module implements an algorithm for breaking sentences into lines, while
minimising the amount of line breaks inside subclauses of a sentence.
It is based on the Knuth-Plass algorithm.
-}

module Diagrams.LineBreaking where

import qualified Data.IntSet as Set
import qualified Data.List as L
import           Data.Ord (comparing)
import qualified Data.Text as T

import qualified Data.Sentence as S
import qualified Data.Forest as F


-- L = desired length = 75
-- badness of a line = abs(#character count - L)
-- badness tolerance <= 5
-- demerits of a line = # of arcs outbound to or inbound from a word in
-- another, later line
-- l = line demerit (inherent demerit caused by a line break, that must be
-- offset by making a good choice)
-- should demerits include badness? Probably, yes.

-- | A line, given a start (inclusive) and end point (exclusive).
data Line = Line !S.WordId !S.WordId

-- | The character count of a sentence
len :: S.Sentence -> Line -> Int
len sentence (Line start end) = maybe 0 finalise (traverse go [start..(end-1)])
  where
    finalise xs = sum xs + spaces
    spaces = end - start
    go w = fmap (T.length . S.wordText) (S.wordNr w sentence)

-- | Badness given a desired length
badness :: S.Sentence -> Int -> Line -> Int
badness sentence desired line = desired - len sentence line

-- | The demerits of breaking after a specified word (given the previous break
-- point).
--
-- It is the number of arcs outbound from or inbound to the line before the
-- break to or from words in another later line.
demerit :: F.Forest -> Line -> Int
demerit forest (Line start end) = (length outbound + length inbound)^3 where
  line = [start .. end - 1]
  outbound =
    [ word
    | word <- line,
      F.Child parent <- [F.statusOf forest word],
      parent >= end]
  inbound =
    [ child
    | word <- line,
      child <- Set.toList $ F.children word forest,
      child >= end]

data Candidate = Candidate {
  totalDemerit   :: !Int,
  candidateBreak :: !S.WordId,
  previous       :: Candidate
} deriving (Eq,Ord)
-- note: in theory we don't need to keep a pointer to every previous candidate
-- a chain (list strict in the head) to previous word ids would suffice.

instance Show Candidate where
  show (Candidate d b p)
    | b == candidateBreak p = show (d,b)
    | otherwise = show (d,b) ++ " <- " ++ show p

extend :: S.WordId -> Int -> Candidate -> Candidate
extend word demerit prev = Candidate (demerit + totalDemerit prev) word prev

chain :: Candidate -> [S.WordId]
chain c0 = go [candidateBreak c0] c0 (previous c0) where
  go acc c cPrev
    | candidateBreak c == 0 =
        acc
    | otherwise =
        go (candidateBreak cPrev:acc) cPrev (previous cPrev)

initCandidate :: Candidate
initCandidate = Candidate 0 0 initCandidate

updateCandidates
  :: (Line -> Int)
  -> (Line -> Int)
  -> Int
  -> [Candidate]
  -> S.WordId
  -> [Candidate]
updateCandidates badness demerit tolerance active breakpoint =
  let line prev = Line (candidateBreak prev + 1) (breakpoint + 1)
      -- a line that includes the words after the last breakpoint, up to
      -- and including the word right before the new break point.
      stale c = badness (line c) < -tolerance
      potential =
        [extend breakpoint d prev
        | prev <- active,
          let b = badness (line prev),
          let d = demerit (line prev) + abs b,
          abs b <= tolerance]
      newCandidate
        | null potential = []
        | otherwise = [L.minimumBy (comparing totalDemerit) potential]
  in newCandidate ++ filter (not . stale) active

candidates :: Int -> Int -> F.Forest -> S.Sentence -> [Candidate]
candidates desired tolerance  forest sentence =
  let bd = badness sentence desired
      dm = demerit forest
      ws = [1..S.wordCount sentence]
  in L.foldl' (updateCandidates bd dm tolerance) [initCandidate] ws

breakpoints :: Int -> Int -> F.Forest -> S.Sentence -> [S.WordId]
breakpoints desired tolerance forest sentence =
  let optimal [] = []
      optimal (cLast:_) = chain (extend (S.wordCount sentence) 0 cLast)
  in optimal (candidates desired tolerance forest sentence) 

break :: Int -> Int -> F.Forest -> S.Sentence -> [[S.Word]]
break desired tolerance forest sentence =
  let bs = breakpoints desired tolerance forest sentence
      line start end = maybe [] id $
        sequence [S.wordNr w sentence | w <- [(start+1)..end]]
  in zipWith line bs (drop 1 bs)

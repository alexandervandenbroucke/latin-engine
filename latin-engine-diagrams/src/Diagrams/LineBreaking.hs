{- |

Module:      Diagrams.LineBreaking
Description: Algorithm for breaking sentences into lines.
Maintainer:  alexander.vandenbroucke@gmail.com

This module implements an algorithm for breaking sentences into lines, while
minimising the amount of line breaks inside subclauses of a sentence.
It is based on the Knuth-Plass algorithm.

The goal of a line breaking algorithm is to choose a sequence of locations
where to break a sentence into lines. Each line has an associated 'badness'.
The badness indicates how far the line's actual length is from the desired line
length. Lines that exceed a certain treshold of badness are rejected.

Moreover, a line also has a 'demerit'. A line gains demerits by having arcs 
in or out of later lines (i.e. it has words whose parent or children are
beyond the break at the end of the current line).
The badness of a line also weighs on the demerits to some extend.

The Knuth-Plass algorithm uses dynamic programming to compute the sequence of
breakpoints that minimises the total demerits.
It does this in worst-case quadratic time, but is mostly linear in practice.
However, the implementation in this module is not optimised, and it is likely
that a significant reduction in the constant factors could be achieved.

-}

module Diagrams.LineBreaking where

import qualified Data.IntSet as Set
import qualified Data.List as L
import           Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Data.Maybe as Maybe
import           Data.Ord (comparing)
import qualified Data.Text as T

import qualified Data.Sentence as S
import qualified Data.Forest as F


-- | A line, given a start (inclusive) and end point (exclusive).
data Line = Line !S.WordId !S.WordId

-- | The character count of a 'Line'.
len :: S.Sentence -> Line -> Int
len sentence (Line start end) = maybe 0 finalise (traverse go [start..(end-1)])
  where
    finalise xs = sum xs + spaces
    spaces = end - start
    go w = fmap (T.length . S.wordText) (S.wordNr w sentence)

-- | Badness of a 'Line' given a desired length.
sentenceBadness
  :: S.Sentence -- ^ the 'S.Sentence' that the 'Line' is a part of
  -> Int        -- ^ the desired length
  -> Line       -- ^ the line
  -> Int
sentenceBadness sentence desired line = desired - len sentence line

-- | The demerits of breaking after a specified word (given the previous break-
-- point).
--
-- It is the number of arcs outbound from or inbound to the line before the
-- break to or from words in another later line.
forestDemerit :: F.Forest -> Line -> Int
forestDemerit forest (Line start end) =
  let line = [start .. end - 1]
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
  in (length outbound + length inbound)^3

-- | A candidate breakpoint.
--
-- This is a breakpoint that can be reached by using breakpoints that do not
-- create any lines that exceed the badness treshold.
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

-- | Extend a candidate.
extend
  :: S.WordId  -- ^ the 'S.WordId' of the new candidate.
  -> Int       -- ^ the demerit of the line formed by breaking the sentence
               --   that remains after the previous 'Candidate' at the new
               --   breakpoint
               --   the new candidate
  -> Candidate -- ^ the previous 'Candidate'.
  -> Candidate
extend word demerit prev = Candidate (demerit + totalDemerit prev) word prev

-- | The chain created by following this Candidate backwards.
chain :: Candidate -> [S.WordId]
chain c0 = go [candidateBreak c0] c0 (previous c0) where
  go acc c cPrev
    | candidateBreak c == 0 =
        acc
    | otherwise =
        go (candidateBreak cPrev:acc) cPrev (previous cPrev)

-- | Initial candidate.
--
-- This corresponds to a breakpoint at the beginning of the sentence.
initCandidate :: Candidate
initCandidate = Candidate 0 0 initCandidate


-- | Update a set of active 'Candidate's.
--
updateCandidates
  :: (Line -> Int) -- ^ badness function
  -> (Line -> Int) -- ^ demerit function
  -> Int           -- ^ tolerance
  -> [Candidate]   -- ^ current active candidate set
  -> S.WordId      -- ^ current break point
  -> [Candidate]   -- ^ new active candidate set
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

-- | The list of active candidate break points for a sentence.
--
--
-- @candidates d t forest sentence@ returns the set of active candidates
-- after breaking @sentence@ into lines, using @forest@ to compute demerits,
-- with desired line length @d@ and tolerance @t@.
--
-- The first element in this list is the last candidate that was found, i.e.
-- which breaks the list the furthest. This is usually the candidate that
-- you want.
candidates :: Int -> Int -> F.Forest -> S.Sentence -> [Candidate]
candidates desired tolerance forest sentence =
  let bd = sentenceBadness sentence desired
      dm = forestDemerit forest
      ws = [1..S.wordCount sentence]
      update cs w =
        case NE.nonEmpty (updateCandidates bd dm tolerance (NE.toList cs) w) of
          Just newCs -> newCs
          Nothing -> extend w 0 (NE.head cs) :| []
          -- If the candidate list becomes empty, that means that we couldn't
          -- find a breakpoint that didn't exceed the badness tolerance, and
          -- all previous breakpoints have become stale. In that case, we set
          -- an overful line, and reset the demerit score. We don't need more
          -- accurate demerit accounting, because any later breakpoints must
          -- start from this breakpoint. Hence, their relative total demerits
          -- are still correct.
  in NE.toList (L.foldl' update (initCandidate :| []) ws)

-- | The list of optimal break points (fewest total demerits) of a sentence.
--
-- @breakpoints d t forest sentence@ computes the list of breakpoints that
-- break @sentence@ into lines of desired length @d@, with tolerance @t@,
-- such that the demerits according to @forest@ are minimised.
-- 
breakpoints :: Int -> Int -> F.Forest -> S.Sentence -> [S.WordId]
breakpoints desired tolerance forest sentence =
  let optimal [] = []
      optimal (cLast:_) = chain (extend (S.wordCount sentence) 0 cLast)
  in optimal (candidates desired tolerance forest sentence) 


-- | Optimally split a sentence into lines.
--
-- @break t d forest sentence@ uses 'breakpoints' to compute optimal
-- breakpoints, and then breaks 'S.Sentence' accordingly.
break :: Int -> Int -> F.Forest -> S.Sentence -> [[S.Word]]
break desired tolerance forest sentence =
  let bs = breakpoints desired tolerance forest sentence
      line start end =
        Maybe.catMaybes [S.wordNr w sentence | w <- [(start+1)..end]]
  in zipWith line bs (drop 1 bs)

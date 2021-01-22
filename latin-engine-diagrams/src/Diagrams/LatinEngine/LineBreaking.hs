{- |

Module:      Diagrams.LatinEngine.LineBreaking
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

If you simply want to use this module to break a sentence into lines, call
'break' with the desired line length, badness tolerance, parse forest and
sentence.

-}

module Diagrams.LatinEngine.LineBreaking
  ( -- * Lines and Metrics
    Line(..),
    len,
    sentenceBadness,
    forestDemerit,
    Metrics(..),
    -- * Candidates
    Candidate(..),
    initCandidate, extend, chain,
    -- * Line breaking
    updateCandidates,
    candidates,
    breakpoints, breakAt, break
  )
where

import qualified Data.IntSet as Set
import qualified Data.List as L
import           Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Data.Maybe as Maybe
import           Data.Ord (comparing)
import qualified Data.Text as T
import           Prelude hiding (break)

import qualified Data.Sentence as S
import qualified Data.Forest as F

-------------------------------------------------------------------------------
-- Lines and Metrics

-- | A line, given a start (inclusive) and end point (exclusive).
data Line = Line !S.WordId !S.WordId

-- | A gives the badness, badness tolerance and demerits of a line
data Metrics = Metrics {
  badness   :: Line -> Int,
  demerits  :: Line -> Int,
  tolerance :: !Int
}

-- | The character count of a 'Line'.
len :: S.Sentence -> Line -> Int
len sentence (Line start end) = maybe 0 finalise (traverse go [start..(end-1)])
  where
    finalise xs = sum xs + spaces
    spaces = end - start
    go w = fmap (T.length . S.wordText) (S.wordNr w sentence)

-- | Badness of a 'Line' given a desired length.
--
-- Negative badness means that the line is too long, postive badness means that
-- the line is too short.
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
      outbound = length (filter farParent line) where
        farParent word = case F.statusOf forest word of
          F.Child parent -> parent >= end
          _ -> False
      inbound = sum (map (Set.size . farChildren) line) where
        farChildren word = Set.filter (>= end) (F.children word forest)
  in (outbound + inbound)^3

-------------------------------------------------------------------------------
-- Candidates


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
-- But this is useful for debugging.

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
  -> Candidate -- ^ the previous 'Candidate'.
  -> Candidate -- ^ the new candidate
extend word demerit prev = Candidate (demerit + totalDemerit prev) word prev

-- | The chain created by following this Candidate backwards.
chain :: Candidate -> [Candidate]
chain c0 = go [c0] c0 (previous c0) where
  go acc c cPrev
    | candidateBreak c == 0 =
        acc
    | otherwise =
        go (cPrev:acc) cPrev (previous cPrev)

-- | Initial candidate.
--
-- This corresponds to a breakpoint at the beginning of the sentence.
initCandidate :: Candidate
initCandidate = Candidate 0 0 initCandidate


--------------------------------------------------------------------------------
-- Actual line breaking

-- | Update a set of active 'Candidate's.
--
updateCandidates
  :: Metrics       -- ^ metrics
  -> [Candidate]   -- ^ current active candidate set
  -> S.WordId      -- ^ current break point
  -> [Candidate]   -- ^ new active candidate set
updateCandidates metrics active breakpoint =
  let line prev = Line (candidateBreak prev + 1) (breakpoint + 1)
      -- a line that includes the words after the last breakpoint, up to
      -- and including the word right before the new break point.
      stale c = badness metrics (line c) < -(tolerance metrics)
      potential =
        [extend breakpoint d prev
        | prev <- active,
          let b = badness metrics (line prev),
          let d = 10*demerits metrics (line prev) + abs b,
          abs b <= tolerance metrics]
      newCandidate
        | null potential = []
        | otherwise = [L.minimumBy (comparing totalDemerit) potential]
  in newCandidate ++ filter (not . stale) active

-- | The list of active candidate break points for a sentence.
--
--
-- @candidates metrics sentence@ returns the set of active candidates
-- after breaking @sentence@ into lines, using @metrics@ to compute demerits,
-- badness and tolerance.
--
-- The first element in this list is the last candidate that was found, i.e.
-- which breaks the list the furthest. This is usually the candidate that
-- you want.
candidates :: Metrics -> S.Sentence -> NonEmpty Candidate
candidates metrics sentence =
  let update cs w =
        let updated = updateCandidates metrics (NE.toList cs) w
        in case NE.nonEmpty updated of
             Just newCs -> newCs
             Nothing -> extend w 0 (NE.head cs) :| []
        -- If the candidate list becomes empty, that means that we couldn't
        -- find a breakpoint that didn't exceed the badness tolerance, and
        -- all previous breakpoints have become stale. In that case, we set
        -- an overful line, and reset the demerit score. We don't need more
        -- accurate demerit accounting, because all later breakpoints must
        -- start from this breakpoint. Hence, their relative total demerits
        -- are still correct.
  in L.foldl' update (initCandidate :| []) [1..S.wordCount sentence-1]

-- | The list of optimal break points (fewest total demerits) of a sentence.
--
-- @breakpoints d t forest sentence@ computes the list of breakpoints that
-- break @sentence@ into lines of desired length @d@, with tolerance @t@,
-- such that the demerits according to @forest@ are minimised.
-- 
breakpoints :: Int -> Int -> F.Forest -> S.Sentence -> [Candidate]
breakpoints desired tolerance forest sentence =
  let wordCount = S.wordCount sentence
      metrics = Metrics{
        badness = sentenceBadness sentence desired,
        demerits = forestDemerit forest,
        tolerance = tolerance
        }
      lastBreakPoint = L.minimumBy (comparing totalDemerit) cs where
        cs = [extend wordCount b prev
             | prev <- NE.toList (candidates metrics sentence),
               let l = Line (candidateBreak prev + 1) (wordCount + 1),
               let b = 10*demerits metrics l - min 0 (badness metrics l)
             ]
      -- The last break point is chosen without looking at positive badness.
      -- This means that the last breakpoint is simply chosen to minimise total
      -- demerits, without incurring demerit for being underfull.
  in chain lastBreakPoint



-- | Break a sentence at the given breakpoints.
breakAt :: S.Sentence -> [S.WordId] -> [[S.Word]]
breakAt sentence bs =
  let line start end =
        Maybe.catMaybes [S.wordNr w sentence | w <- [(start+1)..end]]
  in zipWith line bs (drop 1 bs)

-- | Optimally split a sentence into lines.
--
-- @break t d forest sentence@ uses 'breakpoints' to compute optimal
-- breakpoints, and then breaks 'S.Sentence' accordingly.
break :: Int -> Int -> F.Forest -> S.Sentence -> [[S.Word]]
break desired tolerance forest sentence =
  let bs = breakpoints desired tolerance forest sentence
  in breakAt sentence (map candidateBreak bs)

{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{- |

Module:      Language.Parser
Description: Parser for an inflected language 
Maintainer:  alexander.vandenbroucke@gmail.com

This module implements staged compilation of the parsers defined in
"Language.Parser", inspired by the following paper:

> Oregano: staging regular expressions with Moore Cayley fusion, Jamie Willis,
> Nicolas Wu, Tom Schrijvers, Haskell Symposium 2022.

Unlike their NFA, ours has only a single storage cell rather than a tape,
because 'P.Parser's do not have an 'Applicative' structure.

-}
module Language.Parser.Compile (
  State(..),
  Cell(..),
  compile,
  exec,
  -- * Staged Parsers
  StagedStream(..),
  staged,
  firstMatch,
  parses,
  partial
)
where

import           Data.Kind (Type)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Language.Haskell.TH (CodeQ)
import           Language.Haskell.TH.Syntax (Lift)
import           Language.Parser (Parser(..))
import qualified Language.Parser as P
import qualified Data.Text as T

-- | Non-deterministic Finite-State Automaton (NFA) with a single
-- storage cell storing a value of type @a@. When it accepts it produces
-- a value of type @a@.
--
-- The contents of the storage cell (empty or full) is indicated by the
-- @cell@ type variable.
data State (cell :: Maybe Type) a where
  Accept :: State (Just a) a
  -- ^ Accept the input, and produce the value of type @a@, which we
  -- take from the storage cell.
  Reject :: State cell a
  -- ^ Reject the input
  Split  :: State cell a -> State cell a -> State cell a
  -- ^ Epsilon transition to two separate states
  Input  :: [Char] -> State cell a -> State cell a
  -- ^ Read a class of input characters
  Output :: a -> State (Just a) a -> State Nothing a
  -- ^ Store a value of type @a@ in the storage cell.

instance Show a => Show (State cell a) where
  show Accept       = "Accept"
  show Reject       = "Reject"
  show (Split a b)  = "(" ++ show a ++ ") | (" ++ show b ++ ")"
  show (Input cs s) = "Input [" ++ show cs ++ "]; " ++ show s
  show (Output o s) = "Output " ++ show o ++ "; " ++ show s

-- | Compile a 'P.Parser' to an NFA whose storage cell is initially empty.
compile :: P.Parser a -> State Nothing a
compile parser = compile' parser Accept

-- | Compile a 'P.Parser' given a continuation NFA that expects a value
-- to be present in the storage cell. In other words, the NFA for the
-- parser must store a value in the cell.
compile' :: P.Parser a -> State (Just a) a -> State Nothing a
compile' p k = case p of
  Empty      -> Reject
  Eps x      -> Output x k
  Symbols cs -> Input cs (Output () k)
  pa :<> pb  -> Split (compile' pa k) (compile' pb k)
  pa :.> pb  -> compileNoOut pa (compile' pb k)
  pa :<. pb  -> compile' pa (compileNoOut pb k)

-- | Compile a 'P.Parser' given a continuation NFA.
--
-- The NFA for should not store a value in the storage cell.
compileNoOut :: P.Parser () -> State cell a -> State cell a
compileNoOut p k = case p of
  Empty      -> Reject
  Eps ()     -> k -- do not output a value
  Symbols cs -> Input cs k
  pa :<> pb  -> Split (compileNoOut pa k) (compileNoOut pb k)
  pa :.> pb  -> compileNoOut pa (compileNoOut pb k)
  pa :<. pb  -> compileNoOut pa (compileNoOut pb k)

-- | A data cell, whose contents (full or empty) is indicated by its
-- @cell@ type variable.
data Cell (cell :: Maybe Type) where
  Nihil :: Cell 'Nothing
  Full  :: a -> Cell (Just a)

-- | Execute an NFA on a given input.
exec
  :: forall a b
  . (String -> a -> b -> b) -- ^ Folding function to combine unparsed suffixes
                            --   and outputs from different branches
  -> b                      -- ^ Default value
  -> State Nothing a        -- ^ NFA
  -> String                 -- ^ String to parse
  -> b                      -- ^ Output
exec combine nil0 state str0 = go str0 Nihil state nil0
  where
    go :: String -> Cell cell -> State cell a -> b -> b
    go str cell = \case
      Accept | Full x  <- cell -> combine str x
      Reject                   -> id
      Split a b                -> go str cell a . go str cell b
      Output a s               -> go str (Full a) s
      Input cs s
        | (i:is) <- str, i `elem` cs -> go is cell s
        | otherwise -> id

-- | A stream-like type which has a stageable 'uncons' operator.
class StagedStream s where
  uncons :: CodeQ s -> CodeQ b -> (CodeQ Char -> CodeQ s -> CodeQ b) -> CodeQ b

instance StagedStream String where
  uncons cstr cnil ccons =
    [|| case $$cstr of
          []     -> $$cnil
          (c:cs) -> $$(ccons [||c||] [||cs||])
    ||]

instance StagedStream Text where
  uncons ctext cnil ccons =
    [|| case Text.uncons $$ctext of
          Nothing     -> $$cnil
          Just (c,cs) -> $$(ccons [||c||] [||cs||])
    ||]

instance StagedStream (P.Reverse Text) where
  uncons creverse cnil ccons =
    [|| case Text.unsnoc (P.unReverse $$creverse) of
          Nothing     -> $$cnil
          Just (cs,c) -> $$(ccons [||c||] [||P.Reverse cs||])
    ||]

-- | A stream-like type which has stageable length and take operations.
class StagedStream stream => SplittableStream stream where
  streamLen  :: CodeQ stream -> CodeQ Int
  streamTake :: CodeQ Int -> CodeQ stream -> CodeQ stream

instance SplittableStream String where
  streamLen cstr = [|| length $$cstr ||]
  streamTake cInt cstr = [|| take $$cInt $$cstr ||]

instance SplittableStream Text where
  streamLen ctext = [|| T.length $$ctext ||]
  streamTake cInt ctext = [|| T.take $$cInt $$ctext ||]

instance SplittableStream (P.Reverse Text) where
  streamLen cRev =
    [|| T.length $ P.unReverse $$cRev ||]
  streamTake cInt cStream =
    [|| P.Reverse $ T.takeEnd $$cInt (P.unReverse $$cStream) ||]

-- | Staged compilation of an NFA.
staged
  :: forall a b stream
  .  (Lift a, StagedStream stream)
  => (CodeQ stream -> CodeQ a -> CodeQ b -> CodeQ b)
  -- ^ Folding function to combine code for unparsed suffixes and outputs from
  -- different branches.
  -> CodeQ b
  -- ^ Code for a default value
  -> State Nothing a
  -- ^ NFA
  -> CodeQ stream
  -- ^ Code for the stream to parse
  -> CodeQ b
  -- ^ Code for the output
staged combine cnil0 state0 cstr0 = go cstr0 Nihil state0 cnil0
  where
    go :: CodeQ stream -> Cell cell -> State cell a -> CodeQ b -> CodeQ b
    go cstream cell state cnil = case state of
      Accept | Full x <- cell -> combine cstream [||x||] cnil
      
      Reject    -> cnil
      
      Split a b ->
        [|| let join = $$(go cstream cell b cnil)
            in $$(go cstream cell a [||join||])
        ||]
        -- Using a let binding here is quite important: otherwise the size
        -- of the generated code blows up exponentially.
                   
      Input [] _  -> cnil
      
      Input [c] s -> uncons cstream cnil $ \ci cis ->
        [|| if $$ci == c then $$(go cis cell s cnil) else $$cnil ||]
        
      Input cs s -> uncons cstream cnil $ \ci cis ->
        [|| if $$ci `elem` cs then $$(go cis cell s cnil) else $$cnil ||]
        
      Output a s -> go cstream (Full a) s cnil

-- | All outputs of any prefix of the input, and the remaining suffixes.
parses
  :: (Lift a, StagedStream stream)
  => State Nothing a -> CodeQ stream -> CodeQ [(stream, a)]
parses = staged (\remainder x l -> [|| ($$remainder, $$x) : $$l ||] ) [|| [] ||]

-- | The output of the first match, if any
firstMatch
  :: (Lift a, StagedStream stream)
  => State Nothing a -> CodeQ stream -> CodeQ (Maybe a)
firstMatch = staged (\_ x _ -> [|| Just $$x ||]) [|| Nothing ||]

-- | Parse a word as far as possible.
--
-- The result is a list of triples @(prefix,suffix,x)@ such that each @prefix@
-- is a prefix of the input, @suffix@ is a suffix of the input, and
-- @x@ is the corresponding value returned on parsing @prefix@.
--
partial
  :: forall stream a
  .  (SplittableStream stream, Lift a)
  => State Nothing a
  -> CodeQ stream
  -> CodeQ [(stream, stream, a)]
partial parser input = staged combine [||[]||] parser input
  where
    combine
      :: CodeQ stream
      -> CodeQ a
      -> CodeQ [(stream, stream, a)]
      -> CodeQ [(stream, stream, a)]
    combine remainder x l =
      [||
         let prefix = $$(
               streamTake
                 [|| $$(streamLen input) - $$(streamLen remainder) ||]
                 input
               )
         in (prefix,$$remainder,$$x) : $$l
      ||]

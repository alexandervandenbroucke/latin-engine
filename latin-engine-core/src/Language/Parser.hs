{- |

Module:      Language.Parser
Description: Parser for an inflected language 
Maintainer:  alexander.vandenbroucke@gmail.com

This module implements a parser for the specific purpose of matching the
inflections that are added when a verbs is conjugated, or adjectives and nouns
are declined.

For this reason, it is like a simplified regular expression, but the capability
to reject arbitrary length strings. To be precise, there is no Kleene star.
In addition to recognising whether a 'String' is in a language, a @'Parser' a@
also returns one or more values of type @a@.

Nevertheless, we can define an analogue of the Brozozowski 'derivative', which
gives rise to simple recogniser, 'parse'.
The function 'parses' gives all the intermediate derivatives obtained when
parsing a string.

Because it is useful to start parsing inflected forms from the rear, 'Parser's
can be reversed, and 'String's or 'T.Text's can be parsed from the rear using
'Reverse'.

-}

{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Parser (
  -- * Parser
  Parser(..),
  empty,
  eps,
  string,
  text,
  (#>),
  (.>),
  (<#),
  (<.),
  symbols,
  alpha,
  reverse,
  -- * Parsing Functions
  language,
  epsilon,
  derivative,
  Parse(parse,parses),
  Reverse(Reverse,unReverse))
where

import qualified Data.List as L
import           Data.String (IsString(..))
import qualified Data.Text as T
import           Prelude hiding (reverse)

-------------------------------------------------------------------------------
-- Parser

-- | A parser representation.
data Parser a where
  Empty :: Parser a
  Eps :: a -> Parser a
  (:.>) :: Parser () -> Parser a -> Parser a
  (:<.) :: Parser a -> Parser () -> Parser a
  (:<>) :: Parser a -> Parser a -> Parser a
  Symbols :: [Char] -> Parser ()
  
deriving instance Eq a => Eq (Parser a)
deriving instance Ord a => Ord (Parser a)

instance Show a => Show (Parser a) where
  showsPrec _ Empty str = "empty" ++ str
  showsPrec p (Eps x) str
    | p >= 8 = "(Eps " ++ show x ++ ")" ++ str
    | otherwise = "Eps " ++ show x ++ str
  showsPrec p (pa :.> pb) str
    | p >= 7 = "(" ++ showsPrec 6 pa (" .> " ++ showsPrec 7 pb (")" ++ str))
    | otherwise = showsPrec 6 pa (" .> " ++ showsPrec 7 pb str)
  showsPrec p (pa :<. pb) str
    | p >= 7 = "(" ++ showsPrec 6 pa (" <. " ++ showsPrec 7 pb (")" ++ str))
    | otherwise = showsPrec 6 pa (" <. " ++ showsPrec 7 pb str)
  showsPrec p (pa :<> pb) str
    | p >= 6 = "(" ++ showsPrec 5 pa (" <> " ++ showsPrec 6 pb (")" ++ str))
    | otherwise = showsPrec 5 pa (" <> " ++ showsPrec 6 pb str)
  showsPrec p (Symbols cs) str
    | p >= 8 = "(Symbols " ++ show cs ++ ")" ++ str
    | otherwise = "Symbols " ++ show cs ++ str

instance Functor Parser where
  fmap _ Empty = Empty
  fmap f (Eps x) = Eps (f x)
  fmap f (pb :.> pa) = pb .> fmap f pa
  fmap f (pa :<. pb) = fmap f pa <. pb
  fmap f (pa :<> pb) = fmap f pa <> fmap f pb
  fmap f (Symbols cs) = symbols cs .> eps (f ())

-- | The semigroup implements union.
--
-- L (a <> b) = L(a) U L(b)
instance Semigroup (Parser a) where
  Empty <> p = p
  p <> Empty = p
  (p1 :<> p2) <> p3 = p1 <> (p2 <> p3)
  p1 <> p2 = p1 :<> p2

instance Monoid (Parser a) where
  mempty = empty
  mappend = (<>)

-- | The parser of the empty language.
--
-- @L(empty) = {}@
empty :: Parser a
empty = Empty

-- | A parser that accepts the empty string, and returns a result.
--
-- @L(eps) = { "" }@
eps :: a -> Parser a
eps = Eps

-- | A parser that accepts a string.
--
-- @L(string w) = { w }@
string :: String -> Parser ()
string = foldr (.>) (eps ()) . map (symbols . pure)

-- | A parser that accepts a text.
--
-- @L(text w) = { T.unpack w }
text :: T.Text -> Parser ()
text = string . T.unpack

-- | Concatenate two parsers.
--
-- The language of the concatenation consists of the concatenations of all
-- strings in the first language with all those of the second language.
--
-- The result of the second parser is returned.
--
-- L(a #> b) = { wu | w in L(a), u in L(b) }
(#>) :: Parser b -> Parser a -> Parser a
pb #> pa = fmap (const ()) pb .> pa

-- | Concatenate two parsers.
--
-- The language of the concatenation consists of the concatenations of all
-- strings in the first language with all those of the second language.
--
-- The result of the first parser is returned.
--
-- L(a <# b) = { wu | w in L(a), u in L(b) }
(<#) :: Parser a -> Parser b -> Parser a
pa <# pb = pa <. fmap (const ()) pb

-- | Concatentate two parsers.
--
-- The language of the concatenation consists of concatenations of all strings
-- in the first language with all those of the second language.
--
-- The result of the second parser is returned
--
-- L(a .> b) = { wu | w in L(a), u in L(b) }
--
-- This version enforces right associativity.
(.>) :: Parser () -> Parser a -> Parser a
Eps{} .> p = p
Empty .> _ = empty
_ .> Empty = empty
(pa :.> pb) .> pc = pa .> (pb .> pc)
pa .> pb = pa :.> pb

-- | Concatentate two parsers.
--
-- The language of the concatenation consists of concatenations of all strings
-- in the first language with all those of the second language.
--
-- The result of the first parser is returned
--
-- L(a <. b) = { wu | w in L(a), u in L(b) }
--
-- This version enforces right associativity.
--
(<.) :: Parser a -> Parser () -> Parser a
p <. Eps{} = p
Empty <. _ = empty
_ <. Empty = empty
pa <. (pb :<. pc) = (pa <. pb) <. pc
pa <. pb = pa :<. pb

infixl 7 .>
infixl 7 #>

infixr 7 <.
infixr 7 <#

  
-- | A parser accepting a single character from the list.
symbols :: [Char] -> Parser ()
symbols = Symbols

-- | Any character from the latin alphabet
alpha :: Parser ()
alpha = Symbols (['a'..'z'] ++ ['A'..'Z'])

instance IsString (Parser ()) where
  fromString = string

-- | Parser for the reverse language.
reverse :: Parser a -> Parser a
reverse Empty = Empty
reverse (Eps x) = Eps x
reverse (pb :.> pa) = reverse pa <. reverse pb
reverse (pa :<. pb) = reverse pb .> reverse pa
reverse (pa :<> pa') = reverse pa <> reverse pa'
reverse (Symbols cs) = Symbols cs

-------------------------------------------------------------------------------
-- Parsing functions

-- | The language recognised by a Parser
--
-- The set of all strings and corresponding values recognised by the parser.
language :: Parser a -> [(String,a)]
language (Eps x)    = [("",x)]
language Empty       = []
language (pb :.> pa) =
  [(wb ++ wa,x) | (wb,_) <- language pb, (wa,x) <- language pa]
language (pa :<. pb) =
  [(wa ++ wb,x) | (wb,_) <- language pb, (wa,x) <- language pa]
language (pa :<> pb) = language pa ++ language pb
language (Symbols cs) = [([c],()) | c <- cs]

-- | The epsilon result of the Parser
--
-- The result returned when parsing the empty string.
epsilon :: Parser a -> [a]
epsilon (Eps x) = [x]
epsilon (pa :.> pb) = epsilon pa *> epsilon pb
epsilon (pa :<. pb) = epsilon pa <* epsilon pb
epsilon (pa :<> pf) = epsilon pa <> epsilon pf
epsilon Symbols{} = []
epsilon Empty = []

-- | Check if a parser accepts the empty string (epsilon)
nullable :: Parser a -> Bool
nullable = not . null . epsilon

-- | Brozozowski derivative
derivative :: Parser a -> Char -> Parser a
derivative Eps{} _ =
  Empty
derivative Empty _ =
  Empty
derivative (pa :.> pb) c
  | not (nullable pa) = derivative pa c #> pb
  | otherwise = (derivative pa c #> pb) <> derivative pb c
derivative (pa :<. pb) c
  | xs <- epsilon pa, not (null xs) =
      (derivative pa c <. pb) <> (derivative pb c .> foldMap eps xs)
  | otherwise =
      derivative pa c <. pb
derivative (pa :<> pb) c =
  derivative pa c <> derivative pb c
derivative (Symbols cs) c
  | c `elem` cs = Eps ()
  | otherwise = Empty


-- | A type-class to overload parsing over different types of strings.
class Parse str where
  -- | Parse a @str@
  parse :: Parser a -> str -> [a]
  -- | Return all intermediate 'Parser' states while parsing a @str@
  parses :: Parser a -> str -> [Parser a]

instance Parse String where
  parse p = epsilon . L.foldl' derivative p
  parses = L.scanl' derivative

instance Parse T.Text where
  parse p = epsilon . T.foldl' derivative p
  parses = scanlText derivative where
    scanlText f x0 =
      let f' [] c = [f x0 c, x0] -- this case should never happen
          f' (x:xs) c = f x c:x:xs
      in L.reverse . T.foldl' f' [x0]

-- | A newtype wrapper that does parsing on @str@ starting from the end.
--
-- prop> parse p txt = parse (reverse p) (Reverse txt)
-- prop> parse p (T.reverse txt) = parse p (Reverse txt)
--
-- As a consequence, we have that
--
-- > parse (reverse p) (T.reverse txt) = parse (reverse p) (Reverse txt)
-- >                                   = parse p txt
--
-- The main utility however, comes from using partial 'parses', where this
-- might be quicker than reversing the 'String' or 'T.Text'.
newtype Reverse str = Reverse { unReverse :: str }

instance Parse (Reverse String) where
  parse p = epsilon . foldr (flip derivative) p . unReverse
  parses p = L.scanr (flip derivative) p . unReverse

instance Parse (Reverse T.Text) where
  parse p = epsilon . T.foldr (flip derivative) p . unReverse
  parses p = scanrText (flip derivative) p . unReverse where
    scanrText f x0 =
      let f' c [] = [f c x0,x0] -- this case should never happen
          f' c (x:xs) = f c x:x:xs
      in T.foldr f' [x0]


-- -- | Reverse Brozozowski derivative
-- rderivative :: Char -> Parser a -> Parser a
-- rderivative _ Eps{} =
--   Empty
-- rderivative _ Empty =
--   Empty
-- rderivative c (pa :.> pb)
--   | xs <- epsilon pb, not (null xs) =
--       (pa .> rderivative c pb) <> (rderivative c pa .> foldMap eps xs)
--   | otherwise = pa .> rderivative c pb
-- rderivative c (pa :<> pb) =
--   rderivative c pa <> rderivative c pb
-- rderivative c (Symbols cs)
--   | c `elem` cs = Eps ()
--   | otherwise = Empty

-- -- | Type class to overloade the 'parse' function.
-- --
-- -- prop> parse p str == rparse p (reverse str)
-- class Parse str where
--   -- | Parse a @str@.
--   parse :: Parser a -> str -> [a]
--   -- | Parse a @str@ in reverse (start matching at the end of the string)
--   rparse :: Parser a -> str -> [a]
--   -- | All intermediate parses when parsing @str@
--   parses :: Parser a -> str -> [Parser a]
--   -- | All intermediate parses when parsing @str@ in reverse
--   rparses :: Parser a -> str -> [Parser a]

-- instance Parse String where
--   parse p   = epsilon . L.foldl' derivative p
--   rparse p  = epsilon . foldr rderivative p
--   parses p  = L.scanl derivative p
--   rparses p = L.scanr rderivative p

-- instance Parse T.Text where
--   parse p = epsilon . T.foldl' derivative p
--   rparse p = epsilon . T.foldr rderivative p
--   parses p = scanlText derivative p where
--     scanlText f x0 =
--       let f' [] c = [f x0 c, x0] -- this case should never happen
--           f' (x:xs) c = f x c:x:xs
--       in reverse . T.foldl' f' [x0]
--   rparses p = scanrText rderivative p where
--     scanrText f x0 =
--       let f' c [] = [f c x0,x0] -- this case should never happen
--           f' c (x:xs) = f c x:x:xs
--       in T.foldr f' [x0]

-- suffixparsesText :: Parser a -> T.Text -> [([a],(T.Text,T.Text))]
-- suffixparsesText p txt =
--   let epsilons = map epsilon (rparses p txt)
--       split i = T.splitAt i txt
--   in [(epsi,split i) | (i,epsi) <- zip [0..] epsilons, not (null epsi)]

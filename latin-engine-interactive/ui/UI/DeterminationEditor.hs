{- |

Module:      UI.DeterminationEditor
Description: A widget for displaying determinations of a word
Maintainer:  alexander.vandenbroucke@gmail.com

-}

{-# LANGUAGE OverloadedStrings #-}

module UI.DeterminationEditor (
  determinationWidget,
  editAttr,
  stemAttr,
  inflectedAttr)
where

import           Brick
import           Data.Char (isAlpha)
import qualified Data.List as L
import qualified Data.Sentence as S
import qualified Data.Text as T
import           Language.Latin
import qualified Language.Parser as P

-- | Parse a word as far as possible.
--
-- The result is a list of triples @(stem,infl,det)@ such that each @stem@
-- is a prefix of the word, @post@ is a recognised inflection on the word, and
-- @det@ is the corresponding declension.
--
partial :: T.Text -> [(T.Text,T.Text,Determination)]
partial rawText =
  let text = trim rawText
      trim = T.takeWhile isAlpha . T.dropWhile (not . isAlpha)
      rText = P.Reverse text
      parser = P.reverse (declI <> declII <> declIII <> declIV <> declV)
      ps = P.parses parser rText
  in [(pre,post,det) |
       (n,p) <- zip [0..] ps,
       let (pre,post) = T.splitAt n text,
       det <- P.epsilon p]

-- | Use the hint to guess the declension of a word.
parseHint :: T.Text -> [Declension]
parseHint hint =
  let det = T.dropWhile (/= '<') hint
  in case T.splitOn "," det of
       [nom,gen] ->
         let detNom = [d | (_,_,Determination d NOM S _) <- partial nom]
             detGen = [d | (_,_,Determination d GEN S _) <- partial gen]
         in L.intersect detNom detGen
       _ -> [I,II,III,IV,V]

-- | Attribute name of this widget.
editAttr :: AttrName
editAttr = "determination-editor"

-- | Attribute name of text that is a stem part of the input word.
stemAttr :: AttrName
stemAttr = editAttr <> "parsed"

-- | Attribute name of text that is an inflected part of the input word.
inflectedAttr :: AttrName
inflectedAttr = editAttr <> "unparsed"

-- | Create a new determination widget.
--
-- A determination widget contains a list of possible declensions or
-- conjugations for a particular word.
--
-- The set of possible declensions or conjugations can be limited by providing
-- a hint: a string of the format @< nom, gen@, where @nom@ and @gen@ are the
-- nominative and genitive of the word respectively.
determinationWidget
  :: S.Word  -- ^ Word to determine
  -> T.Text  -- ^ The hint for the word
  -> Widget n
determinationWidget word hint = vBox (map widget $ filt $ partial text) where
  text = S.wordText word
  decls = parseHint hint
  filt = filter (\(_,_,Determination d _ _ _) -> d `elem` decls)
  detWidget (Determination d c m g) = str $
    L.intercalate ", " [show c,show m,show g] ++ " (" ++ show d ++ ")"
  txtWidget pre post =
    withAttr stemAttr (txt pre) <+> withAttr inflectedAttr (txt post)
  widget (pre,post,det) =
    txtWidget pre post <+> padLeft (Pad 1) (detWidget det)
  {- The end result looks like this:
  +---+----+ +---+
  |pre|post| |det|
  +---+----+ +---+
  red green  black

  or whatever colours are chosen for unparsedAttr and parsedAttr
  -}

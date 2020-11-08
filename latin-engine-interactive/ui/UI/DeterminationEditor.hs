{- |

Module:      UI.DeterminationEditor
Description: A widget for displaying determinations of a word
Maintainer:  alexander.vandenbroucke@gmail.com

-}

{-# LANGUAGE OverloadedStrings #-}

module UI.DeterminationEditor (
  determinationWidget,
  editAttr,
  parsedAttr,
  unparsedAttr)
where

import           Brick
import           Data.Char (isAlpha)
import qualified Data.List as L
import qualified Data.Sentence as S
import qualified Data.Text as T
import           Language.Latin
import qualified Language.Parser as P

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

parseHint :: T.Text -> [Declension]
parseHint hint =
  let det = T.dropWhile (/= '<') hint
  in case T.splitOn "," det of
       [nom,gen] ->
         let detNom = [d | (_,_,Determination d NOM S _) <- partial nom]
             detGen = [d | (_,_,Determination d GEN S _) <- partial gen]
         in L.intersect detNom detGen
       _ -> [I,II,III,IV,V]

editAttr :: AttrName
editAttr = "determination-editor"

parsedAttr :: AttrName
parsedAttr = editAttr <> "parsed"

unparsedAttr :: AttrName
unparsedAttr = editAttr <> "unparsed"

determinationWidget :: S.Word -> T.Text -> Widget n
determinationWidget word hint = vBox (map widget $ filt $ partial text) where
  text = S.wordText word
  decls = parseHint hint
  filt = filter (\(_,_,Determination d _ _ _) -> d `elem` decls)
  detWidget (Determination d c m g) = str $
    L.intercalate ", " [show c,show m,show g] ++ " (" ++ show d ++ ")"
  txtWidget pre post =
    withAttr unparsedAttr (txt pre) <+> withAttr parsedAttr (txt post)
  widget (pre,post,det) =
    txtWidget pre post <+> padLeft (Pad 1) (detWidget det)

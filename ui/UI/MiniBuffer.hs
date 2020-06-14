{-# LANGUAGE DeriveFunctor #-}

module UI.MiniBuffer (
  MiniBuffer(..),
  message,
  promptString, prompt, promptRetry, promptNatural,
  abort,
  miniBufferWidget,
  handleMiniBufferEvent
) where

import           Brick
import qualified Brick.Widgets.Edit as E
import           Control.Monad (ap)
import           Data.Char (isNumber)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import           Text.Read (readMaybe)

type MBEditor = E.Editor T.Text

data MiniBuffer n a
  = Return a
  | Message String (MiniBuffer n a)
  | Prompt (Char -> Bool) (MBEditor n) String (String -> MiniBuffer n a)
  | Abort
  deriving Functor

instance Applicative (MiniBuffer n) where
  pure  = return
  (<*>) = ap

instance Monad (MiniBuffer n) where
  return = Return
  Return a >>= f = f a
  Prompt fil editor msg k >>= f = Prompt fil editor msg (\x -> k x >>= f)
  Message msg k >>= f = Message msg (k >>= f)
  Abort >>= _ = Abort

message :: String -> MiniBuffer n ()
message msg = Message msg (return ())

promptString :: n -> String -> MiniBuffer n String
promptString name msg = Prompt (const True) e msg return where
  e = E.editorText name (Just 1) T.empty

prompt :: Read a => n -> String -> MiniBuffer n (Maybe a)
prompt name = fmap readMaybe . promptString name

promptRetry :: Read a => n -> String -> MiniBuffer n a
promptRetry name msg = do
  mx <- prompt name msg
  case mx of
    Nothing -> do
      message "Invalid input. Hit Enter to try again."
      promptRetry name msg
    Just x -> return x

promptNatural :: n -> String -> MiniBuffer n Int
promptNatural name msg = Prompt isNumber e msg k where
  e = E.editorText name (Just 1) T.empty
  k "" = message "Empty input. Press Enter to continue." >> abort
  k input = return (read input)

abort :: MiniBuffer n a
abort = Abort

miniBufferWidget :: MiniBuffer n a -> Widget n
miniBufferWidget (Return _) = emptyWidget
miniBufferWidget (Message msg _) = strWrap msg
miniBufferWidget (Prompt _ e msg _) =
  str msg <+> (txt $ mconcat $ E.getEditContents e)
miniBufferWidget Abort = emptyWidget

handleMiniBufferEvent
  :: MiniBuffer n a -> BrickEvent n () -> EventM n (MiniBuffer n a)
handleMiniBufferEvent _ (VtyEvent (V.EvKey (V.KChar 'g') [V.MCtrl])) =
  return abort
handleMiniBufferEvent (Message _ k) evt
  | VtyEvent (V.EvKey V.KEnter []) <- evt = return k
handleMiniBufferEvent (Prompt fil e msg k) evt
  | VtyEvent (V.EvKey (V.KChar c) []) <- evt, fil c = do
      e' <- E.handleEditorEvent (V.EvKey (V.KChar c) []) e
      return (Prompt fil e' msg k)
  | VtyEvent (V.EvKey (V.KChar _) []) <- evt =
      return (Prompt fil e msg k)
  | VtyEvent (V.EvKey V.KEnter []) <- evt =
      return $ k $ T.unpack $ mconcat $ E.getEditContents $ e
  | VtyEvent vtyEvent <- evt = do
      e' <- E.handleEditorEvent vtyEvent e
      return (Prompt fil e' msg k)
handleMiniBufferEvent mb _ = return mb

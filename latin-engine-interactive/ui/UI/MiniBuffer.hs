{- |

Module:      UI.MiniBuffer
Description: A mini-buffer widget.
Maintainer:  alexander.vandenbroucke@gmail.com

A minibuffer is a small area that can display messages, and accept input (while
showing a prompt).

The minibuffer state is implemented as a Free-monad style data structure.
Using 'MiniBuffer' one essentially builds a script for interaction with the
user: a minibuffer can be one of four things:

1. @'Return' x@: the script has completed, with a result @x@.

   Completed script can be extend with monadic bind '>>='.

2. 'Done': The script is terminated.
   After a script is 'Done', no further actions are performed in the script.

   In particular:

   prop> Done >> mb == Done

3. @'Message' msg mb@: the script is currently displaying a message @msg@,
   after the message has been acknowledged, the script moves to a new state
   @mb@.

4. @'Prompt' accept editor prompt k@: The script is currently displaying a
   prompt @prompt@, and accepting character input.
   The editing of the input is deffered to the underlying @editor@ widget.
   Only characters that are accepted by the predicate 'accept' are passed to
   the @editor@. Once the user confirms the input, the current state of the
   editor is passed to the continuation @k@, which decides the next state.

= Examples

The minibuffer @'message' "Hello, world"@ displays

> Hello, world

and returns @()@.
The minibuffer @'message' "Something went wrong!" >> abort@ displays

> Something went wrong!

and then aborts. Since 'MiniBuffer' is a monad, we can write scripts using
do-notation:

> echo = do str <- promptString () "echo> "
>           if str == "" then return () else message str >> echo

The @()@ in the code above is the name for the underlying editor.
The minibuffer will echo the input in a message, until an empty string is
input.
Several other useful prompt functions are provided, and a primitive prompt
can be created with 'promptPrimitive'.

-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module UI.MiniBuffer (
  -- * Data Type
  MiniBuffer(..),
  isDone,
  -- * Smart constructors
  abort,
  message,
  -- ** Prompts
  promptString, prompt, promptRetry, promptNatural, promptPrimitive,
  -- * Rendering and Event Handling
  miniBufferWidget,
  handleMiniBufferEvent
) where

import           Brick
import qualified Brick.Widgets.Edit as E
import           Control.Lens (Traversal', traversal)
import           Control.Monad (ap, (>=>))
import           Data.Char (isNumber)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import           Text.Read (readMaybe)

-- | The type of editor underlying prompts
type MBEditor = E.Editor T.Text

-- | Minibuffer scripts.
data MiniBuffer n a
  = Pure a
  | Message String (MiniBuffer n a)
  | Prompt (Char -> Bool) (MBEditor n) String (String -> MiniBuffer n a)
  | Done
  deriving Functor

instance Applicative (MiniBuffer n) where
  pure  = Pure
  (<*>) = ap

instance Monad (MiniBuffer n) where
  return = pure
  Pure a >>= f = f a
  Prompt accept editor msg k >>= f = Prompt accept editor msg (k >=> f)
  Message msg k >>= f = Message msg (k >>= f)
  Done >>= _ = Done

-- | A Traversal over the 'MBEditor' in case the 'MiniBuffer' is a 'Prompt'
promptEditor :: Traversal' (MiniBuffer n a) (MBEditor n)
promptEditor = traversal $ \f mb -> case mb of
  Prompt accept editor msg k ->
    (\editor' -> Prompt accept editor' msg k) <$> f editor
  _ -> pure mb


-- | Create a minibuffer displaying a single text message, returning @()@.
message :: String -> MiniBuffer n ()
message msg = Message msg (return ())

-- | Create a prompt that accepts any 'String'.
--
-- @promptString name prmpt@ creates a minibuffer that shows @prmpt@ followed
-- by a text editor named @name@.
promptString :: n -> String -> MiniBuffer n String
promptString name msg = promptPrimitive name (const True) msg ""

-- | Create a prompt that accepts any 'Read'able.
--
-- @prompt name prmpt@ creates a minibufer that shows @prmpt@ followed by
-- a text editor named @name@.
-- Once the input is confirmed, the minibuffer tries to read the input,
-- returning @Just@ the read value or @Nothing@ if it fails.
prompt :: Read a => n -> String -> MiniBuffer n (Maybe a)
prompt name = fmap readMaybe . promptString name

-- | Create a retrying prompt that accepts any 'Read'able.
--
-- This behaves just like 'prompt', except that when the reading fails, it
-- shows an error message and tries again.
promptRetry :: Read a => n -> String -> MiniBuffer n a
promptRetry name msg = do
  mx <- prompt name msg
  case mx of
    Nothing -> do
      message "Invalid input. Hit Enter to try again."
      promptRetry name msg
    Just x -> return x

-- | Create a prompt that accepts only natural numbers.
--
-- This prompt will not accept non-figure characters as input.
promptNatural :: n -> String -> MiniBuffer n Int
promptNatural name msg = Prompt isNumber e msg k where
  e = E.editorText name (Just 1) T.empty
  k "" = message "Empty input. Press Enter to continue." >> abort
  k input = return (read input)

-- | Create a prompt, which returns its input as a 'String'
--
promptPrimitive
  :: n                    -- ^ name of the underlying editor
  -> (Char -> Bool)       -- ^ predicate of characters to accept
  ->  String              -- ^ prompt message
  -> String               -- ^ Initial buffer contents
  -> MiniBuffer n String
promptPrimitive name accept msg initial = Prompt accept e msg return where
  e = E.editorText name (Just 1) (T.pack initial)

-- | Abort the current minibuffer.
--
-- Sets the minibuffer to 'Done'.
abort :: MiniBuffer n a
abort = Done

-- | Return true if the minibuffer has completed.
isDone :: MiniBuffer n a -> Bool
isDone Done = True
isDone _     = False

-- | Render the minibuffer.
miniBufferWidget :: (Show n, Ord n) => MiniBuffer n a -> Widget n
miniBufferWidget (Pure _) = emptyWidget
miniBufferWidget (Message msg _) = strWrap msg
miniBufferWidget (Prompt _ e msg _) =
  str msg <+> E.renderEditor (txt . mconcat) True e
miniBufferWidget Done = emptyWidget

-- | Handle input events for the minibuffer.
--
-- The minibuffer responds to the following keys, in addition to the inputs
-- supported by 'E.Editor':
--
-- @ENTER@: acknowledge message/confirm input.
--
-- @CONTROL + g@: abort
handleMiniBufferEvent :: Eq n => BrickEvent n () -> EventM n (MiniBuffer n a) ()
handleMiniBufferEvent evt = get >>= \case
  -- Handle abort
  _ | VtyEvent key <- evt
    , V.EvKey (V.KChar 'g') [V.MCtrl]  <- key ->
      put abort
  -- Handle 'Message' minibuffer
  Message _ mb
    | VtyEvent key <- evt
    , V.EvKey V.KEnter [] <- key ->
      put mb
  -- Handle 'Prompt' minibuffer
  Prompt accept e _msg k
    -- Pass a character to the editor widget  
    | VtyEvent key <- evt
    , V.EvKey (V.KChar c) [] <- key
    , accept c ->
      zoom promptEditor $ E.handleEditorEvent evt
      
    -- Ignore unacceptable characters
    | VtyEvent key <- evt
    , V.EvKey (V.KChar _) [] <- key ->
      pure ()
      
    -- Accept input when Enter is pressed
    | VtyEvent key <- evt
    , V.EvKey V.KEnter [] <- key ->
      put $ k $ T.unpack $ mconcat $ E.getEditContents e

    -- Pass other events to the editor
    | otherwise ->
      zoom promptEditor $ E.handleEditorEvent evt
      
  -- catch all clause
  _ -> pure ()
      

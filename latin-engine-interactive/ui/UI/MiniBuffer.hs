{- |

Module:      UI.MiniBuffer
Description: A mini-buffer widget.
Maintainer:  alexander.vandenbroucke@gmail.com

A minibuffer is a small area that can display messages, and accept input (while
showing a prompt).

The minibuffer state is implemented as a Free-monad style data structure.
Using 'MiniBuffer' one essentially builds a script for interaction with the
user: a minibuffer can be one of four things:

1. @'Pure' x@: the script has completed, with a result @x@.

   Completed script can be extend with monadic bind '>>='.

2. @'Message' msg mb@: the script is currently displaying a message @msg@,
   after the message has been acknowledged, the script moves to a new state
   @mb@.

3. @'Prompt' accept editor prompt k@: The script is currently displaying a
   prompt @prompt@, and accepting character input.
   The editing of the input is deffered to the underlying @editor@ widget.
   Only characters that are accepted by the predicate 'accept' are passed to
   the @editor@. Once the user confirms the input, the current state of the
   editor is passed to the continuation @k@, which decides the next state.

4. @'Effect' m@: the script executes some effect @m@ in an underlying monad.

= Examples

The minibuffer @'message' "Hello, world"@ displays

> Hello, world

and returns @()@.
The minibuffer @'message' "Something went wrong!"@ displays

> Something went wrong!

Since 'MiniBuffer' is a monad, we can write scripts using
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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

module UI.MiniBuffer (
  -- * Data Type
  MiniBuffer(..),
  -- * Smart constructors
  message,
  -- ** Prompts
  promptString, prompt, promptRetry, promptNatural, promptPrimitive,
  -- * Running
  isPure,
  run,
  runM,
  -- * Rendering and Event Handling
  miniBufferWidget,
  handleMiniBufferEvent
) where

import           Brick
import qualified Brick.Widgets.Edit as E
import           Control.Lens (Traversal', traversal)
import qualified Lens.Micro.Mtl.Internal as Micro
import           Control.Monad (ap, (>=>))
import           Data.Char (isNumber)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import           Text.Read (readMaybe)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.State.Class (MonadState)
import qualified Control.Lens as L
import Control.Monad.IO.Class (MonadIO (liftIO))

-- | The type of editor underlying prompts
type MBEditor = E.Editor T.Text

-- | Minibuffer scripts.
data MiniBuffer n m a
  = Pure a
  | Message String (MiniBuffer n m a)
  | Prompt (Char -> Bool) (MBEditor n) String (String -> MiniBuffer n m a)
  | Effect (m (MiniBuffer n m a))
  deriving Functor

instance Functor m => Applicative (MiniBuffer n m) where
  pure  = Pure
  (<*>) = ap

instance Functor m => Monad (MiniBuffer n m) where
  return = pure
  Pure a >>= f = f a
  Prompt accept editor msg k >>= f = Prompt accept editor msg (k >=> f)
  Message msg k >>= f = Message msg (k >>= f)
  Effect m >>= f = Effect ((>>= f) <$> m)

instance MonadTrans (MiniBuffer n) where
  lift = Effect . fmap Pure

instance MonadState s m => MonadState s (MiniBuffer n m) where
  get = lift get
  put = lift . put

instance MonadFail m => MonadFail (MiniBuffer n m) where
   fail = lift . fail

instance MonadIO m => MonadIO (MiniBuffer n m) where
  liftIO = lift . liftIO

instance (Applicative m, Monoid a) => Semigroup (MiniBuffer n m a) where
  x <> y = (<>) <$> x <*> y

instance (Monoid a, Applicative m) => Monoid (MiniBuffer n m a) where
  mempty = pure mempty
  mappend = (<>)
  
newtype FocusedMB name (m :: * -> *) (zoomedM :: * -> * -> *) (c :: *) (a :: *) = FocusedMB {
  getFocusedMB :: zoomedM (MiniBuffer name m c) a
}

instance Functor (zoomedM (MiniBuffer name m c)) => Functor (FocusedMB name m zoomedM c) where
  fmap f = FocusedMB . fmap f . getFocusedMB

instance Applicative (zoomedM (MiniBuffer name m c)) => Applicative (FocusedMB name m zoomedM c) where
  pure = FocusedMB . pure
  ff <*> fx = FocusedMB $ getFocusedMB ff <*> getFocusedMB fx

type instance Micro.Zoomed (MiniBuffer name m) = FocusedMB name m (Micro.Zoomed m)

instance Micro.Zoom m n s t => Micro.Zoom (MiniBuffer name m) (MiniBuffer name n) s t where
  zoom :: forall c. L.LensLike' (Micro.Zoomed (MiniBuffer name m) c) t s -> MiniBuffer name m c -> MiniBuffer name n c
  zoom l = go
    where
      go :: MiniBuffer name m c -> MiniBuffer name n c
      go (Pure x) = Pure x
      go (Message msg mb) = Message msg (go mb)
      go (Prompt accept editor msg k) = Prompt accept editor msg (go . k)
      go (Effect m) = Effect (go <$> Micro.zoom l' m)

      l' :: L.LensLike' (Micro.Zoomed m (MiniBuffer name m c)) t s
      l' f = getFocusedMB . l (FocusedMB . f)




-- | A Traversal over the 'MBEditor' in case the 'MiniBuffer' is a 'Prompt'
promptEditor :: Traversal' (MiniBuffer n m a) (MBEditor n)
promptEditor = traversal $ \f mb -> case mb of
  Prompt accept editor msg k ->
    (\editor' -> Prompt accept editor' msg k) <$> f editor
  _ -> pure mb

-- | Create a minibuffer displaying a single text message, returning @()@.
message :: String -> MiniBuffer n m ()
message msg = Message msg (Pure ())

-- | Create a prompt that accepts any 'String'.
--
-- @promptString name prmpt@ creates a minibuffer that shows @prmpt@ followed
-- by a text editor named @name@.
promptString :: n -> String -> MiniBuffer n m String
promptString name msg = promptPrimitive name (const True) id msg ""

-- | Create a prompt that accepts any 'Read'able.
--
-- @prompt name prmpt@ creates a minibufer that shows @prmpt@ followed by
-- a text editor named @name@.
-- Once the input is confirmed, the minibuffer tries to read the input,
-- returning @Just@ the read value or @Nothing@ if it fails.
prompt :: Read a => n -> String -> MiniBuffer n m (Maybe a)
prompt name msg = promptPrimitive name (const True) readMaybe msg ""

-- | Create a retrying prompt that accepts any 'Read'able.
--
-- This behaves just like 'prompt', except that when the reading fails, it
-- shows an error message and tries again.
promptRetry :: Functor m => Read a => n -> String -> MiniBuffer n m a
promptRetry name msg = do
  mx <- prompt name msg
  case mx of
    Nothing -> do
      message "Invalid input. Hit Enter to try again."
      promptRetry name msg
    Just x -> return x

-- | Create a prompt that accepts only natural numbers.
--
-- This prompt will not accept non-figure characters as input and will not
-- accept empty input.
promptNatural :: Functor m => n -> String -> MiniBuffer n m Int
promptNatural name msg = Prompt isNumber e msg k where
  e = E.editorText name (Just 1) T.empty
  k "" = promptNatural name msg
  k input = return (read input)

-- | Create a prompt, which returns its input as a 'String'
--
promptPrimitive
  :: n                    -- ^ name of the underlying editor
  -> (Char -> Bool)       -- ^ predicate of characters to accept
  -> (String -> a)        -- ^ how to convert the read 'String' to @a@
  -> String               -- ^ prompt message
  -> String               -- ^ Initial buffer contents
  -> MiniBuffer n m a
promptPrimitive name accept parse msg initial = Prompt accept e msg (Pure . parse) where
  e = E.editorText name (Just 1) (T.pack initial)

-- | Render the minibuffer.
miniBufferWidget :: (Show n, Ord n) => MiniBuffer n m a -> Widget n
miniBufferWidget (Pure _) = emptyWidget
miniBufferWidget (Message msg _) = strWrap msg
miniBufferWidget (Prompt _ e msg _) =
  str msg <+> E.renderEditor (txt . mconcat) True e
miniBufferWidget (Effect _) = strWrap "BUG: EFFECT MINIBUFFER"

-- | Return 'True' if the 'MiniBuffer' is 'Pure'
isPure :: MiniBuffer n m a -> Bool
isPure Pure{} = True
isPure _      = False

-- | Evaluate the minibuffer as far as possible
run :: Monad m => MiniBuffer n m a -> m (Either (MiniBuffer n m a) a)
run mb = case mb of
  Pure x    -> pure $ Right x
  Message{} -> pure $ Left mb
  Prompt{}  -> pure $ Left mb
  Effect m  -> m >>= run

-- | Evaluate the minibuffer within a state as far as possible 
runM :: MonadState (MiniBuffer n m a) m => m ()
runM = get >>= run >>= put . either id Pure

-- | handle input events for the minibuffer.
--
-- The minibuffer responds to the following keys, in addition to the inputs
-- supported by 'E.Editor':
--
-- @ENTER@: acknowledge message/confirm input.
--
-- @CONTROL + g@: abort
--
-- The new minibuffer is already evaluated, i.e.,
--
-- prop> handleMiniBufferEvent (pure ()) >> runM = handleMiniBufferEvent (pure ())
--
handleMiniBufferEvent
  :: Eq n
  => EventM n (MiniBuffer n m ()) ()
  -- ^ How to handle aborts. The new minibuffer set must be fully evaluated.
  -> BrickEvent n ()
  -> EventM n (MiniBuffer n m ()) ()
handleMiniBufferEvent abort evt = get >>= \case
  -- Handle abort
  _ | VtyEvent key <- evt
    , V.EvKey (V.KChar 'g') [V.MCtrl]  <- key ->
      abort
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


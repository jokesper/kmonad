{-|
Module      : KMonad.Keyboard.IO.Windows.LowLevelHookSource
Description : Load and acquire a windows low-level keyboard hook.
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

-}
module KMonad.Keyboard.IO.Windows.LowLevelHookSource
  ( llHook
  )
where

import KMonad.Prelude

import Foreign.Marshal hiding (void)
import Foreign.Ptr
import Foreign.Storable

import KMonad.Keyboard
import KMonad.Keyboard.IO
import KMonad.Keyboard.IO.Windows.Types

--------------------------------------------------------------------------------

-- | Use the windows c-api to `grab` a keyboard
foreign import ccall "grab_kb"
  grab_kb :: IO ()

-- | Release the keyboard hook
foreign import ccall "release_kb"
  release_kb :: IO Word8

-- | Pass a pointer to a buffer to wait_key, when it returns the buffer can be
-- read for the next key event.
foreign import ccall "wait_key"
  c_wait_key :: Ptr WinKeyEvent -> IO Word32

-- | A small wrapper around the C function 'c_wait_key'. Sometimes the read from
-- the internal pipe in the C code gets interrupted. Probably due to events we
-- want to ignore. Therefore we limit retries on empty reads. (See #1003)
wait_key :: HasLogFunc e => Ptr WinKeyEvent -> RIO e ()
wait_key = wait_key' 3
 where
  wait_key' :: HasLogFunc e => Int -> Ptr WinKeyEvent -> RIO e ()
  wait_key' 0 _ = throwIO TooManyEmptyReads
  wait_key' n buffer = do
    read <- liftIO $ c_wait_key buffer
    if
      | read == 0 -> do
        logDebug "Empty read from key-source."
        wait_key' (n-1) buffer
      | fromEnum read == sizeOf (undefined :: WinKeyEvent) -> pure ()
      | otherwise -> throwIO $ UnexpetedNumberOfBytesRead read

--------------------------------------------------------------------------------

-- | Data used to track `connection` to windows process
data LLHook = LLHook
  { _thread :: !(Async ())        -- ^ The thread-id of the listen-process
  , _buffer :: !(Ptr WinKeyEvent) -- ^ Buffer used to communicate with process
  }
makeLenses ''LLHook

-- | Return a KeySource using the Windows low-level hook approach.
llHook :: HasLogFunc e => RIO e (Acquire KeySource)
llHook = mkKeySource llOpen llClose llRead


--------------------------------------------------------------------------------

-- | Ask windows to install a hook and allocate the reading-buffer
llOpen :: HasLogFunc e => RIO e LLHook
llOpen = do
  logInfo "Registering low-level Windows keyboard hook"
  liftIO $ do
    tid <- async grab_kb
    buf <- mallocBytes $ sizeOf (undefined :: WinKeyEvent)
    pure $ LLHook tid buf

-- | Ask windows to unregister the hook and free the data-buffer
llClose :: HasLogFunc e => LLHook -> RIO e ()
llClose ll = do
  logInfo "Unregistering low-level Windows keyboard hook"
  liftIO $ do
    _ <- release_kb
    cancel $ ll^.thread -- This might not be necessary, but it is safer
    free   $ ll^.buffer

-- | Get a new 'KeyEvent' from Windows
--
-- NOTE: This can throw an error if the event fails to convert.
llRead :: HasLogFunc e => LLHook -> RIO e KeyEvent
llRead ll = do
  wait_key $ ll^.buffer
  we <- liftIO $ peek (ll^.buffer)
  either throwIO pure $ fromWinKeyEvent we

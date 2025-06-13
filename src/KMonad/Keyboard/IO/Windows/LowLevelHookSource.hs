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
  c_wait_key :: Ptr WinKeyEvent -> Ptr Word32 -> IO Word32


-- | A small wrapper around the C function 'c_wait_key'. Sometimes the read from
-- the internal pipe in the C code gets interrupted. Probably due to events we
-- want to ignore. Therefore we limit retries on empty reads. (See #1003)
wait_key :: HasLogFunc e => Ptr WinKeyEvent -> RIO e ()
wait_key buffer = do
  (err, read) <- liftIO $ alloca $ \read -> do
    (,) <$> c_wait_key buffer read <*> peek read
  if
    | err /= 0 -> throwIO $ WinErrorWhileReadingPipe err
    | read == 0 -> do
      logDebug "Empty read from key-source."
      wait_key buffer
    | fromEnum read == sizeOf (undefined :: WinKeyEvent) -> pure ()
    | otherwise -> throwIO $ UnexpetedNumberOfBytesRead read

--------------------------------------------------------------------------------

-- | Data used to track `connection` to windows process
newtype LLHook = LLHook (Ptr WinKeyEvent) -- ^ Buffer used to communicate with process
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
    grab_kb
    buf <- mallocBytes $ sizeOf (undefined :: WinKeyEvent)
    pure $ LLHook buf

-- | Ask windows to unregister the hook and free the data-buffer
llClose :: HasLogFunc e => LLHook -> RIO e ()
llClose (LLHook buf) = do
  logInfo "Unregistering low-level Windows keyboard hook"
  liftIO $ do
    _ <- release_kb
    free buf

-- | Get a new 'KeyEvent' from Windows
--
-- NOTE: This can throw an error if the event fails to convert.
llRead :: HasLogFunc e => LLHook -> RIO e KeyEvent
llRead (LLHook buf) = do
  wait_key buf
  we <- liftIO $ peek buf
  either throwIO pure $ fromWinKeyEvent we

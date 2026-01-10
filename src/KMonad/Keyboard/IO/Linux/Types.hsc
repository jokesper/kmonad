{-|
Module      : KMonad.Keyboard.IO.Linux.Types
Description : The types particular to Linux key IO
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

-}
module KMonad.Keyboard.IO.Linux.Types
  ( -- * The LinuxInputEvent datatype, its constructors, and instances
    -- $types
    LinuxInputEvent(..)
  , sync

    -- * Casting between 'KeyEvent' and 'LinuxInputEvent'
    -- $linuxev
  , toLinuxInputEvent
  , fromLinuxInputEvent

    -- * Reexport common modules
  , module KMonad.Keyboard
  , module KMonad.Keyboard.IO
  )
where

import KMonad.Prelude

import Data.Time.Clock.System
import Foreign.C.Types
import Foreign.Storable
import RIO.Partial (toEnum)

import KMonad.Keyboard
import KMonad.Keyboard.IO
import KMonad.Util


--------------------------------------------------------------------------------
-- $helper

sysTime2CTime :: SystemTime -> CTimeVal
sysTime2CTime (MkSystemTime s ns) = CTimeVal (fromIntegral s) (fromIntegral $ ns `div` 1000)

--------------------------------------------------------------------------------
-- $types
--
-- Linux produces a stream of binary data representing all its input events
-- through the \/dev\/input files. Each event is represented by 5 numbers:
-- seconds, microseconds, event-type, event-code, and event-value. For more
-- explanation look at: https://www.kernel.org/doc/Documentation/input/input.txt

data CTimeVal = CTimeVal CTime CSUSeconds
  deriving Show

-- | The LinuxInputEvent datatype ('struct input_event' from '<linux/input.h>')
data LinuxInputEvent = LinuxInputEvent
  { _time :: CTimeVal
  , _eventType :: Word16
  , _eventCode :: Word16
  , _eventValue :: Int32
  } deriving Show

instance Storable CTimeVal where
  #include <sys/time.h>
  sizeOf _ = (#size struct timeval)
  alignment _ = (#alignment struct timeval)
  peek ptr = CTimeVal
    <$> (#peek struct timeval, tv_sec) ptr
    <*> (#peek struct timeval, tv_usec) ptr
  poke ptr (CTimeVal s us) = do
    (#poke struct timeval, tv_sec) ptr s
    (#poke struct timeval, tv_usec) ptr us

instance Storable LinuxInputEvent where
  #include <linux/input.h>
  sizeOf _ = (#size struct input_event)
  alignment _ = (#alignment struct input_event)
  peek ptr = LinuxInputEvent
    <$> (#peek struct input_event, time) ptr
    <*> (#peek struct input_event, type) ptr
    <*> (#peek struct input_event, code) ptr
    <*> (#peek struct input_event, value) ptr
  poke ptr (LinuxInputEvent time ty code value) = do
    (#poke struct input_event, time) ptr time
    (#poke struct input_event, type) ptr ty
    (#poke struct input_event, code) ptr code
    (#poke struct input_event, value) ptr value

-- | Constructor for linux sync events. Whenever you write an event to linux,
-- you need to emit a 'sync' to signal to linux that it should sync all queued
-- updates.
sync :: SystemTime -> LinuxInputEvent
sync time = LinuxInputEvent (sysTime2CTime time) 0 0 0


-------------------------------------------------------------------------------
-- $linuxev
--
-- We only represent a subset of all the possible input events produced by
-- Linux. First of all, we disregard all event types that are not key events, so
-- we quietly ignore all sync and scan events. There other other events that are
-- there to do things like toggle LEDs on your keyboard that we also ignore.
--
-- Furthermore, within the category of KeyEvents, we only register presses and
-- releases, and completely ignore repeat events.
--
-- The correspondence between LinuxInputEvents and core KeyEvents can best be read
-- in the above-mentioned documentation, but the quick version is this:
--   Typ:  1 = KeyEvent            (see below)
--         4 = @scancode@ event    (we neither read nor write)
--         0 = 'sync' event        (we don't read, but do generate for writing)
--   Val:  for keys: 0 = Release, 1 = Press, 2 = Repeat
--         for sync: always 0
--   Code: for keys: an Int value corresponding to a keycode
--           see: https://github.com/torvalds/linux/blob/master/include/uapi/linux/input-event-codes.h
--         for sync: always 0

-- | Translate a 'LinuxInputEvent' to a KMonad 'KeyEvent'
fromLinuxInputEvent :: LinuxInputEvent -> Maybe KeyEvent
fromLinuxInputEvent (LinuxInputEvent _ typ c val)
  | c >= 0x2ff = Nothing
  | typ == 1 && val == 0 = Just . mkRelease $ kc
  | typ == 1 && val == 1 = Just . mkPress   $ kc
  | otherwise = Nothing
  where
    kc = toEnum . fromIntegral $ c -- This is theoretically partial, but practically not

-- | Translate KMonad 'KeyEvent' along with a 'SystemTime' to 'LinuxInputEvent's
-- for writing.
toLinuxInputEvent :: KeyEvent -> SystemTime -> LinuxInputEvent
toLinuxInputEvent e time
  = LinuxInputEvent (sysTime2CTime time) 1 c val
  where
    c   = fromIntegral . fromEnum $ e^.keycode
    val = if e^.switch == Press then 1 else 0

{-# LANGUAGE DeriveAnyClass #-}
{-|
Module      : KMonad.Keyboard.Keycode
Description : Description of all possible keycodes.
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

'Keycode's are represented as a large enum lining up the keycodes defined in the Linux headers.

-}
module KMonad.Keyboard.Keycode
  ( -- * The core Keycode type
    -- $typ
    Keycode(..)

    -- * Naming utilities to refer to Keycodes
    -- $names
  , keyNames

  )
where

import KMonad.Prelude

import qualified KMonad.Util.MultiMap     as Q
import qualified RIO.HashSet       as S
import qualified RIO.Text          as T
import qualified RIO.Text.Partial  as T (head)

--------------------------------------------------------------------------------
-- $typ
--
-- 'Keycode's are defined as a large ADT that mimics the keycodes from the Linux
-- headers:
-- https://github.com/torvalds/linux/blob/master/include/uapi/linux/input-event-codes.h.
--
-- Anywhere there are missing regions in the Linux headers, we've defined
-- @MissingXX@ values for the ADT.
--
-- Calling 'RIO.Partial.toEnum' on a Linux keycode value should produce the
-- corresponding 'Keycode' value and vice-versa.

-- | The 'Keycode' datatype, as an 'Enum' of all the values a 'Keycode' can take.
data Keycode
  = KeyReserved
  | KeyEsc
  | Key1
  | Key2
  | Key3
  | Key4
  | Key5
  | Key6
  | Key7
  | Key8
  | Key9
  | Key0
  | KeyMinus
  | KeyEqual
  | KeyBackspace
  | KeyTab
  | KeyQ
  | KeyW
  | KeyE
  | KeyR
  | KeyT
  | KeyY
  | KeyU
  | KeyI
  | KeyO
  | KeyP
  | KeyLeftBrace
  | KeyRightBrace
  | KeyEnter
  | KeyLeftCtrl
  | KeyA
  | KeyS
  | KeyD
  | KeyF
  | KeyG
  | KeyH
  | KeyJ
  | KeyK
  | KeyL
  | KeySemicolon
  | KeyApostrophe
  | KeyGrave
  | KeyLeftShift
  | KeyBackslash
  | KeyZ
  | KeyX
  | KeyC
  | KeyV
  | KeyB
  | KeyN
  | KeyM
  | KeyComma
  | KeyDot
  | KeySlash
  | KeyRightShift
  | KeyKpAsterisk
  | KeyLeftAlt
  | KeySpace
  | KeyCapsLock
  | KeyF1
  | KeyF2
  | KeyF3
  | KeyF4
  | KeyF5
  | KeyF6
  | KeyF7
  | KeyF8
  | KeyF9
  | KeyF10
  | KeyNumLock
  | KeyScrollLock
  | KeyKp7
  | KeyKp8
  | KeyKp9
  | KeyKpMinus
  | KeyKp4
  | KeyKp5
  | KeyKp6
  | KeyKpPlus
  | KeyKp1
  | KeyKp2
  | KeyKp3
  | KeyKp0
  | KeyKpDot
  | Missing84
  | KeyZenkakuHankaku
  | Key102nd
  | KeyF11
  | KeyF12
  | KeyRo
  | KeyKatakana
  | KeyHiragana
  | KeyHenkan
  | KeyKatakanaHiragana
  | KeyMuhenkan
  | KeyKpjpcomma
  | KeyKpEnter
  | KeyRightCtrl
  | KeyKpSlash
  | KeySysRq
  | KeyRightAlt
  | KeyLinefeed
  | KeyHome
  | KeyUp
  | KeyPageUp
  | KeyLeft
  | KeyRight
  | KeyEnd
  | KeyDown
  | KeyPageDown
  | KeyInsert
  | KeyDelete
  | KeyMacro
  | KeyMute
  | KeyVolumeDown
  | KeyVolumeUp
  | KeyPower
  | KeyKpEqual
  | KeyKpPlusMinus
  | KeyPause
  | KeyScale
  | KeyKpComma
  | KeyHangeul
  | KeyHanja
  | KeyYen
  | KeyLeftMeta
  | KeyRightMeta
  | KeyCompose
  | KeyStop
  | KeyAgain
  | KeyProps
  | KeyUndo
  | KeyFront
  | KeyCopy
  | KeyOpen
  | KeyPaste
  | KeyFind
  | KeyCut
  | KeyHelp
  | KeyMenu
  | KeyCalc
  | KeySetup
  | KeySleep
  | KeyWakeUp
  | KeyFile
  | KeySendFile
  | KeyDeleteFile
  | KeyXfer
  | KeyProg1
  | KeyProg2
  | KeyWww
  | KeyMsDos
  | KeyCoffee
  | KeyDirection
  | KeyCycleWindows
  | KeyMail
  | KeyBookmarks
  | KeyComputer
  | KeyBack
  | KeyForward
  | KeyCloseCd
  | KeyEjectCd
  | KeyEjectCloseCd
  | KeyNextSong
  | KeyPlayPause
  | KeyPreviousSong
  | KeyStopCd
  | KeyRecord
  | KeyRewind
  | KeyPhone
  | KeyIso
  | KeyConfig
  | KeyHomepage
  | KeyRefresh
  | KeyExit
  | KeyMove
  | KeyEdit
  | KeyScrollUp
  | KeyScrollDown
  | KeyKpLeftParen
  | KeyKpRightParen
  | KeyNew
  | KeyRedo
  | KeyF13
  | KeyF14
  | KeyF15
  | KeyF16
  | KeyF17
  | KeyF18
  | KeyF19
  | KeyF20
  | KeyF21
  | KeyF22
  | KeyF23
  | KeyF24
  | Missing195
  | Missing196
  | Missing197
  | Missing198
  | Missing199
  | KeyPlayCd
  | KeyPauseCd
  | KeyProg3
  | KeyProg4
  | KeyDashboard
  | KeySuspend
  | KeyClose
  | KeyPlay
  | KeyFastForward
  | KeyBassBoost
  | KeyPrint
  | KeyHp
  | KeyCamera
  | KeySound
  | KeyQuestion
  | KeyEmail
  | KeyChat
  | KeySearch
  | KeyConnect
  | KeyFinance
  | KeySport
  | KeyShop
  | KeyAlterase
  | KeyCancel
  | KeyBrightnessDown
  | KeyBrightnessUp
  | KeyMedia
  | KeySwitchVideoMode
  | KeyKbdIllumToggle
  | KeyKbdIllumDown
  | KeyKbdIllumUp
  | KeySend
  | KeyReply
  | KeyForwardMail
  | KeySave
  | KeyDocuments
  | KeyBattery
  | KeyBluetooth
  | KeyWlan
  | KeyUwb
  | KeyUnknown
  | KeyVideoNext
  | KeyVideoPrev
  | KeyBrightnessCycle
  | KeyBrightnessZero
  | KeyDisplayOff
  | KeyWimax
  | KeyRfkill
  | KeyMicmute
  | Missing249
  | Missing250
  | Missing251
  | Missing252
  | Missing253
  | Missing254
  | Missing255
  -- Darwin
  | KeyFn
  | KeyLaunchpad
  | KeyMissionCtrl
  | KeySpotlight
  | KeyDictation
  deriving (Eq, Show, Bounded, Enum, Ord, Generic, Hashable, Typeable, Data)


instance Display Keycode where
  textDisplay c = (\t -> "<" <> t <> ">") . fromMaybe (tshow c)
    $ minimumByOf (_Just . folded) cmpName (keyNames ^. at c)
    where cmpName a b =
            -- Prefer the shortest, and if equal, lowercased version
            case compare (T.length a) (T.length b) of
              EQ -> compare (T.head b) (T.head a)
              o  -> o

--------------------------------------------------------------------------------
-- $sets

-- | The set of all existing 'Keycode'
kcAll :: S.HashSet Keycode
kcAll = S.fromList [minBound .. maxBound]

-- | The set of all 'Keycode' that are not of the MissingXX pattern
kcNotMissing :: S.HashSet Keycode
kcNotMissing = S.fromList $ kcAll ^.. folded . filtered (T.isPrefixOf "Key" . tshow)

--------------------------------------------------------------------------------
-- $names

-- | Helper function to generate easy name maps
nameKC :: Foldable t
  => (Keycode -> Text)
  -> t Keycode
  -> Q.MultiMap Keycode Text
nameKC f = Q.mkMultiMap . map go . toList
  where go k = (k, [f k, T.toLower $ f k])

-- | A collection of 'Keycode' to 'Text' mappings
keyNames :: Q.MultiMap Keycode Text
keyNames = mconcat
  [ nameKC tshow               kcAll
  , nameKC (T.drop 3 . tshow)  kcNotMissing
  , aliases
  ]

-- | A collection of useful aliases to refer to keycode names
aliases :: Q.MultiMap Keycode Text
aliases = Q.mkMultiMap
  [ (KeyEnter,            ["ret", "return", "ent"])
  , (KeyMinus,            ["min", "-"])
  , (KeyEqual,            ["eql", "="])
  , (KeySleep,            ["zzz"])
  , (KeySpace,            ["spc"])
  , (KeyPageUp,           ["pgup"])
  , (KeyPageDown,         ["pgdn"])
  , (KeyInsert,           ["ins"])
  , (KeyDelete,           ["del"])
  , (KeyVolumeUp,         ["volu"])
  , (KeyVolumeDown,       ["voldwn", "vold"])
  , (KeyBrightnessUp,     ["brup", "bru"])
  , (KeyBrightnessDown,   ["brdown", "brdwn", "brdn"])
  , (KeyLeftAlt,          ["lalt", "alt"])
  , (KeyRightAlt,         ["ralt"])
  , (KeyCompose,          ["comp", "cmps", "cmp"])
  , (KeyLeftShift,        ["lshift", "lshft", "lsft", "shft", "sft"])
  , (KeyRightShift,       ["rshift", "rshft", "rsft"])
  , (KeyLeftCtrl,         ["lctrl", "lctl", "ctl"])
  , (KeyRightCtrl,        ["rctrl", "rctl"])
  , (KeyLeftMeta,         ["lmeta", "lmet", "met"])
  , (KeyRightMeta,        ["rmeta", "rmet"])
  , (KeyBackspace,        ["bks", "bspc"])
  , (KeyCapsLock,         ["caps"])
  , (Key102nd,            ["102d", "lsgt", "nubs"])
  , (KeyForward,          ["fwd"])
  , (KeyScrollLock,       ["scrlck", "slck"])
  , (KeyScrollUp,         ["scrup", "sup"])
  , (KeyScrollDown,       ["scrdn", "sdwn", "sdn"])
  , (KeyPrint,            ["prnt"])
  , (KeyWakeUp,           ["wkup"])
  , (KeyLeft,             ["lft"])
  , (KeyRight,            ["rght"])
  , (KeyLeftBrace,        ["lbrc", "["])
  , (KeyRightBrace,       ["rbrc", "]"])
  , (KeySemicolon,        ["scln", ";"])
  , (KeyApostrophe,       ["apos", "'", "apo"])
  , (KeyGrave,            ["grv", "`"])
  , (KeyBackslash,        ["bksl", "\\"]) -- NOTE: "\\" here is a 1char string, the first \ is consumed by Haskell as an escape character
  , (KeyComma,            ["comm", ","])
  , (KeyDot,              ["."])
  , (KeySlash,            ["/"])
  , (KeyNumLock,          ["nlck"])
  , (KeyKpSlash,          ["kp/"])
  , (KeyKpEnter,          ["kprt"])
  , (KeyKpPlus,           ["kp+"])
  , (KeyKpAsterisk,       ["kp*"])
  , (KeyKpMinus,          ["kp-"])
  , (KeyKpDot,            ["kp."])
  , (KeySysRq,            ["ssrq", "sys"])
  , (KeyKbdIllumDown,     ["bldn"])
  , (KeyKbdIllumUp,       ["blup"])
  , (KeyNextSong,         ["next"])
  , (KeyPlayPause,        ["pp"])
  , (KeyPreviousSong,     ["prev"])
  , (KeyMicmute,          ["micm"])
  , (KeyCoffee,           ["lock"])
  -- Darwin
  , (KeyLaunchpad,        ["lp"])
  , (KeyMissionCtrl,      ["mctl"])
  , (KeySpotlight,        ["spot"])
  , (KeyDictation,        ["dict"])
  -- Japanese
  , (KeyZenkakuHankaku,   ["zeh"])
  , (KeyMuhenkan,         ["muh"])
  , (KeyHenkan,           ["hen"])
  , (KeyKatakanaHiragana, ["kah"])
  -- HID Usage Names
  , (KeyBackslash,        ["nonuspound"])
  , (KeyCompose,          ["app", "application"])
  , (KeyOpen,             ["exec", "execute"])
  , (KeyFront,            ["sel", "select"])
  , (KeyRo,               ["i1", "int1", "international1"])
  , (KeyKatakanaHiragana, ["i2", "int2", "international2"])
  , (KeyYen,              ["i3", "int3", "international3"])
  , (KeyHenkan,           ["i4", "int4", "international4"])
  , (KeyMuhenkan,         ["i5", "int5", "international5"])
  , (KeyKpjpcomma,        ["i6", "int6", "international6"])
  , (KeyHangeul,          ["l1", "lang1"])
  , (KeyHanja,            ["l2", "lang2"])
  , (KeyKatakana,         ["l3", "lang3"])
  , (KeyHiragana,         ["l4", "lang4"])
  , (KeyZenkakuHankaku,   ["l5", "lang5"])
  , (KeyExit,             ["quit"])
  , (KeyNextSong,         ["nexttrack"])
  , (KeyPreviousSong,     ["previoustrack"])
  -- , (KeyStopCd,           ["stop"]) -- conflict with KeyStop
  , (KeyEjectCd,          ["eject"])
  , (KeyVolumeUp,         ["volumeincrement"])
  , (KeyVolumeDown,       ["volumedecrement"])
  , (KeyMail,             ["emailreader"])
  , (KeyFinance,          ["checkbook"])
  , (KeyCalc,             ["calculator"])
  , (KeyFile,             ["localmachinebrowser"])
  , (KeyWww,              ["internetbrowser"])
  , (KeyCoffee,           ["termlock", "screensaver"])
  , (KeyHelp,             ["helpcenter"])
  , (KeyMedia,            ["imagebrowser"])
  , (KeySound,            ["audiobrowser"])
  , (KeyProps,            ["properties"])
  -- , (KeyHomepage,         ["home"]) -- conflict with KeyHome
  , (KeyForwardMail,      ["forwardmessage"])
  ]

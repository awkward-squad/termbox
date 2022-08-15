{-# LANGUAGE PatternSynonyms #-}

module Termbox.Key
  ( Key (..),
    parseKey,
    pattern KeyCtrlH,
    pattern KeyCtrlLsqBracket,
    pattern KeyCtrl2,
    pattern KeyCtrl3,
    pattern KeyCtrl4,
    pattern KeyCtrl5,
    pattern KeyCtrl7,
    pattern KeyCtrlM,
    pattern KeyCtrlI,
    pattern KeyCtrlUnderscore,
  )
where

import Data.Word (Word16)
import qualified Termbox.Bindings.C as C

-- | A key event.
data Key
  = KeyChar Char
  | KeyArrowDown
  | KeyArrowLeft
  | KeyArrowRight
  | KeyArrowUp
  | KeyBackspace
  | -- | Also: @Ctrl+H@
    KeyCtrlBackspace
  | KeyCtrl6
  | KeyCtrl8
  | KeyCtrlA
  | KeyCtrlB
  | -- | Also: @Ctrl-4@
    KeyCtrlBackslash
  | KeyCtrlC
  | KeyCtrlD
  | KeyCtrlE
  | KeyCtrlF
  | KeyCtrlG
  | KeyCtrlJ
  | KeyCtrlK
  | KeyCtrlL
  | KeyCtrlN
  | KeyCtrlO
  | KeyCtrlP
  | KeyCtrlQ
  | KeyCtrlR
  | -- | Also: @Ctrl-5@
    KeyCtrlRsqBracket
  | KeyCtrlS
  | -- | Also: @Ctrl-/@, @Ctrl-_@
    KeyCtrlSlash
  | -- | Also: @Ctrl+2@
    KeyCtrlTilde
  | KeyCtrlT
  | KeyCtrlU
  | KeyCtrlV
  | KeyCtrlW
  | KeyCtrlX
  | KeyCtrlY
  | KeyCtrlZ
  | KeyDelete
  | KeyEnd
  | -- | Also: @Ctrl-M@
    KeyEnter
  | -- | Also: @Ctrl-[@, @Ctrl-3@
    KeyEsc
  | KeyF1
  | KeyF10
  | KeyF11
  | KeyF12
  | KeyF2
  | KeyF3
  | KeyF4
  | KeyF5
  | KeyF6
  | KeyF7
  | KeyF8
  | KeyF9
  | KeyHome
  | KeyInsert
  | KeyPageDn
  | KeyPageUp
  | KeySpace
  | -- | Also: @Ctrl+I@
    KeyTab
  deriving stock (Eq, Ord, Show)

pattern KeyCtrlH :: Key
pattern KeyCtrlH = KeyCtrlBackspace

pattern KeyCtrlLsqBracket :: Key
pattern KeyCtrlLsqBracket = KeyEsc

pattern KeyCtrl2 :: Key
pattern KeyCtrl2 = KeyCtrlTilde

pattern KeyCtrl3 :: Key
pattern KeyCtrl3 = KeyEsc

pattern KeyCtrl4 :: Key
pattern KeyCtrl4 = KeyCtrlBackslash

pattern KeyCtrl5 :: Key
pattern KeyCtrl5 = KeyCtrlRsqBracket

pattern KeyCtrl7 :: Key
pattern KeyCtrl7 = KeyCtrlSlash

pattern KeyCtrlM :: Key
pattern KeyCtrlM = KeyEnter

pattern KeyCtrlI :: Key
pattern KeyCtrlI = KeyTab

pattern KeyCtrlUnderscore :: Key
pattern KeyCtrlUnderscore = KeyCtrlSlash

parseKey :: Word16 -> Key
parseKey key
  | key == C._TB_KEY_ARROW_DOWN = KeyArrowDown
  | key == C._TB_KEY_ARROW_LEFT = KeyArrowLeft
  | key == C._TB_KEY_ARROW_RIGHT = KeyArrowRight
  | key == C._TB_KEY_ARROW_UP = KeyArrowUp
  | key == C._TB_KEY_BACKSPACE = KeyBackspace
  | key == C._TB_KEY_CTRL_TILDE = KeyCtrlTilde
  | key == C._TB_KEY_CTRL_6 = KeyCtrl6
  | key == C._TB_KEY_CTRL_8 = KeyCtrl8
  | key == C._TB_KEY_CTRL_A = KeyCtrlA
  | key == C._TB_KEY_CTRL_B = KeyCtrlB
  | key == C._TB_KEY_CTRL_BACKSLASH = KeyCtrlBackslash
  | key == C._TB_KEY_CTRL_C = KeyCtrlC
  | key == C._TB_KEY_CTRL_D = KeyCtrlD
  | key == C._TB_KEY_CTRL_E = KeyCtrlE
  | key == C._TB_KEY_CTRL_F = KeyCtrlF
  | key == C._TB_KEY_CTRL_G = KeyCtrlG
  | key == C._TB_KEY_CTRL_H = KeyCtrlBackspace
  | key == C._TB_KEY_CTRL_J = KeyCtrlJ
  | key == C._TB_KEY_CTRL_K = KeyCtrlK
  | key == C._TB_KEY_CTRL_L = KeyCtrlL
  | key == C._TB_KEY_CTRL_N = KeyCtrlN
  | key == C._TB_KEY_CTRL_O = KeyCtrlO
  | key == C._TB_KEY_CTRL_P = KeyCtrlP
  | key == C._TB_KEY_CTRL_Q = KeyCtrlQ
  | key == C._TB_KEY_CTRL_R = KeyCtrlR
  | key == C._TB_KEY_CTRL_RSQ_BRACKET = KeyCtrlRsqBracket
  | key == C._TB_KEY_CTRL_S = KeyCtrlS
  | key == C._TB_KEY_CTRL_SLASH = KeyCtrlSlash
  | key == C._TB_KEY_CTRL_T = KeyCtrlT
  | key == C._TB_KEY_CTRL_U = KeyCtrlU
  | key == C._TB_KEY_CTRL_V = KeyCtrlV
  | key == C._TB_KEY_CTRL_W = KeyCtrlW
  | key == C._TB_KEY_CTRL_X = KeyCtrlX
  | key == C._TB_KEY_CTRL_Y = KeyCtrlY
  | key == C._TB_KEY_CTRL_Z = KeyCtrlZ
  | key == C._TB_KEY_DELETE = KeyDelete
  | key == C._TB_KEY_END = KeyEnd
  | key == C._TB_KEY_ENTER = KeyEnter
  | key == C._TB_KEY_ESC = KeyEsc
  | key == C._TB_KEY_F1 = KeyF1
  | key == C._TB_KEY_F10 = KeyF10
  | key == C._TB_KEY_F11 = KeyF11
  | key == C._TB_KEY_F12 = KeyF12
  | key == C._TB_KEY_F2 = KeyF2
  | key == C._TB_KEY_F3 = KeyF3
  | key == C._TB_KEY_F4 = KeyF4
  | key == C._TB_KEY_F5 = KeyF5
  | key == C._TB_KEY_F6 = KeyF6
  | key == C._TB_KEY_F7 = KeyF7
  | key == C._TB_KEY_F8 = KeyF8
  | key == C._TB_KEY_F9 = KeyF9
  | key == C._TB_KEY_HOME = KeyHome
  | key == C._TB_KEY_INSERT = KeyInsert
  | key == C._TB_KEY_PGDN = KeyPageDn
  | key == C._TB_KEY_PGUP = KeyPageUp
  | key == C._TB_KEY_SPACE = KeySpace
  | key == C._TB_KEY_TAB = KeyTab
  | otherwise = error ("termbox: unknown key " ++ show key)

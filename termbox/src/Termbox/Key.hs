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

import qualified Termbox.Bindings

-- | A key event.
--
-- In a few cases, distinct key sequences map to equivalent key events. Pattern synonyms below are provided for an
-- alternate syntax in these cases, if desired.
data Key
  = KeyChar Char
  | KeyArrowDown
  | KeyArrowLeft
  | KeyArrowRight
  | KeyArrowUp
  | KeyBackspace
  | -- | Also 'KeyCtrlH'
    KeyCtrlBackspace
  | KeyCtrl6
  | KeyCtrl8
  | KeyCtrlA
  | KeyCtrlB
  | -- | Also 'KeyCtrl4'
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
  | -- | Also 'KeyCtrl5'
    KeyCtrlRsqBracket
  | KeyCtrlS
  | -- | Also 'KeyCtrl7', 'KeyCtrlUnderscore'
    KeyCtrlSlash
  | -- | Also 'KeyCtrl2'
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
  | -- | Also 'KeyCtrlM'
    KeyEnter
  | -- | Also 'KeyCtrlLsqBracket', 'KeyCtrl3'
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
  | -- | Also 'KeyCtrlI'
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

parseKey :: Termbox.Bindings.Tb_key -> Key
parseKey = \case
  Termbox.Bindings.TB_KEY_ARROW_DOWN -> KeyArrowDown
  Termbox.Bindings.TB_KEY_ARROW_LEFT -> KeyArrowLeft
  Termbox.Bindings.TB_KEY_ARROW_RIGHT -> KeyArrowRight
  Termbox.Bindings.TB_KEY_ARROW_UP -> KeyArrowUp
  Termbox.Bindings.TB_KEY_BACKSPACE -> KeyBackspace
  Termbox.Bindings.TB_KEY_CTRL_TILDE -> KeyCtrlTilde
  Termbox.Bindings.TB_KEY_CTRL_6 -> KeyCtrl6
  Termbox.Bindings.TB_KEY_CTRL_8 -> KeyCtrl8
  Termbox.Bindings.TB_KEY_CTRL_A -> KeyCtrlA
  Termbox.Bindings.TB_KEY_CTRL_B -> KeyCtrlB
  Termbox.Bindings.TB_KEY_CTRL_BACKSLASH -> KeyCtrlBackslash
  Termbox.Bindings.TB_KEY_CTRL_C -> KeyCtrlC
  Termbox.Bindings.TB_KEY_CTRL_D -> KeyCtrlD
  Termbox.Bindings.TB_KEY_CTRL_E -> KeyCtrlE
  Termbox.Bindings.TB_KEY_CTRL_F -> KeyCtrlF
  Termbox.Bindings.TB_KEY_CTRL_G -> KeyCtrlG
  Termbox.Bindings.TB_KEY_CTRL_H -> KeyCtrlBackspace
  Termbox.Bindings.TB_KEY_CTRL_J -> KeyCtrlJ
  Termbox.Bindings.TB_KEY_CTRL_K -> KeyCtrlK
  Termbox.Bindings.TB_KEY_CTRL_L -> KeyCtrlL
  Termbox.Bindings.TB_KEY_CTRL_N -> KeyCtrlN
  Termbox.Bindings.TB_KEY_CTRL_O -> KeyCtrlO
  Termbox.Bindings.TB_KEY_CTRL_P -> KeyCtrlP
  Termbox.Bindings.TB_KEY_CTRL_Q -> KeyCtrlQ
  Termbox.Bindings.TB_KEY_CTRL_R -> KeyCtrlR
  Termbox.Bindings.TB_KEY_CTRL_RSQ_BRACKET -> KeyCtrlRsqBracket
  Termbox.Bindings.TB_KEY_CTRL_S -> KeyCtrlS
  Termbox.Bindings.TB_KEY_CTRL_SLASH -> KeyCtrlSlash
  Termbox.Bindings.TB_KEY_CTRL_T -> KeyCtrlT
  Termbox.Bindings.TB_KEY_CTRL_U -> KeyCtrlU
  Termbox.Bindings.TB_KEY_CTRL_V -> KeyCtrlV
  Termbox.Bindings.TB_KEY_CTRL_W -> KeyCtrlW
  Termbox.Bindings.TB_KEY_CTRL_X -> KeyCtrlX
  Termbox.Bindings.TB_KEY_CTRL_Y -> KeyCtrlY
  Termbox.Bindings.TB_KEY_CTRL_Z -> KeyCtrlZ
  Termbox.Bindings.TB_KEY_DELETE -> KeyDelete
  Termbox.Bindings.TB_KEY_END -> KeyEnd
  Termbox.Bindings.TB_KEY_ENTER -> KeyEnter
  Termbox.Bindings.TB_KEY_ESC -> KeyEsc
  Termbox.Bindings.TB_KEY_F1 -> KeyF1
  Termbox.Bindings.TB_KEY_F10 -> KeyF10
  Termbox.Bindings.TB_KEY_F11 -> KeyF11
  Termbox.Bindings.TB_KEY_F12 -> KeyF12
  Termbox.Bindings.TB_KEY_F2 -> KeyF2
  Termbox.Bindings.TB_KEY_F3 -> KeyF3
  Termbox.Bindings.TB_KEY_F4 -> KeyF4
  Termbox.Bindings.TB_KEY_F5 -> KeyF5
  Termbox.Bindings.TB_KEY_F6 -> KeyF6
  Termbox.Bindings.TB_KEY_F7 -> KeyF7
  Termbox.Bindings.TB_KEY_F8 -> KeyF8
  Termbox.Bindings.TB_KEY_F9 -> KeyF9
  Termbox.Bindings.TB_KEY_HOME -> KeyHome
  Termbox.Bindings.TB_KEY_INSERT -> KeyInsert
  Termbox.Bindings.TB_KEY_PGDN -> KeyPageDn
  Termbox.Bindings.TB_KEY_PGUP -> KeyPageUp
  Termbox.Bindings.TB_KEY_SPACE -> KeySpace
  Termbox.Bindings.TB_KEY_TAB -> KeyTab
  key -> error ("unknown key: " ++ show key)

module Termbox.Mouse
  ( Mouse (..),
    parseMouse,
  )
where

import Data.Word (Word16)
import qualified Termbox.Bindings

-- | A mouse event.
data Mouse
  = MouseLeft
  | MouseMiddle
  | MouseRelease
  | MouseRight
  | MouseWheelDown
  | MouseWheelUp
  deriving stock (Eq, Ord, Show)

parseMouse :: Word16 -> Mouse
parseMouse key
  | key == Termbox.Bindings._TB_KEY_MOUSE_LEFT = MouseLeft
  | key == Termbox.Bindings._TB_KEY_MOUSE_MIDDLE = MouseMiddle
  | key == Termbox.Bindings._TB_KEY_MOUSE_RELEASE = MouseRelease
  | key == Termbox.Bindings._TB_KEY_MOUSE_RIGHT = MouseRight
  | key == Termbox.Bindings._TB_KEY_MOUSE_WHEEL_DOWN = MouseWheelDown
  | key == Termbox.Bindings._TB_KEY_MOUSE_WHEEL_UP = MouseWheelUp
  | otherwise = error ("termbox: unknown mouse " ++ show key)

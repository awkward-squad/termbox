module Termbox.Mouse
  ( Mouse (..),
    parseMouse,
  )
where

import Data.Word (Word16)
import qualified Termbox.Bindings.C as C

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
  | key == C._TB_KEY_MOUSE_LEFT = MouseLeft
  | key == C._TB_KEY_MOUSE_MIDDLE = MouseMiddle
  | key == C._TB_KEY_MOUSE_RELEASE = MouseRelease
  | key == C._TB_KEY_MOUSE_RIGHT = MouseRight
  | key == C._TB_KEY_MOUSE_WHEEL_DOWN = MouseWheelDown
  | key == C._TB_KEY_MOUSE_WHEEL_UP = MouseWheelUp
  | otherwise = error ("termbox: unknown mouse " ++ show key)

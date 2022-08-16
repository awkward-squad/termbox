module Termbox.Mouse
  ( Mouse (..),
    parseMouse,
  )
where

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

parseMouse :: Termbox.Bindings.Tb_key -> Mouse
parseMouse = \case
  Termbox.Bindings.TB_KEY_MOUSE_LEFT -> MouseLeft
  Termbox.Bindings.TB_KEY_MOUSE_MIDDLE -> MouseMiddle
  Termbox.Bindings.TB_KEY_MOUSE_RELEASE -> MouseRelease
  Termbox.Bindings.TB_KEY_MOUSE_RIGHT -> MouseRight
  Termbox.Bindings.TB_KEY_MOUSE_WHEEL_DOWN -> MouseWheelDown
  Termbox.Bindings.TB_KEY_MOUSE_WHEEL_UP -> MouseWheelUp
  key -> error ("unknown mouse: " ++ show key)

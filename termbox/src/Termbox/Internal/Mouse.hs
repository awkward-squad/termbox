module Termbox.Internal.Mouse
  ( Mouse
      ( Mouse,
        MouseLeft,
        MouseMiddle,
        MouseRelease,
        MouseRight,
        MouseWheelUp,
        MouseWheelDown
      ),
  )
where

import qualified Termbox.Bindings.Hs

-- | A mouse event.
newtype Mouse
  = Mouse Termbox.Bindings.Hs.Tb_key
  deriving stock (Eq, Ord)

instance Show Mouse where
  show = \case
    MouseLeft -> "MouseLeft"
    MouseMiddle -> "MouseMiddle"
    MouseRelease -> "MouseRelease"
    MouseRight -> "MouseRight"
    MouseWheelDown -> "MouseWheelDown"
    MouseWheelUp -> "MouseWheelUp"

pattern MouseLeft :: Mouse
pattern MouseLeft = Mouse Termbox.Bindings.Hs.TB_KEY_MOUSE_LEFT

pattern MouseMiddle :: Mouse
pattern MouseMiddle = Mouse Termbox.Bindings.Hs.TB_KEY_MOUSE_MIDDLE

pattern MouseRelease :: Mouse
pattern MouseRelease = Mouse Termbox.Bindings.Hs.TB_KEY_MOUSE_RELEASE

pattern MouseRight :: Mouse
pattern MouseRight = Mouse Termbox.Bindings.Hs.TB_KEY_MOUSE_RIGHT

pattern MouseWheelDown :: Mouse
pattern MouseWheelDown = Mouse Termbox.Bindings.Hs.TB_KEY_MOUSE_WHEEL_DOWN

pattern MouseWheelUp :: Mouse
pattern MouseWheelUp = Mouse Termbox.Bindings.Hs.TB_KEY_MOUSE_WHEEL_UP

{-# COMPLETE MouseLeft, MouseMiddle, MouseRelease, MouseRight, MouseWheelDown, MouseWheelUp #-}

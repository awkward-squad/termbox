module Termbox.Internal.Color
  ( Color (..),
    defaultColor,
    red,
    green,
    yellow,
    blue,
    magenta,
    cyan,
    white,
    bright,
    color,
    gray,
  )
where

import Data.Coerce (coerce)
import Data.Word (Word16)
import qualified Termbox.Bindings.Hs

-- | A color.
--
-- There are three classes of colors:
--
-- * Basic named colors and their bright variants, such as 'red' and 'bright' 'blue'.
-- * Miscellaneous colors, such as @'color' 33@.
-- * Monochrome colors that range from black (@'gray' 0@) to white (@'gray' 23@).
newtype Color
  = Color Termbox.Bindings.Hs.Tb_color

defaultColor :: Color
defaultColor =
  Color 0

red :: Color
red =
  Color 1

green :: Color
green =
  Color 2

yellow :: Color
yellow =
  Color 3

blue :: Color
blue =
  Color 4

magenta :: Color
magenta =
  Color 5

cyan :: Color
cyan =
  Color 6

white :: Color
white =
  Color 7

-- | Make a basic color brighter.
bright :: Color -> Color
bright =
  coerce bright_

bright_ :: Termbox.Bindings.Hs.Tb_color -> Termbox.Bindings.Hs.Tb_color
bright_ c
  | c <= 7 = c + 8
  | otherwise = c

-- | A miscellaneous color.
--
-- Valid values are in the range @[0, 215]@; values outside of this range are clamped.
color :: Int -> Color
color =
  coerce (fromIntegral @Int @Word16 . (+ 16) . max 0 . min 215)

-- | A monochrome color; black is 0 and white is 23.
--
-- Valid values are in the range @[0, 23]@; values outside of this range are clamped.
gray :: Int -> Color
gray =
  coerce (fromIntegral @Int @Word16 . (+ 232) . max 0 . min 23)

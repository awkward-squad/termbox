module Termbox.Internal.Color
  ( -- * Color
    Color (..),
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

    -- * MaybeColor
    MaybeColor,
    unMaybeColor,
    nothingColor,
    justColor,
  )
where

import Data.Coerce (coerce)
import Data.Word (Word16)
import Termbox.Bindings.Hs

-- | A color.
--
-- There are three classes of colors:
--
-- * Basic named colors and their bright variants, such as 'red' and 'bright' 'blue'.
-- * Miscellaneous colors, such as @'color' 33@.
-- * Monochrome colors that range from black (@'gray' 0@) to white (@'gray' 23@).
newtype Color
  = Color Tb_color
  deriving newtype (Eq)

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

-- | Make a basic color bright.
bright :: Color -> Color
bright =
  coerce bright_

bright_ :: Tb_color -> Tb_color
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

-- This is a more efficient `Maybe Color`; we represent Nothing by WORD_MAX (which isn't a valid termbox color)
newtype MaybeColor
  = MaybeColor Color
  deriving stock (Eq)

unMaybeColor :: MaybeColor -> Tb_color
unMaybeColor (MaybeColor (Color (Tb_color c)))
  | c == maxBound = TB_DEFAULT
  | otherwise = Tb_color c

nothingColor :: MaybeColor
nothingColor =
  MaybeColor (Color (Tb_color maxBound))

justColor :: Color -> MaybeColor
justColor =
  MaybeColor

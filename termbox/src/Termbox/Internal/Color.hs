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
  = Color Tb_attrs
  deriving newtype (Eq)

defaultColor :: Color
defaultColor =
  Color _TB_DEFAULT

red :: Color
red =
  Color _TB_RED

green :: Color
green =
  Color _TB_GREEN

yellow :: Color
yellow =
  Color _TB_YELLOW

blue :: Color
blue =
  Color _TB_BLUE

magenta :: Color
magenta =
  Color _TB_MAGENTA

cyan :: Color
cyan =
  Color _TB_CYAN

white :: Color
white =
  Color _TB_WHITE

-- | Make a basic color bright.
bright :: Color -> Color
bright =
  coerce bright_

bright_ :: Word16 -> Word16
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

unMaybeColor :: MaybeColor -> Tb_attrs
unMaybeColor c
  | c == nothingColor = _TB_DEFAULT
  | otherwise = coerce @_ @Tb_attrs c

nothingColor :: MaybeColor
nothingColor =
  coerce @Word16 maxBound

justColor :: Color -> MaybeColor
justColor =
  MaybeColor

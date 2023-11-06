module Termbox.Internal.Style
  ( Style,
    asForeground,
    asBackground,
    maybeFill,
    fg,
    bg,
    bold,
    underline,
    blink,
  )
where

import Termbox.Bindings.Hs hiding (bg, fg)
import Termbox.Internal.Color (Color, MaybeColor, justColor, nothingColor, unMaybeColor)

data Style = Style
  { foreground :: {-# UNPACK #-} !ColorAndAttr,
    background :: {-# UNPACK #-} !ColorAndAttr
  }

instance Monoid Style where
  mempty =
    Style mempty mempty

-- right-biased
instance Semigroup Style where
  Style a1 b1 <> Style a2 b2 =
    Style (a2 <> a1) (b2 <> b1)

-- Render a style as a foreground `tb_color`.
asForeground :: Style -> Tb_color
asForeground Style {foreground} =
  renderColorAndAttr foreground

-- Render a style as a background `tb_color`.
asBackground :: Style -> Tb_color
asBackground Style {background} =
  renderColorAndAttr background

onlyForeground :: ColorAndAttr -> Style
onlyForeground style =
  mempty {foreground = style}

onlyBackground :: ColorAndAttr -> Style
onlyBackground style =
  mempty {background = style}

maybeFill :: MaybeColor -> Style
maybeFill color =
  onlyBackground ColorAndAttr {color, attr = mempty}

fg :: Color -> Style
fg =
  onlyForeground . onlyColor

bg :: Color -> Style
bg =
  onlyBackground . onlyColor

bold :: Style
bold =
  onlyForeground (onlyAttr TB_BOLD)

underline :: Style
underline =
  onlyForeground (onlyAttr TB_UNDERLINE)

blink :: Style
blink =
  onlyBackground (onlyAttr TB_BOLD)

data ColorAndAttr = ColorAndAttr
  { color :: {-# UNPACK #-} !MaybeColor,
    attr :: {-# UNPACK #-} !Tb_attr
  }

instance Monoid ColorAndAttr where
  mempty =
    ColorAndAttr nothingColor mempty

-- right-biased
instance Semigroup ColorAndAttr where
  ColorAndAttr color1 attr1 <> ColorAndAttr color2 attr2 =
    ColorAndAttr
      (if color2 == nothingColor then color1 else color2)
      (attr1 <> attr2)

renderColorAndAttr :: ColorAndAttr -> Tb_color
renderColorAndAttr ColorAndAttr {color, attr} =
  tb_attr attr (unMaybeColor color)

onlyColor :: Color -> ColorAndAttr
onlyColor color =
  mempty {color = justColor color}

onlyAttr :: Tb_attr -> ColorAndAttr
onlyAttr attr =
  mempty {attr}

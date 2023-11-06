module Termbox2.Bindings.Hs
  ( -- * Types
    Tb_attr
      ( Tb_attr,
        TB_BLINK,
        TB_BOLD,
        TB_BRIGHT,
        TB_DIM,
        TB_INVISIBLE,
        TB_ITALIC,
        TB_OVERLINE,
        TB_REVERSE,
        TB_STRIKEOUT,
        TB_UNDERLINE,
        TB_UNDERLINE_2
      ),
  )
where

import Termbox2.Bindings.Hs.Internal.Attr (Tb_attr (..))

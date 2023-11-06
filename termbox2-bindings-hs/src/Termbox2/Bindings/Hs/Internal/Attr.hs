module Termbox2.Bindings.Hs.Internal.Attr
  ( Tb_attr
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

import Data.Bits ((.|.))
import Data.Word (Word64)
import Termbox2.Bindings.C
  ( _TB_BLINK,
    _TB_BOLD,
    _TB_BRIGHT,
    _TB_DIM,
    _TB_INVISIBLE,
    _TB_ITALIC,
    _TB_OVERLINE,
    _TB_REVERSE,
    _TB_STRIKEOUT,
    _TB_UNDERLINE,
    _TB_UNDERLINE_2,
  )

-- | An attribute.
newtype Tb_attr
  = Tb_attr Word64
  deriving stock (Eq, Ord, Show)

instance Monoid Tb_attr where
  mempty =
    Tb_attr 0

instance Semigroup Tb_attr where
  Tb_attr cx <> Tb_attr cy =
    Tb_attr (cx .|. cy)

pattern TB_BLINK :: Tb_attr
pattern TB_BLINK <-
  ((== Tb_attr _TB_BLINK) -> True)
  where
    TB_BLINK = Tb_attr _TB_BLINK

pattern TB_BOLD :: Tb_attr
pattern TB_BOLD <-
  ((== Tb_attr _TB_BOLD) -> True)
  where
    TB_BOLD = Tb_attr _TB_BOLD

pattern TB_BRIGHT :: Tb_attr
pattern TB_BRIGHT <-
  ((== Tb_attr _TB_BRIGHT) -> True)
  where
    TB_BRIGHT = Tb_attr _TB_BRIGHT

pattern TB_DIM :: Tb_attr
pattern TB_DIM <-
  ((== Tb_attr _TB_DIM) -> True)
  where
    TB_DIM = Tb_attr _TB_DIM

pattern TB_INVISIBLE :: Tb_attr
pattern TB_INVISIBLE <-
  ((== Tb_attr _TB_INVISIBLE) -> True)
  where
    TB_INVISIBLE = Tb_attr _TB_INVISIBLE

pattern TB_ITALIC :: Tb_attr
pattern TB_ITALIC <-
  ((== Tb_attr _TB_ITALIC) -> True)
  where
    TB_ITALIC = Tb_attr _TB_ITALIC

pattern TB_OVERLINE :: Tb_attr
pattern TB_OVERLINE <-
  ((== Tb_attr _TB_OVERLINE) -> True)
  where
    TB_OVERLINE = Tb_attr _TB_OVERLINE

pattern TB_REVERSE :: Tb_attr
pattern TB_REVERSE <-
  ((== Tb_attr _TB_REVERSE) -> True)
  where
    TB_REVERSE = Tb_attr _TB_REVERSE

pattern TB_STRIKEOUT :: Tb_attr
pattern TB_STRIKEOUT <-
  ((== Tb_attr _TB_STRIKEOUT) -> True)
  where
    TB_STRIKEOUT = Tb_attr _TB_STRIKEOUT

pattern TB_UNDERLINE :: Tb_attr
pattern TB_UNDERLINE <-
  ((== Tb_attr _TB_UNDERLINE) -> True)
  where
    TB_UNDERLINE = Tb_attr _TB_UNDERLINE

pattern TB_UNDERLINE_2 :: Tb_attr
pattern TB_UNDERLINE_2 <-
  ((== Tb_attr _TB_UNDERLINE_2) -> True)
  where
    TB_UNDERLINE_2 = Tb_attr _TB_UNDERLINE_2

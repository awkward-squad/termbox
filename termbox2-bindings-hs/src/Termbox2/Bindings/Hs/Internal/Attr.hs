module Termbox2.Bindings.Hs.Internal.Attr
  ( Tb_attr
      ( Tb_attr,
        TB_DEFAULT,
        TB_BLACK,
        TB_BLUE,
        TB_CYAN,
        TB_GREEN,
        TB_HI_BLACK,
        TB_MAGENTA,
        TB_RED,
        TB_WHITE,
        TB_YELLOW,
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
  ( _TB_BLACK,
    _TB_BLINK,
    _TB_BLUE,
    _TB_BOLD,
    _TB_BRIGHT,
    _TB_CYAN,
    _TB_DEFAULT,
    _TB_DIM,
    _TB_GREEN,
    _TB_HI_BLACK,
    _TB_INVISIBLE,
    _TB_ITALIC,
    _TB_MAGENTA,
    _TB_OVERLINE,
    _TB_RED,
    _TB_REVERSE,
    _TB_STRIKEOUT,
    _TB_UNDERLINE,
    _TB_UNDERLINE_2,
    _TB_WHITE,
    _TB_YELLOW,
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

pattern TB_DEFAULT :: Tb_attr
pattern TB_DEFAULT <-
  ((== Tb_attr _TB_DEFAULT) -> True)
  where
    TB_DEFAULT = Tb_attr _TB_DEFAULT

pattern TB_BLACK :: Tb_attr
pattern TB_BLACK <-
  ((== Tb_attr _TB_BLACK) -> True)
  where
    TB_BLACK = Tb_attr _TB_BLACK

pattern TB_BLUE :: Tb_attr
pattern TB_BLUE <-
  ((== Tb_attr _TB_BLUE) -> True)
  where
    TB_BLUE = Tb_attr _TB_BLUE

pattern TB_CYAN :: Tb_attr
pattern TB_CYAN <-
  ((== Tb_attr _TB_CYAN) -> True)
  where
    TB_CYAN = Tb_attr _TB_CYAN

pattern TB_GREEN :: Tb_attr
pattern TB_GREEN <-
  ((== Tb_attr _TB_GREEN) -> True)
  where
    TB_GREEN = Tb_attr _TB_GREEN

pattern TB_HI_BLACK :: Tb_attr
pattern TB_HI_BLACK <-
  ((== Tb_attr _TB_HI_BLACK) -> True)
  where
    TB_HI_BLACK = Tb_attr _TB_HI_BLACK

pattern TB_MAGENTA :: Tb_attr
pattern TB_MAGENTA <-
  ((== Tb_attr _TB_MAGENTA) -> True)
  where
    TB_MAGENTA = Tb_attr _TB_MAGENTA

pattern TB_RED :: Tb_attr
pattern TB_RED <-
  ((== Tb_attr _TB_RED) -> True)
  where
    TB_RED = Tb_attr _TB_RED

pattern TB_WHITE :: Tb_attr
pattern TB_WHITE <-
  ((== Tb_attr _TB_WHITE) -> True)
  where
    TB_WHITE = Tb_attr _TB_WHITE

pattern TB_YELLOW :: Tb_attr
pattern TB_YELLOW <-
  ((== Tb_attr _TB_YELLOW) -> True)
  where
    TB_YELLOW = Tb_attr _TB_YELLOW

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

module Termbox2.Bindings.Hs.Internal.Color
  ( Tb_color
      ( Tb_color,
        TB_DEFAULT,
        TB_BLACK,
        TB_BLUE,
        TB_CYAN,
        TB_GREEN,
        TB_HI_BLACK,
        TB_MAGENTA,
        TB_RED,
        TB_WHITE,
        TB_YELLOW
      ),
  )
where

import Data.Word (Word64)
import Termbox2.Bindings.C

-- | A color.
newtype Tb_color
  = Tb_color Word64
  deriving stock (Eq, Ord)
  deriving newtype (Num, Show)

pattern TB_DEFAULT :: Tb_color
pattern TB_DEFAULT <-
  ((== Tb_color _TB_DEFAULT) -> True)
  where
    TB_DEFAULT = Tb_color _TB_DEFAULT

pattern TB_BLACK :: Tb_color
pattern TB_BLACK <-
  ((== Tb_color _TB_BLACK) -> True)
  where
    TB_BLACK = Tb_color _TB_BLACK

pattern TB_BLUE :: Tb_color
pattern TB_BLUE <-
  ((== Tb_color _TB_BLUE) -> True)
  where
    TB_BLUE = Tb_color _TB_BLUE

pattern TB_CYAN :: Tb_color
pattern TB_CYAN <-
  ((== Tb_color _TB_CYAN) -> True)
  where
    TB_CYAN = Tb_color _TB_CYAN

pattern TB_GREEN :: Tb_color
pattern TB_GREEN <-
  ((== Tb_color _TB_GREEN) -> True)
  where
    TB_GREEN = Tb_color _TB_GREEN

pattern TB_HI_BLACK :: Tb_color
pattern TB_HI_BLACK <-
  ((== Tb_color _TB_HI_BLACK) -> True)
  where
    TB_HI_BLACK = Tb_color _TB_HI_BLACK

pattern TB_MAGENTA :: Tb_color
pattern TB_MAGENTA <-
  ((== Tb_color _TB_MAGENTA) -> True)
  where
    TB_MAGENTA = Tb_color _TB_MAGENTA

pattern TB_RED :: Tb_color
pattern TB_RED <-
  ((== Tb_color _TB_RED) -> True)
  where
    TB_RED = Tb_color _TB_RED

pattern TB_WHITE :: Tb_color
pattern TB_WHITE <-
  ((== Tb_color _TB_WHITE) -> True)
  where
    TB_WHITE = Tb_color _TB_WHITE

pattern TB_YELLOW :: Tb_color
pattern TB_YELLOW <-
  ((== Tb_color _TB_YELLOW) -> True)
  where
    TB_YELLOW = Tb_color _TB_YELLOW

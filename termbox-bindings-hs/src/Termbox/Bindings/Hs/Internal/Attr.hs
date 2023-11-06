module Termbox.Bindings.Hs.Internal.Attr
  ( Tb_attr
      ( Tb_attr,
        TB_BOLD,
        TB_REVERSE,
        TB_UNDERLINE
      ),
  )
where

import Data.Bits ((.|.))
import Data.Word (Word16)
import qualified Termbox.Bindings.C

-- | An attribute.
newtype Tb_attr
  = Tb_attr Word16
  deriving stock (Eq, Ord, Show)

instance Monoid Tb_attr where
  mempty =
    Tb_attr 0

instance Semigroup Tb_attr where
  Tb_attr cx <> Tb_attr cy =
    Tb_attr (cx .|. cy)

pattern TB_BOLD :: Tb_attr
pattern TB_BOLD <-
  ((== Tb_attr Termbox.Bindings.C._TB_BOLD) -> True)
  where
    TB_BOLD = Tb_attr Termbox.Bindings.C._TB_BOLD

pattern TB_REVERSE :: Tb_attr
pattern TB_REVERSE <-
  ((== Tb_attr Termbox.Bindings.C._TB_REVERSE) -> True)
  where
    TB_REVERSE = Tb_attr Termbox.Bindings.C._TB_REVERSE

pattern TB_UNDERLINE :: Tb_attr
pattern TB_UNDERLINE <-
  ((== Tb_attr Termbox.Bindings.C._TB_UNDERLINE) -> True)
  where
    TB_UNDERLINE = Tb_attr Termbox.Bindings.C._TB_UNDERLINE

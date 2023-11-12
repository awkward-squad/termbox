module Termbox2.Bindings.Hs.Internal.InputMode
  ( Tb_input_mode
      ( Tb_input_mode,
        TB_INPUT_ALT,
        TB_INPUT_ESC,
        TB_INPUT_MOUSE
      ),
  )
where

import Data.Bits ((.|.))
import Data.Coerce (coerce)
import Foreign.C.Types (CInt)
import Termbox2.Bindings.C

-- | The input mode.
newtype Tb_input_mode
  = Tb_input_mode CInt
  deriving stock (Eq, Show)

instance Semigroup Tb_input_mode where
  (<>) = coerce ((.|.) :: CInt -> CInt -> CInt)

pattern TB_INPUT_ALT :: Tb_input_mode
pattern TB_INPUT_ALT <-
  ((== Tb_input_mode _TB_INPUT_ALT) -> True)
  where
    TB_INPUT_ALT = Tb_input_mode _TB_INPUT_ALT

pattern TB_INPUT_ESC :: Tb_input_mode
pattern TB_INPUT_ESC <-
  ((== Tb_input_mode _TB_INPUT_ESC) -> True)
  where
    TB_INPUT_ESC = Tb_input_mode _TB_INPUT_ESC

pattern TB_INPUT_MOUSE :: Tb_input_mode
pattern TB_INPUT_MOUSE <-
  ((== Tb_input_mode _TB_INPUT_MOUSE) -> True)
  where
    TB_INPUT_MOUSE = Tb_input_mode _TB_INPUT_MOUSE

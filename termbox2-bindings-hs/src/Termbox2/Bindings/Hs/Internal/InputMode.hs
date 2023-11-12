module Termbox2.Bindings.Hs.Internal.InputMode
  ( Tb_input_mode (Tb_input_mode),
    _TB_INPUT_ALT,
    _TB_INPUT_ESC,
    _TB_INPUT_MOUSE,
  )
where

import Data.Bits (Bits, (.|.))
import Data.Coerce (coerce)
import Foreign.C.Types (CInt)
import Termbox2.Bindings.C qualified as Termbox

-- | The input mode.
newtype Tb_input_mode
  = Tb_input_mode CInt
  deriving stock (Eq, Show)
  deriving newtype (Bits)

instance Semigroup Tb_input_mode where
  (<>) = coerce ((.|.) :: CInt -> CInt -> CInt)

_TB_INPUT_ALT :: Tb_input_mode
_TB_INPUT_ALT =
  Tb_input_mode Termbox._TB_INPUT_ALT

_TB_INPUT_ESC :: Tb_input_mode
_TB_INPUT_ESC =
  Tb_input_mode Termbox._TB_INPUT_ESC

_TB_INPUT_MOUSE :: Tb_input_mode
_TB_INPUT_MOUSE =
  Tb_input_mode Termbox._TB_INPUT_MOUSE

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
import Foreign.C.Types (CInt)
import Termbox2.Bindings.C

-- | The input mode.
newtype Tb_input_mode
  = Tb_input_mode CInt
  deriving stock (Eq)

instance Show Tb_input_mode where
  show mode
    | mode == TB_INPUT_ALT = "TB_INPUT_ALT"
    | mode == TB_INPUT_ESC = "TB_INPUT_ESC"
    | mode == TB_INPUT_MOUSE = "TB_INPUT_MOUSE"
    | mode == TB_INPUT_ALT <> TB_INPUT_MOUSE = "TB_INPUT_ALT <> TB_INPUT_MOUSE"
    | mode == TB_INPUT_ESC <> TB_INPUT_MOUSE = "TB_INPUT_ESC <> TB_INPUT_MOUSE"
    | otherwise = "Tb_input_mode " ++ show mode

instance Semigroup Tb_input_mode where
  Tb_input_mode x <> Tb_input_mode y =
    Tb_input_mode (x .|. y)

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

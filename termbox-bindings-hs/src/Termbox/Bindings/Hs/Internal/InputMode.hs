module Termbox.Bindings.Hs.Internal.InputMode
  ( Tb_input_mode
      ( Tb_input_mode,
        TB_INPUT_CURRENT,
        TB_INPUT_ALT,
        TB_INPUT_ESC,
        TB_INPUT_MOUSE
      ),
  )
where

import Foreign.C.Types (CInt)
import qualified Termbox.Bindings.C

-- | The input mode.
newtype Tb_input_mode
  = Tb_input_mode CInt
  deriving stock (Eq, Ord)

instance Show Tb_input_mode where
  show = \case
    TB_INPUT_CURRENT -> "TB_INPUT_CURRENT"
    TB_INPUT_ALT -> "TB_INPUT_ALT"
    TB_INPUT_ESC -> "TB_INPUT_ESC"
    TB_INPUT_MOUSE -> "TB_INPUT_MOUSE"

pattern TB_INPUT_CURRENT :: Tb_input_mode
pattern TB_INPUT_CURRENT <-
  ((== Tb_input_mode Termbox.Bindings.C._TB_INPUT_CURRENT) -> True)
  where
    TB_INPUT_CURRENT = Tb_input_mode Termbox.Bindings.C._TB_INPUT_CURRENT

pattern TB_INPUT_ALT :: Tb_input_mode
pattern TB_INPUT_ALT <-
  ((== Tb_input_mode Termbox.Bindings.C._TB_INPUT_ALT) -> True)
  where
    TB_INPUT_ALT = Tb_input_mode Termbox.Bindings.C._TB_INPUT_ALT

pattern TB_INPUT_ESC :: Tb_input_mode
pattern TB_INPUT_ESC <-
  ((== Tb_input_mode Termbox.Bindings.C._TB_INPUT_ESC) -> True)
  where
    TB_INPUT_ESC = Tb_input_mode Termbox.Bindings.C._TB_INPUT_ESC

pattern TB_INPUT_MOUSE :: Tb_input_mode
pattern TB_INPUT_MOUSE <-
  ((== Tb_input_mode Termbox.Bindings.C._TB_INPUT_MOUSE) -> True)
  where
    TB_INPUT_MOUSE = Tb_input_mode Termbox.Bindings.C._TB_INPUT_MOUSE

{-# COMPLETE TB_INPUT_CURRENT, TB_INPUT_ALT, TB_INPUT_ESC, TB_INPUT_MOUSE #-}

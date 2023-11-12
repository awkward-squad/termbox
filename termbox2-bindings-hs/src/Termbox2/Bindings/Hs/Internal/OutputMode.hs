module Termbox2.Bindings.Hs.Internal.OutputMode
  ( Tb_output_mode
      ( Tb_output_mode,
        TB_OUTPUT_216,
        TB_OUTPUT_256,
        TB_OUTPUT_GRAYSCALE,
        TB_OUTPUT_NORMAL,
        TB_OUTPUT_TRUECOLOR
      ),
  )
where

import Foreign.C.Types (CInt)
import Termbox2.Bindings.C

-- | The output mode.
newtype Tb_output_mode
  = Tb_output_mode CInt
  deriving stock (Eq)

instance Show Tb_output_mode where
  show = \case
    TB_OUTPUT_216 -> "TB_OUTPUT_216"
    TB_OUTPUT_256 -> "TB_OUTPUT_256"
    TB_OUTPUT_GRAYSCALE -> "TB_OUTPUT_GRAYSCALE"
    TB_OUTPUT_NORMAL -> "TB_OUTPUT_NORMAL"
    TB_OUTPUT_TRUECOLOR -> "TB_OUTPUT_TRUECOLOR"
    Tb_output_mode mode -> show ("Tb_output_mode " ++ show mode)

pattern TB_OUTPUT_216 :: Tb_output_mode
pattern TB_OUTPUT_216 <-
  ((== Tb_output_mode _TB_OUTPUT_216) -> True)
  where
    TB_OUTPUT_216 = Tb_output_mode _TB_OUTPUT_216

pattern TB_OUTPUT_256 :: Tb_output_mode
pattern TB_OUTPUT_256 <-
  ((== Tb_output_mode _TB_OUTPUT_256) -> True)
  where
    TB_OUTPUT_256 = Tb_output_mode _TB_OUTPUT_256

pattern TB_OUTPUT_GRAYSCALE :: Tb_output_mode
pattern TB_OUTPUT_GRAYSCALE <-
  ((== Tb_output_mode _TB_OUTPUT_GRAYSCALE) -> True)
  where
    TB_OUTPUT_GRAYSCALE = Tb_output_mode _TB_OUTPUT_GRAYSCALE

pattern TB_OUTPUT_NORMAL :: Tb_output_mode
pattern TB_OUTPUT_NORMAL <-
  ((== Tb_output_mode _TB_OUTPUT_NORMAL) -> True)
  where
    TB_OUTPUT_NORMAL = Tb_output_mode _TB_OUTPUT_NORMAL

pattern TB_OUTPUT_TRUECOLOR :: Tb_output_mode
pattern TB_OUTPUT_TRUECOLOR <-
  ((== Tb_output_mode _TB_OUTPUT_TRUECOLOR) -> True)
  where
    TB_OUTPUT_TRUECOLOR = Tb_output_mode _TB_OUTPUT_TRUECOLOR

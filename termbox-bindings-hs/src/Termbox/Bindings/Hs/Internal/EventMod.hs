module Termbox.Bindings.Hs.Internal.EventMod
  ( Tb_event_mod
      ( Tb_event_mod,
        TB_MOD_ALT,
        TB_MOD_MOTION
      ),
  )
where

import Data.Word (Word8)
import qualified Termbox.Bindings.C

-- | An event modifier.
newtype Tb_event_mod
  = Tb_event_mod Word8
  deriving stock (Eq, Ord, Show)

pattern TB_MOD_ALT :: Tb_event_mod
pattern TB_MOD_ALT <-
  ((== Tb_event_mod Termbox.Bindings.C._TB_MOD_ALT) -> True)
  where
    TB_MOD_ALT = Tb_event_mod Termbox.Bindings.C._TB_MOD_ALT

pattern TB_MOD_MOTION :: Tb_event_mod
pattern TB_MOD_MOTION <-
  ((== Tb_event_mod Termbox.Bindings.C._TB_MOD_MOTION) -> True)
  where
    TB_MOD_MOTION = Tb_event_mod Termbox.Bindings.C._TB_MOD_MOTION

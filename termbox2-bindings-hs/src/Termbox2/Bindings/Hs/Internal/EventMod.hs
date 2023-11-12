module Termbox2.Bindings.Hs.Internal.EventMod
  ( Tb_event_mod (Tb_event_mod),
    _TB_MOD_ALT,
    _TB_MOD_CTRL,
    _TB_MOD_SHIFT,
    _TB_MOD_MOTION,
  )
where

import Data.Bits (Bits, (.|.))
import Data.Coerce (coerce)
import Data.Word (Word8)
import Termbox2.Bindings.C qualified as Termbox

-- | An event modifier.
newtype Tb_event_mod
  = Tb_event_mod Word8
  deriving stock (Eq, Ord, Show)
  deriving newtype (Bits)

instance Semigroup Tb_event_mod where
  (<>) = coerce ((.|.) :: Word8 -> Word8 -> Word8)

_TB_MOD_ALT :: Tb_event_mod
_TB_MOD_ALT =
  Tb_event_mod Termbox._TB_MOD_ALT

_TB_MOD_CTRL :: Tb_event_mod
_TB_MOD_CTRL =
  Tb_event_mod Termbox._TB_MOD_CTRL

_TB_MOD_SHIFT :: Tb_event_mod
_TB_MOD_SHIFT =
  Tb_event_mod Termbox._TB_MOD_SHIFT

_TB_MOD_MOTION :: Tb_event_mod
_TB_MOD_MOTION =
  Tb_event_mod Termbox._TB_MOD_MOTION

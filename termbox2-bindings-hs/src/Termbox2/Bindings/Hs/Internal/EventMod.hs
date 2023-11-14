module Termbox2.Bindings.Hs.Internal.EventMod
  ( Tb_event_mod (Tb_event_mod),
    _TB_MOD_ALT,
    _TB_MOD_CTRL,
    _TB_MOD_SHIFT,
    _TB_MOD_MOTION,
  )
where

import Data.Bits (Bits, (.&.), (.|.))
import Data.Coerce (coerce)
import Data.List qualified as List
import Data.Maybe (mapMaybe)
import Data.Word (Word8)
import Termbox2.Bindings.C qualified as Termbox

-- | An event modifier.
newtype Tb_event_mod
  = Tb_event_mod Word8
  deriving stock (Eq, Ord)
  deriving newtype (Bits)

instance Show Tb_event_mod where
  show x
    | uncomponents bs == x =
        if null bs
          then "mempty"
          else List.intercalate " <> " (map show1 bs)
    | otherwise = "Tb_event_mod " ++ show (coerce @_ @Word8 x)
    where
      bs = components x

instance Monoid Tb_event_mod where
  mempty = Tb_event_mod 0

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

-- Random helpers that power the Show instance

-- break a mod into its one-bit components
components :: Tb_event_mod -> [Tb_event_mod]
components x =
  mapMaybe
    (\b -> if contains b x then Just b else Nothing)
    [_TB_MOD_ALT, _TB_MOD_CTRL, _TB_MOD_SHIFT, _TB_MOD_MOTION]

-- combine one-bit components into a mod
uncomponents :: [Tb_event_mod] -> Tb_event_mod
uncomponents =
  coerce @([Word8] -> Word8) (List.foldl' (.|.) 0)

-- show one bit
show1 :: Tb_event_mod -> String
show1 x
  | x == _TB_MOD_ALT = "_TB_MOD_ALT"
  | x == _TB_MOD_CTRL = "_TB_MOD_CTRL"
  | x == _TB_MOD_SHIFT = "_TB_MOD_SHIFT"
  | x == _TB_MOD_MOTION = "_TB_MOD_MOTION"
  | otherwise = "" -- impossible

-- contains x y: does y contain single-bit x?
contains :: Tb_event_mod -> Tb_event_mod -> Bool
contains (Tb_event_mod x) (Tb_event_mod y) =
  x .&. y == x

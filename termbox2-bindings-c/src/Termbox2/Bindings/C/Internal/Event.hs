module Termbox2.Bindings.C.Internal.Event
  ( Tb_event (..),
  )
where

import Data.Int (Int32)
import Data.Word (Word16, Word32, Word8)
import GHC.Generics (Generic)
import Prelude hiding (mod)

-- | An event.
data {-# CTYPE "termbox2.h" "struct tb_event" #-} Tb_event = Tb_event
  { type_ :: {-# UNPACK #-} !Word8,
    mod :: {-# UNPACK #-} !Word8,
    key :: {-# UNPACK #-} !Word16,
    ch :: {-# UNPACK #-} !Word32,
    w :: {-# UNPACK #-} !Int32,
    h :: {-# UNPACK #-} !Int32,
    x :: {-# UNPACK #-} !Int32,
    y :: {-# UNPACK #-} !Int32
  }
  deriving stock (Eq, Generic, Ord, Show)

module Termbox2.Bindings.C.Internal.Cell
  ( Tb_cell (..),
  )
where

import Data.Word (Word16, Word32)
import Foreign.C (CSize)
import Foreign.Ptr (Ptr)
import GHC.Generics (Generic)

-- | A cell.
data {-# CTYPE "termbox2.h" "struct tb_cell" #-} Tb_cell = Tb_cell
  { -- | A unicode character.
    ch :: {-# UNPACK #-} !Word32,
    -- | Foreground attribute.
    fg :: {-# UNPACK #-} !Word16,
    -- | Background attribute.
    bg :: {-# UNPACK #-} !Word16,
    -- | A grapheme cluster.
    ech :: {-# UNPACK #-} !(Ptr Word32),
    -- | Length of @ech@, in bytes; 0 means use @ch@.
    nech :: {-# UNPACK #-} !CSize,
    -- | Capacity of @ech@, in bytes.
    cech :: {-# UNPACK #-} !CSize
  }
  deriving stock (Eq, Generic, Ord, Show)

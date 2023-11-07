module Termbox2.Bindings.C.Internal.Cell
  ( Tb_cell (..),
  )
where

import Data.Word (Word32, Word64)
import Foreign.C (CSize)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable)
import Foreign.Storable qualified as Storable
import GHC.Generics (Generic)

-- | A cell.
data {-# CTYPE "termbox2.h" "struct tb_cell" #-} Tb_cell = Tb_cell
  { -- | A unicode character.
    ch :: {-# UNPACK #-} !Word32,
    -- | Foreground attribute.
    fg :: {-# UNPACK #-} !Word64,
    -- | Background attribute.
    bg :: {-# UNPACK #-} !Word64,
    -- | A grapheme cluster.
    ech :: {-# UNPACK #-} !(Ptr Word32),
    -- | Length of @ech@; 0 means use @ch@.
    nech :: {-# UNPACK #-} !CSize,
    -- | Capacity of @ech@.
    cech :: {-# UNPACK #-} !CSize
  }
  deriving stock (Eq, Generic, Ord, Show)

instance Storable Tb_cell where
  sizeOf :: Tb_cell -> Int
  sizeOf _ =
    20 + sizeofPtr + sizeofCSize + sizeofCSize

  alignment :: Tb_cell -> Int
  alignment _ =
    4

  peek :: Ptr Tb_cell -> IO Tb_cell
  peek ptr =
    Tb_cell
      <$> Storable.peekByteOff ptr 0
      <*> Storable.peekByteOff ptr 4
      <*> Storable.peekByteOff ptr 12
      <*> Storable.peekByteOff ptr 20
      <*> Storable.peekByteOff ptr (20 + sizeofPtr)
      <*> Storable.peekByteOff ptr (20 + sizeofPtr + sizeofCSize)

  poke :: Ptr Tb_cell -> Tb_cell -> IO ()
  poke ptr Tb_cell {ch, fg, bg, ech, nech, cech} = do
    Storable.pokeByteOff ptr 0 ch
    Storable.pokeByteOff ptr 4 fg
    Storable.pokeByteOff ptr 12 bg
    Storable.pokeByteOff ptr 20 ech
    Storable.pokeByteOff ptr (20 + sizeofPtr) nech
    Storable.pokeByteOff ptr (20 + sizeofPtr + sizeofCSize) cech

sizeofPtr :: Int
sizeofPtr =
  Storable.sizeOf (undefined :: Ptr ())

sizeofCSize :: Int
sizeofCSize =
  Storable.sizeOf (undefined :: CSize)

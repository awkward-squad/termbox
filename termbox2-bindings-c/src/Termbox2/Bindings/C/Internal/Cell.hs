module Termbox2.Bindings.C.Internal.Cell
  ( Tb_cell (..),
  )
where

import Data.Word (Word16, Word32)
import Foreign.C (CSize)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable (..))
import qualified Foreign.Storable as Storable
import GHC.Generics (Generic)

-- | A cell.
data Tb_cell = Tb_cell
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

instance Storable Tb_cell where
  sizeOf :: Tb_cell -> Int
  sizeOf _ =
    8 + sizeofPtr + sizeofCSize + sizeofCSize

  alignment :: Tb_cell -> Int
  alignment _ =
    4

  peek :: Ptr Tb_cell -> IO Tb_cell
  peek ptr =
    Tb_cell
      <$> Storable.peekByteOff ptr 0
      <*> Storable.peekByteOff ptr 4
      <*> Storable.peekByteOff ptr 6
      <*> Storable.peekByteOff ptr 8
      <*> Storable.peekByteOff ptr (8 + sizeofPtr)
      <*> Storable.peekByteOff ptr (8 + sizeofPtr + sizeofCSize)

  poke :: Ptr Tb_cell -> Tb_cell -> IO ()
  poke ptr Tb_cell {ch, fg, bg, ech, nech, cech} = do
    Storable.pokeByteOff ptr 0 ch
    Storable.pokeByteOff ptr 4 fg
    Storable.pokeByteOff ptr 6 bg
    Storable.pokeByteOff ptr 8 ech
    Storable.pokeByteOff ptr (8 + sizeofPtr) nech
    Storable.pokeByteOff ptr (8 + sizeofPtr + sizeofCSize) cech

sizeofPtr :: Int
sizeofPtr =
  sizeOf (undefined :: Ptr Word32)

sizeofCSize :: Int
sizeofCSize =
  sizeOf (undefined :: CSize)

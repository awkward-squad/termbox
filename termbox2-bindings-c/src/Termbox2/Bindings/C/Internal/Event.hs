module Termbox2.Bindings.C.Internal.Event
  ( Tb_event (..),
  )
where

import Data.Int (Int32)
import Data.Word (Word16, Word32, Word8)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable (..))
import qualified Foreign.Storable as Storable
import GHC.Generics (Generic)
import Prelude hiding (mod)

-- | An event.
data Tb_event = Tb_event
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

instance Storable Tb_event where
  sizeOf :: Tb_event -> Int
  sizeOf _ =
    24

  alignment :: Tb_event -> Int
  alignment _ =
    4

  peek :: Ptr Tb_event -> IO Tb_event
  peek ptr =
    Tb_event
      <$> Storable.peekByteOff ptr 0
      <*> Storable.peekByteOff ptr 1
      <*> Storable.peekByteOff ptr 2
      <*> Storable.peekByteOff ptr 4
      <*> Storable.peekByteOff ptr 8
      <*> Storable.peekByteOff ptr 12
      <*> Storable.peekByteOff ptr 16
      <*> Storable.peekByteOff ptr 20

  poke :: Ptr Tb_event -> Tb_event -> IO ()
  poke ptr Tb_event {type_, mod, key, ch, w, h, x, y} = do
    Storable.pokeByteOff ptr 0 type_
    Storable.pokeByteOff ptr 1 mod
    Storable.pokeByteOff ptr 2 key
    Storable.pokeByteOff ptr 4 ch
    Storable.pokeByteOff ptr 8 w
    Storable.pokeByteOff ptr 12 h
    Storable.pokeByteOff ptr 16 x
    Storable.pokeByteOff ptr 20 y

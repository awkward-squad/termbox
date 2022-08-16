module Termbox.Cell
  ( Cell (..),
  )
where

import Data.Char (chr, ord)
import Data.Coerce (coerce)
import Data.Word (Word16, Word32)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable)
import qualified Foreign.Storable as Storable
import Termbox.Attr (Attr (Attr))
import qualified Termbox.Bindings

-- | A cell contains a character, foreground attribute, and background attribute.
data Cell
  = Cell !Char !Attr !Attr
  deriving stock (Eq, Show)

instance Storable Cell where
  sizeOf :: Cell -> Int
  sizeOf _ =
    8

  alignment :: Cell -> Int
  alignment _ =
    4

  peek :: Ptr Cell -> IO Cell
  peek ptr = do
    Cell
      <$> (chr . fromIntegral @Word32 @Int <$> Storable.peekByteOff ptr 0)
      <*> (coerce @(IO Word16) (Storable.peekByteOff ptr 4))
      <*> (coerce @(IO Word16) (Storable.peekByteOff ptr 6))

  poke :: Ptr Cell -> Cell -> IO ()
  poke ptr (Cell ch fg bg) = do
    Storable.pokeByteOff ptr 0 (fromIntegral @Int @Word32 (ord ch))
    Storable.pokeByteOff ptr 4 (coerce @_ @Word16 fg)
    Storable.pokeByteOff ptr 6 (coerce @_ @Word16 bg)

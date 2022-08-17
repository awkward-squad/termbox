module Termbox.Size
  ( Size (..),
  )
where

data Size = Size
  { width :: {-# UNPACK #-} !Int,
    height :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Ord, Show)

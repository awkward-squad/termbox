module Termbox.Size
  ( Size (..),
  )
where

-- | A terminal size.
data Size = Size
  { width :: {-# UNPACK #-} !Int,
    height :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Ord, Show)

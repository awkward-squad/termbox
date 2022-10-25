module Termbox.Internal.Size
  ( Size (..),
  )
where

import GHC.Generics (Generic)

-- | A terminal size.
data Size = Size
  { width :: {-# UNPACK #-} !Int,
    height :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Generic, Ord, Show)

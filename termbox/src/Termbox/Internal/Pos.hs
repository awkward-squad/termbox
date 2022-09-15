module Termbox.Internal.Pos
  ( Pos (..),
  )
where

-- | A position.
data Pos = Pos
  { row :: {-# UNPACK #-} !Int,
    col :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Ord, Show)

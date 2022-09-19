module Termbox.Internal.Pos
  ( Pos (..),
    posUp,
    posDown,
    posLeft,
    posRight,
  )
where

-- | A position.
data Pos = Pos
  { row :: {-# UNPACK #-} !Int,
    col :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Ord, Show)

-- | Move a position up.
posUp :: Int -> Pos -> Pos
posUp n (Pos row col) =
  Pos (row - n) col

-- | Move a position down.
posDown :: Int -> Pos -> Pos
posDown n (Pos row col) =
  Pos (row + n) col

-- | Move a position left.
posLeft :: Int -> Pos -> Pos
posLeft n (Pos row col) =
  Pos row (col - n)

-- | Move a position right.
posRight :: Int -> Pos -> Pos
posRight n (Pos row col) =
  Pos row (col + n)

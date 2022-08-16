module Termbox.Cells
  ( Cells (..),
    set,
  )
where

import Data.Char (ord)
import Data.Semigroup (Semigroup)
import Data.Word (Word32)
import Termbox.Attr (attrToTbAttr)
import qualified Termbox.Bindings
import Termbox.Cell (Cell (Cell))

-- | A grid of cells. Create with 'set' and combine with ('<>').
newtype Cells
  = Cells (IO ())
  deriving newtype (Monoid, Semigroup)

-- | Set a single cell's value (column, then row).
set :: Int -> Int -> Cell -> Cells
set col row (Cell ch fg bg) =
  Cells
    ( Termbox.Bindings.tb_change_cell
        col
        row
        (fromIntegral @Int @Word32 (ord ch))
        (attrToTbAttr fg)
        (attrToTbAttr bg)
    )

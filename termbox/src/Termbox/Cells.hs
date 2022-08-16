module Termbox.Cells
  ( Cells (..),
    set,
  )
where

import Termbox.Attr (Attr (Attr))
import qualified Termbox.Bindings
import Termbox.Cell (Cell (Cell))

-- | A grid of cells. Create with 'set' and combine with ('<>').
newtype Cells
  = Cells (IO ())
  deriving newtype (Monoid, Semigroup)

-- | Set a single cell's value (column, then row).
set :: Int -> Int -> Cell -> Cells
set col row (Cell ch (Attr fg) (Attr bg)) =
  Cells (Termbox.Bindings.tb_change_cell col row ch fg bg)

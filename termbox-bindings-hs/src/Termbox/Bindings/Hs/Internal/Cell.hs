module Termbox.Bindings.Hs.Internal.Cell
  ( Tb_cell (..),
    cellToCCell,
  )
where

import GHC.Generics (Generic)
import qualified Termbox.Bindings.C
import Termbox.Bindings.Hs.Internal.Attr (Tb_attr (..))
import Termbox.Bindings.Hs.Internal.Prelude (charToWord32)

-- | A cell.
data Tb_cell = Tb_cell
  { -- | A unicode character.
    ch :: {-# UNPACK #-} !Char,
    -- | Foreground attribute.
    fg :: {-# UNPACK #-} !Tb_attr,
    -- | Background attribute.
    bg :: {-# UNPACK #-} !Tb_attr
  }
  deriving stock (Eq, Generic, Ord, Show)

cellToCCell :: Tb_cell -> Termbox.Bindings.C.Tb_cell
cellToCCell Tb_cell {ch, fg = Tb_attr fg, bg = Tb_attr bg} =
  Termbox.Bindings.C.Tb_cell
    { ch = charToWord32 ch,
      fg,
      bg
    }
{-# INLINE cellToCCell #-}

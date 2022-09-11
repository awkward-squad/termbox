module Termbox.Cell
  ( -- * Cell
    Cell,
    drawCell,
    char,

    -- ** Color
    fg,
    bg,

    -- ** Style
    bold,
    underline,
    blink,
  )
where

import qualified Data.Char as Char
import Data.String (IsString (..))
import Foreign.C.Types (CInt (CInt), CWchar (CWchar))
import qualified Termbox.Bindings
import Termbox.Color (Color (Color))

-- | A single cell.
--
-- * Create a cell with 'char', or with a string literal.
-- * Set a cell's color with 'fg', 'bg'.
-- * Style a cell with 'bold', 'underline', 'blink'.
data Cell
  = CellEmpty
  | CellNonEmpty NonEmptyCell

instance {-# OVERLAPS #-} IsString [Cell] where
  fromString =
    map char

data NonEmptyCell = NonEmptyCell
  { cellChar :: {-# UNPACK #-} !Char, -- invariant: width 1
    cellFg :: {-# UNPACK #-} !Termbox.Bindings.Tb_color,
    cellBg :: {-# UNPACK #-} !Termbox.Bindings.Tb_color
  }

drawCell :: Int -> Int -> Cell -> IO ()
drawCell col row = \case
  CellEmpty -> pure ()
  CellNonEmpty NonEmptyCell {cellChar, cellFg, cellBg} ->
    Termbox.Bindings.tb_change_cell col row cellChar cellFg cellBg

-- | Create a cell from a character.
--
-- If the character is not 1 character wide, it will not be displayed.
char :: Char -> Cell
char c =
  case wcwidth (charToCWchar c) of
    1 ->
      CellNonEmpty
        NonEmptyCell
          { cellChar = c,
            cellFg = Termbox.Bindings.Tb_color 0,
            cellBg = Termbox.Bindings.Tb_color 0
          }
    _ -> CellEmpty

-- | Set the foreground color of a cell.
fg :: Color -> Cell -> Cell
fg (Color color) = \case
  CellEmpty -> CellEmpty
  CellNonEmpty image -> CellNonEmpty image {cellFg = color}

-- | Set the background color of a cell.
bg :: Color -> Cell -> Cell
bg (Color color) = \case
  CellEmpty -> CellEmpty
  CellNonEmpty image -> CellNonEmpty image {cellBg = color}

-- | Make a cell bold.
bold :: Cell -> Cell
bold = \case
  CellEmpty -> CellEmpty
  CellNonEmpty image@NonEmptyCell {cellFg} ->
    CellNonEmpty image {cellFg = Termbox.Bindings.tb_attr Termbox.Bindings.TB_BOLD cellFg}

-- | Make a cell underlined.
underline :: Cell -> Cell
underline = \case
  CellEmpty -> CellEmpty
  CellNonEmpty image@NonEmptyCell {cellFg} ->
    CellNonEmpty image {cellFg = Termbox.Bindings.tb_attr Termbox.Bindings.TB_UNDERLINE cellFg}

-- | Make a cell blink.
blink :: Cell -> Cell
blink = \case
  CellEmpty -> CellEmpty
  CellNonEmpty image@NonEmptyCell {cellBg} ->
    -- not a typo; bold background is blink
    CellNonEmpty image {cellBg = Termbox.Bindings.tb_attr Termbox.Bindings.TB_BOLD cellBg}

charToCWchar :: Char -> CWchar
charToCWchar =
  fromIntegral @Int @CWchar . Char.ord

foreign import capi unsafe "wchar.h wcwidth"
  wcwidth :: CWchar -> CInt

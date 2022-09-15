module Termbox.Internal.Cell
  ( -- * Cell
    Cell (..),
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
import Termbox.Internal.Color (Color (Color))

-- | A single cell.
--
-- * Create a cell with 'char', or with a string literal.
-- * Set a cell's color with 'fg', 'bg'.
-- * Style a cell with 'bold', 'underline', 'blink'.
data Cell
  = CellEmpty
  | CellFg
      {-# UNPACK #-} !Char -- invariant: char is width 1
      {-# UNPACK #-} !Termbox.Bindings.Tb_color -- fg
  | CellFgBlink
      {-# UNPACK #-} !Char -- invariant: char is width 1
      {-# UNPACK #-} !Termbox.Bindings.Tb_color -- fg
  | CellFgBg
      {-# UNPACK #-} !Char -- invariant: char is width 1
      {-# UNPACK #-} !Termbox.Bindings.Tb_color -- fg
      {-# UNPACK #-} !Termbox.Bindings.Tb_color -- bg

instance {-# OVERLAPS #-} IsString [Cell] where
  fromString =
    map char

drawCell :: Termbox.Bindings.Tb_color -> Int -> Int -> Cell -> IO ()
drawCell bg0 col row = \case
  CellEmpty -> pure ()
  CellFg ch fg_ -> Termbox.Bindings.tb_change_cell col row ch fg_ bg0
  CellFgBlink ch fg_ -> Termbox.Bindings.tb_change_cell col row ch fg_ (makeBold bg0) -- bold background = blink
  CellFgBg ch fg_ bg_ -> Termbox.Bindings.tb_change_cell col row ch fg_ bg_

-- | Create a cell from a character.
--
-- If the character is not 1 character wide, it will not be displayed.
char :: Char -> Cell
char ch =
  case wcwidth (charToCWchar ch) of
    1 -> CellFg ch Termbox.Bindings.TB_DEFAULT
    _ -> CellEmpty

-- | Set the foreground color of a cell.
fg :: Color -> Cell -> Cell
fg (Color color) = \case
  CellEmpty -> CellEmpty
  CellFg ch _ -> CellFg ch color
  CellFgBlink ch _ -> CellFgBlink ch color
  CellFgBg ch _ bg_ -> CellFgBg ch color bg_

-- | Set the background color of a cell.
bg :: Color -> Cell -> Cell
bg (Color color) = \case
  CellEmpty -> CellEmpty
  CellFg ch fg_ -> CellFgBg ch fg_ color
  CellFgBlink ch fg_ -> CellFgBg ch fg_ (makeBold color) -- bold background = blink
  CellFgBg ch fg_ _ -> CellFgBg ch fg_ color

-- | Make a cell bold.
bold :: Cell -> Cell
bold = \case
  CellEmpty -> CellEmpty
  CellFg ch fg_ -> CellFg ch (makeBold fg_)
  CellFgBlink ch fg_ -> CellFgBlink ch (makeBold fg_)
  CellFgBg ch fg_ bg_ -> CellFgBg ch (makeBold fg_) bg_

-- | Make a cell underlined.
underline :: Cell -> Cell
underline = \case
  CellEmpty -> CellEmpty
  CellFg ch fg_ -> CellFg ch (makeUnderline fg_)
  CellFgBlink ch fg_ -> CellFgBlink ch (makeUnderline fg_)
  CellFgBg ch fg_ bg_ -> CellFgBg ch (makeUnderline fg_) bg_

-- | Make a cell blink.
blink :: Cell -> Cell
blink = \case
  CellEmpty -> CellEmpty
  CellFg ch fg_ -> CellFgBlink ch fg_
  CellFgBlink ch fg_ -> CellFgBlink ch fg_
  CellFgBg ch fg_ bg_ -> CellFgBg ch fg_ (makeBold bg_) -- bold background = blink

makeBold :: Termbox.Bindings.Tb_color -> Termbox.Bindings.Tb_color
makeBold =
  Termbox.Bindings.tb_attr Termbox.Bindings.TB_BOLD

makeUnderline :: Termbox.Bindings.Tb_color -> Termbox.Bindings.Tb_color
makeUnderline =
  Termbox.Bindings.tb_attr Termbox.Bindings.TB_UNDERLINE

charToCWchar :: Char -> CWchar
charToCWchar =
  fromIntegral @Int @CWchar . Char.ord

foreign import capi unsafe "wchar.h wcwidth"
  wcwidth :: CWchar -> CInt

module Termbox.Internal.Scene
  ( Scene,
    drawScene,
    set,
    fill,
    cursor,
  )
where

import qualified Termbox.Bindings
import Termbox.Internal.Cell (Cell, drawCell)
import Termbox.Internal.Color (Color (Color))
import Termbox.Internal.Pos (Pos (..))

-- | A scene.
--
-- * Set individual characters with 'set'.
-- * Set the background fill color with 'fill'.
-- * Set the cursor position with 'cursor'.
-- * Combine scenes together with @<>@.
data Scene = Scene
  { sceneFill :: Maybe Termbox.Bindings.Tb_color,
    sceneDraw :: Termbox.Bindings.Tb_color -> IO ()
  }

instance Monoid Scene where
  mempty =
    Scene
      { sceneFill = Nothing,
        sceneDraw = \_ -> pure ()
      }

instance Semigroup Scene where
  Scene fill0 draw0 <> Scene fill1 draw1 =
    Scene
      { sceneFill =
          case fill1 of
            Nothing -> fill0
            Just _ -> fill1,
        sceneDraw =
          \color -> do
            draw0 color
            draw1 color
      }

-- Draw a scene.
drawScene :: Scene -> IO ()
drawScene Scene {sceneFill, sceneDraw} = do
  let background =
        case sceneFill of
          Nothing -> Termbox.Bindings.TB_DEFAULT
          Just color -> color

  Termbox.Bindings.tb_set_clear_attributes Termbox.Bindings.TB_DEFAULT background
  Termbox.Bindings.tb_clear
  sceneDraw background
  Termbox.Bindings.tb_present

-- | Set the background fill color.
fill :: Color -> Scene
fill (Color color) =
  mempty {sceneFill = Just color}

-- | Set a single cell.
set :: Pos -> Cell -> Scene
set Pos {col, row} img =
  mempty {sceneDraw = \_ -> drawCell col row img}

-- | Set the cursor position.
cursor :: Pos -> Scene
cursor Pos {col, row} =
  mempty {sceneDraw = \_ -> Termbox.Bindings.tb_set_cursor (Just (col, row))}
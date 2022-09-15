module Termbox.Internal.Scene
  ( Scene,
    drawScene,
    set,
    fill,
    cursor,
  )
where

import Data.Foldable (for_)
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
  { sceneFill :: Maybe Color,
    sceneDraw :: IO ()
  }

instance Monoid Scene where
  mempty =
    Scene
      { sceneFill = Nothing,
        sceneDraw = pure ()
      }

instance Semigroup Scene where
  Scene fill0 draw0 <> Scene fill1 draw1 =
    Scene
      { sceneFill =
          case fill1 of
            Nothing -> fill0
            Just _ -> fill1,
        sceneDraw = do
          draw0
          draw1
      }

-- Draw a scene.
drawScene :: Scene -> IO ()
drawScene Scene {sceneFill, sceneDraw} = do
  for_ sceneFill \(Color color) ->
    Termbox.Bindings.tb_set_clear_attributes 0 color
  Termbox.Bindings.tb_clear
  sceneDraw
  Termbox.Bindings.tb_present

-- | Set the background fill color.
fill :: Color -> Scene
fill color =
  mempty {sceneFill = Just color}

-- | Set a single cell.
set :: Pos -> Cell -> Scene
set Pos {col, row} img =
  mempty {sceneDraw = drawCell col row img}

-- | Set the cursor position.
cursor :: Pos -> Scene
cursor Pos {col, row} =
  mempty {sceneDraw = Termbox.Bindings.tb_set_cursor (Just (col, row))}

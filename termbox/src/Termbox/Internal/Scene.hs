module Termbox.Internal.Scene
  ( Scene,
    render,
    cell,
    fill,
    cursor,
  )
where

import qualified Termbox.Bindings.Hs
import Termbox.Internal.Cell (Cell, drawCell)
import Termbox.Internal.Color (Color (Color))
import Termbox.Internal.Pos (Pos (..))

-- | A scene.
--
-- * Set individual cells with 'cell'.
-- * Set the background fill color with 'fill'.
-- * Set the cursor position with 'cursor'.
-- * Combine scenes together with @<>@.
data Scene = Scene
  { sceneFill :: Maybe Termbox.Bindings.Hs.Tb_color,
    sceneDraw :: Termbox.Bindings.Hs.Tb_color -> IO ()
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

-- | Render a scene.
render :: Scene -> IO ()
render Scene {sceneFill, sceneDraw} = do
  let background =
        case sceneFill of
          Nothing -> Termbox.Bindings.Hs.TB_DEFAULT
          Just color -> color
  Termbox.Bindings.Hs.tb_set_clear_attributes Termbox.Bindings.Hs.TB_DEFAULT background
  Termbox.Bindings.Hs.tb_clear
  sceneDraw background
  Termbox.Bindings.Hs.tb_present

-- | Set the background fill color.
fill :: Color -> Scene
fill (Color color) =
  mempty {sceneFill = Just color}

-- | Set a single cell.
cell :: Pos -> Cell -> Scene
cell Pos {col, row} img =
  mempty {sceneDraw = \bg -> drawCell bg col row img}

-- | Set the cursor position.
cursor :: Pos -> Scene
cursor Pos {col, row} =
  mempty {sceneDraw = \_ -> Termbox.Bindings.Hs.tb_set_cursor (Just (col, row))}

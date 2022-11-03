-- |
-- This module provides a @reactive-banana@ FRP interface to @termbox@, a simple C library for writing text-based user
-- interfaces: <https://github.com/termbox/termbox>
--
-- See also:
--
-- * @<https://hackage.haskell.org/package/termbox-tea termbox-tea>@, an Elm Architecture interface.
--
-- This module is intended to be imported qualified.
--
-- ==== __👉 Quick start example__
--
-- This @termbox@ program displays the number of keys pressed.
--
-- @
-- {-\# LANGUAGE DuplicateRecordFields \#-}
-- {-\# LANGUAGE ImportQualifiedPost \#-}
-- {-\# LANGUAGE LambdaCase \#-}
-- {-\# LANGUAGE OverloadedRecordDot \#-}
-- {-\# LANGUAGE OverloadedStrings \#-}
-- {-\# LANGUAGE NoFieldSelectors \#-}
--
-- import Data.Foldable (fold)
-- import Reactive.Banana ((\<\@\>))
-- import Reactive.Banana qualified as Banana
-- import Termbox.Banana qualified as Termbox
--
-- main :: IO ()
-- main =
--   Termbox.'run' network \>\>= \\case
--     Left err -\> putStrLn (\"Termbox program failed to initialize: \" ++ show err)
--     Right n -\> putStrLn (\"Pressed \" ++ show n ++ \" keys.\")
--
-- network :: Banana.MonadMoment m =\> Termbox.'Inputs' -\> m (Termbox.'Outputs' Int)
-- network inputs = do
--   keysPressed <- Banana.accumB 0 ((+ 1) \<$ inputs.keys)
--   pure
--     Termbox.'Outputs'
--       { scene = render \<$\> keysPressed,
--         done = Banana.filterJust (isDone \<$\> keysPressed \<\@\> inputs.keys)
--       }
--   where
--     isDone :: Int -\> Termbox.'Termbox.Banana.Key' -\> Maybe Int
--     isDone n = \\case
--       Termbox.'Termbox.Banana.KeyEsc' -\> Just n
--       _ -\> Nothing
--
-- render :: Int -\> Termbox.'Termbox.Banana.Scene'
-- render keysPressed =
--   fold
--     [ string
--         Termbox.'Termbox.Banana.Pos' {row = 2, col = 4}
--         (\"Number of keys pressed: \" ++ map Termbox.'Termbox.Banana.char' (show keysPressed)),
--       string
--         Termbox.'Termbox.Banana.Pos' {row = 4, col = 4}
--         (\"Press \" ++ map (Termbox.'Termbox.Banana.bold' . Termbox.'Termbox.Banana.char') \"Esc\" ++ \" to quit.\")
--     ]
--
-- string :: Termbox.'Termbox.Banana.Pos' -\> [Termbox.'Termbox.Banana.Cell'] -\> Termbox.'Termbox.Banana.Scene'
-- string pos cells =
--   foldMap (\\(i, cell) -\> Termbox.'Termbox.Banana.cell' (Termbox.'Termbox.Banana.posRight' i pos) cell) (zip [0 ..] cells)
-- @
module Termbox.Banana
  ( -- * Main
    Inputs (..),
    Outputs (..),
    run,
    Termbox.InitError (..),

    -- * Terminal contents

    -- ** Scene
    Termbox.Scene,
    Termbox.cell,
    Termbox.fill,
    Termbox.cursor,

    -- ** Cell
    Termbox.Cell,
    Termbox.char,
    Termbox.fg,
    Termbox.bg,
    Termbox.bold,
    Termbox.underline,
    Termbox.blink,

    -- ** Colors
    Termbox.Color,

    -- *** Basic colors
    Termbox.defaultColor,
    Termbox.red,
    Termbox.green,
    Termbox.yellow,
    Termbox.blue,
    Termbox.magenta,
    Termbox.cyan,
    Termbox.white,
    Termbox.bright,

    -- *** 216 miscellaneous colors
    Termbox.color,

    -- *** 24 monochrome colors
    Termbox.gray,

    -- * Event handling
    Termbox.Key (..),
    Termbox.Mouse (..),
    Termbox.MouseButton (..),

    -- * Miscellaneous types
    Termbox.Pos (..),
    Termbox.posUp,
    Termbox.posDown,
    Termbox.posLeft,
    Termbox.posRight,
    Termbox.Size (..),
  )
where

import Control.Concurrent.MVar
import Control.Monad.IO.Class (liftIO)
import Data.Void (Void)
import qualified Reactive.Banana as Banana
import qualified Reactive.Banana.Frameworks as Banana
import qualified Termbox

-- | The inputs to a @termbox@ FRP network.
data Inputs = Inputs
  { -- | The initial terminal size.
    initialSize :: !Termbox.Size,
    -- | Key events.
    keys :: !(Banana.Event Termbox.Key),
    -- | Resize events.
    resizes :: !(Banana.Event Termbox.Size),
    -- | Mouse events.
    mouses :: !(Banana.Event Termbox.Mouse)
  }

-- | The outputs from a @termbox@ FRP network.
data Outputs a = Outputs
  { -- | The scene to render.
    scene :: !(Banana.Behavior Termbox.Scene),
    -- | The events of arbitrary values, on the first of which is relevant, which causes 'run' to return.
    --
    -- /Note/: Wrapping this event in 'Banana.once' is not necessary, as this library does so internally.
    done :: !(Banana.Event a)
  }

-- | Run a @termbox@ FRP network.
--
-- @run@ either:
--
--   * Returns immediately with an 'InitError'.
--   * Returns the first value emitted by @done@.
run ::
  -- | The FRP network.
  (Inputs -> Banana.MomentIO (Outputs a)) ->
  -- | The result of the FRP network.
  IO (Either Termbox.InitError a)
run program =
  Termbox.run (run_ program)

run_ :: (Inputs -> Banana.MomentIO (Outputs a)) -> IO a
run_ program = do
  initialSize <- Termbox.getSize

  doneVar <- newEmptyMVar
  (keysAddHandler, fireKey) <- Banana.newAddHandler
  (resizesAddHandler, fireResize) <- Banana.newAddHandler
  (mousesAddHandler, fireMouse) <- Banana.newAddHandler

  network <-
    Banana.compile do
      keys <- Banana.fromAddHandler keysAddHandler
      resizes <- Banana.fromAddHandler resizesAddHandler
      mouses <- Banana.fromAddHandler mousesAddHandler

      Outputs {scene, done} <- program Inputs {initialSize, keys, resizes, mouses}
      let render = Termbox.render <$> scene

      -- Render the first scene, and again every time it changes.
      liftIO =<< Banana.valueB render
      Banana.reactimate' =<< Banana.changes render

      -- Smuggle `done` values out via `doneVar` (only the first matters)
      done1 <- Banana.once done
      Banana.reactimate (putMVar doneVar <$> done1)

  Banana.actuate network

  let loop = do
        Termbox.poll @Void >>= \case
          Termbox.EventKey key -> fireKey key
          Termbox.EventResize size -> fireResize size
          Termbox.EventMouse mouse -> fireMouse mouse
        tryReadMVar doneVar >>= \case
          Nothing -> loop
          Just result -> pure result

  loop

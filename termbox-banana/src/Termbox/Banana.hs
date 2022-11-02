-- |
-- This module provides an Elm Architecture interface to @termbox@, a simple C library for writing text-based user
-- interfaces: <https://github.com/termbox/termbox>
--
-- This module is intended to be imported qualified.
--
-- ==== __👉 Quick start example__
--
-- This @termbox@ program displays the number of keys pressed.
--
-- @
-- {-\# LANGUAGE DerivingStrategies \#-}
-- {-\# LANGUAGE DuplicateRecordFields \#-}
-- {-\# LANGUAGE ImportQualifiedPost \#-}
-- {-\# LANGUAGE LambdaCase \#-}
-- {-\# LANGUAGE OverloadedRecordDot \#-}
-- {-\# LANGUAGE OverloadedStrings \#-}
-- {-\# LANGUAGE NoFieldSelectors \#-}
--
-- import Data.Foldable (fold)
-- import Data.Void (Void)
-- import Termbox.Tea qualified as Termbox
--
-- main :: IO ()
-- main = do
--   result <-
--     Termbox.'run'
--       Termbox.'Program'
--         { initialize,
--           pollEvent,
--           handleEvent,
--           handleEventError,
--           render,
--           finished
--         }
--   case result of
--     Left err -> putStrLn ("Termbox program failed to initialize: " ++ show err)
--     Right state -> putStrLn ("Final state: " ++ show state)
--
-- data MyState = MyState
--   { keysPressed :: Int,
--     pressedEsc :: Bool
--   }
--   deriving stock (Show)
--
-- initialize :: Termbox.'Size' -> MyState
-- initialize _size =
--   MyState
--     { keysPressed = 0,
--       pressedEsc = False
--     }
--
-- pollEvent :: Maybe (IO Void)
-- pollEvent =
--   Nothing
--
-- handleEvent :: MyState -> Termbox.'Event' Void -> IO MyState
-- handleEvent state = \\case
--   Termbox.'EventKey' key ->
--     pure
--       MyState
--         { keysPressed = state.keysPressed + 1,
--           pressedEsc =
--             case key of
--               Termbox.'KeyEsc' -> True
--               _ -> False
--         }
--   _ -> pure state
--
-- handleEventError :: MyState -> IO MyState
-- handleEventError state =
--   pure state
--
-- render :: MyState -> Termbox.'Scene'
-- render state =
--   fold
--     [ string
--         Termbox.'Pos' {row = 2, col = 4}
--         ("Number of keys pressed: " ++ map Termbox.'char' (show state.keysPressed))
--     , string
--         Termbox.'Pos' {row = 4, col = 4}
--         ("Press " ++ map (Termbox.'bold' . Termbox.'char') "Esc" ++ " to quit.")
--     ]
--
-- finished :: MyState -> Bool
-- finished state =
--   state.pressedEsc
--
-- string :: Termbox.'Pos' -> [Termbox.'Cell'] -> Termbox.'Scene'
-- string pos cells =
--   foldMap
--     (\\(i, cell) ->
--       Termbox.'cell'
--         Termbox.'Pos' {row = pos.row, col = pos.col + i}
--         cell)
--     (zip [0 ..] cells)
-- @
module Termbox.Banana
  ( -- * Termbox
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
    Termbox.Event (..),
    Termbox.Key (..),
    Termbox.Mouse (..),

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
import Control.Exception (mask, onException)
import Control.Monad.IO.Class (liftIO)
import Data.Void (Void)
import qualified Reactive.Banana as Banana
import qualified Reactive.Banana.Frameworks as Banana
import qualified Termbox

-- | The inputs to a @termbox@ FRP network.
data Inputs = Inputs
  { -- | The initial terminal size.
    initialSize :: Termbox.Size,
    -- | Key events.
    keyEvents :: Banana.Event Termbox.Key,
    -- | Resize events.
    resizeEvents :: Banana.Event Termbox.Size,
    -- | Mouse events.
    mouseEvents :: Banana.Event (Termbox.Mouse, Termbox.Pos)
  }

-- | The outputs from a @termbox@ FRP network.
data Outputs a = Outputs
  { sceneBehavior :: !(Banana.Behavior Termbox.Scene),
    doneEvent :: !(Banana.Event a)
  }

-- | Run a @termbox@ FRP network.
run ::
  -- | The FRP network.
  (Inputs -> Banana.MomentIO (Outputs a)) ->
  -- | The result of the FRP network.
  IO (Either Termbox.InitError a)
run program =
  mask \unmask ->
    Termbox.initialize >>= \case
      Left err -> pure (Left err)
      Right () -> do
        result <- unmask (run_ program) `onException` Termbox.shutdown
        Termbox.shutdown
        pure (Right result)

run_ :: (Inputs -> Banana.MomentIO (Outputs a)) -> IO a
run_ program = do
  initialSize <- Termbox.getSize

  doneVar <- newEmptyMVar
  (keyEventAddHandler, fireKeyEvent) <- Banana.newAddHandler
  (resizeEventAddHandler, fireResizeEvent) <- Banana.newAddHandler
  (mouseEventAddHandler, fireMouseEvent) <- Banana.newAddHandler

  network <-
    Banana.compile do
      keyEvents <- Banana.fromAddHandler keyEventAddHandler
      resizeEvents <- Banana.fromAddHandler resizeEventAddHandler
      mouseEvents <- Banana.fromAddHandler mouseEventAddHandler

      Outputs {sceneBehavior, doneEvent} <- program Inputs {initialSize, keyEvents, resizeEvents, mouseEvents}
      let renderBehavior = Termbox.render <$> sceneBehavior

      -- Render the first scene, and again every time it changes.
      liftIO =<< Banana.valueB renderBehavior
      Banana.reactimate' =<< Banana.changes renderBehavior

      -- Smuggle `doneEvent` values out via `doneVar` (only the first matters)
      Banana.reactimate (putMVar doneVar <$> doneEvent)

  Banana.actuate network

  let loop = do
        Termbox.poll @Void >>= \case
          Termbox.EventKey key -> fireKeyEvent key
          Termbox.EventResize size -> fireResizeEvent size
          Termbox.EventMouse mouse pos -> fireMouseEvent (mouse, pos)
        tryReadMVar doneVar >>= \case
          Nothing -> loop
          Just result -> pure result

  loop

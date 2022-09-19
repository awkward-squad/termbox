-- |
--
-- This module provides a high-level wrapper around @termbox@, a simple C library for writing text-based user
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
-- import Foreign.C.Error (Errno)
-- import Termbox qualified
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
-- handleEventError :: MyState -> Errno -> IO MyState
-- handleEventError state _errno =
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
module Termbox
  ( -- * Termbox
    Program (..),
    run,
    InitError (..),

    -- * Terminal contents

    -- ** Scene
    Scene,
    cell,
    fill,
    cursor,

    -- ** Cell
    Cell,
    char,
    fg,
    bg,
    bold,
    underline,
    blink,

    -- ** Colors
    Color,

    -- *** Basic colors
    defaultColor,
    red,
    green,
    yellow,
    blue,
    magenta,
    cyan,
    white,
    bright,

    -- *** 216 miscellaneous colors
    color,

    -- *** 24 monochrome colors
    gray,

    -- * Event handling
    Event (..),
    Key
      ( ..,
        KeyCtrlH,
        KeyCtrlLsqBracket,
        KeyCtrl2,
        KeyCtrl3,
        KeyCtrl4,
        KeyCtrl5,
        KeyCtrl7,
        KeyCtrlM,
        KeyCtrlI,
        KeyCtrlUnderscore
      ),
    Mouse (..),

    -- * Miscellaneous types
    Pos (..),
    posUp,
    posDown,
    posLeft,
    posRight,
    Size (..),
  )
where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad (forever)
import Foreign.C.Error (Errno)
import qualified Ki
import qualified Termbox.Bindings
import Termbox.Internal.Cell (Cell, bg, blink, bold, char, fg, underline)
import Termbox.Internal.Color
  ( Color,
    blue,
    bright,
    color,
    cyan,
    defaultColor,
    gray,
    green,
    magenta,
    red,
    white,
    yellow,
  )
import Termbox.Internal.Event (Event (..), poll)
import Termbox.Internal.Key
  ( Key (..),
    pattern KeyCtrl2,
    pattern KeyCtrl3,
    pattern KeyCtrl4,
    pattern KeyCtrl5,
    pattern KeyCtrl7,
    pattern KeyCtrlH,
    pattern KeyCtrlI,
    pattern KeyCtrlLsqBracket,
    pattern KeyCtrlM,
    pattern KeyCtrlUnderscore,
  )
import Termbox.Internal.Mouse (Mouse (..))
import Termbox.Internal.Pos (Pos (..), posDown, posLeft, posRight, posUp)
import Termbox.Internal.Scene (Scene, cell, cursor, drawScene, fill)
import Termbox.Internal.Size (Size (..))

-- | Termbox initialization errors.
data InitError
  = FailedToOpenTTY
  | PipeTrapError
  | UnsupportedTerminal
  deriving stock (Show)

instance Exception InitError

-- | A termbox program.
data Program e s = Program
  { -- | The initial state, given the initial terminal size.
    initialize :: Size -> s,
    -- | Poll for a user event. Every value that this @IO@ action returns is provided to @handleEvent@.
    pollEvent :: Maybe (IO e),
    -- | Handle an event.
    handleEvent :: s -> Event e -> IO s,
    -- | Handle an error that occurred during polling.
    handleEventError :: s -> Errno -> IO s,
    -- | Render the current state.
    render :: s -> Scene,
    -- | Is the current state finished?
    finished :: s -> Bool
  }

-- | Run a @termbox@ program, which either returns immediately with an 'InitError', or once the program state is
-- finished.
run :: Program e s -> IO (Either InitError s)
run program = do
  mask \unmask ->
    Termbox.Bindings.tb_init >>= \case
      Left err ->
        (pure . Left) case err of
          Termbox.Bindings.TB_EFAILED_TO_OPEN_TTY -> FailedToOpenTTY
          Termbox.Bindings.TB_EPIPE_TRAP_ERROR -> PipeTrapError
          Termbox.Bindings.TB_EUNSUPPORTED_TERMINAL -> UnsupportedTerminal
      Right () -> do
        result <- unmask (runProgram program) `onException` shutdown
        shutdown
        pure (Right result)

runProgram :: Program e s -> IO s
runProgram Program {initialize, pollEvent, handleEvent, handleEventError, render, finished} = do
  _ <- Termbox.Bindings.tb_select_input_mode Termbox.Bindings.TB_INPUT_MOUSE
  _ <- Termbox.Bindings.tb_select_output_mode Termbox.Bindings.TB_OUTPUT_256
  width <- Termbox.Bindings.tb_width
  height <- Termbox.Bindings.tb_height

  let state0 =
        initialize Size {width, height}

  let loop0 doPoll =
        let loop s0 =
              if finished s0
                then pure s0
                else do
                  drawScene (render s0)
                  result <- doPoll
                  s1 <-
                    case result of
                      Left errno -> handleEventError s0 errno
                      Right event -> handleEvent s0 event
                  loop s1
         in loop

  case pollEvent of
    Nothing -> loop0 poll state0
    Just pollEvent1 -> do
      eventVar <- newEmptyMVar

      Ki.scoped \scope -> do
        Ki.fork_ scope do
          forever do
            event <- pollEvent1
            putMVar eventVar (Right (EventUser event))

        Ki.fork_ scope do
          forever do
            event <- poll
            putMVar eventVar event

        loop0 (takeMVar eventVar) state0

shutdown :: IO ()
shutdown = do
  _ <- Termbox.Bindings.tb_select_output_mode Termbox.Bindings.TB_OUTPUT_NORMAL
  Termbox.Bindings.tb_shutdown

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
-- import Foreign.C.Error (Errno)
-- import Termbox qualified
--
-- main :: IO ()
-- main = do
--   result <-
--     Termbox.'run'
--       Termbox.'Program'
--         { initialize,
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
-- handleEvent :: MyState -> Termbox.'Event' -> IO MyState
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
--       Termbox.'set'
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
    set,
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

    -- *** Bright basic colors
    brightDefaultColor,
    brightRed,
    brightGreen,
    brightYellow,
    brightBlue,
    brightMagenta,
    brightCyan,
    brightWhite,

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
    Size (..),
  )
where

import Control.Exception
import Foreign.C.Error (Errno)
import qualified Termbox.Bindings
import Termbox.Internal.Cell (Cell, bg, blink, bold, char, fg, underline)
import Termbox.Internal.Color
  ( Color,
    blue,
    brightBlue,
    brightCyan,
    brightDefaultColor,
    brightGreen,
    brightMagenta,
    brightRed,
    brightWhite,
    brightYellow,
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
import Termbox.Internal.Pos (Pos (..))
import Termbox.Internal.Scene (Scene, cursor, drawScene, fill, set)
import Termbox.Internal.Size (Size (..))

-- | Termbox initialization errors.
data InitError
  = FailedToOpenTTY
  | PipeTrapError
  | UnsupportedTerminal
  deriving stock (Show)

instance Exception InitError

-- | A termbox program.
data Program a = Program
  { -- | The initial state, given the initial terminal size.
    initialize :: Size -> a,
    -- | Handle an event.
    handleEvent :: a -> Event -> IO a,
    -- | Handle an error that occurred during polling.
    handleEventError :: a -> Errno -> IO a,
    -- | Render the current state.
    render :: a -> Scene,
    -- | Is the current state finished?
    finished :: a -> Bool
  }

-- | Run a @termbox@ program, which either returns immediately with an 'InitError', or once the program state is
-- finished.
run :: Program a -> IO (Either InitError a)
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

runProgram :: Program a -> IO a
runProgram Program {initialize, handleEvent, handleEventError, render, finished} = do
  _ <- Termbox.Bindings.tb_select_input_mode Termbox.Bindings.TB_INPUT_MOUSE
  _ <- Termbox.Bindings.tb_select_output_mode Termbox.Bindings.TB_OUTPUT_256
  width <- Termbox.Bindings.tb_width
  height <- Termbox.Bindings.tb_height
  loop (initialize Size {width, height})
  where
    loop s0 =
      if finished s0
        then pure s0
        else do
          drawScene (render s0)
          result <- poll
          s1 <-
            case result of
              Left errno -> handleEventError s0 errno
              Right event -> handleEvent s0 event
          loop s1

shutdown :: IO ()
shutdown = do
  _ <- Termbox.Bindings.tb_select_output_mode Termbox.Bindings.TB_OUTPUT_NORMAL
  Termbox.Bindings.tb_shutdown

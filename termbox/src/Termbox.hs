-- |
-- A @termbox@ program is typically constructed as an infinite loop that:
--
-- 1. Renders a scene.
-- 2. Polls for an event.
--
-- For example, this progam simply displays the number of keys pressed, and
-- quits on @Esc@:
--
-- @
-- {-\# LANGUAGE BlockArguments \#-}
-- {-\# LANGUAGE LambdaCase \#-}
--
-- import qualified Termbox
--
-- main :: IO ()
-- main =
--   Termbox.'run' \\_pos render poll -\>
--     loop render poll 0
--
-- loop :: (Termbox.'Scene' -> IO ()) -> IO Termbox.'Event' -> Int -> IO ()
-- loop render poll n = do
--   render (string (Termbox.'Pos' 0 0) (show n))
--
--   poll >>= \\case
--     Termbox.'EventKey' Termbox.'KeyEsc' -> pure ()
--     _ -> loop render poll (n+1)
--
-- string :: Termbox.'Pos' -> String -> Termbox.'Scene'
-- string (Termbox.'Pos' row col) cs =
--   foldMap (\\(i, c) -> Termbox.'set' (Termbox.'Pos' row (col + i)) (Termbox.'char' c)) (zip [0..] cs)
-- @
--
-- Other termbox features include cell attributes (style, color), cursor
-- display, and mouse click handling.
--
-- This module is intended to be imported qualified.
module Termbox
  ( -- * Running a @termbox@ program
    Program (..),
    run,
    InitError (..),

    -- * Terminal contents

    -- ** Scene
    Scene,
    fill,
    set,
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
    black,
    red,
    green,
    yellow,
    blue,
    magenta,
    cyan,
    white,

    -- *** Bright basic colors
    brightBlack,
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
import Termbox.Cell (Cell, bg, blink, bold, char, fg, underline)
import Termbox.Color
  ( Color,
    black,
    blue,
    brightBlack,
    brightBlue,
    brightCyan,
    brightGreen,
    brightMagenta,
    brightRed,
    brightWhite,
    brightYellow,
    color,
    cyan,
    gray,
    green,
    magenta,
    red,
    white,
    yellow,
  )
import Termbox.Event (Event (..), poll)
import Termbox.Key
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
import Termbox.Mouse (Mouse (..))
import Termbox.Pos (Pos (..))
import Termbox.Scene (Scene, cursor, drawScene, fill, set)
import Termbox.Size (Size (..))

-- | Termbox initialization errors.
data InitError
  = FailedToOpenTTY
  | PipeTrapError
  | UnsupportedTerminal
  deriving stock (Show)

instance Exception InitError

-- | A termbox program.
data Program s = Program
  { -- | The initial state, given the initial terminal size.
    initialize :: Size -> s,
    -- | Handle an event.
    handleEvent :: s -> Event -> IO s,
    -- | Handle an error that occurred during polling.
    handleEventError :: s -> Errno -> IO s,
    -- | Render the current state.
    render :: s -> Scene,
    -- | Is the current state finished?
    finished :: s -> Bool
  }

-- | Run a @termbox@ program.
--
-- The function provided to @run@ is provided:
--
--   * The initial terminal size
--   * An action that renders a scene
--   * An action that polls for an event indefinitely
run :: Program s -> IO (Either InitError s)
run Program {initialize, handleEvent, handleEventError, render, finished} = do
  mask \unmask ->
    Termbox.Bindings.tb_init >>= \case
      Left err ->
        (pure . Left) case err of
          Termbox.Bindings.TB_EFAILED_TO_OPEN_TTY -> FailedToOpenTTY
          Termbox.Bindings.TB_EPIPE_TRAP_ERROR -> PipeTrapError
          Termbox.Bindings.TB_EUNSUPPORTED_TERMINAL -> UnsupportedTerminal
      Right () -> do
        result <-
          unmask
            ( do
                _ <- Termbox.Bindings.tb_select_input_mode Termbox.Bindings.TB_INPUT_MOUSE
                _ <- Termbox.Bindings.tb_select_output_mode Termbox.Bindings.TB_OUTPUT_256
                width <- Termbox.Bindings.tb_width
                height <- Termbox.Bindings.tb_height
                let loop s0 =
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
                loop (initialize Size {width, height})
            )
            `onException` shutdown
        shutdown
        pure (Right result)

shutdown :: IO ()
shutdown = do
  _ <- Termbox.Bindings.tb_select_output_mode Termbox.Bindings.TB_OUTPUT_NORMAL
  Termbox.Bindings.tb_shutdown

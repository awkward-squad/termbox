{-# LANGUAGE PatternSynonyms #-}

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
-- {-\# LANGUAGE LambdaCase \#-}
--
-- import qualified Termbox
--
-- main :: IO ()
-- main =
--   Termbox.'run' (\\_width _height render poll -> loop render poll 0)
--
-- loop :: (Termbox.'Cells' -> Termbox.'Cursor' -> IO ()) -> IO Termbox.'Event' -> Int -> IO ()
-- loop render poll n = do
--   render (string 0 0 (show n)) Termbox.'NoCursor'
--
--   poll >>= \\case
--     Termbox.'EventKey' Termbox.'KeyEsc' -> pure ()
--     _ -> loop render poll (n+1)
--
-- string :: Int -> Int -> String -> Termbox.'Cells'
-- string col row =
--   foldMap (\\(i, c) -> Termbox.'set' (col + i) row (Termbox.'Cell' c 0 0)) . zip [0..]
-- @
--
-- Other termbox features include cell attributes (style, color), cursor
-- display, and mouse click handling.
--
-- This module is intended to be imported qualified.
module Termbox
  ( -- * Initialization
    run,
    InitError (..),

    -- * Terminal contents
    set,
    Cells,
    Cell (..),
    Cursor (..),

    -- * Event handling
    Event (..),
    Key (..),
    -- $key-aliases
    pattern KeyCtrlH,
    pattern KeyCtrlLsqBracket,
    pattern KeyCtrl2,
    pattern KeyCtrl3,
    pattern KeyCtrl4,
    pattern KeyCtrl5,
    pattern KeyCtrl7,
    pattern KeyCtrlM,
    pattern KeyCtrlI,
    pattern KeyCtrlUnderscore,
    Mouse (..),
    PollError (..),

    -- * Attributes
    Attr,

    -- ** Colors

    -- *** Basic colors
    black,
    red,
    green,
    yellow,
    blue,
    magenta,
    cyan,
    white,

    -- **** Bright basic colors
    brightBlack,
    brightRed,
    brightGreen,
    brightYellow,
    brightBlue,
    brightMagenta,
    brightCyan,
    brightWhite,

    -- *** Miscllaneous colors
    color0,
    color1,
    color2,
    color3,
    color4,
    color5,
    color6,
    color7,
    color8,
    color9,
    color10,
    color11,
    color12,
    color13,
    color14,
    color15,
    color16,
    color17,
    color18,
    color19,
    color20,
    color21,
    color22,
    color23,
    color24,
    color25,
    color26,
    color27,
    color28,
    color29,
    color30,
    color31,
    color32,
    color33,
    color34,
    color35,
    color36,
    color37,
    color38,
    color39,
    color40,
    color41,
    color42,
    color43,
    color44,
    color45,
    color46,
    color47,
    color48,
    color49,
    color50,
    color51,
    color52,
    color53,
    color54,
    color55,
    color56,
    color57,
    color58,
    color59,
    color60,
    color61,
    color62,
    color63,
    color64,
    color65,
    color66,
    color67,
    color68,
    color69,
    color70,
    color71,
    color72,
    color73,
    color74,
    color75,
    color76,
    color77,
    color78,
    color79,
    color80,
    color81,
    color82,
    color83,
    color84,
    color85,
    color86,
    color87,
    color88,
    color89,
    color90,
    color91,
    color92,
    color93,
    color94,
    color95,
    color96,
    color97,
    color98,
    color99,
    color100,
    color101,
    color102,
    color103,
    color104,
    color105,
    color106,
    color107,
    color108,
    color109,
    color110,
    color111,
    color112,
    color113,
    color114,
    color115,
    color116,
    color117,
    color118,
    color119,
    color120,
    color121,
    color122,
    color123,
    color124,
    color125,
    color126,
    color127,
    color128,
    color129,
    color130,
    color131,
    color132,
    color133,
    color134,
    color135,
    color136,
    color137,
    color138,
    color139,
    color140,
    color141,
    color142,
    color143,
    color144,
    color145,
    color146,
    color147,
    color148,
    color149,
    color150,
    color151,
    color152,
    color153,
    color154,
    color155,
    color156,
    color157,
    color158,
    color159,
    color160,
    color161,
    color162,
    color163,
    color164,
    color165,
    color166,
    color167,
    color168,
    color169,
    color170,
    color171,
    color172,
    color173,
    color174,
    color175,
    color176,
    color177,
    color178,
    color179,
    color180,
    color181,
    color182,
    color183,
    color184,
    color185,
    color186,
    color187,
    color188,
    color189,
    color190,
    color191,
    color192,
    color193,
    color194,
    color195,
    color196,
    color197,
    color198,
    color199,
    color200,
    color201,
    color202,
    color203,
    color204,
    color205,
    color206,
    color207,
    color208,
    color209,
    color210,
    color211,
    color212,
    color213,
    color214,
    color215,

    -- *** Grayscale colors
    gray0,
    gray1,
    gray2,
    gray3,
    gray4,
    gray5,
    gray6,
    gray7,
    gray8,
    gray9,
    gray10,
    gray11,
    gray12,
    gray13,
    gray14,
    gray15,
    gray16,
    gray17,
    gray18,
    gray19,
    gray20,
    gray21,
    gray22,
    gray23,

    -- ** Modifiers
    bold,
    underline,
    reverse,
  )
where

import Control.Exception
import Foreign.C.Types (CInt)
import Termbox.Attr
import qualified Termbox.Bindings
import Termbox.Cell (Cell (Cell))
import Termbox.Cells (Cells (Cells), set)
import Termbox.Event (Event (..), PollError (..), poll)
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
import Prelude hiding (reverse)

-- | A cursor.
data Cursor
  = -- | Column, then row
    Cursor !Int !Int
  | NoCursor

-- $key-aliases
-- In a few cases, distinct key sequences map to equivalent key events. The pattern synonyms below are provided for an
-- alternate syntax in these cases, if desired.

-- | Termbox initialization errors.
data InitError
  = FailedToOpenTTY
  | PipeTrapError
  | UnsupportedTerminal
  deriving stock (Show)

instance Exception InitError

-- | Run a @termbox@ program and restore the terminal state afterwards.
--
-- The function provided to @run@ is provided:
--
--   * The initial terminal width
--   * The initial terminal height
--   * An action that renders a scene
--   * An action that polls for an event indefinitely
--
-- /Throws/: 'InitError'
run :: (Int -> Int -> (Cells -> Cursor -> IO ()) -> IO Event -> IO a) -> IO a
run action = do
  mask $ \unmask -> do
    initResult <- Termbox.Bindings.tb_init
    case () of
      _ | initResult == 0 -> do
        result <-
          unmask
            ( do
                _ <- Termbox.Bindings.tb_select_input_mode Termbox.Bindings.TB_INPUT_MOUSE
                _ <- Termbox.Bindings.tb_select_output_mode Termbox.Bindings.TB_OUTPUT_256
                width <- Termbox.Bindings.tb_width
                height <- Termbox.Bindings.tb_height
                action width height render poll
            )
            `onException` shutdown
        shutdown
        pure result
      _ | initResult == fromIntegral @CInt @Int Termbox.Bindings._TB_EFAILED_TO_OPEN_TTY -> throwIO FailedToOpenTTY
      _ | initResult == fromIntegral @CInt @Int Termbox.Bindings._TB_EPIPE_TRAP_ERROR -> throwIO PipeTrapError
      _
        | initResult == fromIntegral @CInt @Int Termbox.Bindings._TB_EUNSUPPORTED_TERMINAL ->
            throwIO UnsupportedTerminal
      _ -> error ("termbox: unknown tb_init error " ++ show initResult)

-- | Render a scene.
render :: Cells -> Cursor -> IO ()
render (Cells cells) cursor = do
  Termbox.Bindings.tb_set_clear_attributes 0 0
  Termbox.Bindings.tb_clear
  cells
  case cursor of
    Cursor col row -> Termbox.Bindings.tb_set_cursor col row
    NoCursor ->
      Termbox.Bindings.tb_set_cursor
        (fromIntegral @CInt @Int Termbox.Bindings._TB_HIDE_CURSOR)
        (fromIntegral @CInt @Int Termbox.Bindings._TB_HIDE_CURSOR)
  Termbox.Bindings.tb_present

shutdown :: IO ()
shutdown = do
  _ <- Termbox.Bindings.tb_select_output_mode Termbox.Bindings.TB_OUTPUT_NORMAL
  Termbox.Bindings.tb_shutdown

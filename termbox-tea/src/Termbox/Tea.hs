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
module Termbox.Tea
  ( -- * Termbox
    Program (..),
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
import Control.Exception
import Control.Monad (forever)
import qualified Ki
import qualified Termbox

-- | A @termbox@ program, parameterized by state __@s@__.
data Program s = forall e.
  Program
  { -- | The initial state, given the initial terminal size.
    initialize :: Termbox.Size -> s,
    -- | Poll for a user event. Every value that this @IO@ action returns is provided to @handleEvent@.
    pollEvent :: Maybe (IO e),
    -- | Handle an event.
    handleEvent :: s -> Termbox.Event e -> IO s,
    -- | Render the current state.
    render :: s -> Termbox.Scene,
    -- | Is the current state finished?
    finished :: s -> Bool
  }

-- | Run a @termbox@ program.
--
-- Either returns immediately with an 'InitError', or once the program state is finished with the final state.
run :: Program s -> IO (Either Termbox.InitError s)
run program = do
  mask \unmask ->
    Termbox.initialize >>= \case
      Left err -> pure (Left err)
      Right () -> do
        result <- unmask (runProgram program) `onException` Termbox.shutdown
        Termbox.shutdown
        pure (Right result)

runProgram :: Program s -> IO s
runProgram Program {initialize, pollEvent, handleEvent, render, finished} = do
  state0 <- initialize <$> Termbox.getSize

  let loop0 doPoll =
        let loop s0 =
              if finished s0
                then pure s0
                else do
                  Termbox.render (render s0)
                  event <- doPoll
                  s1 <- handleEvent s0 event
                  loop s1
         in loop

  case pollEvent of
    Nothing -> loop0 Termbox.poll state0
    Just pollEvent1 -> do
      eventVar <- newEmptyMVar

      Ki.scoped \scope -> do
        Ki.fork_ scope do
          forever do
            event <- pollEvent1
            putMVar eventVar (Termbox.EventUser event)

        Ki.fork_ scope do
          forever do
            event <- Termbox.poll
            putMVar eventVar event

        loop0 (takeMVar eventVar) state0

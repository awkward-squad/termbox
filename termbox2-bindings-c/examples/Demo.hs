{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Main (main) where

import Data.Bits ((.|.))
import Foreign.C.String (withCString)
import Foreign.Marshal (alloca)
import Foreign.Storable (peek)
import Termbox2.Bindings.C
import Text.Printf (printf)

-- This simple example ignores all result codes :)

main :: IO ()
main = do
  tb_init
  withCString "hello from haskell" (tb_print 0 0 (_TB_RED .|. _TB_BOLD) _TB_DEFAULT)
  withCString "press any key" (tb_print 0 1 _TB_GREEN _TB_DEFAULT)
  tb_present
  event <-
    alloca \eventPtr -> do
      tb_poll_event eventPtr
      peek eventPtr
  withCString
    ( printf
        "event: type=%d mod=%d key=%d ch=%d w=%d h=%d x=%d y=%d"
        event.type_
        event.mod
        event.key
        event.ch
        event.w
        event.h
        event.x
        event.y
    )
    (tb_print 0 2 _TB_YELLOW _TB_DEFAULT)
  tb_present
  withCString "press any key to quit" (tb_print 0 3 _TB_BLUE _TB_DEFAULT)
  tb_present
  alloca tb_poll_event
  tb_shutdown
  pure ()

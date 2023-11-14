{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Main (main) where

import Data.Text qualified as Text
import Termbox2.Bindings.Hs
import Text.Printf (printf)

-- This simple example ignores all result codes :)

main :: IO ()
main = do
  tb_init
  tb_print 0 0 (TB_RED <> TB_BOLD) TB_DEFAULT "hello from haskell"
  tb_print 0 1 TB_GREEN TB_DEFAULT "press any key"
  tb_present
  Right event <- tb_poll_event
  tb_print
    0
    2
    TB_YELLOW
    TB_DEFAULT
    ( Text.pack
        ( printf
            "event: type=%s mod=%s key=%s ch=%d w=%d h=%d x=%d y=%d"
            (show event.type_)
            (show event.mod)
            (show event.key)
            event.ch
            event.w
            event.h
            event.x
            event.y
        )
    )
  tb_present
  tb_print 0 3 TB_BLUE TB_DEFAULT "press any key to quit"
  tb_present
  tb_poll_event
  tb_shutdown
  pure ()

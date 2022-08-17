module Termbox.Event
  ( Event (..),
    poll,
    PollError (..),
  )
where

import Control.Exception (Exception, throwIO)
import Data.Int (Int32)
import qualified Termbox.Bindings
import Termbox.Key (Key (KeyChar), parseKey)
import Termbox.Mouse (Mouse, parseMouse)
import Termbox.Size (Size (..))
import Prelude hiding (mod)

-- | A input event.
data Event
  = -- | Key event
    EventKey !Key
  | -- | Resize event.
    EventResize !Size
  | -- | Mouse event (column, then row)
    EventMouse !Mouse !Int !Int
  deriving stock (Eq, Show)

-- | Block until an 'Event' arrives.
--
-- /Throws/: 'PollError'
poll :: IO Event
poll =
  Termbox.Bindings.tb_poll_event >>= \case
    Left _errno -> throwIO PollError
    Right event -> pure (parseEvent event)

-- | An error occurred when polling, due to mysterious circumstances that are not well-documented in the original C
-- codebase.
data PollError
  = PollError
  deriving stock (Show)

instance Exception PollError

-- Parse an 'Event' from a 'TbEvent'.
parseEvent :: Termbox.Bindings.Tb_event -> Event
parseEvent
  Termbox.Bindings.Tb_event
    { Termbox.Bindings.type_,
      Termbox.Bindings.mod = _,
      Termbox.Bindings.key,
      Termbox.Bindings.ch,
      Termbox.Bindings.w,
      Termbox.Bindings.h,
      Termbox.Bindings.x,
      Termbox.Bindings.y
    } =
    case type_ of
      Termbox.Bindings.TB_EVENT_KEY ->
        EventKey (if ch == '\0' then parseKey key else KeyChar ch)
      Termbox.Bindings.TB_EVENT_RESIZE ->
        EventResize
          Size
            { width = fromIntegral @Int32 @Int w,
              height = fromIntegral @Int32 @Int h
            }
      Termbox.Bindings.TB_EVENT_MOUSE ->
        EventMouse (parseMouse key) (fromIntegral @Int32 @Int x) (fromIntegral @Int32 @Int y)

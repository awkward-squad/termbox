module Termbox.Internal.Event
  ( Event (..),
    poll,
  )
where

import Data.Int (Int32)
import qualified Termbox.Bindings
import Termbox.Internal.Key (Key (KeyChar), parseKey)
import Termbox.Internal.Mouse (Mouse, parseMouse)
import Termbox.Internal.Pos (Pos (..))
import Termbox.Internal.Size (Size (..))
import Prelude hiding (mod)

-- | A input event.
data Event e
  = -- | Key event
    EventKey !Key
  | -- | Resize event
    EventResize !Size
  | -- | Mouse event
    EventMouse !Mouse !Pos
  | -- | User event
    EventUser !e
  deriving stock (Eq, Show)

-- Block until an Event arrives.
poll :: IO (Either () (Event e))
poll = do
  result <- Termbox.Bindings.tb_poll_event
  pure (parseEvent <$> result)

-- Parse an Event from a TbEvent.
parseEvent :: Termbox.Bindings.Tb_event -> Event e
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
      Termbox.Bindings.TB_EVENT_KEY -> EventKey (if ch == '\0' then parseKey key else KeyChar ch)
      Termbox.Bindings.TB_EVENT_RESIZE ->
        EventResize
          Size
            { width = fromIntegral @Int32 @Int w,
              height = fromIntegral @Int32 @Int h
            }
      Termbox.Bindings.TB_EVENT_MOUSE ->
        EventMouse
          (parseMouse key)
          Pos
            { row = fromIntegral @Int32 @Int y,
              col = fromIntegral @Int32 @Int x
            }

module Termbox.Bindings.Hs.Internal.EventType
  ( Tb_event_type
      ( Tb_event_type,
        TB_EVENT_KEY,
        TB_EVENT_MOUSE,
        TB_EVENT_RESIZE
      ),
  )
where

import Data.Word (Word8)
import qualified Termbox.Bindings.C

-- | An event type.
newtype Tb_event_type
  = Tb_event_type Word8
  deriving stock (Eq, Ord)

instance Show Tb_event_type where
  show = \case
    TB_EVENT_KEY -> "TB_EVENT_KEY"
    TB_EVENT_MOUSE -> "TB_EVENT_MOUSE"
    TB_EVENT_RESIZE -> "TB_EVENT_RESIZE"

pattern TB_EVENT_KEY :: Tb_event_type
pattern TB_EVENT_KEY <-
  ((== Tb_event_type Termbox.Bindings.C._TB_EVENT_KEY) -> True)
  where
    TB_EVENT_KEY = Tb_event_type Termbox.Bindings.C._TB_EVENT_KEY

pattern TB_EVENT_MOUSE :: Tb_event_type
pattern TB_EVENT_MOUSE <-
  ((== Tb_event_type Termbox.Bindings.C._TB_EVENT_MOUSE) -> True)
  where
    TB_EVENT_MOUSE = Tb_event_type Termbox.Bindings.C._TB_EVENT_MOUSE

pattern TB_EVENT_RESIZE :: Tb_event_type
pattern TB_EVENT_RESIZE <-
  ((== Tb_event_type Termbox.Bindings.C._TB_EVENT_RESIZE) -> True)
  where
    TB_EVENT_RESIZE = Tb_event_type Termbox.Bindings.C._TB_EVENT_RESIZE

{-# COMPLETE TB_EVENT_KEY, TB_EVENT_MOUSE, TB_EVENT_RESIZE #-}
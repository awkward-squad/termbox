module Termbox2.Bindings.Hs.Internal.Event
  ( Tb_event (..),
    makeEvent,
  )
where

import Data.Int (Int32)
import GHC.Generics (Generic)
import Termbox2.Bindings.C qualified as Termbox
import Termbox2.Bindings.Hs.Internal.EventMod (Tb_event_mod (..))
import Termbox2.Bindings.Hs.Internal.EventType (Tb_event_type (..))
import Termbox2.Bindings.Hs.Internal.Key (Tb_key (..))
import Termbox2.Bindings.Hs.Internal.Prelude (word32ToChar)
import Prelude hiding (mod)

-- | An event.
data Tb_event = Tb_event
  { type_ :: {-# UNPACK #-} !Tb_event_type,
    mod :: {-# UNPACK #-} !Tb_event_mod,
    key :: {-# UNPACK #-} !Tb_key,
    ch :: {-# UNPACK #-} !Char,
    w :: {-# UNPACK #-} !Int32,
    h :: {-# UNPACK #-} !Int32,
    x :: {-# UNPACK #-} !Int32,
    y :: {-# UNPACK #-} !Int32
  }
  deriving stock (Eq, Generic, Ord, Show)

makeEvent :: Termbox.Tb_event -> Tb_event
makeEvent Termbox.Tb_event {type_, mod, key, ch, w, h, x, y} =
  Tb_event
    { type_ = Tb_event_type type_,
      mod = Tb_event_mod mod,
      key = Tb_key key,
      ch = word32ToChar ch,
      w,
      h,
      x,
      y
    }
{-# INLINE makeEvent #-}

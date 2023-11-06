module Termbox.Bindings.Hs.Internal.Functions
  ( tb_init,
    tb_init_fd,
    tb_init_file,
    tb_select_input_mode,
    tb_select_output_mode,
    tb_width,
    tb_height,
    tb_peek_event,
    tb_poll_event,
    tb_attr,
    tb_set_cursor,
    tb_put_cell,
    tb_change_cell,
    tb_set_clear_attributes,
  )
where

import Data.Bits ((.|.))
import Data.Coerce (coerce)
import Data.Word (Word16)
import Foreign.C.String (withCString)
import Foreign.Marshal.Alloc (alloca)
import qualified Foreign.Storable as Storable
import System.Posix.Types (Fd (Fd))
import qualified Termbox.Bindings.C
import Termbox.Bindings.Hs.Internal.Attr (Tb_attr (..))
import Termbox.Bindings.Hs.Internal.Cell (Tb_cell, cellToCCell)
import Termbox.Bindings.Hs.Internal.Color (Tb_color (..))
import Termbox.Bindings.Hs.Internal.Event (Tb_event, ceventToEvent)
import Termbox.Bindings.Hs.Internal.InitError (Tb_init_error (..))
import Termbox.Bindings.Hs.Internal.InputMode (Tb_input_mode (..))
import Termbox.Bindings.Hs.Internal.OutputMode (Tb_output_mode (..))
import Termbox.Bindings.Hs.Internal.Prelude (charToWord32, cintToInt, intToCInt)

-- | Set the attribute of a color.
tb_attr :: Tb_attr -> Tb_color -> Tb_color
tb_attr =
  coerce ((.|.) :: Word16 -> Word16 -> Word16)

-- | Set a cell value in the back buffer.
tb_change_cell ::
  -- | x
  Int ->
  -- | y
  Int ->
  -- | ch
  Char ->
  -- | fg
  Tb_color ->
  -- | bg
  Tb_color ->
  IO ()
tb_change_cell cx cy c (Tb_color foreground) (Tb_color background) =
  Termbox.Bindings.C.tb_change_cell (intToCInt cx) (intToCInt cy) (charToWord32 c) foreground background

-- | Get the terminal height.
tb_height :: IO Int
tb_height =
  cintToInt <$> Termbox.Bindings.C.tb_height

-- | Initialize the @termbox@ library.
tb_init :: IO (Either Tb_init_error ())
tb_init = do
  code <- Termbox.Bindings.C.tb_init
  pure
    if code == 0
      then Right ()
      else Left (Tb_init_error code)

-- | Initialize the @termbox@ library.
--
-- > tb_init = tb_init_fd(0)
tb_init_fd :: Fd -> IO (Either Tb_init_error ())
tb_init_fd (Fd fd) = do
  code <- Termbox.Bindings.C.tb_init_fd fd
  pure
    if code == 0
      then Right ()
      else Left (Tb_init_error code)

-- | Initialize the @termbox@ library.
--
-- > tb_init = tb_init_file("/dev/tty")
tb_init_file :: FilePath -> IO (Either Tb_init_error ())
tb_init_file file = do
  code <-
    withCString file \c_file ->
      Termbox.Bindings.C.tb_init_file c_file
  pure
    if code == 0
      then Right ()
      else Left (Tb_init_error code)

-- | Wait up to a number of milliseconds for an event.
tb_peek_event :: Int -> IO (Either () (Maybe Tb_event))
tb_peek_event timeout =
  alloca \c_event -> do
    result <- Termbox.Bindings.C.tb_peek_event c_event (intToCInt timeout)
    if result < 0
      then pure (Left ())
      else
        if result == 0
          then pure (Right Nothing)
          else Right . Just . ceventToEvent <$> Storable.peek c_event

-- | Wait for an event.
tb_poll_event :: IO (Either () Tb_event)
tb_poll_event =
  alloca \c_event -> do
    result <- Termbox.Bindings.C.tb_poll_event c_event
    if result < 0
      then pure (Left ())
      else Right . ceventToEvent <$> Storable.peek c_event

-- | Set a cell value in the back buffer.
tb_put_cell ::
  -- | x
  Int ->
  -- | y
  Int ->
  -- | cell
  Tb_cell ->
  IO ()
tb_put_cell cx cy cell =
  alloca \c_cell -> do
    Storable.poke c_cell (cellToCCell cell)
    Termbox.Bindings.C.tb_put_cell (intToCInt cx) (intToCInt cy) c_cell

-- | Get\/set the input mode.
tb_select_input_mode :: Tb_input_mode -> IO Tb_input_mode
tb_select_input_mode =
  coerce Termbox.Bindings.C.tb_select_input_mode

-- | Get\/set the output mode.
tb_select_output_mode :: Tb_output_mode -> IO Tb_output_mode
tb_select_output_mode =
  coerce Termbox.Bindings.C.tb_select_output_mode

-- | Set the foreground and background attributes that 'tb_clear' clears the back buffer with.
tb_set_clear_attributes ::
  -- | fg
  Tb_color ->
  -- | bg
  Tb_color ->
  IO ()
tb_set_clear_attributes (Tb_color foreground) (Tb_color background) =
  Termbox.Bindings.C.tb_set_clear_attributes foreground background

-- | Set the cursor location, or hide it.
tb_set_cursor ::
  -- | x, y
  Maybe (Int, Int) ->
  IO ()
tb_set_cursor = \case
  Nothing -> Termbox.Bindings.C.tb_set_cursor Termbox.Bindings.C._TB_HIDE_CURSOR Termbox.Bindings.C._TB_HIDE_CURSOR
  Just (cx, cy) -> Termbox.Bindings.C.tb_set_cursor (intToCInt cx) (intToCInt cy)

-- | Get the terminal width.
tb_width :: IO Int
tb_width =
  cintToInt <$> Termbox.Bindings.C.tb_width

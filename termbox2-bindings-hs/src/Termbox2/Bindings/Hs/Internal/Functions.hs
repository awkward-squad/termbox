module Termbox2.Bindings.Hs.Internal.Functions
  ( tb_clear,
    tb_extend_cell,
    tb_get_fds,
    tb_get_input_mode,
    tb_get_output_mode,
    tb_height,
    tb_hide_cursor,
    tb_init,
    tb_init_fd,
    tb_init_file,
    tb_init_rwfd,
    tb_invalidate,
    tb_last_errno,
    tb_peek_event,
    tb_poll_event,
    tb_present,
    tb_print,
    tb_print_ex,
    tb_send,
    tb_set_cell,
    tb_set_cell_ex,
    tb_set_clear_attrs,
    tb_set_cursor,
    tb_set_input_mode,
    tb_set_output_mode,
    tb_shutdown,
    tb_strerror,
    tb_width,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Unsafe qualified as ByteString
import Data.Coerce (coerce)
import Data.Text (Text)
import Data.Text.Foreign qualified as Text (peekCStringLen, withCString)
import Foreign.C (CInt, Errno (..), withCString)
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.Marshal (alloca, lengthArray0, withArrayLen)
import Foreign.Storable qualified as Storable
import System.Posix.Types (Fd (Fd))
import Termbox2.Bindings.C qualified as Termbox
import Termbox2.Bindings.Hs.Internal.Attr (Tb_attr (Tb_attr))
import Termbox2.Bindings.Hs.Internal.Error (Tb_error (Tb_error))
import Termbox2.Bindings.Hs.Internal.Event (Tb_event, makeEvent)
import Termbox2.Bindings.Hs.Internal.InputMode (Tb_input_mode (Tb_input_mode))
import Termbox2.Bindings.Hs.Internal.OutputMode (Tb_output_mode (Tb_output_mode))
import Termbox2.Bindings.Hs.Internal.Prelude

-- | Clear the back buffer.
tb_clear :: IO (Either Tb_error ())
tb_clear =
  check Termbox.tb_clear

-- | Append a code point to a cell in the back buffer.
tb_extend_cell ::
  -- | x
  Int ->
  -- | y
  Int ->
  -- | ch
  Char ->
  IO (Either Tb_error ())
tb_extend_cell x y ch =
  check (Termbox.tb_extend_cell (intToCInt x) (intToCInt y) (charToWord32 ch))

-- | Get the terminal and resize file descriptors.
tb_get_fds :: IO (Either Tb_error (Fd, Fd))
tb_get_fds =
  alloca \p1 ->
    alloca \p2 -> do
      code <- Termbox.tb_get_fds p1 p2
      if code == Termbox._TB_OK
        then do
          fd1 <- Storable.peek p1
          fd2 <- Storable.peek p2
          pure (Right (Fd fd1, Fd fd2))
        else pure (Left (Tb_error code))

-- | Get the input mode.
tb_get_input_mode :: IO (Either Tb_error Tb_input_mode)
tb_get_input_mode = do
  n <- Termbox.tb_set_input_mode Termbox._TB_INPUT_CURRENT
  pure
    if n < 0
      then Left (Tb_error n)
      else Right (Tb_input_mode n)

-- | Get the output mode.
tb_get_output_mode :: IO (Either Tb_error Tb_output_mode)
tb_get_output_mode = do
  n <- Termbox.tb_set_output_mode Termbox._TB_OUTPUT_CURRENT
  pure
    if n < 0
      then Left (Tb_error n)
      else Right (Tb_output_mode n)

-- | Get the terminal height.
tb_height :: IO (Either Tb_error Int)
tb_height = do
  n <- Termbox.tb_height
  pure
    if n < 0
      then Left (Tb_error n)
      else Right (cintToInt n)

-- | Hide the cursor in the back buffer.
tb_hide_cursor :: IO (Either Tb_error ())
tb_hide_cursor =
  check Termbox.tb_hide_cursor

-- | Initialize the @termbox@ library.
tb_init :: IO (Either Tb_error ())
tb_init =
  check Termbox.tb_init

-- | Initialize the @termbox@ library.
--
-- > tb_init = tb_init_fd(0)
tb_init_fd :: Fd -> IO (Either Tb_error ())
tb_init_fd (Fd fd) =
  check (Termbox.tb_init_fd fd)

-- | Initialize the @termbox@ library.
--
-- > tb_init = tb_init_file("/dev/tty")
tb_init_file :: FilePath -> IO (Either Tb_error ())
tb_init_file path =
  withCString path \c_path ->
    check (Termbox.tb_init_file c_path)

-- | Initialize the @termbox@ library.
--
-- > tb_init = tb_init_rwfd(0, 0)
tb_init_rwfd :: Fd -> Fd -> IO (Either Tb_error ())
tb_init_rwfd (Fd r) (Fd w) =
  check (Termbox.tb_init_rwfd r w)

-- | Invalidate the screen, causing a redraw. This is mainly used after switching output modes.
tb_invalidate :: IO (Either Tb_error ())
tb_invalidate =
  check Termbox.tb_invalidate

-- | Get the last error this library observed, in certain cases.
tb_last_errno :: IO Errno
tb_last_errno =
  coerce Termbox.tb_last_errno

-- | Wait up to a number of milliseconds for an event.
tb_peek_event :: Int -> IO (Either Tb_error Tb_event)
tb_peek_event ms =
  alloca \eventPtr -> do
    code <- Termbox.tb_peek_event eventPtr (intToCInt ms)
    if code == Termbox._TB_OK
      then do
        event <- Storable.peek eventPtr
        pure (Right (makeEvent event))
      else pure (Left (Tb_error code))

-- | Wait for an event.
tb_poll_event :: IO (Either Tb_error Tb_event)
tb_poll_event =
  alloca \eventPtr -> do
    code <- Termbox.tb_poll_event eventPtr
    if code == Termbox._TB_OK
      then do
        event <- Storable.peek eventPtr
        pure (Right (makeEvent event))
      else pure (Left (Tb_error code))

-- | Synchronize the back buffer with the terminal.
tb_present :: IO (Either Tb_error ())
tb_present =
  check Termbox.tb_present

-- | Print a string to the back buffer.
-- tb_print :: CInt -> CInt -> Word64 -> Word64 -> CString -> IO CInt
tb_print ::
  -- | x
  Int ->
  -- | y
  Int ->
  -- | fg
  Tb_attr ->
  -- | fg
  Tb_attr ->
  -- | str
  Text ->
  IO (Either Tb_error ())
tb_print x y (Tb_attr fg) (Tb_attr bg) str =
  Text.withCString str \cstr ->
    check (Termbox.tb_print (intToCInt x) (intToCInt y) fg bg cstr)

-- | Print a string to the back buffer and return its width.
-- tb_print_ex :: CInt -> CInt -> Word64 -> Word64 -> Ptr CSize -> CString -> IO CInt
tb_print_ex ::
  -- | x
  Int ->
  -- | y
  Int ->
  -- | fg
  Tb_attr ->
  -- | fg
  Tb_attr ->
  -- | str
  Text ->
  IO (Either Tb_error Int)
tb_print_ex x y (Tb_attr fg) (Tb_attr bg) str =
  alloca \sizePtr ->
    Text.withCString str \cstr -> do
      code <- Termbox.tb_print_ex (intToCInt x) (intToCInt y) fg bg sizePtr cstr
      if code == Termbox._TB_OK
        then do
          size <- Storable.peek sizePtr
          pure (Right (csizeToInt size))
        else pure (Left (Tb_error code))

-- | Send raw bytes to the terminal.
tb_send :: ByteString -> IO (Either Tb_error ())
tb_send bytes =
  ByteString.unsafeUseAsCStringLen bytes \(cstr, len) ->
    check (Termbox.tb_send cstr (intToCSize len))

-- | Set a cell value in the back buffer.
tb_set_cell ::
  -- | x
  Int ->
  -- | y
  Int ->
  -- | ch
  Char ->
  -- | fg
  Tb_attr ->
  -- | bg
  Tb_attr ->
  IO (Either Tb_error ())
tb_set_cell x y ch (Tb_attr fg) (Tb_attr bg) =
  check (Termbox.tb_set_cell (intToCInt x) (intToCInt y) (charToWord32 ch) fg bg)

-- | Set a cell value in the back buffer.
tb_set_cell_ex ::
  -- | x
  Int ->
  -- | y
  Int ->
  -- | chs
  [Char] ->
  -- | fg
  Tb_attr ->
  -- | bg
  Tb_attr ->
  IO (Either Tb_error ())
tb_set_cell_ex x y chs (Tb_attr fg) (Tb_attr bg) =
  withArrayLen (map charToWord32 chs) \len cchs ->
    check (Termbox.tb_set_cell_ex (intToCInt x) (intToCInt y) cchs (intToCSize len) fg bg)

-- | Set the foreground and background attributes that @tb_clear@ clears the back buffer with.
tb_set_clear_attrs ::
  -- | fg
  Tb_attr ->
  -- | bg
  Tb_attr ->
  IO (Either Tb_error ())
tb_set_clear_attrs (Tb_attr fg) (Tb_attr bg) =
  check (Termbox.tb_set_clear_attrs fg bg)

-- | Set the cursor in the back buffer.
tb_set_cursor ::
  -- | x
  Int ->
  -- | y
  Int ->
  IO (Either Tb_error ())
tb_set_cursor x y =
  check (Termbox.tb_set_cursor (intToCInt x) (intToCInt y))

-- | Set the input mode.
tb_set_input_mode :: Tb_input_mode -> IO (Either Tb_error ())
tb_set_input_mode (Tb_input_mode mode) =
  check (Termbox.tb_set_input_mode mode)

-- | Get the output mode.
tb_set_output_mode :: Tb_output_mode -> IO (Either Tb_error ())
tb_set_output_mode (Tb_output_mode mode) =
  check (Termbox.tb_set_output_mode mode)

-- | Shutdown the @termbox@ library.
tb_shutdown :: IO (Either Tb_error ())
tb_shutdown =
  check Termbox.tb_shutdown

-- | Convert an error code to a string.
tb_strerror :: Tb_error -> IO Text
tb_strerror (Tb_error code) = do
  ConstPtr c_string <- Termbox.tb_strerror code
  len <- lengthArray0 0 c_string
  Text.peekCStringLen (c_string, len)

-- | Get the terminal width.
tb_width :: IO (Either Tb_error Int)
tb_width = do
  n <- Termbox.tb_width
  pure
    if n < 0
      then Left (Tb_error n)
      else Right (cintToInt n)

------------------------------------------------------------------------------------------------------------------------

check :: IO CInt -> IO (Either Tb_error ())
check action = do
  code <- action
  pure
    if code == Termbox._TB_OK
      then Right ()
      else Left (Tb_error code)
{-# INLINE check #-}

module Termbox2.Bindings.C.Internal.Functions
  ( tb_cell_buffer,
    tb_clear,
    tb_extend_cell,
    tb_get_fds,
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
    tb_set_func,
    tb_set_input_mode,
    tb_set_output_mode,
    tb_shutdown,
    tb_strerror,
    tb_width,
  )
where

import Data.Word (Word32, Word64)
import Foreign.C (CChar (..), CInt (..), CSize (..), CString)
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.Ptr (FunPtr, Ptr)
import Termbox2.Bindings.C.Internal.Cell (Tb_cell)
import Termbox2.Bindings.C.Internal.Event (Tb_event)

-- | Get a pointer to the back buffer.
foreign import capi unsafe "termbox2.h tb_cell_buffer"
  tb_cell_buffer :: IO (Ptr Tb_cell)

-- | Clear the back buffer.
foreign import capi unsafe "termbox2.h tb_clear"
  tb_clear :: IO CInt

-- | Append a code point to a cell in the back buffer.
foreign import capi unsafe "termbox2.h tb_extend_cell"
  tb_extend_cell :: CInt -> CInt -> Word32 -> IO CInt

-- | Get the terminal and resize file descriptors.
foreign import capi unsafe "termbox2.h tb_get_fds"
  tb_get_fds :: Ptr CInt -> Ptr CInt -> IO CInt

-- | Get the terminal height.
foreign import capi unsafe "termbox2.h tb_height"
  tb_height :: IO CInt

-- | Hide the cursor in the back buffer.
foreign import capi unsafe "termbox2.h tb_hide_cursor"
  tb_hide_cursor :: IO CInt

-- | Initialize the @termbox@ library.
foreign import capi unsafe "termbox2.h tb_init"
  tb_init :: IO CInt

-- | Initialize the @termbox@ library.
--
-- > tb_init = tb_init_fd(0)
foreign import capi unsafe "termbox2.h tb_init_fd"
  tb_init_fd :: CInt -> IO CInt

-- | Initialize the @termbox@ library.
--
-- > tb_init = tb_init_file("/dev/tty")
foreign import capi unsafe "termbox2.h tb_init_file"
  tb_init_file :: CString -> IO CInt

-- | Initialize the @termbox@ library.
--
-- > tb_init = tb_init_rwfd(0, 0)
foreign import capi unsafe "termbox2.h tb_init_rwfd"
  tb_init_rwfd :: CInt -> CInt -> IO CInt

-- | Invalidate the screen, causing a redraw. This is mainly used after switching output modes.
foreign import capi unsafe "termbox2.h tb_invalidate"
  tb_invalidate :: IO CInt

-- | Get the last error returned by this library.
foreign import capi unsafe "termbox2.h tb_last_errno"
  tb_last_errno :: IO CInt

-- | Wait up to a number of milliseconds for an event.
foreign import capi interruptible "termbox2.h tb_peek_event"
  tb_peek_event :: Ptr Tb_event -> CInt -> IO CInt

-- | Wait for an event.
foreign import capi interruptible "termbox2.h tb_poll_event"
  tb_poll_event :: Ptr Tb_event -> IO CInt

-- | Synchronize the back buffer with the terminal.
foreign import capi unsafe "termbox2.h tb_present"
  tb_present :: IO CInt

-- | Print a string to the back buffer.
foreign import capi unsafe "termbox2.h tb_print"
  tb_print :: CInt -> CInt -> Word64 -> Word64 -> CString -> IO CInt

-- | Print a string to the back buffer.
foreign import capi unsafe "termbox2.h tb_print_ex"
  tb_print_ex :: CInt -> CInt -> Word64 -> Word64 -> Ptr CSize -> CString -> IO CInt

-- | Send raw bytes to the terminal.
foreign import capi unsafe "termbox2.h tb_send"
  tb_send :: CString -> CSize -> IO CInt

-- | Set a cell value in the back buffer.
foreign import capi unsafe "termbox2.h tb_set_cell"
  tb_set_cell :: CInt -> CInt -> Word32 -> Word64 -> Word64 -> IO CInt

-- | Set a cell value in the back buffer.
foreign import capi unsafe "termbox2.h tb_set_cell_ex"
  tb_set_cell_ex :: CInt -> CInt -> Ptr Word32 -> CSize -> Word64 -> Word64 -> IO CInt

-- | Set the foreground and background attributes that @tb_clear@ clears the back buffer with.
foreign import capi unsafe "termbox2.h tb_set_clear_attrs"
  tb_set_clear_attrs :: Word64 -> Word64 -> IO CInt

-- | Set the cursor in the back buffer.
foreign import capi unsafe "termbox2.h tb_set_cursor"
  tb_set_cursor :: CInt -> CInt -> IO CInt

-- | Set or clear custom escape sequence functions.
foreign import capi unsafe "termbox2.h tb_set_func"
  tb_set_func :: CInt -> FunPtr (Ptr Tb_event -> Ptr CSize -> IO CInt) -> IO CInt

-- | Get or set the input mode.
foreign import capi unsafe "termbox2.h tb_set_input_mode"
  tb_set_input_mode :: CInt -> IO CInt

-- | Get or set the output mode.
foreign import capi unsafe "termbox2.h tb_set_output_mode"
  tb_set_output_mode :: CInt -> IO CInt

-- | Shutdown the @termbox@ library.
foreign import capi unsafe "termbox2.h tb_shutdown"
  tb_shutdown :: IO CInt

-- | Convert an error code to a string.
foreign import capi unsafe "termbox2.h tb_strerror"
  tb_strerror :: CInt -> IO (ConstPtr CChar)

-- | Get the terminal height.
foreign import capi unsafe "termbox2.h tb_width"
  tb_width :: IO CInt

module Termbox2.Bindings.C.Internal.Functions
  ( tb_attr_width,
    tb_cell_buffer,
    tb_clear,
    tb_extend_cell,
    tb_get_fds,
    tb_has_egc,
    tb_has_truecolor,
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
    tb_utf8_char_length,
    tb_utf8_char_to_unicode,
    tb_utf8_unicode_to_char,
    tb_version,
    tb_width,
  )
where

import Data.Word (Word32, Word64)
import Foreign.C (CChar (..), CInt (..), CSize (..), CString)
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.Ptr (FunPtr, Ptr)
import Termbox2.Bindings.C.Internal.Cell (Tb_cell)
import Termbox2.Bindings.C.Internal.Event (Tb_event)

foreign import capi unsafe "termbox2.h tb_attr_width"
  tb_attr_width :: CInt

foreign import capi unsafe "termbox2.h tb_cell_buffer"
  tb_cell_buffer :: IO (Ptr Tb_cell)

foreign import capi unsafe "termbox2.h tb_clear"
  tb_clear :: IO CInt

foreign import capi unsafe "termbox2.h tb_extend_cell"
  tb_extend_cell :: CInt -> CInt -> Word32 -> IO CInt

foreign import capi unsafe "termbox2.h tb_get_fds"
  tb_get_fds :: Ptr CInt -> Ptr CInt -> IO CInt

foreign import capi unsafe "termbox2.h tb_has_egc"
  tb_has_egc :: CInt

foreign import capi unsafe "termbox2.h tb_has_truecolor"
  tb_has_truecolor :: CInt

foreign import capi unsafe "termbox2.h tb_height"
  tb_height :: IO CInt

foreign import capi unsafe "termbox2.h tb_hide_cursor"
  tb_hide_cursor :: IO CInt

foreign import capi unsafe "termbox2.h tb_init"
  tb_init :: IO CInt

foreign import capi unsafe "termbox2.h tb_init_fd"
  tb_init_fd :: CInt -> IO CInt

foreign import capi unsafe "termbox2.h tb_init_file"
  tb_init_file :: CString -> IO CInt

foreign import capi unsafe "termbox2.h tb_init_rwfd"
  tb_init_rwfd :: CInt -> CInt -> IO CInt

foreign import capi unsafe "termbox2.h tb_invalidate"
  tb_invalidate :: IO CInt

foreign import capi unsafe "termbox2.h tb_last_errno"
  tb_last_errno :: IO CInt

foreign import capi interruptible "termbox2.h tb_peek_event"
  tb_peek_event :: Ptr Tb_event -> CInt -> IO CInt

foreign import capi interruptible "termbox2.h tb_poll_event"
  tb_poll_event :: Ptr Tb_event -> IO CInt

foreign import capi unsafe "termbox2.h tb_present"
  tb_present :: IO CInt

foreign import capi unsafe "termbox2.h tb_print"
  tb_print :: CInt -> CInt -> Word64 -> Word64 -> CString -> IO CInt

foreign import capi unsafe "termbox2.h tb_print_ex"
  tb_print_ex :: CInt -> CInt -> Word64 -> Word64 -> Ptr CSize -> CString -> IO CInt

foreign import capi unsafe "termbox2.h tb_send"
  tb_send :: CString -> CSize -> IO CInt

foreign import capi unsafe "termbox2.h tb_set_cell"
  tb_set_cell :: CInt -> CInt -> Word32 -> Word64 -> Word64 -> IO CInt

foreign import capi unsafe "termbox2.h tb_set_cell_ex"
  tb_set_cell_ex :: CInt -> CInt -> Ptr Word32 -> CSize -> Word64 -> Word64 -> IO CInt

foreign import capi unsafe "termbox2.h tb_set_clear_attrs"
  tb_set_clear_attrs :: Word64 -> Word64 -> IO CInt

foreign import capi unsafe "termbox2.h tb_set_cursor"
  tb_set_cursor :: CInt -> CInt -> IO CInt

foreign import capi unsafe "termbox2.h tb_set_func"
  tb_set_func :: CInt -> FunPtr (Ptr Tb_event -> Ptr CSize -> IO CInt) -> IO CInt

foreign import capi unsafe "termbox2.h tb_set_input_mode"
  tb_set_input_mode :: CInt -> IO CInt

foreign import capi unsafe "termbox2.h tb_set_output_mode"
  tb_set_output_mode :: CInt -> IO CInt

foreign import capi unsafe "termbox2.h tb_shutdown"
  tb_shutdown :: IO CInt

foreign import capi unsafe "termbox2.h tb_strerror"
  tb_strerror :: CInt -> IO (ConstPtr CChar)

foreign import capi unsafe "termbox2.h tb_utf8_char_length"
  tb_utf8_char_length :: CChar -> IO CInt

foreign import capi unsafe "termbox2.h tb_utf8_char_to_unicode"
  tb_utf8_char_to_unicode :: Ptr Word32 -> ConstPtr CChar -> IO CInt

foreign import capi unsafe "termbox2.h tb_utf8_unicode_to_char"
  tb_utf8_unicode_to_char :: Ptr CChar -> Word32 -> IO CInt

foreign import capi unsafe "termbox2.h tb_version"
  tb_version :: ConstPtr CChar

foreign import capi unsafe "termbox2.h tb_width"
  tb_width :: IO CInt

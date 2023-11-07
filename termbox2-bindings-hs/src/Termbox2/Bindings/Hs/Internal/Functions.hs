module Termbox2.Bindings.Hs.Internal.Functions
  ( -- tb_attr_width,
    -- tb_cell_buffer,
    -- tb_clear,
    -- tb_extend_cell,
    -- tb_get_fds,
    -- tb_has_egc,
    -- tb_has_truecolor,
    -- tb_height,
    -- tb_hide_cursor,
    tb_init,
    -- tb_init_fd,
    -- tb_init_file,
    -- tb_init_rwfd,
    -- tb_invalidate,
    -- tb_last_errno,
    -- tb_peek_event,
    -- tb_poll_event,
    -- tb_present,
    -- tb_print,
    -- tb_print_ex,
    -- tb_send,
    -- tb_set_cell,
    -- tb_set_cell_ex,
    -- tb_set_clear_attrs,
    -- tb_set_cursor,
    -- tb_set_func,
    -- tb_set_input_mode,
    -- tb_set_output_mode,
    -- tb_shutdown,
    -- tb_strerror,
    -- tb_version,
    -- tb_width,
  )
where

import Termbox2.Bindings.C qualified as Termbox
import Termbox2.Bindings.Hs.Internal.Error (Tb_error (Tb_error))

-- | Initialize the @termbox@ library.
tb_init :: IO (Either Tb_error ())
tb_init = do
  code <- Termbox.tb_init
  pure
    if code == Termbox._TB_OK
      then Right ()
      else Left (Tb_error code)

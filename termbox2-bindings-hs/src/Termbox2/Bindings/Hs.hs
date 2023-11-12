module Termbox2.Bindings.Hs
  ( -- * Functions

    -- ** Initialize / shutdown
    tb_init,
    tb_init_fd,
    tb_init_file,
    tb_init_rwfd,
    tb_shutdown,

    -- ** Get and set the input mode
    tb_get_input_mode,
    tb_set_input_mode,

    -- ** Get and set the output mode
    tb_get_output_mode,
    tb_set_output_mode,

    -- ** Get terminal dimensions
    tb_width,
    tb_height,

    -- ** Poll for events
    tb_peek_event,
    tb_poll_event,
    tb_event_mod_has,
    tb_get_fds,

    -- ** Set cell contents
    tb_set_cell,
    tb_set_cell_ex,
    tb_extend_cell,
    tb_print,
    tb_print_ex,

    -- ** Set and hide the cursor
    tb_set_cursor,
    tb_hide_cursor,

    -- ** Send raw bytes
    tb_send,

    -- ** Clear and synchronize the back buffer
    tb_clear,
    tb_set_clear_attrs,
    tb_present,
    tb_invalidate,

    -- ** Custom escape sequences
    tb_set_func,

    -- ** Error utils
    tb_last_errno,
    tb_strerror,

    -- * Types
    Tb_attr
      ( Tb_attr,
        TB_BLINK,
        TB_BOLD,
        TB_BRIGHT,
        TB_DIM,
        TB_INVISIBLE,
        TB_ITALIC,
        TB_OVERLINE,
        TB_REVERSE,
        TB_STRIKEOUT,
        TB_UNDERLINE,
        TB_UNDERLINE_2
      ),
    Tb_error
      ( Tb_error,
        TB_ERR,
        TB_ERR_CAP_COLLISION,
        TB_ERR_INIT_ALREADY,
        TB_ERR_INIT_OPEN,
        TB_ERR_MEM,
        TB_ERR_NEED_MORE,
        TB_ERR_NOT_INIT,
        TB_ERR_NO_EVENT,
        TB_ERR_NO_TERM,
        TB_ERR_OUT_OF_BOUNDS,
        TB_ERR_POLL,
        TB_ERR_READ,
        TB_ERR_RESIZE_IOCTL,
        TB_ERR_RESIZE_PIPE,
        TB_ERR_RESIZE_POLL,
        TB_ERR_RESIZE_READ,
        TB_ERR_RESIZE_SIGACTION,
        TB_ERR_RESIZE_SSCANF,
        TB_ERR_RESIZE_WRITE,
        TB_ERR_TCGETATTR,
        TB_ERR_TCSETATTR,
        TB_ERR_UNSUPPORTED_TERM
      ),
    Tb_event (..),
    Tb_event_mod,
    _TB_MOD_ALT,
    _TB_MOD_CTRL,
    _TB_MOD_SHIFT,
    _TB_MOD_MOTION,
    Tb_event_type
      ( TB_EVENT_KEY,
        TB_EVENT_MOUSE,
        TB_EVENT_RESIZE
      ),
    Tb_input_mode
      ( Tb_input_mode,
        TB_INPUT_ALT,
        TB_INPUT_ESC,
        TB_INPUT_MOUSE
      ),
    Tb_key
      ( Tb_key,
        TB_KEY_ARROW_DOWN,
        TB_KEY_ARROW_LEFT,
        TB_KEY_ARROW_RIGHT,
        TB_KEY_ARROW_UP,
        TB_KEY_BACKSPACE,
        TB_KEY_BACKSPACE2,
        TB_KEY_BACK_TAB,
        TB_KEY_CTRL_2,
        TB_KEY_CTRL_3,
        TB_KEY_CTRL_4,
        TB_KEY_CTRL_5,
        TB_KEY_CTRL_6,
        TB_KEY_CTRL_7,
        TB_KEY_CTRL_8,
        TB_KEY_CTRL_A,
        TB_KEY_CTRL_B,
        TB_KEY_CTRL_BACKSLASH,
        TB_KEY_CTRL_C,
        TB_KEY_CTRL_D,
        TB_KEY_CTRL_E,
        TB_KEY_CTRL_F,
        TB_KEY_CTRL_G,
        TB_KEY_CTRL_H,
        TB_KEY_CTRL_I,
        TB_KEY_CTRL_J,
        TB_KEY_CTRL_K,
        TB_KEY_CTRL_L,
        TB_KEY_CTRL_LSQ_BRACKET,
        TB_KEY_CTRL_M,
        TB_KEY_CTRL_N,
        TB_KEY_CTRL_O,
        TB_KEY_CTRL_P,
        TB_KEY_CTRL_Q,
        TB_KEY_CTRL_R,
        TB_KEY_CTRL_RSQ_BRACKET,
        TB_KEY_CTRL_S,
        TB_KEY_CTRL_SLASH,
        TB_KEY_CTRL_T,
        TB_KEY_CTRL_TILDE,
        TB_KEY_CTRL_U,
        TB_KEY_CTRL_UNDERSCORE,
        TB_KEY_CTRL_V,
        TB_KEY_CTRL_W,
        TB_KEY_CTRL_X,
        TB_KEY_CTRL_Y,
        TB_KEY_CTRL_Z,
        TB_KEY_DELETE,
        TB_KEY_END,
        TB_KEY_ENTER,
        TB_KEY_ESC,
        TB_KEY_F1,
        TB_KEY_F10,
        TB_KEY_F11,
        TB_KEY_F12,
        TB_KEY_F2,
        TB_KEY_F3,
        TB_KEY_F4,
        TB_KEY_F5,
        TB_KEY_F6,
        TB_KEY_F7,
        TB_KEY_F8,
        TB_KEY_F9,
        TB_KEY_HOME,
        TB_KEY_INSERT,
        TB_KEY_MOUSE_LEFT,
        TB_KEY_MOUSE_MIDDLE,
        TB_KEY_MOUSE_RELEASE,
        TB_KEY_MOUSE_RIGHT,
        TB_KEY_MOUSE_WHEEL_DOWN,
        TB_KEY_MOUSE_WHEEL_UP,
        TB_KEY_PGDN,
        TB_KEY_PGUP,
        TB_KEY_SPACE,
        TB_KEY_TAB
      ),
    Tb_output_mode
      ( TB_OUTPUT_216,
        TB_OUTPUT_256,
        TB_OUTPUT_GRAYSCALE,
        TB_OUTPUT_NORMAL,
        TB_OUTPUT_TRUECOLOR
      ),

    -- ** Colors

    -- _TB_DEFAULT,
    -- _TB_BLACK,
    -- _TB_HI_BLACK,
    -- _TB_BLUE,
    -- _TB_CYAN,
    -- _TB_GREEN,
    -- _TB_MAGENTA,
    -- _TB_RED,
    -- _TB_WHITE,
    -- _TB_YELLOW,

    -- ** Attributes

    -- _TB_BLINK,
    -- _TB_BOLD,
    -- _TB_BRIGHT,
    -- _TB_DIM,
    -- _TB_INVISIBLE,
    -- _TB_ITALIC,
    -- _TB_OVERLINE,
    -- _TB_REVERSE,
    -- _TB_STRIKEOUT,
    -- _TB_UNDERLINE,
    -- _TB_UNDERLINE_2,

    -- ** Function types
  )
where

-- _TB_FUNC_EXTRACT_PRE,
-- _TB_FUNC_EXTRACT_POST,

import Termbox2.Bindings.Hs.Internal.Attr (Tb_attr (..))
import Termbox2.Bindings.Hs.Internal.Error (Tb_error (..))
import Termbox2.Bindings.Hs.Internal.Event (Tb_event (..))
import Termbox2.Bindings.Hs.Internal.EventMod (Tb_event_mod, tb_event_mod_has, _TB_MOD_ALT, _TB_MOD_CTRL, _TB_MOD_MOTION, _TB_MOD_SHIFT)
import Termbox2.Bindings.Hs.Internal.EventType (Tb_event_type (..))
import Termbox2.Bindings.Hs.Internal.Functions
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
    tb_set_func,
    tb_set_input_mode,
    tb_set_output_mode,
    tb_shutdown,
    tb_strerror,
    tb_width,
  )
import Termbox2.Bindings.Hs.Internal.InputMode (Tb_input_mode (..))
import Termbox2.Bindings.Hs.Internal.Key (Tb_key (..))
import Termbox2.Bindings.Hs.Internal.OutputMode (Tb_output_mode (..))

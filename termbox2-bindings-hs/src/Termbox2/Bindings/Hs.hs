module Termbox2.Bindings.Hs
  ( -- * Functions

    -- ** Initialize / shutdown

    -- tb_init,
    -- tb_init_fd,
    -- tb_init_file,
    -- tb_init_rwfd,
    -- tb_shutdown,

    -- ** Get\/set the input\/output mode

    -- tb_set_input_mode,
    -- tb_set_output_mode,

    -- ** Get terminal dimensions

    -- tb_width,
    -- tb_height,

    -- ** Poll for events

    -- tb_peek_event,
    -- tb_poll_event,
    -- tb_get_fds,

    -- ** Set cell contents

    -- tb_set_cell,
    -- tb_set_cell_ex,
    -- tb_extend_cell,
    -- tb_print,
    -- tb_print_ex,
    -- tb_cell_buffer,

    -- ** Set\/hide the cursor

    -- tb_set_cursor,
    -- tb_hide_cursor,

    -- ** Send raw bytes

    -- tb_send,

    -- ** Clear and synchronize the back buffer

    -- tb_clear,
    -- tb_set_clear_attrs,
    -- tb_present,
    -- tb_invalidate,

    -- ** Custom escape sequences

    -- tb_set_func,

    -- ** Error utils

    -- tb_last_errno,
    -- tb_strerror,

    -- ** Query compile-time constants

    -- tb_attr_width,
    -- tb_has_egc,
    -- tb_has_truecolor,
    -- tb_version,

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
    -- Tb_event (..),
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

    -- ** Event types

    -- _TB_EVENT_KEY,
    -- _TB_EVENT_RESIZE,
    -- _TB_EVENT_MOUSE,

    -- ** Key modifiers

    -- _TB_MOD_ALT,
    -- _TB_MOD_CTRL,
    -- _TB_MOD_MOTION,
    -- _TB_MOD_SHIFT,

    -- ** Input modes

    -- _TB_INPUT_CURRENT,
    -- _TB_INPUT_ALT,
    -- _TB_INPUT_ESC,
    -- _TB_INPUT_MOUSE,

    -- ** Output modes

    -- _TB_OUTPUT_CURRENT,
    -- _TB_OUTPUT_216,
    -- _TB_OUTPUT_256,
    -- _TB_OUTPUT_GRAYSCALE,
    -- _TB_OUTPUT_NORMAL,
    -- _TB_OUTPUT_TRUECOLOR,

    -- ** Result codes

    -- _TB_OK,

    -- *** Error codes

    -- _TB_ERR,
    -- _TB_ERR_CAP_COLLISION,
    -- _TB_ERR_INIT_ALREADY,
    -- _TB_ERR_INIT_OPEN,
    -- _TB_ERR_MEM,
    -- _TB_ERR_NEED_MORE,
    -- _TB_ERR_NOT_INIT,
    -- _TB_ERR_NO_EVENT,
    -- _TB_ERR_NO_TERM,
    -- _TB_ERR_OUT_OF_BOUNDS,
    -- _TB_ERR_POLL,
    -- _TB_ERR_READ,
    -- _TB_ERR_RESIZE_IOCTL,
    -- _TB_ERR_RESIZE_PIPE,
    -- _TB_ERR_RESIZE_POLL,
    -- _TB_ERR_RESIZE_READ,
    -- _TB_ERR_RESIZE_SIGACTION,
    -- _TB_ERR_RESIZE_SSCANF,
    -- _TB_ERR_RESIZE_WRITE,
    -- _TB_ERR_TCGETATTR,
    -- _TB_ERR_TCSETATTR,
    -- _TB_ERR_UNSUPPORTED_TERM,

    -- ** Function types
  )
where

-- _TB_FUNC_EXTRACT_PRE,
-- _TB_FUNC_EXTRACT_POST,

import Termbox2.Bindings.Hs.Internal.Attr (Tb_attr (..))
import Termbox2.Bindings.Hs.Internal.Key (Tb_key (..))

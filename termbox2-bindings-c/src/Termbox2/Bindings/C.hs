module Termbox2.Bindings.C
  ( -- * Objects
    Tb_cell (..),
    Tb_event (..),

    -- * Constants

    -- ** Keys
    _TB_KEY_ARROW_DOWN,
    _TB_KEY_ARROW_LEFT,
    _TB_KEY_ARROW_RIGHT,
    _TB_KEY_ARROW_UP,
    _TB_KEY_BACKSPACE,
    _TB_KEY_BACKSPACE2,
    _TB_KEY_BACK_TAB,
    _TB_KEY_CTRL_2,
    _TB_KEY_CTRL_3,
    _TB_KEY_CTRL_4,
    _TB_KEY_CTRL_5,
    _TB_KEY_CTRL_6,
    _TB_KEY_CTRL_7,
    _TB_KEY_CTRL_8,
    _TB_KEY_CTRL_A,
    _TB_KEY_CTRL_B,
    _TB_KEY_CTRL_BACKSLASH,
    _TB_KEY_CTRL_C,
    _TB_KEY_CTRL_D,
    _TB_KEY_CTRL_E,
    _TB_KEY_CTRL_F,
    _TB_KEY_CTRL_G,
    _TB_KEY_CTRL_H,
    _TB_KEY_CTRL_I,
    _TB_KEY_CTRL_J,
    _TB_KEY_CTRL_K,
    _TB_KEY_CTRL_L,
    _TB_KEY_CTRL_LSQ_BRACKET,
    _TB_KEY_CTRL_M,
    _TB_KEY_CTRL_N,
    _TB_KEY_CTRL_O,
    _TB_KEY_CTRL_P,
    _TB_KEY_CTRL_Q,
    _TB_KEY_CTRL_R,
    _TB_KEY_CTRL_RSQ_BRACKET,
    _TB_KEY_CTRL_S,
    _TB_KEY_CTRL_SLASH,
    _TB_KEY_CTRL_T,
    _TB_KEY_CTRL_TILDE,
    _TB_KEY_CTRL_U,
    _TB_KEY_CTRL_UNDERSCORE,
    _TB_KEY_CTRL_V,
    _TB_KEY_CTRL_W,
    _TB_KEY_CTRL_X,
    _TB_KEY_CTRL_Y,
    _TB_KEY_CTRL_Z,
    _TB_KEY_DELETE,
    _TB_KEY_END,
    _TB_KEY_ENTER,
    _TB_KEY_ESC,
    _TB_KEY_F1,
    _TB_KEY_F10,
    _TB_KEY_F11,
    _TB_KEY_F12,
    _TB_KEY_F2,
    _TB_KEY_F3,
    _TB_KEY_F4,
    _TB_KEY_F5,
    _TB_KEY_F6,
    _TB_KEY_F7,
    _TB_KEY_F8,
    _TB_KEY_F9,
    _TB_KEY_HOME,
    _TB_KEY_INSERT,
    _TB_KEY_MOUSE_LEFT,
    _TB_KEY_MOUSE_MIDDLE,
    _TB_KEY_MOUSE_RELEASE,
    _TB_KEY_MOUSE_RIGHT,
    _TB_KEY_MOUSE_WHEEL_DOWN,
    _TB_KEY_MOUSE_WHEEL_UP,
    _TB_KEY_PGDN,
    _TB_KEY_PGUP,
    _TB_KEY_SPACE,
    _TB_KEY_TAB,

    -- ** Colors
    _TB_DEFAULT,
    _TB_BLACK,
    _TB_HI_BLACK,
    _TB_BLUE,
    _TB_CYAN,
    _TB_GREEN,
    _TB_MAGENTA,
    _TB_RED,
    _TB_WHITE,
    _TB_YELLOW,

    -- ** Attributes
    _TB_BLINK,
    _TB_BOLD,
    _TB_BRIGHT,
    _TB_DIM,
    _TB_INVISIBLE,
    _TB_ITALIC,
    _TB_OVERLINE,
    _TB_REVERSE,
    _TB_STRIKEOUT,
    _TB_UNDERLINE,
    _TB_UNDERLINE_2,

    -- ** Event types
    _TB_EVENT_KEY,
    _TB_EVENT_RESIZE,
    _TB_EVENT_MOUSE,

    -- ** Key modifiers
    _TB_MOD_ALT,
    _TB_MOD_CTRL,
    _TB_MOD_MOTION,
    _TB_MOD_SHIFT,

    -- ** Input modes
    _TB_INPUT_CURRENT,
    _TB_INPUT_ALT,
    _TB_INPUT_ESC,
    _TB_INPUT_MOUSE,

    -- ** Output modes
    _TB_OUTPUT_CURRENT,
    _TB_OUTPUT_216,
    _TB_OUTPUT_256,
    _TB_OUTPUT_GRAYSCALE,
    _TB_OUTPUT_NORMAL,
    _TB_OUTPUT_TRUECOLOR,

    -- ** Result codes
    _TB_OK,

    -- *** Error codes
    _TB_ERR,
    _TB_ERR_CAP_COLLISION,
    _TB_ERR_INIT_ALREADY,
    _TB_ERR_INIT_OPEN,
    _TB_ERR_MEM,
    _TB_ERR_NEED_MORE,
    _TB_ERR_NOT_INIT,
    _TB_ERR_NO_EVENT,
    _TB_ERR_NO_TERM,
    _TB_ERR_OUT_OF_BOUNDS,
    _TB_ERR_POLL,
    _TB_ERR_READ,
    _TB_ERR_RESIZE_IOCTL,
    _TB_ERR_RESIZE_PIPE,
    _TB_ERR_RESIZE_POLL,
    _TB_ERR_RESIZE_READ,
    _TB_ERR_RESIZE_SIGACTION,
    _TB_ERR_RESIZE_SSCANF,
    _TB_ERR_RESIZE_WRITE,
    _TB_ERR_TCGETATTR,
    _TB_ERR_TCSETATTR,
    _TB_ERR_UNSUPPORTED_TERM,

    -- ** Function types
    _TB_FUNC_EXTRACT_PRE,
    _TB_FUNC_EXTRACT_POST,

    -- ** Buffer sizes
    _TB_OPT_PRINTF_BUF,
    _TB_OPT_READ_BUF,
  )
where

import Termbox2.Bindings.C.Internal.Cell (Tb_cell (..))
import Termbox2.Bindings.C.Internal.Constants
  ( _TB_BLACK,
    _TB_BLINK,
    _TB_BLUE,
    _TB_BOLD,
    _TB_BRIGHT,
    _TB_CYAN,
    _TB_DEFAULT,
    _TB_DIM,
    _TB_ERR,
    _TB_ERR_CAP_COLLISION,
    _TB_ERR_INIT_ALREADY,
    _TB_ERR_INIT_OPEN,
    _TB_ERR_MEM,
    _TB_ERR_NEED_MORE,
    _TB_ERR_NOT_INIT,
    _TB_ERR_NO_EVENT,
    _TB_ERR_NO_TERM,
    _TB_ERR_OUT_OF_BOUNDS,
    _TB_ERR_POLL,
    _TB_ERR_READ,
    _TB_ERR_RESIZE_IOCTL,
    _TB_ERR_RESIZE_PIPE,
    _TB_ERR_RESIZE_POLL,
    _TB_ERR_RESIZE_READ,
    _TB_ERR_RESIZE_SIGACTION,
    _TB_ERR_RESIZE_SSCANF,
    _TB_ERR_RESIZE_WRITE,
    _TB_ERR_TCGETATTR,
    _TB_ERR_TCSETATTR,
    _TB_ERR_UNSUPPORTED_TERM,
    _TB_EVENT_KEY,
    _TB_EVENT_MOUSE,
    _TB_EVENT_RESIZE,
    _TB_FUNC_EXTRACT_POST,
    _TB_FUNC_EXTRACT_PRE,
    _TB_GREEN,
    _TB_HI_BLACK,
    _TB_INPUT_ALT,
    _TB_INPUT_CURRENT,
    _TB_INPUT_ESC,
    _TB_INPUT_MOUSE,
    _TB_INVISIBLE,
    _TB_ITALIC,
    _TB_KEY_ARROW_DOWN,
    _TB_KEY_ARROW_LEFT,
    _TB_KEY_ARROW_RIGHT,
    _TB_KEY_ARROW_UP,
    _TB_KEY_BACKSPACE,
    _TB_KEY_BACKSPACE2,
    _TB_KEY_BACK_TAB,
    _TB_KEY_CTRL_2,
    _TB_KEY_CTRL_3,
    _TB_KEY_CTRL_4,
    _TB_KEY_CTRL_5,
    _TB_KEY_CTRL_6,
    _TB_KEY_CTRL_7,
    _TB_KEY_CTRL_8,
    _TB_KEY_CTRL_A,
    _TB_KEY_CTRL_B,
    _TB_KEY_CTRL_BACKSLASH,
    _TB_KEY_CTRL_C,
    _TB_KEY_CTRL_D,
    _TB_KEY_CTRL_E,
    _TB_KEY_CTRL_F,
    _TB_KEY_CTRL_G,
    _TB_KEY_CTRL_H,
    _TB_KEY_CTRL_I,
    _TB_KEY_CTRL_J,
    _TB_KEY_CTRL_K,
    _TB_KEY_CTRL_L,
    _TB_KEY_CTRL_LSQ_BRACKET,
    _TB_KEY_CTRL_M,
    _TB_KEY_CTRL_N,
    _TB_KEY_CTRL_O,
    _TB_KEY_CTRL_P,
    _TB_KEY_CTRL_Q,
    _TB_KEY_CTRL_R,
    _TB_KEY_CTRL_RSQ_BRACKET,
    _TB_KEY_CTRL_S,
    _TB_KEY_CTRL_SLASH,
    _TB_KEY_CTRL_T,
    _TB_KEY_CTRL_TILDE,
    _TB_KEY_CTRL_U,
    _TB_KEY_CTRL_UNDERSCORE,
    _TB_KEY_CTRL_V,
    _TB_KEY_CTRL_W,
    _TB_KEY_CTRL_X,
    _TB_KEY_CTRL_Y,
    _TB_KEY_CTRL_Z,
    _TB_KEY_DELETE,
    _TB_KEY_END,
    _TB_KEY_ENTER,
    _TB_KEY_ESC,
    _TB_KEY_F1,
    _TB_KEY_F10,
    _TB_KEY_F11,
    _TB_KEY_F12,
    _TB_KEY_F2,
    _TB_KEY_F3,
    _TB_KEY_F4,
    _TB_KEY_F5,
    _TB_KEY_F6,
    _TB_KEY_F7,
    _TB_KEY_F8,
    _TB_KEY_F9,
    _TB_KEY_HOME,
    _TB_KEY_INSERT,
    _TB_KEY_MOUSE_LEFT,
    _TB_KEY_MOUSE_MIDDLE,
    _TB_KEY_MOUSE_RELEASE,
    _TB_KEY_MOUSE_RIGHT,
    _TB_KEY_MOUSE_WHEEL_DOWN,
    _TB_KEY_MOUSE_WHEEL_UP,
    _TB_KEY_PGDN,
    _TB_KEY_PGUP,
    _TB_KEY_SPACE,
    _TB_KEY_TAB,
    _TB_MAGENTA,
    _TB_MOD_ALT,
    _TB_MOD_CTRL,
    _TB_MOD_MOTION,
    _TB_MOD_SHIFT,
    _TB_OK,
    _TB_OPT_PRINTF_BUF,
    _TB_OPT_READ_BUF,
    _TB_OUTPUT_216,
    _TB_OUTPUT_256,
    _TB_OUTPUT_CURRENT,
    _TB_OUTPUT_GRAYSCALE,
    _TB_OUTPUT_NORMAL,
    _TB_OUTPUT_TRUECOLOR,
    _TB_OVERLINE,
    _TB_RED,
    _TB_REVERSE,
    _TB_STRIKEOUT,
    _TB_UNDERLINE,
    _TB_UNDERLINE_2,
    _TB_WHITE,
    _TB_YELLOW,
  )
import Termbox2.Bindings.C.Internal.Event (Tb_event (..))


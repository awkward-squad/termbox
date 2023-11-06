module Termbox2.Bindings.C.Internal.Constants
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
where

import Data.Word (Word16, Word64, Word8)
import Foreign.C (CInt (..))

foreign import capi "termbox2.h value TB_BLACK" _TB_BLACK :: Word64

foreign import capi "termbox2.h value TB_BLINK" _TB_BLINK :: Word64

foreign import capi "termbox2.h value TB_BLUE" _TB_BLUE :: Word64

foreign import capi "termbox2.h value TB_BOLD" _TB_BOLD :: Word64

foreign import capi "termbox2.h value TB_BRIGHT" _TB_BRIGHT :: Word64

foreign import capi "termbox2.h value TB_CYAN" _TB_CYAN :: Word64

foreign import capi "termbox2.h value TB_DEFAULT" _TB_DEFAULT :: Word64

foreign import capi "termbox2.h value TB_DIM" _TB_DIM :: Word64

foreign import capi "termbox2.h value TB_ERR" _TB_ERR :: CInt

foreign import capi "termbox2.h value TB_ERR_CAP_COLLISION" _TB_ERR_CAP_COLLISION :: CInt

foreign import capi "termbox2.h value TB_ERR_INIT_ALREADY" _TB_ERR_INIT_ALREADY :: CInt

foreign import capi "termbox2.h value TB_ERR_INIT_OPEN" _TB_ERR_INIT_OPEN :: CInt

foreign import capi "termbox2.h value TB_ERR_MEM" _TB_ERR_MEM :: CInt

foreign import capi "termbox2.h value TB_ERR_NEED_MORE" _TB_ERR_NEED_MORE :: CInt

foreign import capi "termbox2.h value TB_ERR_NOT_INIT" _TB_ERR_NOT_INIT :: CInt

foreign import capi "termbox2.h value TB_ERR_NO_EVENT" _TB_ERR_NO_EVENT :: CInt

foreign import capi "termbox2.h value TB_ERR_NO_TERM" _TB_ERR_NO_TERM :: CInt

foreign import capi "termbox2.h value TB_ERR_OUT_OF_BOUNDS" _TB_ERR_OUT_OF_BOUNDS :: CInt

foreign import capi "termbox2.h value TB_ERR_POLL" _TB_ERR_POLL :: CInt

foreign import capi "termbox2.h value TB_ERR_READ" _TB_ERR_READ :: CInt

foreign import capi "termbox2.h value TB_ERR_RESIZE_IOCTL" _TB_ERR_RESIZE_IOCTL :: CInt

foreign import capi "termbox2.h value TB_ERR_RESIZE_PIPE" _TB_ERR_RESIZE_PIPE :: CInt

foreign import capi "termbox2.h value TB_ERR_RESIZE_POLL" _TB_ERR_RESIZE_POLL :: CInt

foreign import capi "termbox2.h value TB_ERR_RESIZE_READ" _TB_ERR_RESIZE_READ :: CInt

foreign import capi "termbox2.h value TB_ERR_RESIZE_SIGACTION" _TB_ERR_RESIZE_SIGACTION :: CInt

foreign import capi "termbox2.h value TB_ERR_RESIZE_SSCANF" _TB_ERR_RESIZE_SSCANF :: CInt

foreign import capi "termbox2.h value TB_ERR_RESIZE_WRITE" _TB_ERR_RESIZE_WRITE :: CInt

foreign import capi "termbox2.h value TB_ERR_TCGETATTR" _TB_ERR_TCGETATTR :: CInt

foreign import capi "termbox2.h value TB_ERR_TCSETATTR" _TB_ERR_TCSETATTR :: CInt

foreign import capi "termbox2.h value TB_ERR_UNSUPPORTED_TERM" _TB_ERR_UNSUPPORTED_TERM :: CInt

foreign import capi "termbox2.h value TB_EVENT_KEY" _TB_EVENT_KEY :: Word8

foreign import capi "termbox2.h value TB_EVENT_MOUSE" _TB_EVENT_MOUSE :: Word8

foreign import capi "termbox2.h value TB_EVENT_RESIZE" _TB_EVENT_RESIZE :: Word8

foreign import capi "termbox2.h value TB_FUNC_EXTRACT_POST" _TB_FUNC_EXTRACT_POST :: CInt

foreign import capi "termbox2.h value TB_FUNC_EXTRACT_PRE" _TB_FUNC_EXTRACT_PRE :: CInt

foreign import capi "termbox2.h value TB_GREEN" _TB_GREEN :: Word64

foreign import capi "termbox2.h value TB_HI_BLACK" _TB_HI_BLACK :: Word64

foreign import capi "termbox2.h value TB_INPUT_ALT" _TB_INPUT_ALT :: CInt

foreign import capi "termbox2.h value TB_INPUT_CURRENT" _TB_INPUT_CURRENT :: CInt

foreign import capi "termbox2.h value TB_INPUT_ESC" _TB_INPUT_ESC :: CInt

foreign import capi "termbox2.h value TB_INPUT_MOUSE" _TB_INPUT_MOUSE :: CInt

foreign import capi "termbox2.h value TB_INVISIBLE" _TB_INVISIBLE :: Word64

foreign import capi "termbox2.h value TB_ITALIC" _TB_ITALIC :: Word64

foreign import capi "termbox2.h value TB_KEY_ARROW_DOWN" _TB_KEY_ARROW_DOWN :: Word16

foreign import capi "termbox2.h value TB_KEY_ARROW_LEFT" _TB_KEY_ARROW_LEFT :: Word16

foreign import capi "termbox2.h value TB_KEY_ARROW_RIGHT" _TB_KEY_ARROW_RIGHT :: Word16

foreign import capi "termbox2.h value TB_KEY_ARROW_UP" _TB_KEY_ARROW_UP :: Word16

foreign import capi "termbox2.h value TB_KEY_BACKSPACE" _TB_KEY_BACKSPACE :: Word16

foreign import capi "termbox2.h value TB_KEY_BACKSPACE2" _TB_KEY_BACKSPACE2 :: Word16

foreign import capi "termbox2.h value TB_KEY_BACK_TAB" _TB_KEY_BACK_TAB :: Word16

foreign import capi "termbox2.h value TB_KEY_CTRL_2" _TB_KEY_CTRL_2 :: Word16

foreign import capi "termbox2.h value TB_KEY_CTRL_3" _TB_KEY_CTRL_3 :: Word16

foreign import capi "termbox2.h value TB_KEY_CTRL_4" _TB_KEY_CTRL_4 :: Word16

foreign import capi "termbox2.h value TB_KEY_CTRL_5" _TB_KEY_CTRL_5 :: Word16

foreign import capi "termbox2.h value TB_KEY_CTRL_6" _TB_KEY_CTRL_6 :: Word16

foreign import capi "termbox2.h value TB_KEY_CTRL_7" _TB_KEY_CTRL_7 :: Word16

foreign import capi "termbox2.h value TB_KEY_CTRL_8" _TB_KEY_CTRL_8 :: Word16

foreign import capi "termbox2.h value TB_KEY_CTRL_A" _TB_KEY_CTRL_A :: Word16

foreign import capi "termbox2.h value TB_KEY_CTRL_B" _TB_KEY_CTRL_B :: Word16

foreign import capi "termbox2.h value TB_KEY_CTRL_BACKSLASH" _TB_KEY_CTRL_BACKSLASH :: Word16

foreign import capi "termbox2.h value TB_KEY_CTRL_C" _TB_KEY_CTRL_C :: Word16

foreign import capi "termbox2.h value TB_KEY_CTRL_D" _TB_KEY_CTRL_D :: Word16

foreign import capi "termbox2.h value TB_KEY_CTRL_E" _TB_KEY_CTRL_E :: Word16

foreign import capi "termbox2.h value TB_KEY_CTRL_F" _TB_KEY_CTRL_F :: Word16

foreign import capi "termbox2.h value TB_KEY_CTRL_G" _TB_KEY_CTRL_G :: Word16

foreign import capi "termbox2.h value TB_KEY_CTRL_H" _TB_KEY_CTRL_H :: Word16

foreign import capi "termbox2.h value TB_KEY_CTRL_I" _TB_KEY_CTRL_I :: Word16

foreign import capi "termbox2.h value TB_KEY_CTRL_J" _TB_KEY_CTRL_J :: Word16

foreign import capi "termbox2.h value TB_KEY_CTRL_K" _TB_KEY_CTRL_K :: Word16

foreign import capi "termbox2.h value TB_KEY_CTRL_L" _TB_KEY_CTRL_L :: Word16

foreign import capi "termbox2.h value TB_KEY_CTRL_LSQ_BRACKET" _TB_KEY_CTRL_LSQ_BRACKET :: Word16

foreign import capi "termbox2.h value TB_KEY_CTRL_M" _TB_KEY_CTRL_M :: Word16

foreign import capi "termbox2.h value TB_KEY_CTRL_N" _TB_KEY_CTRL_N :: Word16

foreign import capi "termbox2.h value TB_KEY_CTRL_O" _TB_KEY_CTRL_O :: Word16

foreign import capi "termbox2.h value TB_KEY_CTRL_P" _TB_KEY_CTRL_P :: Word16

foreign import capi "termbox2.h value TB_KEY_CTRL_Q" _TB_KEY_CTRL_Q :: Word16

foreign import capi "termbox2.h value TB_KEY_CTRL_R" _TB_KEY_CTRL_R :: Word16

foreign import capi "termbox2.h value TB_KEY_CTRL_RSQ_BRACKET" _TB_KEY_CTRL_RSQ_BRACKET :: Word16

foreign import capi "termbox2.h value TB_KEY_CTRL_S" _TB_KEY_CTRL_S :: Word16

foreign import capi "termbox2.h value TB_KEY_CTRL_SLASH" _TB_KEY_CTRL_SLASH :: Word16

foreign import capi "termbox2.h value TB_KEY_CTRL_T" _TB_KEY_CTRL_T :: Word16

foreign import capi "termbox2.h value TB_KEY_CTRL_TILDE" _TB_KEY_CTRL_TILDE :: Word16

foreign import capi "termbox2.h value TB_KEY_CTRL_U" _TB_KEY_CTRL_U :: Word16

foreign import capi "termbox2.h value TB_KEY_CTRL_UNDERSCORE" _TB_KEY_CTRL_UNDERSCORE :: Word16

foreign import capi "termbox2.h value TB_KEY_CTRL_V" _TB_KEY_CTRL_V :: Word16

foreign import capi "termbox2.h value TB_KEY_CTRL_W" _TB_KEY_CTRL_W :: Word16

foreign import capi "termbox2.h value TB_KEY_CTRL_X" _TB_KEY_CTRL_X :: Word16

foreign import capi "termbox2.h value TB_KEY_CTRL_Y" _TB_KEY_CTRL_Y :: Word16

foreign import capi "termbox2.h value TB_KEY_CTRL_Z" _TB_KEY_CTRL_Z :: Word16

foreign import capi "termbox2.h value TB_KEY_DELETE" _TB_KEY_DELETE :: Word16

foreign import capi "termbox2.h value TB_KEY_END" _TB_KEY_END :: Word16

foreign import capi "termbox2.h value TB_KEY_ENTER" _TB_KEY_ENTER :: Word16

foreign import capi "termbox2.h value TB_KEY_ESC" _TB_KEY_ESC :: Word16

foreign import capi "termbox2.h value TB_KEY_F1" _TB_KEY_F1 :: Word16

foreign import capi "termbox2.h value TB_KEY_F10" _TB_KEY_F10 :: Word16

foreign import capi "termbox2.h value TB_KEY_F11" _TB_KEY_F11 :: Word16

foreign import capi "termbox2.h value TB_KEY_F12" _TB_KEY_F12 :: Word16

foreign import capi "termbox2.h value TB_KEY_F2" _TB_KEY_F2 :: Word16

foreign import capi "termbox2.h value TB_KEY_F3" _TB_KEY_F3 :: Word16

foreign import capi "termbox2.h value TB_KEY_F4" _TB_KEY_F4 :: Word16

foreign import capi "termbox2.h value TB_KEY_F5" _TB_KEY_F5 :: Word16

foreign import capi "termbox2.h value TB_KEY_F6" _TB_KEY_F6 :: Word16

foreign import capi "termbox2.h value TB_KEY_F7" _TB_KEY_F7 :: Word16

foreign import capi "termbox2.h value TB_KEY_F8" _TB_KEY_F8 :: Word16

foreign import capi "termbox2.h value TB_KEY_F9" _TB_KEY_F9 :: Word16

foreign import capi "termbox2.h value TB_KEY_HOME" _TB_KEY_HOME :: Word16

foreign import capi "termbox2.h value TB_KEY_INSERT" _TB_KEY_INSERT :: Word16

foreign import capi "termbox2.h value TB_KEY_MOUSE_LEFT" _TB_KEY_MOUSE_LEFT :: Word16

foreign import capi "termbox2.h value TB_KEY_MOUSE_MIDDLE" _TB_KEY_MOUSE_MIDDLE :: Word16

foreign import capi "termbox2.h value TB_KEY_MOUSE_RELEASE" _TB_KEY_MOUSE_RELEASE :: Word16

foreign import capi "termbox2.h value TB_KEY_MOUSE_RIGHT" _TB_KEY_MOUSE_RIGHT :: Word16

foreign import capi "termbox2.h value TB_KEY_MOUSE_WHEEL_DOWN" _TB_KEY_MOUSE_WHEEL_DOWN :: Word16

foreign import capi "termbox2.h value TB_KEY_MOUSE_WHEEL_UP" _TB_KEY_MOUSE_WHEEL_UP :: Word16

foreign import capi "termbox2.h value TB_KEY_PGDN" _TB_KEY_PGDN :: Word16

foreign import capi "termbox2.h value TB_KEY_PGUP" _TB_KEY_PGUP :: Word16

foreign import capi "termbox2.h value TB_KEY_SPACE" _TB_KEY_SPACE :: Word16

foreign import capi "termbox2.h value TB_KEY_TAB" _TB_KEY_TAB :: Word16

foreign import capi "termbox2.h value TB_MAGENTA" _TB_MAGENTA :: Word64

foreign import capi "termbox2.h value TB_MOD_ALT" _TB_MOD_ALT :: Word8

foreign import capi "termbox2.h value TB_MOD_CTRL" _TB_MOD_CTRL :: Word8

foreign import capi "termbox2.h value TB_MOD_MOTION" _TB_MOD_MOTION :: Word8

foreign import capi "termbox2.h value TB_MOD_SHIFT" _TB_MOD_SHIFT :: Word8

foreign import capi "termbox2.h value TB_OK" _TB_OK :: CInt

foreign import capi "termbox2.h value TB_OPT_PRINTF_BUF" _TB_OPT_PRINTF_BUF :: CInt

foreign import capi "termbox2.h value TB_OPT_READ_BUF" _TB_OPT_READ_BUF :: CInt

foreign import capi "termbox2.h value TB_OUTPUT_216" _TB_OUTPUT_216 :: CInt

foreign import capi "termbox2.h value TB_OUTPUT_256" _TB_OUTPUT_256 :: CInt

foreign import capi "termbox2.h value TB_OUTPUT_CURRENT" _TB_OUTPUT_CURRENT :: CInt

foreign import capi "termbox2.h value TB_OUTPUT_GRAYSCALE" _TB_OUTPUT_GRAYSCALE :: CInt

foreign import capi "termbox2.h value TB_OUTPUT_NORMAL" _TB_OUTPUT_NORMAL :: CInt

foreign import capi "termbox2.h value TB_OUTPUT_TRUECOLOR" _TB_OUTPUT_TRUECOLOR :: CInt

foreign import capi "termbox2.h value TB_OVERLINE" _TB_OVERLINE :: Word64

foreign import capi "termbox2.h value TB_RED" _TB_RED :: Word64

foreign import capi "termbox2.h value TB_REVERSE" _TB_REVERSE :: Word64

foreign import capi "termbox2.h value TB_STRIKEOUT" _TB_STRIKEOUT :: Word64

foreign import capi "termbox2.h value TB_UNDERLINE" _TB_UNDERLINE :: Word64

foreign import capi "termbox2.h value TB_UNDERLINE_2" _TB_UNDERLINE_2 :: Word64

foreign import capi "termbox2.h value TB_WHITE" _TB_WHITE :: Word64

foreign import capi "termbox2.h value TB_YELLOW" _TB_YELLOW :: Word64

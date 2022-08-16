module Termbox.Bindings.C
  ( -- * Functions

    -- ** Initialize / shutdown
    tb_init,
    tb_init_fd,
    tb_init_file,
    tb_shutdown,

    -- ** Get terminal dimensions
    tb_width,
    tb_height,

    -- ** Clear and synchronize the back buffer
    tb_clear,
    tb_set_clear_attributes,
    tb_present,

    -- ** Set a cell
    tb_set_cursor,
    tb_put_cell,
    tb_change_cell,
    tb_cell_buffer,

    -- ** Set input / output mode
    tb_select_input_mode,
    tb_select_output_mode,

    -- ** Poll for events
    tb_peek_event,
    tb_poll_event,

    -- * Objects
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

    -- ** Alt modifiers
    _TB_MOD_ALT,
    _TB_MOD_MOTION,

    -- ** Colors
    _TB_DEFAULT,
    _TB_BLACK,
    _TB_BLUE,
    _TB_CYAN,
    _TB_GREEN,
    _TB_MAGENTA,
    _TB_RED,
    _TB_WHITE,
    _TB_YELLOW,

    -- ** Attributes
    _TB_BOLD,
    _TB_REVERSE,
    _TB_UNDERLINE,

    -- ** Event types
    _TB_EVENT_KEY,
    _TB_EVENT_MOUSE,
    _TB_EVENT_RESIZE,

    -- ** 'tb_init' error codes
    _TB_EFAILED_TO_OPEN_TTY,
    _TB_EPIPE_TRAP_ERROR,
    _TB_EUNSUPPORTED_TERMINAL,

    -- ** Hide cursor
    _TB_HIDE_CURSOR,

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
  )
where

import Data.Int (Int32)
import Data.Word
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (CInt))
import Foreign.Ptr (Ptr)
import Foreign.Storable
import Prelude hiding (mod)

------------------------------------------------------------------------------------------------------------------------
-- Functions

-- | Get a pointer to the back buffer.
foreign import capi unsafe "termbox.h tb_cell_buffer"
  tb_cell_buffer :: IO (Ptr Tb_cell)

-- | Set a cell value in the back buffer.
foreign import capi unsafe "termbox.h tb_change_cell"
  tb_change_cell ::
    -- | x
    CInt ->
    -- | y
    CInt ->
    -- | ch
    Word32 ->
    -- | fg
    Word16 ->
    -- | bg
    Word16 ->
    IO ()

-- | Clear the back buffer.
foreign import capi unsafe "termbox.h tb_clear"
  tb_clear :: IO ()

-- | Get the terminal height.
foreign import capi unsafe "termbox.h tb_height"
  tb_height :: IO CInt

-- | Initialize the @termbox@ library.
foreign import capi unsafe "termbox.h tb_init"
  tb_init :: IO CInt

-- | Initialize the @termbox@ library.
--
-- > tb_init = tb_init_file("/dev/tty")
foreign import capi unsafe "termbox.h tb_init_file"
  tb_init_file :: CString -> IO CInt

-- | Initialize the @termbox@ library.
--
-- > tb_init = tb_init_fd(0)
foreign import capi unsafe "termbox.h tb_init_fd"
  tb_init_fd :: CInt -> IO CInt

-- | Wait for an event.
foreign import capi safe "termbox.h tb_peek_event"
  tb_peek_event ::
    Ptr Tb_event ->
    -- | Timeout (in milliseconds).
    CInt ->
    IO CInt

-- | Wait for an event.
foreign import capi safe "termbox.h tb_poll_event"
  tb_poll_event :: Ptr Tb_event -> IO CInt

-- | Synchronize the back buffer with the terminal.
foreign import capi unsafe "termbox.h tb_present"
  tb_present :: IO ()

-- | Set a cell value in the back buffer.
foreign import capi unsafe "termbox.h tb_put_cell"
  tb_put_cell ::
    -- | x
    CInt ->
    -- | y
    CInt ->
    -- | cell
    Ptr Tb_cell ->
    IO ()

-- | Set the input mode.
foreign import capi unsafe "termbox.h tb_select_input_mode"
  tb_select_input_mode :: CInt -> IO CInt

-- | Set the output mode.
foreign import capi unsafe "termbox.h tb_select_output_mode"
  tb_select_output_mode :: CInt -> IO CInt

-- | Set the foreground and background attributes that 'tb_clear' clears the back buffer with.
foreign import capi unsafe "termbox.h tb_set_clear_attributes"
  tb_set_clear_attributes :: Word16 -> Word16 -> IO ()

-- | Set the cursor location, or hide it.
foreign import capi unsafe "termbox.h tb_set_cursor"
  tb_set_cursor ::
    -- | x
    CInt ->
    -- | y
    CInt ->
    IO ()

-- | Shutdown the @termbox@ library.
foreign import capi unsafe "termbox.h tb_shutdown"
  tb_shutdown :: IO ()

-- | Get the terminal width.
foreign import capi unsafe "termbox.h tb_width"
  tb_width :: IO CInt

------------------------------------------------------------------------------------------------------------------------
-- Objects

-- | A cell.
data Tb_cell = Tb_cell
  { -- | A unicode character.
    ch :: {-# UNPACK #-} !Word32,
    -- | Foreground attribute.
    fg :: {-# UNPACK #-} !Word16,
    -- | Background attribute.
    bg :: {-# UNPACK #-} !Word16
  }

-- | An event.
data Tb_event = Tb_event
  { type_ :: {-# UNPACK #-} !Word8,
    mod :: {-# UNPACK #-} !Word8,
    key :: {-# UNPACK #-} !Word16,
    ch :: {-# UNPACK #-} !Word32,
    w :: {-# UNPACK #-} !Int32,
    h :: {-# UNPACK #-} !Int32,
    x :: {-# UNPACK #-} !Int32,
    y :: {-# UNPACK #-} !Int32
  }

instance Storable Tb_event where
  sizeOf :: Tb_event -> Int
  sizeOf _ =
    24

  alignment :: Tb_event -> Int
  alignment _ =
    4

  peek :: Ptr Tb_event -> IO Tb_event
  peek ptr =
    Tb_event
      <$> peekByteOff ptr 0
      <*> peekByteOff ptr 1
      <*> peekByteOff ptr 2
      <*> peekByteOff ptr 4
      <*> peekByteOff ptr 8
      <*> peekByteOff ptr 12
      <*> peekByteOff ptr 16
      <*> peekByteOff ptr 20

  poke :: Ptr Tb_event -> Tb_event -> IO ()
  poke ptr Tb_event {type_, mod, key, ch, w, h, x, y} = do
    pokeByteOff ptr 0 type_
    pokeByteOff ptr 1 mod
    pokeByteOff ptr 2 key
    pokeByteOff ptr 4 ch
    pokeByteOff ptr 8 w
    pokeByteOff ptr 12 h
    pokeByteOff ptr 16 x
    pokeByteOff ptr 20 y

------------------------------------------------------------------------------------------------------------------------
-- Constants

foreign import capi "termbox.h value TB_BLACK" _TB_BLACK :: Word16

foreign import capi "termbox.h value TB_BLUE" _TB_BLUE :: Word16

foreign import capi "termbox.h value TB_BOLD" _TB_BOLD :: Word16

foreign import capi "termbox.h value TB_CYAN" _TB_CYAN :: Word16

foreign import capi "termbox.h value TB_DEFAULT" _TB_DEFAULT :: Word16

foreign import capi "termbox.h value TB_EFAILED_TO_OPEN_TTY" _TB_EFAILED_TO_OPEN_TTY :: CInt

foreign import capi "termbox.h value TB_EPIPE_TRAP_ERROR" _TB_EPIPE_TRAP_ERROR :: CInt

foreign import capi "termbox.h value TB_EUNSUPPORTED_TERMINAL" _TB_EUNSUPPORTED_TERMINAL :: CInt

foreign import capi "termbox.h value TB_EVENT_KEY" _TB_EVENT_KEY :: Word8

foreign import capi "termbox.h value TB_EVENT_MOUSE" _TB_EVENT_MOUSE :: Word8

foreign import capi "termbox.h value TB_EVENT_RESIZE" _TB_EVENT_RESIZE :: Word8

foreign import capi "termbox.h value TB_GREEN" _TB_GREEN :: Word16

foreign import capi "termbox.h value TB_HIDE_CURSOR" _TB_HIDE_CURSOR :: CInt

foreign import capi "termbox.h value TB_INPUT_ALT" _TB_INPUT_ALT :: CInt

foreign import capi "termbox.h value TB_INPUT_CURRENT" _TB_INPUT_CURRENT :: CInt

foreign import capi "termbox.h value TB_INPUT_ESC" _TB_INPUT_ESC :: CInt

foreign import capi "termbox.h value TB_INPUT_MOUSE" _TB_INPUT_MOUSE :: CInt

foreign import capi "termbox.h value TB_KEY_ARROW_DOWN" _TB_KEY_ARROW_DOWN :: Word16

foreign import capi "termbox.h value TB_KEY_ARROW_LEFT" _TB_KEY_ARROW_LEFT :: Word16

foreign import capi "termbox.h value TB_KEY_ARROW_RIGHT" _TB_KEY_ARROW_RIGHT :: Word16

foreign import capi "termbox.h value TB_KEY_ARROW_UP" _TB_KEY_ARROW_UP :: Word16

foreign import capi "termbox.h value TB_KEY_BACKSPACE" _TB_KEY_BACKSPACE :: Word16

foreign import capi "termbox.h value TB_KEY_BACKSPACE2" _TB_KEY_BACKSPACE2 :: Word16

foreign import capi "termbox.h value TB_KEY_CTRL_2" _TB_KEY_CTRL_2 :: Word16

foreign import capi "termbox.h value TB_KEY_CTRL_3" _TB_KEY_CTRL_3 :: Word16

foreign import capi "termbox.h value TB_KEY_CTRL_4" _TB_KEY_CTRL_4 :: Word16

foreign import capi "termbox.h value TB_KEY_CTRL_5" _TB_KEY_CTRL_5 :: Word16

foreign import capi "termbox.h value TB_KEY_CTRL_6" _TB_KEY_CTRL_6 :: Word16

foreign import capi "termbox.h value TB_KEY_CTRL_7" _TB_KEY_CTRL_7 :: Word16

foreign import capi "termbox.h value TB_KEY_CTRL_8" _TB_KEY_CTRL_8 :: Word16

foreign import capi "termbox.h value TB_KEY_CTRL_A" _TB_KEY_CTRL_A :: Word16

foreign import capi "termbox.h value TB_KEY_CTRL_B" _TB_KEY_CTRL_B :: Word16

foreign import capi "termbox.h value TB_KEY_CTRL_BACKSLASH" _TB_KEY_CTRL_BACKSLASH :: Word16

foreign import capi "termbox.h value TB_KEY_CTRL_C" _TB_KEY_CTRL_C :: Word16

foreign import capi "termbox.h value TB_KEY_CTRL_D" _TB_KEY_CTRL_D :: Word16

foreign import capi "termbox.h value TB_KEY_CTRL_E" _TB_KEY_CTRL_E :: Word16

foreign import capi "termbox.h value TB_KEY_CTRL_F" _TB_KEY_CTRL_F :: Word16

foreign import capi "termbox.h value TB_KEY_CTRL_G" _TB_KEY_CTRL_G :: Word16

foreign import capi "termbox.h value TB_KEY_CTRL_H" _TB_KEY_CTRL_H :: Word16

foreign import capi "termbox.h value TB_KEY_CTRL_I" _TB_KEY_CTRL_I :: Word16

foreign import capi "termbox.h value TB_KEY_CTRL_J" _TB_KEY_CTRL_J :: Word16

foreign import capi "termbox.h value TB_KEY_CTRL_K" _TB_KEY_CTRL_K :: Word16

foreign import capi "termbox.h value TB_KEY_CTRL_L" _TB_KEY_CTRL_L :: Word16

foreign import capi "termbox.h value TB_KEY_CTRL_LSQ_BRACKET" _TB_KEY_CTRL_LSQ_BRACKET :: Word16

foreign import capi "termbox.h value TB_KEY_CTRL_M" _TB_KEY_CTRL_M :: Word16

foreign import capi "termbox.h value TB_KEY_CTRL_N" _TB_KEY_CTRL_N :: Word16

foreign import capi "termbox.h value TB_KEY_CTRL_O" _TB_KEY_CTRL_O :: Word16

foreign import capi "termbox.h value TB_KEY_CTRL_P" _TB_KEY_CTRL_P :: Word16

foreign import capi "termbox.h value TB_KEY_CTRL_Q" _TB_KEY_CTRL_Q :: Word16

foreign import capi "termbox.h value TB_KEY_CTRL_R" _TB_KEY_CTRL_R :: Word16

foreign import capi "termbox.h value TB_KEY_CTRL_RSQ_BRACKET" _TB_KEY_CTRL_RSQ_BRACKET :: Word16

foreign import capi "termbox.h value TB_KEY_CTRL_S" _TB_KEY_CTRL_S :: Word16

foreign import capi "termbox.h value TB_KEY_CTRL_SLASH" _TB_KEY_CTRL_SLASH :: Word16

foreign import capi "termbox.h value TB_KEY_CTRL_T" _TB_KEY_CTRL_T :: Word16

foreign import capi "termbox.h value TB_KEY_CTRL_TILDE" _TB_KEY_CTRL_TILDE :: Word16

foreign import capi "termbox.h value TB_KEY_CTRL_U" _TB_KEY_CTRL_U :: Word16

foreign import capi "termbox.h value TB_KEY_CTRL_UNDERSCORE" _TB_KEY_CTRL_UNDERSCORE :: Word16

foreign import capi "termbox.h value TB_KEY_CTRL_V" _TB_KEY_CTRL_V :: Word16

foreign import capi "termbox.h value TB_KEY_CTRL_W" _TB_KEY_CTRL_W :: Word16

foreign import capi "termbox.h value TB_KEY_CTRL_X" _TB_KEY_CTRL_X :: Word16

foreign import capi "termbox.h value TB_KEY_CTRL_Y" _TB_KEY_CTRL_Y :: Word16

foreign import capi "termbox.h value TB_KEY_CTRL_Z" _TB_KEY_CTRL_Z :: Word16

foreign import capi "termbox.h value TB_KEY_DELETE" _TB_KEY_DELETE :: Word16

foreign import capi "termbox.h value TB_KEY_END" _TB_KEY_END :: Word16

foreign import capi "termbox.h value TB_KEY_ENTER" _TB_KEY_ENTER :: Word16

foreign import capi "termbox.h value TB_KEY_ESC" _TB_KEY_ESC :: Word16

foreign import capi "termbox.h value TB_KEY_F1" _TB_KEY_F1 :: Word16

foreign import capi "termbox.h value TB_KEY_F10" _TB_KEY_F10 :: Word16

foreign import capi "termbox.h value TB_KEY_F11" _TB_KEY_F11 :: Word16

foreign import capi "termbox.h value TB_KEY_F12" _TB_KEY_F12 :: Word16

foreign import capi "termbox.h value TB_KEY_F2" _TB_KEY_F2 :: Word16

foreign import capi "termbox.h value TB_KEY_F3" _TB_KEY_F3 :: Word16

foreign import capi "termbox.h value TB_KEY_F4" _TB_KEY_F4 :: Word16

foreign import capi "termbox.h value TB_KEY_F5" _TB_KEY_F5 :: Word16

foreign import capi "termbox.h value TB_KEY_F6" _TB_KEY_F6 :: Word16

foreign import capi "termbox.h value TB_KEY_F7" _TB_KEY_F7 :: Word16

foreign import capi "termbox.h value TB_KEY_F8" _TB_KEY_F8 :: Word16

foreign import capi "termbox.h value TB_KEY_F9" _TB_KEY_F9 :: Word16

foreign import capi "termbox.h value TB_KEY_HOME" _TB_KEY_HOME :: Word16

foreign import capi "termbox.h value TB_KEY_INSERT" _TB_KEY_INSERT :: Word16

foreign import capi "termbox.h value TB_KEY_MOUSE_LEFT" _TB_KEY_MOUSE_LEFT :: Word16

foreign import capi "termbox.h value TB_KEY_MOUSE_MIDDLE" _TB_KEY_MOUSE_MIDDLE :: Word16

foreign import capi "termbox.h value TB_KEY_MOUSE_RELEASE" _TB_KEY_MOUSE_RELEASE :: Word16

foreign import capi "termbox.h value TB_KEY_MOUSE_RIGHT" _TB_KEY_MOUSE_RIGHT :: Word16

foreign import capi "termbox.h value TB_KEY_MOUSE_WHEEL_DOWN" _TB_KEY_MOUSE_WHEEL_DOWN :: Word16

foreign import capi "termbox.h value TB_KEY_MOUSE_WHEEL_UP" _TB_KEY_MOUSE_WHEEL_UP :: Word16

foreign import capi "termbox.h value TB_KEY_PGDN" _TB_KEY_PGDN :: Word16

foreign import capi "termbox.h value TB_KEY_PGUP" _TB_KEY_PGUP :: Word16

foreign import capi "termbox.h value TB_KEY_SPACE" _TB_KEY_SPACE :: Word16

foreign import capi "termbox.h value TB_KEY_TAB" _TB_KEY_TAB :: Word16

foreign import capi "termbox.h value TB_MAGENTA" _TB_MAGENTA :: Word16

foreign import capi "termbox.h value TB_MOD_ALT" _TB_MOD_ALT :: Word8

foreign import capi "termbox.h value TB_MOD_MOTION" _TB_MOD_MOTION :: Word8

foreign import capi "termbox.h value TB_OUTPUT_216" _TB_OUTPUT_216 :: CInt

foreign import capi "termbox.h value TB_OUTPUT_256" _TB_OUTPUT_256 :: CInt

foreign import capi "termbox.h value TB_OUTPUT_CURRENT" _TB_OUTPUT_CURRENT :: CInt

foreign import capi "termbox.h value TB_OUTPUT_GRAYSCALE" _TB_OUTPUT_GRAYSCALE :: CInt

foreign import capi "termbox.h value TB_OUTPUT_NORMAL" _TB_OUTPUT_NORMAL :: CInt

foreign import capi "termbox.h value TB_RED" _TB_RED :: Word16

foreign import capi "termbox.h value TB_REVERSE" _TB_REVERSE :: Word16

foreign import capi "termbox.h value TB_UNDERLINE" _TB_UNDERLINE :: Word16

foreign import capi "termbox.h value TB_WHITE" _TB_WHITE :: Word16

foreign import capi "termbox.h value TB_YELLOW" _TB_YELLOW :: Word16

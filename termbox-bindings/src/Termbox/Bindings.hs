module Termbox.Bindings
  ( -- * Functions

    -- ** Initialize / shutdown
    tb_init,
    tb_init_fd,
    -- tb_init_file,
    Termbox.Bindings.C.tb_shutdown,

    -- ** Get terminal dimensions
    tb_width,
    tb_height,

    -- ** Clear and synchronize the back buffer
    Termbox.Bindings.C.tb_clear,
    Termbox.Bindings.C.tb_set_clear_attributes,
    Termbox.Bindings.C.tb_present,

    -- ** Set a cell
    tb_set_cursor,
    tb_put_cell,
    tb_change_cell,
    -- tb_cell_buffer,

    -- ** Set input / output mode
    tb_select_input_mode,
    tb_select_output_mode,

    -- ** Poll for events
    tb_peek_event,
    tb_poll_event,

    -- * Types
    Attribute
      ( Attribute,
        TB_DEFAULT,
        TB_BLACK,
        TB_BLUE,
        TB_CYAN,
        TB_GREEN,
        TB_MAGENTA,
        TB_RED,
        TB_WHITE,
        TB_YELLOW,
        TB_BOLD,
        TB_REVERSE,
        TB_UNDERLINE
      ),
    Termbox.Bindings.C.Tb_cell (..),
    Termbox.Bindings.C.Tb_event (..),

    -- * Constants

    -- ** Keys
    Termbox.Bindings.C._TB_KEY_ARROW_DOWN,
    Termbox.Bindings.C._TB_KEY_ARROW_LEFT,
    Termbox.Bindings.C._TB_KEY_ARROW_RIGHT,
    Termbox.Bindings.C._TB_KEY_ARROW_UP,
    Termbox.Bindings.C._TB_KEY_BACKSPACE,
    Termbox.Bindings.C._TB_KEY_BACKSPACE2,
    Termbox.Bindings.C._TB_KEY_CTRL_2,
    Termbox.Bindings.C._TB_KEY_CTRL_3,
    Termbox.Bindings.C._TB_KEY_CTRL_4,
    Termbox.Bindings.C._TB_KEY_CTRL_5,
    Termbox.Bindings.C._TB_KEY_CTRL_6,
    Termbox.Bindings.C._TB_KEY_CTRL_7,
    Termbox.Bindings.C._TB_KEY_CTRL_8,
    Termbox.Bindings.C._TB_KEY_CTRL_A,
    Termbox.Bindings.C._TB_KEY_CTRL_B,
    Termbox.Bindings.C._TB_KEY_CTRL_BACKSLASH,
    Termbox.Bindings.C._TB_KEY_CTRL_C,
    Termbox.Bindings.C._TB_KEY_CTRL_D,
    Termbox.Bindings.C._TB_KEY_CTRL_E,
    Termbox.Bindings.C._TB_KEY_CTRL_F,
    Termbox.Bindings.C._TB_KEY_CTRL_G,
    Termbox.Bindings.C._TB_KEY_CTRL_H,
    Termbox.Bindings.C._TB_KEY_CTRL_I,
    Termbox.Bindings.C._TB_KEY_CTRL_J,
    Termbox.Bindings.C._TB_KEY_CTRL_K,
    Termbox.Bindings.C._TB_KEY_CTRL_L,
    Termbox.Bindings.C._TB_KEY_CTRL_LSQ_BRACKET,
    Termbox.Bindings.C._TB_KEY_CTRL_M,
    Termbox.Bindings.C._TB_KEY_CTRL_N,
    Termbox.Bindings.C._TB_KEY_CTRL_O,
    Termbox.Bindings.C._TB_KEY_CTRL_P,
    Termbox.Bindings.C._TB_KEY_CTRL_Q,
    Termbox.Bindings.C._TB_KEY_CTRL_R,
    Termbox.Bindings.C._TB_KEY_CTRL_RSQ_BRACKET,
    Termbox.Bindings.C._TB_KEY_CTRL_S,
    Termbox.Bindings.C._TB_KEY_CTRL_SLASH,
    Termbox.Bindings.C._TB_KEY_CTRL_T,
    Termbox.Bindings.C._TB_KEY_CTRL_TILDE,
    Termbox.Bindings.C._TB_KEY_CTRL_U,
    Termbox.Bindings.C._TB_KEY_CTRL_UNDERSCORE,
    Termbox.Bindings.C._TB_KEY_CTRL_V,
    Termbox.Bindings.C._TB_KEY_CTRL_W,
    Termbox.Bindings.C._TB_KEY_CTRL_X,
    Termbox.Bindings.C._TB_KEY_CTRL_Y,
    Termbox.Bindings.C._TB_KEY_CTRL_Z,
    Termbox.Bindings.C._TB_KEY_DELETE,
    Termbox.Bindings.C._TB_KEY_END,
    Termbox.Bindings.C._TB_KEY_ENTER,
    Termbox.Bindings.C._TB_KEY_ESC,
    Termbox.Bindings.C._TB_KEY_F1,
    Termbox.Bindings.C._TB_KEY_F10,
    Termbox.Bindings.C._TB_KEY_F11,
    Termbox.Bindings.C._TB_KEY_F12,
    Termbox.Bindings.C._TB_KEY_F2,
    Termbox.Bindings.C._TB_KEY_F3,
    Termbox.Bindings.C._TB_KEY_F4,
    Termbox.Bindings.C._TB_KEY_F5,
    Termbox.Bindings.C._TB_KEY_F6,
    Termbox.Bindings.C._TB_KEY_F7,
    Termbox.Bindings.C._TB_KEY_F8,
    Termbox.Bindings.C._TB_KEY_F9,
    Termbox.Bindings.C._TB_KEY_HOME,
    Termbox.Bindings.C._TB_KEY_INSERT,
    Termbox.Bindings.C._TB_KEY_MOUSE_LEFT,
    Termbox.Bindings.C._TB_KEY_MOUSE_MIDDLE,
    Termbox.Bindings.C._TB_KEY_MOUSE_RELEASE,
    Termbox.Bindings.C._TB_KEY_MOUSE_RIGHT,
    Termbox.Bindings.C._TB_KEY_MOUSE_WHEEL_DOWN,
    Termbox.Bindings.C._TB_KEY_MOUSE_WHEEL_UP,
    Termbox.Bindings.C._TB_KEY_PGDN,
    Termbox.Bindings.C._TB_KEY_PGUP,
    Termbox.Bindings.C._TB_KEY_SPACE,
    Termbox.Bindings.C._TB_KEY_TAB,

    -- ** Alt modifiers
    Termbox.Bindings.C._TB_MOD_ALT,
    Termbox.Bindings.C._TB_MOD_MOTION,

    -- ** Event types
    Termbox.Bindings.C._TB_EVENT_KEY,
    Termbox.Bindings.C._TB_EVENT_MOUSE,
    Termbox.Bindings.C._TB_EVENT_RESIZE,

    -- ** 'tb_init' error codes
    Termbox.Bindings.C._TB_EFAILED_TO_OPEN_TTY,
    Termbox.Bindings.C._TB_EPIPE_TRAP_ERROR,
    Termbox.Bindings.C._TB_EUNSUPPORTED_TERMINAL,

    -- ** Hide cursor
    Termbox.Bindings.C._TB_HIDE_CURSOR,

    -- ** Input modes
    Termbox.Bindings.C._TB_INPUT_CURRENT,
    Termbox.Bindings.C._TB_INPUT_ALT,
    Termbox.Bindings.C._TB_INPUT_ESC,
    Termbox.Bindings.C._TB_INPUT_MOUSE,

    -- ** Output modes
    Termbox.Bindings.C._TB_OUTPUT_CURRENT,
    Termbox.Bindings.C._TB_OUTPUT_216,
    Termbox.Bindings.C._TB_OUTPUT_256,
    Termbox.Bindings.C._TB_OUTPUT_GRAYSCALE,
    Termbox.Bindings.C._TB_OUTPUT_NORMAL,
  )
where

import Data.Bits ((.|.))
import Data.Semigroup (Semigroup (..))
import Data.Word
import Foreign.C.Error (Errno, getErrno)
import Foreign.C.Types (CInt)
import Foreign.Marshal.Alloc (alloca)
import qualified Foreign.Storable as Storable
import System.Posix.Types (Fd (Fd))
import qualified Termbox.Bindings.C
import Prelude hiding (mod)

------------------------------------------------------------------------------------------------------------------------
-- Functions

-- | Get a pointer to the back buffer.
-- foreign import capi unsafe "termbox.h tb_cell_buffer"
--   tb_cell_buffer :: IO (Ptr Tb_cell)

-- | Set a cell value in the back buffer.
tb_change_cell ::
  -- | x
  Int ->
  -- | y
  Int ->
  -- | ch
  Word32 ->
  -- | fg
  Attribute ->
  -- | bg
  Attribute ->
  IO ()
tb_change_cell x y ch (Attribute fg) (Attribute bg) =
  Termbox.Bindings.C.tb_change_cell (intToCInt x) (intToCInt y) ch fg bg

-- | Get the terminal height.
tb_height :: IO Int
tb_height =
  cintToInt <$> Termbox.Bindings.C.tb_height

-- | Initialize the @termbox@ library.
tb_init :: IO Int
tb_init =
  cintToInt <$> Termbox.Bindings.C.tb_init

-- | Initialize the @termbox@ library.
--
-- > tb_init = tb_init_fd(0)
tb_init_fd :: Fd -> IO Int
tb_init_fd (Fd fd) =
  cintToInt <$> Termbox.Bindings.C.tb_init_fd fd

-- | Wait for an event.
tb_peek_event ::
  -- | Timeout (in milliseconds).
  CInt ->
  IO (Either Errno (Maybe Termbox.Bindings.C.Tb_event))
tb_peek_event timeout =
  alloca $ \c_event -> do
    result <- Termbox.Bindings.C.tb_peek_event c_event timeout
    if result < 0
      then Left <$> getErrno
      else
        if result == 0
          then pure (Right Nothing)
          else Right . Just <$> Storable.peek c_event

-- | Wait for an event.
tb_poll_event :: IO (Either Errno Termbox.Bindings.C.Tb_event)
tb_poll_event =
  alloca $ \c_event -> do
    result <- Termbox.Bindings.C.tb_poll_event c_event
    if result < 0
      then Left <$> getErrno
      else Right <$> Storable.peek c_event

-- | Set a cell value in the back buffer.
tb_put_cell ::
  -- | x
  Int ->
  -- | y
  Int ->
  -- | cell
  Termbox.Bindings.C.Tb_cell ->
  IO ()
tb_put_cell x y cell =
  alloca $ \c_cell -> do
    Storable.poke c_cell cell
    Termbox.Bindings.C.tb_put_cell (intToCInt x) (intToCInt y) c_cell

-- | Set the input mode.
tb_select_input_mode :: Int -> IO Int
tb_select_input_mode mode =
  cintToInt <$> Termbox.Bindings.C.tb_select_input_mode (intToCInt mode)

-- | Set the output mode.
tb_select_output_mode :: Int -> IO Int
tb_select_output_mode mode =
  cintToInt <$> Termbox.Bindings.C.tb_select_output_mode (intToCInt mode)

-- | Set the cursor location, or hide it.
tb_set_cursor ::
  -- | x
  Int ->
  -- | y
  Int ->
  IO ()
tb_set_cursor x y =
  Termbox.Bindings.C.tb_set_cursor (intToCInt x) (intToCInt y)

-- | Get the terminal width.
tb_width :: IO Int
tb_width =
  cintToInt <$> Termbox.Bindings.C.tb_width

------------------------------------------------------------------------------------------------------------------------
-- Objects

newtype Attribute
  = Attribute Word16
  deriving stock (Eq)
  deriving newtype (Num, Show)

instance Semigroup Attribute where
  Attribute x <> Attribute y =
    Attribute (x .|. y)

pattern TB_DEFAULT :: Attribute
pattern TB_DEFAULT <-
  ((== Attribute Termbox.Bindings.C._TB_DEFAULT) -> True)
  where
    TB_DEFAULT = Attribute Termbox.Bindings.C._TB_DEFAULT

pattern TB_BLACK :: Attribute
pattern TB_BLACK <-
  ((== Attribute Termbox.Bindings.C._TB_BLACK) -> True)
  where
    TB_BLACK = Attribute Termbox.Bindings.C._TB_BLACK

pattern TB_BLUE :: Attribute
pattern TB_BLUE <-
  ((== Attribute Termbox.Bindings.C._TB_BLUE) -> True)
  where
    TB_BLUE = Attribute Termbox.Bindings.C._TB_BLUE

pattern TB_CYAN :: Attribute
pattern TB_CYAN <-
  ((== Attribute Termbox.Bindings.C._TB_CYAN) -> True)
  where
    TB_CYAN = Attribute Termbox.Bindings.C._TB_CYAN

pattern TB_GREEN :: Attribute
pattern TB_GREEN <-
  ((== Attribute Termbox.Bindings.C._TB_GREEN) -> True)
  where
    TB_GREEN = Attribute Termbox.Bindings.C._TB_GREEN

pattern TB_MAGENTA :: Attribute
pattern TB_MAGENTA <-
  ((== Attribute Termbox.Bindings.C._TB_MAGENTA) -> True)
  where
    TB_MAGENTA = Attribute Termbox.Bindings.C._TB_MAGENTA

pattern TB_RED :: Attribute
pattern TB_RED <-
  ((== Attribute Termbox.Bindings.C._TB_RED) -> True)
  where
    TB_RED = Attribute Termbox.Bindings.C._TB_RED

pattern TB_WHITE :: Attribute
pattern TB_WHITE <-
  ((== Attribute Termbox.Bindings.C._TB_WHITE) -> True)
  where
    TB_WHITE = Attribute Termbox.Bindings.C._TB_WHITE

pattern TB_YELLOW :: Attribute
pattern TB_YELLOW <-
  ((== Attribute Termbox.Bindings.C._TB_YELLOW) -> True)
  where
    TB_YELLOW = Attribute Termbox.Bindings.C._TB_YELLOW

pattern TB_BOLD :: Attribute
pattern TB_BOLD <-
  ((== Attribute Termbox.Bindings.C._TB_BOLD) -> True)
  where
    TB_BOLD = Attribute Termbox.Bindings.C._TB_BOLD

pattern TB_REVERSE :: Attribute
pattern TB_REVERSE <-
  ((== Attribute Termbox.Bindings.C._TB_REVERSE) -> True)
  where
    TB_REVERSE = Attribute Termbox.Bindings.C._TB_REVERSE

pattern TB_UNDERLINE :: Attribute
pattern TB_UNDERLINE <-
  ((== Attribute Termbox.Bindings.C._TB_UNDERLINE) -> True)
  where
    TB_UNDERLINE = Attribute Termbox.Bindings.C._TB_UNDERLINE

--

cintToInt :: CInt -> Int
cintToInt = fromIntegral
{-# INLINE cintToInt #-}

intToCInt :: Int -> CInt
intToCInt = fromIntegral
{-# INLINE intToCInt #-}

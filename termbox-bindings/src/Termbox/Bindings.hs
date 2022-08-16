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
    tb_set_clear_attributes,
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
    Tb_attr
      ( Tb_attr,
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
    Tb_cell (..),
    Termbox.Bindings.C.Tb_event (..),
    Tb_input_mode
      ( Tb_input_mode,
        TB_INPUT_CURRENT,
        TB_INPUT_ALT,
        TB_INPUT_ESC,
        TB_INPUT_MOUSE
      ),
    Tb_output_mode
      ( Tb_output_mode,
        TB_OUTPUT_CURRENT,
        TB_OUTPUT_216,
        TB_OUTPUT_256,
        TB_OUTPUT_GRAYSCALE,
        TB_OUTPUT_NORMAL
      ),

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
  )
where

import Data.Bits ((.|.))
import Data.Coerce (coerce)
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
  Tb_attr ->
  -- | bg
  Tb_attr ->
  IO ()
tb_change_cell x y c (Tb_attr foreground) (Tb_attr background) =
  Termbox.Bindings.C.tb_change_cell (intToCInt x) (intToCInt y) c foreground background

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
tb_select_input_mode :: Tb_input_mode -> IO Tb_input_mode
tb_select_input_mode =
  coerce Termbox.Bindings.C.tb_select_input_mode

-- | Set the output mode.
tb_select_output_mode :: Tb_output_mode -> IO Tb_output_mode
tb_select_output_mode =
  coerce Termbox.Bindings.C.tb_select_output_mode

-- | Set the foreground and background attributes that 'tb_clear' clears the back buffer with.
tb_set_clear_attributes ::
  -- | fg
  Tb_attr ->
  -- | bg
  Tb_attr ->
  IO ()
tb_set_clear_attributes (Tb_attr foreground) (Tb_attr background) =
  Termbox.Bindings.C.tb_set_clear_attributes foreground background

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

-- | An attribute.
newtype Tb_attr
  = Tb_attr Word16
  deriving stock (Eq)
  deriving newtype (Num, Show)

instance Semigroup Tb_attr where
  Tb_attr x <> Tb_attr y =
    Tb_attr (x .|. y)

pattern TB_DEFAULT :: Tb_attr
pattern TB_DEFAULT <-
  ((== Tb_attr Termbox.Bindings.C._TB_DEFAULT) -> True)
  where
    TB_DEFAULT = Tb_attr Termbox.Bindings.C._TB_DEFAULT

pattern TB_BLACK :: Tb_attr
pattern TB_BLACK <-
  ((== Tb_attr Termbox.Bindings.C._TB_BLACK) -> True)
  where
    TB_BLACK = Tb_attr Termbox.Bindings.C._TB_BLACK

pattern TB_BLUE :: Tb_attr
pattern TB_BLUE <-
  ((== Tb_attr Termbox.Bindings.C._TB_BLUE) -> True)
  where
    TB_BLUE = Tb_attr Termbox.Bindings.C._TB_BLUE

pattern TB_CYAN :: Tb_attr
pattern TB_CYAN <-
  ((== Tb_attr Termbox.Bindings.C._TB_CYAN) -> True)
  where
    TB_CYAN = Tb_attr Termbox.Bindings.C._TB_CYAN

pattern TB_GREEN :: Tb_attr
pattern TB_GREEN <-
  ((== Tb_attr Termbox.Bindings.C._TB_GREEN) -> True)
  where
    TB_GREEN = Tb_attr Termbox.Bindings.C._TB_GREEN

pattern TB_MAGENTA :: Tb_attr
pattern TB_MAGENTA <-
  ((== Tb_attr Termbox.Bindings.C._TB_MAGENTA) -> True)
  where
    TB_MAGENTA = Tb_attr Termbox.Bindings.C._TB_MAGENTA

pattern TB_RED :: Tb_attr
pattern TB_RED <-
  ((== Tb_attr Termbox.Bindings.C._TB_RED) -> True)
  where
    TB_RED = Tb_attr Termbox.Bindings.C._TB_RED

pattern TB_WHITE :: Tb_attr
pattern TB_WHITE <-
  ((== Tb_attr Termbox.Bindings.C._TB_WHITE) -> True)
  where
    TB_WHITE = Tb_attr Termbox.Bindings.C._TB_WHITE

pattern TB_YELLOW :: Tb_attr
pattern TB_YELLOW <-
  ((== Tb_attr Termbox.Bindings.C._TB_YELLOW) -> True)
  where
    TB_YELLOW = Tb_attr Termbox.Bindings.C._TB_YELLOW

pattern TB_BOLD :: Tb_attr
pattern TB_BOLD <-
  ((== Tb_attr Termbox.Bindings.C._TB_BOLD) -> True)
  where
    TB_BOLD = Tb_attr Termbox.Bindings.C._TB_BOLD

pattern TB_REVERSE :: Tb_attr
pattern TB_REVERSE <-
  ((== Tb_attr Termbox.Bindings.C._TB_REVERSE) -> True)
  where
    TB_REVERSE = Tb_attr Termbox.Bindings.C._TB_REVERSE

pattern TB_UNDERLINE :: Tb_attr
pattern TB_UNDERLINE <-
  ((== Tb_attr Termbox.Bindings.C._TB_UNDERLINE) -> True)
  where
    TB_UNDERLINE = Tb_attr Termbox.Bindings.C._TB_UNDERLINE

-- | A cell.
data Tb_cell = Tb_cell
  { -- | A unicode character.
    ch :: {-# UNPACK #-} !Word32,
    -- | Foreground attribute.
    fg :: {-# UNPACK #-} !Tb_attr,
    -- | Background attribute.
    bg :: {-# UNPACK #-} !Tb_attr
  }

newtype Tb_input_mode
  = Tb_input_mode CInt
  deriving stock (Eq)

instance Show Tb_input_mode where
  show = \case
    TB_INPUT_CURRENT -> "TB_INPUT_CURRENT"
    TB_INPUT_ALT -> "TB_INPUT_ALT"
    TB_INPUT_ESC -> "TB_INPUT_ESC"
    TB_INPUT_MOUSE -> "TB_INPUT_MOUSE"

pattern TB_INPUT_CURRENT :: Tb_input_mode
pattern TB_INPUT_CURRENT <-
  ((== Tb_input_mode Termbox.Bindings.C._TB_INPUT_CURRENT) -> True)
  where
    TB_INPUT_CURRENT = Tb_input_mode Termbox.Bindings.C._TB_INPUT_CURRENT

pattern TB_INPUT_ALT :: Tb_input_mode
pattern TB_INPUT_ALT <-
  ((== Tb_input_mode Termbox.Bindings.C._TB_INPUT_ALT) -> True)
  where
    TB_INPUT_ALT = Tb_input_mode Termbox.Bindings.C._TB_INPUT_ALT

pattern TB_INPUT_ESC :: Tb_input_mode
pattern TB_INPUT_ESC <-
  ((== Tb_input_mode Termbox.Bindings.C._TB_INPUT_ESC) -> True)
  where
    TB_INPUT_ESC = Tb_input_mode Termbox.Bindings.C._TB_INPUT_ESC

pattern TB_INPUT_MOUSE :: Tb_input_mode
pattern TB_INPUT_MOUSE <-
  ((== Tb_input_mode Termbox.Bindings.C._TB_INPUT_MOUSE) -> True)
  where
    TB_INPUT_MOUSE = Tb_input_mode Termbox.Bindings.C._TB_INPUT_MOUSE

{-# COMPLETE TB_INPUT_CURRENT, TB_INPUT_ALT, TB_INPUT_ESC, TB_INPUT_MOUSE #-}

newtype Tb_output_mode
  = Tb_output_mode CInt
  deriving stock (Eq)

instance Show Tb_output_mode where
  show = \case
    TB_OUTPUT_CURRENT -> "TB_OUTPUT_CURRENT"
    TB_OUTPUT_216 -> "TB_OUTPUT_216"
    TB_OUTPUT_256 -> "TB_OUTPUT_256"
    TB_OUTPUT_GRAYSCALE -> "TB_OUTPUT_GRAYSCALE"
    TB_OUTPUT_NORMAL -> "TB_OUTPUT_NORMAL"

pattern TB_OUTPUT_CURRENT :: Tb_output_mode
pattern TB_OUTPUT_CURRENT <-
  ((== Tb_output_mode Termbox.Bindings.C._TB_OUTPUT_CURRENT) -> True)
  where
    TB_OUTPUT_CURRENT = Tb_output_mode Termbox.Bindings.C._TB_OUTPUT_CURRENT

pattern TB_OUTPUT_216 :: Tb_output_mode
pattern TB_OUTPUT_216 <-
  ((== Tb_output_mode Termbox.Bindings.C._TB_OUTPUT_216) -> True)
  where
    TB_OUTPUT_216 = Tb_output_mode Termbox.Bindings.C._TB_OUTPUT_216

pattern TB_OUTPUT_256 :: Tb_output_mode
pattern TB_OUTPUT_256 <-
  ((== Tb_output_mode Termbox.Bindings.C._TB_OUTPUT_256) -> True)
  where
    TB_OUTPUT_256 = Tb_output_mode Termbox.Bindings.C._TB_OUTPUT_256

pattern TB_OUTPUT_GRAYSCALE :: Tb_output_mode
pattern TB_OUTPUT_GRAYSCALE <-
  ((== Tb_output_mode Termbox.Bindings.C._TB_OUTPUT_GRAYSCALE) -> True)
  where
    TB_OUTPUT_GRAYSCALE = Tb_output_mode Termbox.Bindings.C._TB_OUTPUT_GRAYSCALE

pattern TB_OUTPUT_NORMAL :: Tb_output_mode
pattern TB_OUTPUT_NORMAL <-
  ((== Tb_output_mode Termbox.Bindings.C._TB_OUTPUT_NORMAL) -> True)
  where
    TB_OUTPUT_NORMAL = Tb_output_mode Termbox.Bindings.C._TB_OUTPUT_NORMAL

{-# COMPLETE TB_OUTPUT_CURRENT, TB_OUTPUT_216, TB_OUTPUT_256, TB_OUTPUT_GRAYSCALE, TB_OUTPUT_NORMAL #-}

--

cintToInt :: CInt -> Int
cintToInt = fromIntegral
{-# INLINE cintToInt #-}

intToCInt :: Int -> CInt
intToCInt = fromIntegral
{-# INLINE intToCInt #-}

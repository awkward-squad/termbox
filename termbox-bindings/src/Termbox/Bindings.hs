module Termbox.Bindings
  ( -- * Functions

    -- ** Initialize / shutdown
    tb_init,
    tb_init_fd,
    tb_init_file,
    Termbox.Bindings.C.tb_shutdown,

    -- ** Get\/set input\/output mode
    tb_select_input_mode,
    tb_select_output_mode,

    -- ** Get terminal dimensions
    tb_width,
    tb_height,

    -- ** Poll for events
    tb_peek_event,
    tb_poll_event,

    -- ** Style a color
    tb_attr,

    -- ** Set a cell
    tb_set_cursor,
    tb_put_cell,
    tb_change_cell,

    -- ** Clear and synchronize the back buffer
    Termbox.Bindings.C.tb_clear,
    tb_set_clear_attributes,
    Termbox.Bindings.C.tb_present,

    -- * Types
    Tb_attr
      ( Tb_attr,
        TB_BOLD,
        TB_REVERSE,
        TB_UNDERLINE
      ),
    Tb_cell (..),
    Tb_color
      ( Tb_color,
        TB_DEFAULT,
        TB_BLACK,
        TB_BLUE,
        TB_CYAN,
        TB_GREEN,
        TB_MAGENTA,
        TB_RED,
        TB_WHITE,
        TB_YELLOW
      ),
    Tb_event (..),
    Tb_event_mod
      ( Tb_event_mod,
        TB_MOD_ALT,
        TB_MOD_MOTION
      ),
    Tb_event_type
      ( Tb_event_type,
        TB_EVENT_KEY,
        TB_EVENT_MOUSE,
        TB_EVENT_RESIZE
      ),
    Tb_input_mode
      ( Tb_input_mode,
        TB_INPUT_CURRENT,
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
      ( Tb_output_mode,
        TB_OUTPUT_CURRENT,
        TB_OUTPUT_216,
        TB_OUTPUT_256,
        TB_OUTPUT_GRAYSCALE,
        TB_OUTPUT_NORMAL
      ),

    -- * Constants

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
import Data.Int (Int32)
import Data.Word (Word16, Word32, Word8)
import Foreign.C.Error (Errno, getErrno)
import Foreign.C.String (withCString)
import Foreign.C.Types (CInt)
import Foreign.Marshal.Alloc (alloca)
import qualified Foreign.Storable as Storable
import System.Posix.Types (Fd (Fd))
import qualified Termbox.Bindings.C
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (mod)

------------------------------------------------------------------------------------------------------------------------
-- Functions

-- | Set the attribute of a color.
tb_attr :: Tb_attr -> Tb_color -> Tb_color
tb_attr =
  coerce ((.|.) :: Word16 -> Word16 -> Word16)

-- | Set a cell value in the back buffer.
tb_change_cell ::
  -- | x
  Int ->
  -- | y
  Int ->
  -- | ch
  Word32 ->
  -- | fg
  Tb_color ->
  -- | bg
  Tb_color ->
  IO ()
tb_change_cell cx cy c (Tb_color foreground) (Tb_color background) =
  Termbox.Bindings.C.tb_change_cell (intToCInt cx) (intToCInt cy) c foreground background

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

-- | Initialize the @termbox@ library.
--
-- > tb_init = tb_init_file("/dev/tty")
tb_init_file :: FilePath -> IO Int
tb_init_file file =
  withCString file \c_file ->
    cintToInt <$> Termbox.Bindings.C.tb_init_file c_file

-- | Wait for an event.
tb_peek_event ::
  -- | Timeout (in milliseconds).
  CInt ->
  IO (Either Errno (Maybe Tb_event))
tb_peek_event timeout =
  alloca \c_event -> do
    result <- Termbox.Bindings.C.tb_peek_event c_event timeout
    if result < 0
      then Left <$> getErrno
      else
        if result == 0
          then pure (Right Nothing)
          else Right . Just . ceventToEvent <$> Storable.peek c_event

-- | Wait for an event.
tb_poll_event :: IO (Either Errno Tb_event)
tb_poll_event =
  alloca \c_event -> do
    result <- Termbox.Bindings.C.tb_poll_event c_event
    if result < 0
      then Left <$> getErrno
      else Right . ceventToEvent <$> Storable.peek c_event

-- | Set a cell value in the back buffer.
tb_put_cell ::
  -- | x
  Int ->
  -- | y
  Int ->
  -- | cell
  Tb_cell ->
  IO ()
tb_put_cell cx cy cell =
  alloca \c_cell -> do
    Storable.poke c_cell (cellToCCell cell)
    Termbox.Bindings.C.tb_put_cell (intToCInt cx) (intToCInt cy) c_cell

-- | Get\/set the input mode.
tb_select_input_mode :: Tb_input_mode -> IO Tb_input_mode
tb_select_input_mode =
  coerce Termbox.Bindings.C.tb_select_input_mode

-- | Get\/set the output mode.
tb_select_output_mode :: Tb_output_mode -> IO Tb_output_mode
tb_select_output_mode =
  coerce Termbox.Bindings.C.tb_select_output_mode

-- | Set the foreground and background attributes that 'tb_clear' clears the back buffer with.
tb_set_clear_attributes ::
  -- | fg
  Tb_color ->
  -- | bg
  Tb_color ->
  IO ()
tb_set_clear_attributes (Tb_color foreground) (Tb_color background) =
  Termbox.Bindings.C.tb_set_clear_attributes foreground background

-- | Set the cursor location, or hide it.
tb_set_cursor ::
  -- | x
  Int ->
  -- | y
  Int ->
  IO ()
tb_set_cursor cx cy =
  Termbox.Bindings.C.tb_set_cursor (intToCInt cx) (intToCInt cy)

-- | Get the terminal width.
tb_width :: IO Int
tb_width =
  cintToInt <$> Termbox.Bindings.C.tb_width

------------------------------------------------------------------------------------------------------------------------
-- Objects

-- | An attribute.
newtype Tb_attr
  = Tb_attr Word16
  deriving stock (Eq, Ord, Show)

instance Semigroup Tb_attr where
  Tb_attr cx <> Tb_attr cy =
    Tb_attr (cx .|. cy)

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
  deriving stock (Eq, Ord, Show)

cellToCCell :: Tb_cell -> Termbox.Bindings.C.Tb_cell
cellToCCell =
  unsafeCoerce -- equivalent record types that only differ by newtype wrappers

-- | A color.
newtype Tb_color
  = Tb_color Word16
  deriving stock (Eq, Ord)
  deriving newtype (Num, Show)

pattern TB_DEFAULT :: Tb_color
pattern TB_DEFAULT <-
  ((== Tb_color Termbox.Bindings.C._TB_DEFAULT) -> True)
  where
    TB_DEFAULT = Tb_color Termbox.Bindings.C._TB_DEFAULT

pattern TB_BLACK :: Tb_color
pattern TB_BLACK <-
  ((== Tb_color Termbox.Bindings.C._TB_BLACK) -> True)
  where
    TB_BLACK = Tb_color Termbox.Bindings.C._TB_BLACK

pattern TB_BLUE :: Tb_color
pattern TB_BLUE <-
  ((== Tb_color Termbox.Bindings.C._TB_BLUE) -> True)
  where
    TB_BLUE = Tb_color Termbox.Bindings.C._TB_BLUE

pattern TB_CYAN :: Tb_color
pattern TB_CYAN <-
  ((== Tb_color Termbox.Bindings.C._TB_CYAN) -> True)
  where
    TB_CYAN = Tb_color Termbox.Bindings.C._TB_CYAN

pattern TB_GREEN :: Tb_color
pattern TB_GREEN <-
  ((== Tb_color Termbox.Bindings.C._TB_GREEN) -> True)
  where
    TB_GREEN = Tb_color Termbox.Bindings.C._TB_GREEN

pattern TB_MAGENTA :: Tb_color
pattern TB_MAGENTA <-
  ((== Tb_color Termbox.Bindings.C._TB_MAGENTA) -> True)
  where
    TB_MAGENTA = Tb_color Termbox.Bindings.C._TB_MAGENTA

pattern TB_RED :: Tb_color
pattern TB_RED <-
  ((== Tb_color Termbox.Bindings.C._TB_RED) -> True)
  where
    TB_RED = Tb_color Termbox.Bindings.C._TB_RED

pattern TB_WHITE :: Tb_color
pattern TB_WHITE <-
  ((== Tb_color Termbox.Bindings.C._TB_WHITE) -> True)
  where
    TB_WHITE = Tb_color Termbox.Bindings.C._TB_WHITE

pattern TB_YELLOW :: Tb_color
pattern TB_YELLOW <-
  ((== Tb_color Termbox.Bindings.C._TB_YELLOW) -> True)
  where
    TB_YELLOW = Tb_color Termbox.Bindings.C._TB_YELLOW

-- | An event.
data Tb_event = Tb_event
  { type_ :: {-# UNPACK #-} !Tb_event_type,
    mod :: {-# UNPACK #-} !Tb_event_mod,
    key :: {-# UNPACK #-} !Tb_key,
    ch :: {-# UNPACK #-} !Word32,
    w :: {-# UNPACK #-} !Int32,
    h :: {-# UNPACK #-} !Int32,
    x :: {-# UNPACK #-} !Int32,
    y :: {-# UNPACK #-} !Int32
  }
  deriving stock (Eq, Ord, Show)

ceventToEvent :: Termbox.Bindings.C.Tb_event -> Tb_event
ceventToEvent =
  unsafeCoerce -- equivalent record types that only differ by newtype wrappers

-- | An event modifier.
newtype Tb_event_mod
  = Tb_event_mod Word8
  deriving stock (Eq, Ord, Show)

pattern TB_MOD_ALT :: Tb_event_mod
pattern TB_MOD_ALT <-
  ((== Tb_event_mod Termbox.Bindings.C._TB_MOD_ALT) -> True)
  where
    TB_MOD_ALT = Tb_event_mod Termbox.Bindings.C._TB_MOD_ALT

pattern TB_MOD_MOTION :: Tb_event_mod
pattern TB_MOD_MOTION <-
  ((== Tb_event_mod Termbox.Bindings.C._TB_MOD_MOTION) -> True)
  where
    TB_MOD_MOTION = Tb_event_mod Termbox.Bindings.C._TB_MOD_MOTION

-- | An event type.
newtype Tb_event_type
  = Tb_event_type Word8
  deriving stock (Eq, Ord)

instance Show Tb_event_type where
  show = \case
    TB_EVENT_KEY -> "TB_EVENT_KEY"
    TB_EVENT_MOUSE -> "TB_EVENT_MOUSE"
    TB_EVENT_RESIZE -> "TB_EVENT_RESIZE"

pattern TB_EVENT_KEY :: Tb_event_type
pattern TB_EVENT_KEY <-
  ((== Tb_event_type Termbox.Bindings.C._TB_EVENT_KEY) -> True)
  where
    TB_EVENT_KEY = Tb_event_type Termbox.Bindings.C._TB_EVENT_KEY

pattern TB_EVENT_MOUSE :: Tb_event_type
pattern TB_EVENT_MOUSE <-
  ((== Tb_event_type Termbox.Bindings.C._TB_EVENT_MOUSE) -> True)
  where
    TB_EVENT_MOUSE = Tb_event_type Termbox.Bindings.C._TB_EVENT_MOUSE

pattern TB_EVENT_RESIZE :: Tb_event_type
pattern TB_EVENT_RESIZE <-
  ((== Tb_event_type Termbox.Bindings.C._TB_EVENT_RESIZE) -> True)
  where
    TB_EVENT_RESIZE = Tb_event_type Termbox.Bindings.C._TB_EVENT_RESIZE

{-# COMPLETE TB_EVENT_KEY, TB_EVENT_MOUSE, TB_EVENT_RESIZE #-}

-- | The input mode.
newtype Tb_input_mode
  = Tb_input_mode CInt
  deriving stock (Eq, Ord)

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

newtype Tb_key
  = Tb_key Word16
  deriving stock (Eq, Ord)

instance Show Tb_key where
  show = \case
    TB_KEY_F1 -> "TB_KEY_F1"
    TB_KEY_F2 -> "TB_KEY_F2"
    TB_KEY_F3 -> "TB_KEY_F3"
    TB_KEY_F4 -> "TB_KEY_F4"
    TB_KEY_F5 -> "TB_KEY_F5"
    TB_KEY_F6 -> "TB_KEY_F6"
    TB_KEY_F7 -> "TB_KEY_F7"
    TB_KEY_F8 -> "TB_KEY_F8"
    TB_KEY_F9 -> "TB_KEY_F9"
    TB_KEY_F10 -> "TB_KEY_F10"
    TB_KEY_F11 -> "TB_KEY_F11"
    TB_KEY_F12 -> "TB_KEY_F12"
    TB_KEY_INSERT -> "TB_KEY_INSERT"
    TB_KEY_DELETE -> "TB_KEY_DELETE"
    TB_KEY_HOME -> "TB_KEY_HOME"
    TB_KEY_END -> "TB_KEY_END"
    TB_KEY_PGUP -> "TB_KEY_PGUP"
    TB_KEY_PGDN -> "TB_KEY_PGDN"
    TB_KEY_ARROW_UP -> "TB_KEY_ARROW_UP"
    TB_KEY_ARROW_DOWN -> "TB_KEY_ARROW_DOWN"
    TB_KEY_ARROW_LEFT -> "TB_KEY_ARROW_LEFT"
    TB_KEY_ARROW_RIGHT -> "TB_KEY_ARROW_RIGHT"
    TB_KEY_MOUSE_LEFT -> "TB_KEY_MOUSE_LEFT"
    TB_KEY_MOUSE_RIGHT -> "TB_KEY_MOUSE_RIGHT"
    TB_KEY_MOUSE_MIDDLE -> "TB_KEY_MOUSE_MIDDLE"
    TB_KEY_MOUSE_RELEASE -> "TB_KEY_MOUSE_RELEASE"
    TB_KEY_MOUSE_WHEEL_UP -> "TB_KEY_MOUSE_WHEEL_UP"
    TB_KEY_MOUSE_WHEEL_DOWN -> "TB_KEY_MOUSE_WHEEL_DOWN"
    TB_KEY_CTRL_TILDE -> "TB_KEY_CTRL_TILDE"
    TB_KEY_CTRL_A -> "TB_KEY_CTRL_A"
    TB_KEY_CTRL_B -> "TB_KEY_CTRL_B"
    TB_KEY_CTRL_C -> "TB_KEY_CTRL_C"
    TB_KEY_CTRL_D -> "TB_KEY_CTRL_D"
    TB_KEY_CTRL_E -> "TB_KEY_CTRL_E"
    TB_KEY_CTRL_F -> "TB_KEY_CTRL_F"
    TB_KEY_CTRL_G -> "TB_KEY_CTRL_G"
    TB_KEY_BACKSPACE -> "TB_KEY_BACKSPACE"
    TB_KEY_TAB -> "TB_KEY_TAB"
    TB_KEY_CTRL_J -> "TB_KEY_CTRL_J"
    TB_KEY_CTRL_K -> "TB_KEY_CTRL_K"
    TB_KEY_CTRL_L -> "TB_KEY_CTRL_L"
    TB_KEY_ENTER -> "TB_KEY_ENTER"
    TB_KEY_CTRL_N -> "TB_KEY_CTRL_N"
    TB_KEY_CTRL_O -> "TB_KEY_CTRL_O"
    TB_KEY_CTRL_P -> "TB_KEY_CTRL_P"
    TB_KEY_CTRL_Q -> "TB_KEY_CTRL_Q"
    TB_KEY_CTRL_R -> "TB_KEY_CTRL_R"
    TB_KEY_CTRL_S -> "TB_KEY_CTRL_S"
    TB_KEY_CTRL_T -> "TB_KEY_CTRL_T"
    TB_KEY_CTRL_U -> "TB_KEY_CTRL_U"
    TB_KEY_CTRL_V -> "TB_KEY_CTRL_V"
    TB_KEY_CTRL_W -> "TB_KEY_CTRL_W"
    TB_KEY_CTRL_X -> "TB_KEY_CTRL_X"
    TB_KEY_CTRL_Y -> "TB_KEY_CTRL_Y"
    TB_KEY_CTRL_Z -> "TB_KEY_CTRL_Z"
    TB_KEY_ESC -> "TB_KEY_ESC"
    TB_KEY_CTRL_4 -> "TB_KEY_CTRL_4"
    TB_KEY_CTRL_5 -> "TB_KEY_CTRL_5"
    TB_KEY_CTRL_6 -> "TB_KEY_CTRL_6"
    TB_KEY_CTRL_7 -> "TB_KEY_CTRL_7"
    TB_KEY_SPACE -> "TB_KEY_SPACE"
    TB_KEY_BACKSPACE2 -> "TB_KEY_BACKSPACE2"
    k -> error ("Unknown key: " ++ show k)

pattern TB_KEY_ARROW_DOWN :: Tb_key
pattern TB_KEY_ARROW_DOWN <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_ARROW_DOWN) -> True)
  where
    TB_KEY_ARROW_DOWN = Tb_key Termbox.Bindings.C._TB_KEY_ARROW_DOWN

pattern TB_KEY_ARROW_LEFT :: Tb_key
pattern TB_KEY_ARROW_LEFT <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_ARROW_LEFT) -> True)
  where
    TB_KEY_ARROW_LEFT = Tb_key Termbox.Bindings.C._TB_KEY_ARROW_LEFT

pattern TB_KEY_ARROW_RIGHT :: Tb_key
pattern TB_KEY_ARROW_RIGHT <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_ARROW_RIGHT) -> True)
  where
    TB_KEY_ARROW_RIGHT = Tb_key Termbox.Bindings.C._TB_KEY_ARROW_RIGHT

pattern TB_KEY_ARROW_UP :: Tb_key
pattern TB_KEY_ARROW_UP <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_ARROW_UP) -> True)
  where
    TB_KEY_ARROW_UP = Tb_key Termbox.Bindings.C._TB_KEY_ARROW_UP

pattern TB_KEY_BACKSPACE :: Tb_key
pattern TB_KEY_BACKSPACE <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_BACKSPACE) -> True)
  where
    TB_KEY_BACKSPACE = Tb_key Termbox.Bindings.C._TB_KEY_BACKSPACE

pattern TB_KEY_BACKSPACE2 :: Tb_key
pattern TB_KEY_BACKSPACE2 <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_BACKSPACE2) -> True)
  where
    TB_KEY_BACKSPACE2 = Tb_key Termbox.Bindings.C._TB_KEY_BACKSPACE2

pattern TB_KEY_CTRL_2 :: Tb_key
pattern TB_KEY_CTRL_2 <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_CTRL_2) -> True)
  where
    TB_KEY_CTRL_2 = Tb_key Termbox.Bindings.C._TB_KEY_CTRL_2

pattern TB_KEY_CTRL_3 :: Tb_key
pattern TB_KEY_CTRL_3 <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_CTRL_3) -> True)
  where
    TB_KEY_CTRL_3 = Tb_key Termbox.Bindings.C._TB_KEY_CTRL_3

pattern TB_KEY_CTRL_4 :: Tb_key
pattern TB_KEY_CTRL_4 <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_CTRL_4) -> True)
  where
    TB_KEY_CTRL_4 = Tb_key Termbox.Bindings.C._TB_KEY_CTRL_4

pattern TB_KEY_CTRL_5 :: Tb_key
pattern TB_KEY_CTRL_5 <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_CTRL_5) -> True)
  where
    TB_KEY_CTRL_5 = Tb_key Termbox.Bindings.C._TB_KEY_CTRL_5

pattern TB_KEY_CTRL_6 :: Tb_key
pattern TB_KEY_CTRL_6 <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_CTRL_6) -> True)
  where
    TB_KEY_CTRL_6 = Tb_key Termbox.Bindings.C._TB_KEY_CTRL_6

pattern TB_KEY_CTRL_7 :: Tb_key
pattern TB_KEY_CTRL_7 <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_CTRL_7) -> True)
  where
    TB_KEY_CTRL_7 = Tb_key Termbox.Bindings.C._TB_KEY_CTRL_7

pattern TB_KEY_CTRL_8 :: Tb_key
pattern TB_KEY_CTRL_8 <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_CTRL_8) -> True)
  where
    TB_KEY_CTRL_8 = Tb_key Termbox.Bindings.C._TB_KEY_CTRL_8

pattern TB_KEY_CTRL_A :: Tb_key
pattern TB_KEY_CTRL_A <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_CTRL_A) -> True)
  where
    TB_KEY_CTRL_A = Tb_key Termbox.Bindings.C._TB_KEY_CTRL_A

pattern TB_KEY_CTRL_B :: Tb_key
pattern TB_KEY_CTRL_B <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_CTRL_B) -> True)
  where
    TB_KEY_CTRL_B = Tb_key Termbox.Bindings.C._TB_KEY_CTRL_B

pattern TB_KEY_CTRL_BACKSLASH :: Tb_key
pattern TB_KEY_CTRL_BACKSLASH <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_CTRL_BACKSLASH) -> True)
  where
    TB_KEY_CTRL_BACKSLASH = Tb_key Termbox.Bindings.C._TB_KEY_CTRL_BACKSLASH

pattern TB_KEY_CTRL_C :: Tb_key
pattern TB_KEY_CTRL_C <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_CTRL_C) -> True)
  where
    TB_KEY_CTRL_C = Tb_key Termbox.Bindings.C._TB_KEY_CTRL_C

pattern TB_KEY_CTRL_D :: Tb_key
pattern TB_KEY_CTRL_D <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_CTRL_D) -> True)
  where
    TB_KEY_CTRL_D = Tb_key Termbox.Bindings.C._TB_KEY_CTRL_D

pattern TB_KEY_CTRL_E :: Tb_key
pattern TB_KEY_CTRL_E <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_CTRL_E) -> True)
  where
    TB_KEY_CTRL_E = Tb_key Termbox.Bindings.C._TB_KEY_CTRL_E

pattern TB_KEY_CTRL_F :: Tb_key
pattern TB_KEY_CTRL_F <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_CTRL_F) -> True)
  where
    TB_KEY_CTRL_F = Tb_key Termbox.Bindings.C._TB_KEY_CTRL_F

pattern TB_KEY_CTRL_G :: Tb_key
pattern TB_KEY_CTRL_G <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_CTRL_G) -> True)
  where
    TB_KEY_CTRL_G = Tb_key Termbox.Bindings.C._TB_KEY_CTRL_G

pattern TB_KEY_CTRL_H :: Tb_key
pattern TB_KEY_CTRL_H <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_CTRL_H) -> True)
  where
    TB_KEY_CTRL_H = Tb_key Termbox.Bindings.C._TB_KEY_CTRL_H

pattern TB_KEY_CTRL_I :: Tb_key
pattern TB_KEY_CTRL_I <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_CTRL_I) -> True)
  where
    TB_KEY_CTRL_I = Tb_key Termbox.Bindings.C._TB_KEY_CTRL_I

pattern TB_KEY_CTRL_J :: Tb_key
pattern TB_KEY_CTRL_J <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_CTRL_J) -> True)
  where
    TB_KEY_CTRL_J = Tb_key Termbox.Bindings.C._TB_KEY_CTRL_J

pattern TB_KEY_CTRL_K :: Tb_key
pattern TB_KEY_CTRL_K <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_CTRL_K) -> True)
  where
    TB_KEY_CTRL_K = Tb_key Termbox.Bindings.C._TB_KEY_CTRL_K

pattern TB_KEY_CTRL_L :: Tb_key
pattern TB_KEY_CTRL_L <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_CTRL_L) -> True)
  where
    TB_KEY_CTRL_L = Tb_key Termbox.Bindings.C._TB_KEY_CTRL_L

pattern TB_KEY_CTRL_LSQ_BRACKET :: Tb_key
pattern TB_KEY_CTRL_LSQ_BRACKET <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_CTRL_LSQ_BRACKET) -> True)
  where
    TB_KEY_CTRL_LSQ_BRACKET = Tb_key Termbox.Bindings.C._TB_KEY_CTRL_LSQ_BRACKET

pattern TB_KEY_CTRL_M :: Tb_key
pattern TB_KEY_CTRL_M <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_CTRL_M) -> True)
  where
    TB_KEY_CTRL_M = Tb_key Termbox.Bindings.C._TB_KEY_CTRL_M

pattern TB_KEY_CTRL_N :: Tb_key
pattern TB_KEY_CTRL_N <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_CTRL_N) -> True)
  where
    TB_KEY_CTRL_N = Tb_key Termbox.Bindings.C._TB_KEY_CTRL_N

pattern TB_KEY_CTRL_O :: Tb_key
pattern TB_KEY_CTRL_O <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_CTRL_O) -> True)
  where
    TB_KEY_CTRL_O = Tb_key Termbox.Bindings.C._TB_KEY_CTRL_O

pattern TB_KEY_CTRL_P :: Tb_key
pattern TB_KEY_CTRL_P <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_CTRL_P) -> True)
  where
    TB_KEY_CTRL_P = Tb_key Termbox.Bindings.C._TB_KEY_CTRL_P

pattern TB_KEY_CTRL_Q :: Tb_key
pattern TB_KEY_CTRL_Q <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_CTRL_Q) -> True)
  where
    TB_KEY_CTRL_Q = Tb_key Termbox.Bindings.C._TB_KEY_CTRL_Q

pattern TB_KEY_CTRL_R :: Tb_key
pattern TB_KEY_CTRL_R <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_CTRL_R) -> True)
  where
    TB_KEY_CTRL_R = Tb_key Termbox.Bindings.C._TB_KEY_CTRL_R

pattern TB_KEY_CTRL_RSQ_BRACKET :: Tb_key
pattern TB_KEY_CTRL_RSQ_BRACKET <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_CTRL_RSQ_BRACKET) -> True)
  where
    TB_KEY_CTRL_RSQ_BRACKET = Tb_key Termbox.Bindings.C._TB_KEY_CTRL_RSQ_BRACKET

pattern TB_KEY_CTRL_S :: Tb_key
pattern TB_KEY_CTRL_S <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_CTRL_S) -> True)
  where
    TB_KEY_CTRL_S = Tb_key Termbox.Bindings.C._TB_KEY_CTRL_S

pattern TB_KEY_CTRL_SLASH :: Tb_key
pattern TB_KEY_CTRL_SLASH <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_CTRL_SLASH) -> True)
  where
    TB_KEY_CTRL_SLASH = Tb_key Termbox.Bindings.C._TB_KEY_CTRL_SLASH

pattern TB_KEY_CTRL_T :: Tb_key
pattern TB_KEY_CTRL_T <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_CTRL_T) -> True)
  where
    TB_KEY_CTRL_T = Tb_key Termbox.Bindings.C._TB_KEY_CTRL_T

pattern TB_KEY_CTRL_TILDE :: Tb_key
pattern TB_KEY_CTRL_TILDE <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_CTRL_TILDE) -> True)
  where
    TB_KEY_CTRL_TILDE = Tb_key Termbox.Bindings.C._TB_KEY_CTRL_TILDE

pattern TB_KEY_CTRL_U :: Tb_key
pattern TB_KEY_CTRL_U <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_CTRL_U) -> True)
  where
    TB_KEY_CTRL_U = Tb_key Termbox.Bindings.C._TB_KEY_CTRL_U

pattern TB_KEY_CTRL_UNDERSCORE :: Tb_key
pattern TB_KEY_CTRL_UNDERSCORE <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_CTRL_UNDERSCORE) -> True)
  where
    TB_KEY_CTRL_UNDERSCORE = Tb_key Termbox.Bindings.C._TB_KEY_CTRL_UNDERSCORE

pattern TB_KEY_CTRL_V :: Tb_key
pattern TB_KEY_CTRL_V <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_CTRL_V) -> True)
  where
    TB_KEY_CTRL_V = Tb_key Termbox.Bindings.C._TB_KEY_CTRL_V

pattern TB_KEY_CTRL_W :: Tb_key
pattern TB_KEY_CTRL_W <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_CTRL_W) -> True)
  where
    TB_KEY_CTRL_W = Tb_key Termbox.Bindings.C._TB_KEY_CTRL_W

pattern TB_KEY_CTRL_X :: Tb_key
pattern TB_KEY_CTRL_X <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_CTRL_X) -> True)
  where
    TB_KEY_CTRL_X = Tb_key Termbox.Bindings.C._TB_KEY_CTRL_X

pattern TB_KEY_CTRL_Y :: Tb_key
pattern TB_KEY_CTRL_Y <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_CTRL_Y) -> True)
  where
    TB_KEY_CTRL_Y = Tb_key Termbox.Bindings.C._TB_KEY_CTRL_Y

pattern TB_KEY_CTRL_Z :: Tb_key
pattern TB_KEY_CTRL_Z <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_CTRL_Z) -> True)
  where
    TB_KEY_CTRL_Z = Tb_key Termbox.Bindings.C._TB_KEY_CTRL_Z

pattern TB_KEY_DELETE :: Tb_key
pattern TB_KEY_DELETE <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_DELETE) -> True)
  where
    TB_KEY_DELETE = Tb_key Termbox.Bindings.C._TB_KEY_DELETE

pattern TB_KEY_END :: Tb_key
pattern TB_KEY_END <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_END) -> True)
  where
    TB_KEY_END = Tb_key Termbox.Bindings.C._TB_KEY_END

pattern TB_KEY_ENTER :: Tb_key
pattern TB_KEY_ENTER <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_ENTER) -> True)
  where
    TB_KEY_ENTER = Tb_key Termbox.Bindings.C._TB_KEY_ENTER

pattern TB_KEY_ESC :: Tb_key
pattern TB_KEY_ESC <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_ESC) -> True)
  where
    TB_KEY_ESC = Tb_key Termbox.Bindings.C._TB_KEY_ESC

pattern TB_KEY_F1 :: Tb_key
pattern TB_KEY_F1 <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_F1) -> True)
  where
    TB_KEY_F1 = Tb_key Termbox.Bindings.C._TB_KEY_F1

pattern TB_KEY_F10 :: Tb_key
pattern TB_KEY_F10 <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_F10) -> True)
  where
    TB_KEY_F10 = Tb_key Termbox.Bindings.C._TB_KEY_F10

pattern TB_KEY_F11 :: Tb_key
pattern TB_KEY_F11 <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_F11) -> True)
  where
    TB_KEY_F11 = Tb_key Termbox.Bindings.C._TB_KEY_F11

pattern TB_KEY_F12 :: Tb_key
pattern TB_KEY_F12 <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_F12) -> True)
  where
    TB_KEY_F12 = Tb_key Termbox.Bindings.C._TB_KEY_F12

pattern TB_KEY_F2 :: Tb_key
pattern TB_KEY_F2 <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_F2) -> True)
  where
    TB_KEY_F2 = Tb_key Termbox.Bindings.C._TB_KEY_F2

pattern TB_KEY_F3 :: Tb_key
pattern TB_KEY_F3 <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_F3) -> True)
  where
    TB_KEY_F3 = Tb_key Termbox.Bindings.C._TB_KEY_F3

pattern TB_KEY_F4 :: Tb_key
pattern TB_KEY_F4 <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_F4) -> True)
  where
    TB_KEY_F4 = Tb_key Termbox.Bindings.C._TB_KEY_F4

pattern TB_KEY_F5 :: Tb_key
pattern TB_KEY_F5 <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_F5) -> True)
  where
    TB_KEY_F5 = Tb_key Termbox.Bindings.C._TB_KEY_F5

pattern TB_KEY_F6 :: Tb_key
pattern TB_KEY_F6 <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_F6) -> True)
  where
    TB_KEY_F6 = Tb_key Termbox.Bindings.C._TB_KEY_F6

pattern TB_KEY_F7 :: Tb_key
pattern TB_KEY_F7 <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_F7) -> True)
  where
    TB_KEY_F7 = Tb_key Termbox.Bindings.C._TB_KEY_F7

pattern TB_KEY_F8 :: Tb_key
pattern TB_KEY_F8 <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_F8) -> True)
  where
    TB_KEY_F8 = Tb_key Termbox.Bindings.C._TB_KEY_F8

pattern TB_KEY_F9 :: Tb_key
pattern TB_KEY_F9 <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_F9) -> True)
  where
    TB_KEY_F9 = Tb_key Termbox.Bindings.C._TB_KEY_F9

pattern TB_KEY_HOME :: Tb_key
pattern TB_KEY_HOME <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_HOME) -> True)
  where
    TB_KEY_HOME = Tb_key Termbox.Bindings.C._TB_KEY_HOME

pattern TB_KEY_INSERT :: Tb_key
pattern TB_KEY_INSERT <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_INSERT) -> True)
  where
    TB_KEY_INSERT = Tb_key Termbox.Bindings.C._TB_KEY_INSERT

pattern TB_KEY_MOUSE_LEFT :: Tb_key
pattern TB_KEY_MOUSE_LEFT <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_MOUSE_LEFT) -> True)
  where
    TB_KEY_MOUSE_LEFT = Tb_key Termbox.Bindings.C._TB_KEY_MOUSE_LEFT

pattern TB_KEY_MOUSE_MIDDLE :: Tb_key
pattern TB_KEY_MOUSE_MIDDLE <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_MOUSE_MIDDLE) -> True)
  where
    TB_KEY_MOUSE_MIDDLE = Tb_key Termbox.Bindings.C._TB_KEY_MOUSE_MIDDLE

pattern TB_KEY_MOUSE_RELEASE :: Tb_key
pattern TB_KEY_MOUSE_RELEASE <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_MOUSE_RELEASE) -> True)
  where
    TB_KEY_MOUSE_RELEASE = Tb_key Termbox.Bindings.C._TB_KEY_MOUSE_RELEASE

pattern TB_KEY_MOUSE_RIGHT :: Tb_key
pattern TB_KEY_MOUSE_RIGHT <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_MOUSE_RIGHT) -> True)
  where
    TB_KEY_MOUSE_RIGHT = Tb_key Termbox.Bindings.C._TB_KEY_MOUSE_RIGHT

pattern TB_KEY_MOUSE_WHEEL_DOWN :: Tb_key
pattern TB_KEY_MOUSE_WHEEL_DOWN <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_MOUSE_WHEEL_DOWN) -> True)
  where
    TB_KEY_MOUSE_WHEEL_DOWN = Tb_key Termbox.Bindings.C._TB_KEY_MOUSE_WHEEL_DOWN

pattern TB_KEY_MOUSE_WHEEL_UP :: Tb_key
pattern TB_KEY_MOUSE_WHEEL_UP <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_MOUSE_WHEEL_UP) -> True)
  where
    TB_KEY_MOUSE_WHEEL_UP = Tb_key Termbox.Bindings.C._TB_KEY_MOUSE_WHEEL_UP

pattern TB_KEY_PGDN :: Tb_key
pattern TB_KEY_PGDN <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_PGDN) -> True)
  where
    TB_KEY_PGDN = Tb_key Termbox.Bindings.C._TB_KEY_PGDN

pattern TB_KEY_PGUP :: Tb_key
pattern TB_KEY_PGUP <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_PGUP) -> True)
  where
    TB_KEY_PGUP = Tb_key Termbox.Bindings.C._TB_KEY_PGUP

pattern TB_KEY_SPACE :: Tb_key
pattern TB_KEY_SPACE <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_SPACE) -> True)
  where
    TB_KEY_SPACE = Tb_key Termbox.Bindings.C._TB_KEY_SPACE

pattern TB_KEY_TAB :: Tb_key
pattern TB_KEY_TAB <-
  ((== Tb_key Termbox.Bindings.C._TB_KEY_TAB) -> True)
  where
    TB_KEY_TAB = Tb_key Termbox.Bindings.C._TB_KEY_TAB

-- | The output mode.
newtype Tb_output_mode
  = Tb_output_mode CInt
  deriving stock (Eq, Ord)

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

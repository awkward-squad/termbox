-- |
--
-- This module provides a high-level wrapper around @termbox@, a simple C library for writing text-based user
-- interfaces: <https://github.com/termbox/termbox>
--
-- You may prefer to use one of the following interfaces instead:
--
-- * @<https://hackage.haskell.org/package/termbox-banana termbox-banana>@, a @reactive-banana@ FRP interface.
-- * @<https://hackage.haskell.org/package/termbox-tea termbox-tea>@, an Elm Architecture interface.
--
-- This module is intended to be imported qualified.
module Termbox
  ( -- * Termbox
    initialize,
    shutdown,
    InitError (..),

    -- * Terminal contents

    -- ** Scene
    Scene,
    render,
    cell,
    fill,
    cursor,

    -- ** Cell
    Cell,
    char,
    fg,
    bg,
    bold,
    underline,
    blink,

    -- ** Colors
    Color,

    -- *** Basic colors
    defaultColor,
    red,
    green,
    yellow,
    blue,
    magenta,
    cyan,
    white,
    bright,

    -- *** 216 miscellaneous colors
    color,

    -- *** 24 monochrome colors
    gray,

    -- * Event handling
    Event (..),
    Key
      ( ..,
        KeyCtrlH,
        KeyCtrlLsqBracket,
        KeyCtrl2,
        KeyCtrl3,
        KeyCtrl4,
        KeyCtrl5,
        KeyCtrl7,
        KeyCtrlM,
        KeyCtrlI,
        KeyCtrlUnderscore
      ),
    Mouse (..),
    poll,

    -- * Miscellaneous types
    Pos (..),
    posUp,
    posDown,
    posLeft,
    posRight,
    Size (..),
    getSize,
  )
where

import Termbox.Internal.Cell (Cell, bg, blink, bold, char, fg, underline)
import Termbox.Internal.Color
  ( Color,
    blue,
    bright,
    color,
    cyan,
    defaultColor,
    gray,
    green,
    magenta,
    red,
    white,
    yellow,
  )
import Termbox.Internal.Event (Event (..), poll)
import Termbox.Internal.Key
  ( Key (..),
    pattern KeyCtrl2,
    pattern KeyCtrl3,
    pattern KeyCtrl4,
    pattern KeyCtrl5,
    pattern KeyCtrl7,
    pattern KeyCtrlH,
    pattern KeyCtrlI,
    pattern KeyCtrlLsqBracket,
    pattern KeyCtrlM,
    pattern KeyCtrlUnderscore,
  )
import Termbox.Internal.Main (InitError (..), initialize, shutdown)
import Termbox.Internal.Mouse
  ( Mouse
      ( MouseLeft,
        MouseMiddle,
        MouseRelease,
        MouseRight,
        MouseWheelDown,
        MouseWheelUp
      ),
  )
import Termbox.Internal.Pos (Pos (..), posDown, posLeft, posRight, posUp)
import Termbox.Internal.Scene (Scene, cell, cursor, fill, render)
import Termbox.Internal.Size (Size (..), getSize)

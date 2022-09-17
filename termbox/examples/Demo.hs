module Main (main) where

import Data.Foldable (fold)
import Data.Void (Void)
import Foreign.C.Error (Errno)
import qualified Termbox

main :: IO ()
main = do
  result <- Termbox.run Termbox.Program {initialize, pollEvent, handleEvent, handleEventError, render, finished}
  case result of
    Left err -> print err
    Right _state -> pure ()

data State = State
  { lastEvent :: Maybe (Termbox.Event Void),
    bright :: Bool
  }

initialize :: Termbox.Size -> State
initialize _size =
  State
    { lastEvent = Nothing,
      bright = False
    }

pollEvent :: Maybe (IO Void)
pollEvent =
  Nothing

handleEvent :: State -> Termbox.Event Void -> IO State
handleEvent State {bright} event =
  pure
    State
      { lastEvent = Just event,
        bright =
          case event of
            Termbox.EventKey (Termbox.KeyChar '*') -> not bright
            _ -> bright
      }

handleEventError :: State -> Errno -> IO State
handleEventError state _err =
  pure state

render :: State -> Termbox.Scene
render State {lastEvent, bright} =
  fold
    [ string Termbox.Pos {row = 1, col = 2} "Welcome to Termbox. Try typing, clicking, and resizing the terminal!",
      string Termbox.Pos {row = 3, col = 2} "Latest event: ",
      case lastEvent of
        Nothing -> mempty
        Just event -> string Termbox.Pos {row = 3, col = 16} (map (Termbox.bold . Termbox.char) (show event)),
      string Termbox.Pos {row = 5, col = 2} "default, red, green, yellow, blue, magenta, cyan, white",
      foldMap
        ( \(i, color) ->
            let width = 4
             in rect
                  Rect
                    { pos = Termbox.Pos {row = 6, col = 2 + (i * width)},
                      size = Termbox.Size {width, height = 2},
                      color = if bright then Termbox.bright color else color
                    }
        )
        ( zip
            [0 :: Int ..]
            [ Termbox.defaultColor,
              Termbox.red,
              Termbox.green,
              Termbox.yellow,
              Termbox.blue,
              Termbox.magenta,
              Termbox.cyan,
              Termbox.white
            ]
        ),
      string
        Termbox.Pos {row = 6, col = 35}
        ("Press " ++ [Termbox.bold (Termbox.char '*')] ++ " to toggle brightness."),
      string
        Termbox.Pos {row = 7, col = 35}
        ( let selected = map (Termbox.bg (Termbox.gray 20) . Termbox.fg (Termbox.gray 0))
           in if bright
                then "normal " ++ selected "bright"
                else selected "normal" ++ " bright"
        ),
      string Termbox.Pos {row = 9, col = 2} "color 0 .. color 215",
      let coords :: [Termbox.Pos]
          coords = do
            row <- [10, 12 ..]
            col <- take 32 [2, 6 ..]
            pure Termbox.Pos {row, col}
       in fold
            ( zipWith
                ( \i pos ->
                    fold
                      [ rect
                          Rect
                            { pos,
                              size = Termbox.Size {width = 4, height = 2},
                              color = Termbox.color i
                            },
                        string pos (map (Termbox.bg (Termbox.color i) . Termbox.char) (show i))
                      ]
                )
                [(0 :: Int) .. 215]
                coords
            ),
      string Termbox.Pos {row = 25, col = 2} "gray 0 .. gray 23",
      foldMap
        ( \i ->
            let pos = Termbox.Pos {row = 26, col = 2 + (i * width)}
                width = 4
             in fold
                  [ rect
                      Rect
                        { pos,
                          size = Termbox.Size {width, height = 2},
                          color = Termbox.gray i
                        },
                    string pos (map (Termbox.bg (Termbox.gray i) . Termbox.char) (show i))
                  ]
        )
        [0 .. 23],
      string Termbox.Pos {row = 29, col = 2} (map Termbox.bold "This text is bold."),
      string Termbox.Pos {row = 29, col = 21} (map Termbox.underline "This text is underlined."),
      string Termbox.Pos {row = 29, col = 46} (map Termbox.blink "This text is blinking (maybe)."),
      string Termbox.Pos {row = 31, col = 2} "Press Esc to quit!"
    ]

finished :: State -> Bool
finished State {lastEvent} =
  case lastEvent of
    Just (Termbox.EventKey Termbox.KeyEsc) -> True
    _ -> False

string :: Termbox.Pos -> [Termbox.Cell] -> Termbox.Scene
string Termbox.Pos {row, col = col0} =
  mconcat . zipWith (\col c -> Termbox.cell Termbox.Pos {col, row} c) [col0 ..]

data Rect = Rect
  { pos :: Termbox.Pos,
    size :: Termbox.Size,
    color :: Termbox.Color
  }

rect :: Rect -> Termbox.Scene
rect Rect {pos = Termbox.Pos {row = row0, col = col0}, size = Termbox.Size {width, height}, color} =
  foldMap
    (\(col, row) -> Termbox.cell Termbox.Pos {row, col} (Termbox.bg color (Termbox.char ' ')))
    ((,) <$> [col0 .. col0 + width - 1] <*> [row0 .. row0 + height - 1])

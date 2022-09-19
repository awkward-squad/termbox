module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Monad (forever)
import Foreign.C.Error (Errno)
import GHC.Clock (getMonotonicTime)
import qualified Ki
import qualified Termbox
import Text.Printf (printf)

main :: IO ()
main = do
  t0 <- getMonotonicTime
  result <-
    Ki.scoped \scope -> do
      timeVar <- newEmptyMVar
      Ki.fork_ scope do
        forever do
          threadDelay 100_000
          getMonotonicTime >>= putMVar timeVar
      Termbox.run
        Termbox.Program
          { initialize,
            pollEvent = pollEvent t0 timeVar,
            handleEvent = handleEvent,
            handleEventError,
            render,
            finished
          }
  case result of
    Left err -> print err
    Right _state -> pure ()

data State = State
  { elapsed :: Double,
    lastKey :: Maybe Termbox.Key,
    bright :: Bool
  }

initialize :: Termbox.Size -> State
initialize _size =
  State
    { elapsed = 0,
      lastKey = Nothing,
      bright = False
    }

pollEvent :: Double -> MVar Double -> Maybe (IO Double)
pollEvent t0 timeVar =
  Just do
    t1 <- takeMVar timeVar
    pure (t1 - t0)

handleEvent :: State -> Termbox.Event Double -> IO State
handleEvent State {elapsed, lastKey, bright} = \case
  Termbox.EventKey key ->
    pure
      State
        { elapsed,
          lastKey = Just key,
          bright =
            case key of
              Termbox.KeyChar '*' -> not bright
              _ -> bright
        }
  Termbox.EventUser elapsed1 ->
    pure
      State
        { elapsed = elapsed1,
          lastKey,
          bright
        }
  _ ->
    pure
      State
        { elapsed,
          lastKey,
          bright
        }

handleEventError :: State -> Errno -> IO State
handleEventError state _err =
  pure state

render :: State -> Termbox.Scene
render State {elapsed, lastKey, bright} =
  renderBox Termbox.Pos {row = 1, col = 2} $
    vcat
      [ string "Welcome to Termbox. Try typing, clicking, and resizing the terminal!",
        string " ",
        hcat
          [ string "Elapsed time: ",
            string (map Termbox.char (printf "%.1fs" elapsed))
          ],
        hcat
          [ string "Latest key press: ",
            case lastKey of
              Nothing -> emptyBox
              Just event -> string (map (Termbox.bold . Termbox.char) (show event))
          ],
        string " ",
        string "default red green yellow blue magenta cyan white",
        hcat
          [ rect
              Rect
                { size = Termbox.Size {width = 7, height = 2},
                  color = brighten Termbox.defaultColor
                },
            string " ",
            rect
              Rect
                { size = Termbox.Size {width = 3, height = 2},
                  color = brighten Termbox.red
                },
            string " ",
            rect
              Rect
                { size = Termbox.Size {width = 5, height = 2},
                  color = brighten Termbox.green
                },
            string " ",
            rect
              Rect
                { size = Termbox.Size {width = 6, height = 2},
                  color = brighten Termbox.yellow
                },
            string " ",
            rect
              Rect
                { size = Termbox.Size {width = 4, height = 2},
                  color = brighten Termbox.blue
                },
            string " ",
            rect
              Rect
                { size = Termbox.Size {width = 7, height = 2},
                  color = brighten Termbox.magenta
                },
            string " ",
            rect
              Rect
                { size = Termbox.Size {width = 4, height = 2},
                  color = brighten Termbox.cyan
                },
            string " ",
            rect
              Rect
                { size = Termbox.Size {width = 5, height = 2},
                  color = brighten Termbox.white
                },
            string " ",
            vcat
              [ string ("Press " ++ [Termbox.bold (Termbox.char '*')] ++ " to toggle brightness."),
                string
                  ( let selected = map (Termbox.bg (Termbox.gray 20) . Termbox.fg (Termbox.gray 0))
                     in if bright
                          then "normal " ++ selected "bright"
                          else selected "normal" ++ " bright"
                  )
              ]
          ],
        string " ",
        string "color 0 .. color 215",
        vcat
          ( map
              ( \is ->
                  hcat
                    ( map
                        ( \i ->
                            acat
                              [ rect
                                  Rect
                                    { size = Termbox.Size {width = 4, height = 2},
                                      color = Termbox.color i
                                    },
                                string (map (Termbox.bg (Termbox.color i) . Termbox.char) (show i))
                              ]
                        )
                        is
                    )
              )
              (chunksOf 24 [0 .. 215])
          ),
        string " ",
        string "gray 0 .. gray 23",
        hcat
          ( map
              ( \i ->
                  acat
                    [ rect
                        Rect
                          { size = Termbox.Size {width = 4, height = 2},
                            color = Termbox.gray i
                          },
                      string (map (Termbox.bg (Termbox.gray i) . Termbox.char) (show i))
                    ]
              )
              [0 .. 23]
          ),
        string " ",
        hcat
          [ string (map Termbox.bold "This text is bold."),
            string " ",
            string (map Termbox.underline "This text is underlined."),
            string " ",
            string (map Termbox.blink "This text is blinking (maybe).")
          ],
        string " ",
        hcat
          [ string "Press ",
            string (map Termbox.bold "Esc"),
            string " to quit!"
          ]
      ]
  where
    brighten :: Termbox.Color -> Termbox.Color
    brighten =
      if bright
        then Termbox.bright
        else id

finished :: State -> Bool
finished State {lastKey} =
  case lastKey of
    Just Termbox.KeyEsc -> True
    _ -> False

data Box
  = Box !Termbox.Size Content

data Content
  = E
  | O !Termbox.Cell
  | A !Box !Box
  | H !Box !Box
  | V !Box !Box

emptyBox :: Box
emptyBox =
  Box (Termbox.Size 0 0) E

one :: Termbox.Cell -> Box
one cell =
  Box (Termbox.Size 1 1) (O cell) -- assume it's not an empty cell *shrug*

acat :: [Box] -> Box
acat =
  foldr f emptyBox
  where
    f :: Box -> Box -> Box
    f box1@(Box (Termbox.Size w1 h1) _) box2@(Box (Termbox.Size w2 h2) _) =
      Box (Termbox.Size (max w1 w2) (max h1 h2)) (A box1 box2)

hcat :: [Box] -> Box
hcat =
  foldr f emptyBox
  where
    f :: Box -> Box -> Box
    f box1@(Box (Termbox.Size w1 h1) _) box2@(Box (Termbox.Size w2 h2) _) =
      Box (Termbox.Size (w1 + w2) (max h1 h2)) (H box1 box2)

vcat :: [Box] -> Box
vcat =
  foldr f emptyBox
  where
    f :: Box -> Box -> Box
    f box1@(Box (Termbox.Size w1 h1) _) box2@(Box (Termbox.Size w2 h2) _) =
      Box (Termbox.Size (max w1 w2) (h1 + h2)) (V box1 box2)

renderBox :: Termbox.Pos -> Box -> Termbox.Scene
renderBox pos (Box _ content) =
  case content of
    E -> mempty
    O cell -> Termbox.cell pos cell
    A box1 box2 -> renderBox pos box1 <> renderBox pos box2
    H box1@(Box (Termbox.Size w1 _) _) box2 -> renderBox pos box1 <> renderBox (Termbox.posRight w1 pos) box2
    V box1@(Box (Termbox.Size _ h1) _) box2 -> renderBox pos box1 <> renderBox (Termbox.posDown h1 pos) box2

string :: [Termbox.Cell] -> Box
string =
  hcat . map one

data Rect = Rect
  { size :: Termbox.Size,
    color :: Termbox.Color
  }

rect :: Rect -> Box
rect Rect {size = Termbox.Size {width, height}, color} =
  vcat (replicate height (string (replicate width (Termbox.bg color (Termbox.char ' ')))))

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = \case
  [] -> []
  xs ->
    let (ys, zs) = splitAt n xs
     in ys : chunksOf n zs

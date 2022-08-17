{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Foldable (fold)
import qualified Termbox

main :: IO ()
main =
  Termbox.run \_pos render poll -> do
    let loop :: Maybe Termbox.Event -> IO ()
        loop lastEvent = do
          render (scene lastEvent)
          poll >>= \case
            Termbox.EventKey Termbox.KeyEsc -> pure ()
            event -> loop (Just event)
    loop Nothing
  where
    scene :: Maybe Termbox.Event -> Termbox.Scene
    scene maybeEvent =
      fold
        [ string 2 1 "Try typing, clicking, and resizing the terminal!",
          fold
            [ string 2 3 "Here's the latest event I processed:",
              case maybeEvent of
                Nothing -> mempty
                Just event -> string 39 3 (map (Termbox.bold . Termbox.char) (show event))
            ],
          string 2 7 "Let's check out what colors and styles Termbox has to offer.",
          foldMap
            ( \(i, color, name) ->
                let col = 2 + (i * w)
                    row = 9
                    w = 14
                    h = 4
                 in fold
                      [ string col row name,
                        rectangle col (row + 1) (col + w - 1) (row + h) (Termbox.bg color (Termbox.char ' '))
                      ]
            )
            [ ((0 :: Int), Termbox.black, "black"),
              (1, Termbox.red, "red"),
              (2, Termbox.green, "green"),
              (3, Termbox.yellow, "yellow"),
              (4, Termbox.blue, "blue"),
              (5, Termbox.magenta, "magenta"),
              (6, Termbox.cyan, "cyan"),
              (7, Termbox.white, "white")
            ],
          foldMap
            ( \(i, color, name) ->
                let col = 2 + ((i - 8) * w)
                    row = 15
                    w = 14
                    h = 4
                 in fold
                      [ string col row name,
                        rectangle col (row + 1) (col + w - 1) (row + h) (Termbox.bg color (Termbox.char ' '))
                      ]
            )
            [ ((8 :: Int), Termbox.brightBlack, "brightBlack"),
              (9, Termbox.brightRed, "brightRed"),
              (10, Termbox.brightGreen, "brightGreen"),
              (11, Termbox.brightYellow, "brightYellow"),
              (12, Termbox.brightBlue, "brightBlue"),
              (13, Termbox.brightMagenta, "brightMagenta"),
              (14, Termbox.brightCyan, "brightCyan"),
              (15, Termbox.brightWhite, "brightWhite")
            ],
          string 2 21 "color0 .. color215",
          let coords :: [(Int, Int)]
              coords = do
                row <- [22, 24 ..]
                col <- take 32 [2, 6 ..]
                pure (col, row)
           in fold
                ( zipWith
                    ( \i (col, row) ->
                        fold
                          [ rectangle col row (col + 3) (row + 1) (Termbox.bg (Termbox.color i) (Termbox.char ' ')),
                            string col row (map (Termbox.bg (Termbox.color i) . Termbox.char) (show i))
                          ]
                    )
                    [(0 :: Int) .. 215]
                    coords
                ),
          string 2 37 "gray0 .. gray23",
          let w :: Int
              w = 9
              h :: Int
              h = 5
              coords :: [(Int, Int)]
              coords = do
                row <- [38, 38 + h ..]
                col <- take 12 [2, 2 + w ..]
                pure (col, row)
           in fold
                ( zipWith
                    ( \i (col, row) ->
                        fold
                          [ rectangle
                              col
                              row
                              (col + w - 1)
                              (row + h - 1)
                              (Termbox.bg (Termbox.gray i) (Termbox.char ' ')),
                            string col row (map (Termbox.bg (Termbox.gray i) . Termbox.char) (show i))
                          ]
                    )
                    [(0 :: Int) .. 23]
                    coords
                ),
          string 2 49 (map Termbox.bold "This text is bold."),
          string 21 49 (map Termbox.underline "This text is underlined."),
          string 46 49 (map Termbox.blink "This text is blinking (maybe)."),
          string 2 53 "Press Esc to quit!"
        ]

string :: Int -> Int -> [Termbox.Cell] -> Termbox.Scene
string x0 row =
  mconcat . zipWith (\col c -> Termbox.set Termbox.Pos {col, row} c) [x0 ..]

rectangle :: Int -> Int -> Int -> Int -> Termbox.Cell -> Termbox.Scene
rectangle x0 y0 x1 y1 c =
  foldMap (\(col, row) -> Termbox.set Termbox.Pos {col, row} c) ((,) <$> [x0 .. x1] <*> [y0 .. y1])

import Data.Foldable (fold)
import qualified Termbox

main :: IO ()
main =
  Termbox.run \_w _h render poll -> do
    let loop :: Maybe Termbox.Event -> IO ()
        loop lastEvent = do
          render (cells lastEvent) Termbox.NoCursor
          poll >>= \case
            Termbox.EventKey Termbox.KeyEsc -> pure ()
            event -> loop (Just event)
    loop Nothing
  where
    cells :: Maybe Termbox.Event -> Termbox.Cells
    cells maybeEvent =
      fold
        [ string 2 1 Termbox.white Termbox.black "Try typing, clicking, and resizing the terminal!",
          fold
            [ string 2 3 Termbox.white Termbox.black "Here's the latest event I processed:",
              case maybeEvent of
                Nothing -> mempty
                Just event -> string 39 3 (Termbox.bold Termbox.white) Termbox.black (show event)
            ],
          string 2 7 Termbox.white Termbox.black "Let's check out what colors Termbox has to offer.",
          foldMap
            ( \(i, attr, name) ->
                let col = 2 + (i * w)
                    row = 9
                    w = 14
                    h = 4
                 in fold
                      [ string col row Termbox.white Termbox.black name,
                        rectangle col (row + 1) (col + w - 1) (row + h) (Termbox.Cell ' ' Termbox.white attr)
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
            ( \(i, attr, name) ->
                let col = 2 + ((i - 8) * w)
                    row = 15
                    w = 14
                    h = 4
                 in fold
                      [ string col row Termbox.white Termbox.black name,
                        rectangle col (row + 1) (col + w - 1) (row + h) (Termbox.Cell ' ' Termbox.white attr)
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
          string 2 21 Termbox.white Termbox.black "color0 .. color215",
          let coords :: [(Int, Int)]
              coords = do
                row <- [22, 24 ..]
                col <- take 32 [2, 6 ..]
                pure (col, row)
           in fold
                ( zipWith3
                    ( \i (col, row) attr ->
                        fold
                          [ rectangle col row (col + 3) (row + 1) (Termbox.Cell ' ' Termbox.white attr),
                            string col row Termbox.white attr (show i)
                          ]
                    )
                    [(0 :: Int) ..]
                    coords
                    [ Termbox.color0,
                      Termbox.color1,
                      Termbox.color2,
                      Termbox.color3,
                      Termbox.color4,
                      Termbox.color5,
                      Termbox.color6,
                      Termbox.color7,
                      Termbox.color8,
                      Termbox.color9,
                      Termbox.color10,
                      Termbox.color11,
                      Termbox.color12,
                      Termbox.color13,
                      Termbox.color14,
                      Termbox.color15,
                      Termbox.color16,
                      Termbox.color17,
                      Termbox.color18,
                      Termbox.color19,
                      Termbox.color20,
                      Termbox.color21,
                      Termbox.color22,
                      Termbox.color23,
                      Termbox.color24,
                      Termbox.color25,
                      Termbox.color26,
                      Termbox.color27,
                      Termbox.color28,
                      Termbox.color29,
                      Termbox.color30,
                      Termbox.color31,
                      Termbox.color32,
                      Termbox.color33,
                      Termbox.color34,
                      Termbox.color35,
                      Termbox.color36,
                      Termbox.color37,
                      Termbox.color38,
                      Termbox.color39,
                      Termbox.color40,
                      Termbox.color41,
                      Termbox.color42,
                      Termbox.color43,
                      Termbox.color44,
                      Termbox.color45,
                      Termbox.color46,
                      Termbox.color47,
                      Termbox.color48,
                      Termbox.color49,
                      Termbox.color50,
                      Termbox.color51,
                      Termbox.color52,
                      Termbox.color53,
                      Termbox.color54,
                      Termbox.color55,
                      Termbox.color56,
                      Termbox.color57,
                      Termbox.color58,
                      Termbox.color59,
                      Termbox.color60,
                      Termbox.color61,
                      Termbox.color62,
                      Termbox.color63,
                      Termbox.color64,
                      Termbox.color65,
                      Termbox.color66,
                      Termbox.color67,
                      Termbox.color68,
                      Termbox.color69,
                      Termbox.color70,
                      Termbox.color71,
                      Termbox.color72,
                      Termbox.color73,
                      Termbox.color74,
                      Termbox.color75,
                      Termbox.color76,
                      Termbox.color77,
                      Termbox.color78,
                      Termbox.color79,
                      Termbox.color80,
                      Termbox.color81,
                      Termbox.color82,
                      Termbox.color83,
                      Termbox.color84,
                      Termbox.color85,
                      Termbox.color86,
                      Termbox.color87,
                      Termbox.color88,
                      Termbox.color89,
                      Termbox.color90,
                      Termbox.color91,
                      Termbox.color92,
                      Termbox.color93,
                      Termbox.color94,
                      Termbox.color95,
                      Termbox.color96,
                      Termbox.color97,
                      Termbox.color98,
                      Termbox.color99,
                      Termbox.color100,
                      Termbox.color101,
                      Termbox.color102,
                      Termbox.color103,
                      Termbox.color104,
                      Termbox.color105,
                      Termbox.color106,
                      Termbox.color107,
                      Termbox.color108,
                      Termbox.color109,
                      Termbox.color110,
                      Termbox.color111,
                      Termbox.color112,
                      Termbox.color113,
                      Termbox.color114,
                      Termbox.color115,
                      Termbox.color116,
                      Termbox.color117,
                      Termbox.color118,
                      Termbox.color119,
                      Termbox.color120,
                      Termbox.color121,
                      Termbox.color122,
                      Termbox.color123,
                      Termbox.color124,
                      Termbox.color125,
                      Termbox.color126,
                      Termbox.color127,
                      Termbox.color128,
                      Termbox.color129,
                      Termbox.color130,
                      Termbox.color131,
                      Termbox.color132,
                      Termbox.color133,
                      Termbox.color134,
                      Termbox.color135,
                      Termbox.color136,
                      Termbox.color137,
                      Termbox.color138,
                      Termbox.color139,
                      Termbox.color140,
                      Termbox.color141,
                      Termbox.color142,
                      Termbox.color143,
                      Termbox.color144,
                      Termbox.color145,
                      Termbox.color146,
                      Termbox.color147,
                      Termbox.color148,
                      Termbox.color149,
                      Termbox.color150,
                      Termbox.color151,
                      Termbox.color152,
                      Termbox.color153,
                      Termbox.color154,
                      Termbox.color155,
                      Termbox.color156,
                      Termbox.color157,
                      Termbox.color158,
                      Termbox.color159,
                      Termbox.color160,
                      Termbox.color161,
                      Termbox.color162,
                      Termbox.color163,
                      Termbox.color164,
                      Termbox.color165,
                      Termbox.color166,
                      Termbox.color167,
                      Termbox.color168,
                      Termbox.color169,
                      Termbox.color170,
                      Termbox.color171,
                      Termbox.color172,
                      Termbox.color173,
                      Termbox.color174,
                      Termbox.color175,
                      Termbox.color176,
                      Termbox.color177,
                      Termbox.color178,
                      Termbox.color179,
                      Termbox.color180,
                      Termbox.color181,
                      Termbox.color182,
                      Termbox.color183,
                      Termbox.color184,
                      Termbox.color185,
                      Termbox.color186,
                      Termbox.color187,
                      Termbox.color188,
                      Termbox.color189,
                      Termbox.color190,
                      Termbox.color191,
                      Termbox.color192,
                      Termbox.color193,
                      Termbox.color194,
                      Termbox.color195,
                      Termbox.color196,
                      Termbox.color197,
                      Termbox.color198,
                      Termbox.color199,
                      Termbox.color200,
                      Termbox.color201,
                      Termbox.color202,
                      Termbox.color203,
                      Termbox.color204,
                      Termbox.color205,
                      Termbox.color206,
                      Termbox.color207,
                      Termbox.color208,
                      Termbox.color209,
                      Termbox.color210,
                      Termbox.color211,
                      Termbox.color212,
                      Termbox.color213,
                      Termbox.color214,
                      Termbox.color215
                    ]
                ),
          string 2 37 Termbox.white Termbox.black "gray0 .. gray23",
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
                ( zipWith3
                    ( \i (col, row) attr ->
                        fold
                          [ rectangle col row (col + w - 1) (row + h - 1) (Termbox.Cell ' ' Termbox.white attr),
                            string col row Termbox.white attr (show i)
                          ]
                    )
                    [(0 :: Int) ..]
                    coords
                    [ Termbox.gray0,
                      Termbox.gray1,
                      Termbox.gray2,
                      Termbox.gray3,
                      Termbox.gray4,
                      Termbox.gray5,
                      Termbox.gray6,
                      Termbox.gray7,
                      Termbox.gray8,
                      Termbox.gray9,
                      Termbox.gray10,
                      Termbox.gray11,
                      Termbox.gray12,
                      Termbox.gray13,
                      Termbox.gray14,
                      Termbox.gray15,
                      Termbox.gray16,
                      Termbox.gray17,
                      Termbox.gray18,
                      Termbox.gray19,
                      Termbox.gray20,
                      Termbox.gray21,
                      Termbox.gray22,
                      Termbox.gray23
                    ]
                ),
          string 2 51 Termbox.white Termbox.black "Press Esc to quit!"
        ]

string :: Int -> Int -> Termbox.Attr -> Termbox.Attr -> [Char] -> Termbox.Cells
string x0 y fg bg =
  mconcat . zipWith (\x c -> Termbox.set x y (Termbox.Cell c fg bg)) [x0 ..]

rectangle :: Int -> Int -> Int -> Int -> Termbox.Cell -> Termbox.Cells
rectangle x0 y0 x1 y1 c =
  foldMap (\(x, y) -> Termbox.set x y c) ((,) <$> [x0 .. x1] <*> [y0 .. y1])

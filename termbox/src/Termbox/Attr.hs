module Termbox.Attr
  ( Attr,
    black,
    blue,
    bold,
    brightBlack,
    brightBlue,
    brightCyan,
    brightGreen,
    brightMagenta,
    brightRed,
    brightWhite,
    brightYellow,
    color0,
    color1,
    color10,
    color100,
    color101,
    color102,
    color103,
    color104,
    color105,
    color106,
    color107,
    color108,
    color109,
    color11,
    color110,
    color111,
    color112,
    color113,
    color114,
    color115,
    color116,
    color117,
    color118,
    color119,
    color12,
    color120,
    color121,
    color122,
    color123,
    color124,
    color125,
    color126,
    color127,
    color128,
    color129,
    color13,
    color130,
    color131,
    color132,
    color133,
    color134,
    color135,
    color136,
    color137,
    color138,
    color139,
    color14,
    color140,
    color141,
    color142,
    color143,
    color144,
    color145,
    color146,
    color147,
    color148,
    color149,
    color15,
    color150,
    color151,
    color152,
    color153,
    color154,
    color155,
    color156,
    color157,
    color158,
    color159,
    color16,
    color160,
    color161,
    color162,
    color163,
    color164,
    color165,
    color166,
    color167,
    color168,
    color169,
    color17,
    color170,
    color171,
    color172,
    color173,
    color174,
    color175,
    color176,
    color177,
    color178,
    color179,
    color18,
    color180,
    color181,
    color182,
    color183,
    color184,
    color185,
    color186,
    color187,
    color188,
    color189,
    color19,
    color190,
    color191,
    color192,
    color193,
    color194,
    color195,
    color196,
    color197,
    color198,
    color199,
    color2,
    color20,
    color200,
    color201,
    color202,
    color203,
    color204,
    color205,
    color206,
    color207,
    color208,
    color209,
    color21,
    color210,
    color211,
    color212,
    color213,
    color214,
    color215,
    color22,
    color23,
    color24,
    color25,
    color26,
    color27,
    color28,
    color29,
    color3,
    color30,
    color31,
    color32,
    color33,
    color34,
    color35,
    color36,
    color37,
    color38,
    color39,
    color4,
    color40,
    color41,
    color42,
    color43,
    color44,
    color45,
    color46,
    color47,
    color48,
    color49,
    color5,
    color50,
    color51,
    color52,
    color53,
    color54,
    color55,
    color56,
    color57,
    color58,
    color59,
    color6,
    color60,
    color61,
    color62,
    color63,
    color64,
    color65,
    color66,
    color67,
    color68,
    color69,
    color7,
    color70,
    color71,
    color72,
    color73,
    color74,
    color75,
    color76,
    color77,
    color78,
    color79,
    color8,
    color80,
    color81,
    color82,
    color83,
    color84,
    color85,
    color86,
    color87,
    color88,
    color89,
    color9,
    color90,
    color91,
    color92,
    color93,
    color94,
    color95,
    color96,
    color97,
    color98,
    color99,
    cyan,
    gray0,
    gray1,
    gray10,
    gray11,
    gray12,
    gray13,
    gray14,
    gray15,
    gray16,
    gray17,
    gray18,
    gray19,
    gray2,
    gray20,
    gray21,
    gray22,
    gray23,
    gray3,
    gray4,
    gray5,
    gray6,
    gray7,
    gray8,
    gray9,
    green,
    magenta,
    red,
    reverse,
    underline,
    white,
    yellow,
    --
    attrToWord,
    wordToAttr,
  )
where

import Data.Bits ((.&.), (.|.))
import Data.Word (Word16)
import qualified Termbox.Bindings.C as C
import Prelude hiding (reverse)

-- | A cell attribute, which includes its color, and whether or not it is
-- bold, underlined, and/or reversed.
--
-- A cell can only have one color, but may be (for example) bold /and/
-- underlined.
data Attr
  = Attr !Word16 {- color -} !Word16 {- attr -}
  deriving stock (Eq, Show)

wordToAttr :: Word16 -> Attr
wordToAttr w =
  Attr (w .&. 0x00FF) (w .&. 0xFF00)

attrToWord :: Attr -> Word16
attrToWord (Attr x y) =
  x .|. y

black :: Attr
black =
  Attr 0 0

red :: Attr
red =
  Attr 1 0

green :: Attr
green =
  Attr 2 0

yellow :: Attr
yellow =
  Attr 3 0

blue :: Attr
blue =
  Attr 4 0

magenta :: Attr
magenta =
  Attr 5 0

cyan :: Attr
cyan =
  Attr 6 0

white :: Attr
white =
  Attr 7 0

brightBlack :: Attr
brightBlack =
  Attr 8 0

brightRed :: Attr
brightRed =
  Attr 9 0

brightGreen :: Attr
brightGreen =
  Attr 10 0

brightYellow :: Attr
brightYellow =
  Attr 11 0

brightBlue :: Attr
brightBlue =
  Attr 12 0

brightMagenta :: Attr
brightMagenta =
  Attr 13 0

brightCyan :: Attr
brightCyan =
  Attr 14 0

brightWhite :: Attr
brightWhite =
  Attr 15 0

color0 :: Attr
color0 =
  Attr 16 0

color1 :: Attr
color1 =
  Attr 17 0

color2 :: Attr
color2 =
  Attr 18 0

color3 :: Attr
color3 =
  Attr 19 0

color4 :: Attr
color4 =
  Attr 20 0

color5 :: Attr
color5 =
  Attr 21 0

color6 :: Attr
color6 =
  Attr 22 0

color7 :: Attr
color7 =
  Attr 23 0

color8 :: Attr
color8 =
  Attr 24 0

color9 :: Attr
color9 =
  Attr 25 0

color10 :: Attr
color10 =
  Attr 26 0

color11 :: Attr
color11 =
  Attr 27 0

color12 :: Attr
color12 =
  Attr 28 0

color13 :: Attr
color13 =
  Attr 29 0

color14 :: Attr
color14 =
  Attr 30 0

color15 :: Attr
color15 =
  Attr 31 0

color16 :: Attr
color16 =
  Attr 32 0

color17 :: Attr
color17 =
  Attr 33 0

color18 :: Attr
color18 =
  Attr 34 0

color19 :: Attr
color19 =
  Attr 35 0

color20 :: Attr
color20 =
  Attr 36 0

color21 :: Attr
color21 =
  Attr 37 0

color22 :: Attr
color22 =
  Attr 38 0

color23 :: Attr
color23 =
  Attr 39 0

color24 :: Attr
color24 =
  Attr 40 0

color25 :: Attr
color25 =
  Attr 41 0

color26 :: Attr
color26 =
  Attr 42 0

color27 :: Attr
color27 =
  Attr 43 0

color28 :: Attr
color28 =
  Attr 44 0

color29 :: Attr
color29 =
  Attr 45 0

color30 :: Attr
color30 =
  Attr 46 0

color31 :: Attr
color31 =
  Attr 47 0

color32 :: Attr
color32 =
  Attr 48 0

color33 :: Attr
color33 =
  Attr 49 0

color34 :: Attr
color34 =
  Attr 50 0

color35 :: Attr
color35 =
  Attr 51 0

color36 :: Attr
color36 =
  Attr 52 0

color37 :: Attr
color37 =
  Attr 53 0

color38 :: Attr
color38 =
  Attr 54 0

color39 :: Attr
color39 =
  Attr 55 0

color40 :: Attr
color40 =
  Attr 56 0

color41 :: Attr
color41 =
  Attr 57 0

color42 :: Attr
color42 =
  Attr 58 0

color43 :: Attr
color43 =
  Attr 59 0

color44 :: Attr
color44 =
  Attr 60 0

color45 :: Attr
color45 =
  Attr 61 0

color46 :: Attr
color46 =
  Attr 62 0

color47 :: Attr
color47 =
  Attr 63 0

color48 :: Attr
color48 =
  Attr 64 0

color49 :: Attr
color49 =
  Attr 65 0

color50 :: Attr
color50 =
  Attr 66 0

color51 :: Attr
color51 =
  Attr 67 0

color52 :: Attr
color52 =
  Attr 68 0

color53 :: Attr
color53 =
  Attr 69 0

color54 :: Attr
color54 =
  Attr 70 0

color55 :: Attr
color55 =
  Attr 71 0

color56 :: Attr
color56 =
  Attr 72 0

color57 :: Attr
color57 =
  Attr 73 0

color58 :: Attr
color58 =
  Attr 74 0

color59 :: Attr
color59 =
  Attr 75 0

color60 :: Attr
color60 =
  Attr 76 0

color61 :: Attr
color61 =
  Attr 77 0

color62 :: Attr
color62 =
  Attr 78 0

color63 :: Attr
color63 =
  Attr 79 0

color64 :: Attr
color64 =
  Attr 80 0

color65 :: Attr
color65 =
  Attr 81 0

color66 :: Attr
color66 =
  Attr 82 0

color67 :: Attr
color67 =
  Attr 83 0

color68 :: Attr
color68 =
  Attr 84 0

color69 :: Attr
color69 =
  Attr 85 0

color70 :: Attr
color70 =
  Attr 86 0

color71 :: Attr
color71 =
  Attr 87 0

color72 :: Attr
color72 =
  Attr 88 0

color73 :: Attr
color73 =
  Attr 89 0

color74 :: Attr
color74 =
  Attr 90 0

color75 :: Attr
color75 =
  Attr 91 0

color76 :: Attr
color76 =
  Attr 92 0

color77 :: Attr
color77 =
  Attr 93 0

color78 :: Attr
color78 =
  Attr 94 0

color79 :: Attr
color79 =
  Attr 95 0

color80 :: Attr
color80 =
  Attr 96 0

color81 :: Attr
color81 =
  Attr 97 0

color82 :: Attr
color82 =
  Attr 98 0

color83 :: Attr
color83 =
  Attr 99 0

color84 :: Attr
color84 =
  Attr 100 0

color85 :: Attr
color85 =
  Attr 101 0

color86 :: Attr
color86 =
  Attr 102 0

color87 :: Attr
color87 =
  Attr 103 0

color88 :: Attr
color88 =
  Attr 104 0

color89 :: Attr
color89 =
  Attr 105 0

color90 :: Attr
color90 =
  Attr 106 0

color91 :: Attr
color91 =
  Attr 107 0

color92 :: Attr
color92 =
  Attr 108 0

color93 :: Attr
color93 =
  Attr 109 0

color94 :: Attr
color94 =
  Attr 110 0

color95 :: Attr
color95 =
  Attr 111 0

color96 :: Attr
color96 =
  Attr 112 0

color97 :: Attr
color97 =
  Attr 113 0

color98 :: Attr
color98 =
  Attr 114 0

color99 :: Attr
color99 =
  Attr 115 0

color100 :: Attr
color100 =
  Attr 116 0

color101 :: Attr
color101 =
  Attr 117 0

color102 :: Attr
color102 =
  Attr 118 0

color103 :: Attr
color103 =
  Attr 119 0

color104 :: Attr
color104 =
  Attr 120 0

color105 :: Attr
color105 =
  Attr 121 0

color106 :: Attr
color106 =
  Attr 122 0

color107 :: Attr
color107 =
  Attr 123 0

color108 :: Attr
color108 =
  Attr 124 0

color109 :: Attr
color109 =
  Attr 125 0

color110 :: Attr
color110 =
  Attr 126 0

color111 :: Attr
color111 =
  Attr 127 0

color112 :: Attr
color112 =
  Attr 128 0

color113 :: Attr
color113 =
  Attr 129 0

color114 :: Attr
color114 =
  Attr 130 0

color115 :: Attr
color115 =
  Attr 131 0

color116 :: Attr
color116 =
  Attr 132 0

color117 :: Attr
color117 =
  Attr 133 0

color118 :: Attr
color118 =
  Attr 134 0

color119 :: Attr
color119 =
  Attr 135 0

color120 :: Attr
color120 =
  Attr 136 0

color121 :: Attr
color121 =
  Attr 137 0

color122 :: Attr
color122 =
  Attr 138 0

color123 :: Attr
color123 =
  Attr 139 0

color124 :: Attr
color124 =
  Attr 140 0

color125 :: Attr
color125 =
  Attr 141 0

color126 :: Attr
color126 =
  Attr 142 0

color127 :: Attr
color127 =
  Attr 143 0

color128 :: Attr
color128 =
  Attr 144 0

color129 :: Attr
color129 =
  Attr 145 0

color130 :: Attr
color130 =
  Attr 146 0

color131 :: Attr
color131 =
  Attr 147 0

color132 :: Attr
color132 =
  Attr 148 0

color133 :: Attr
color133 =
  Attr 149 0

color134 :: Attr
color134 =
  Attr 150 0

color135 :: Attr
color135 =
  Attr 151 0

color136 :: Attr
color136 =
  Attr 152 0

color137 :: Attr
color137 =
  Attr 153 0

color138 :: Attr
color138 =
  Attr 154 0

color139 :: Attr
color139 =
  Attr 155 0

color140 :: Attr
color140 =
  Attr 156 0

color141 :: Attr
color141 =
  Attr 157 0

color142 :: Attr
color142 =
  Attr 158 0

color143 :: Attr
color143 =
  Attr 159 0

color144 :: Attr
color144 =
  Attr 160 0

color145 :: Attr
color145 =
  Attr 161 0

color146 :: Attr
color146 =
  Attr 162 0

color147 :: Attr
color147 =
  Attr 163 0

color148 :: Attr
color148 =
  Attr 164 0

color149 :: Attr
color149 =
  Attr 165 0

color150 :: Attr
color150 =
  Attr 166 0

color151 :: Attr
color151 =
  Attr 167 0

color152 :: Attr
color152 =
  Attr 168 0

color153 :: Attr
color153 =
  Attr 169 0

color154 :: Attr
color154 =
  Attr 170 0

color155 :: Attr
color155 =
  Attr 171 0

color156 :: Attr
color156 =
  Attr 172 0

color157 :: Attr
color157 =
  Attr 173 0

color158 :: Attr
color158 =
  Attr 174 0

color159 :: Attr
color159 =
  Attr 175 0

color160 :: Attr
color160 =
  Attr 176 0

color161 :: Attr
color161 =
  Attr 177 0

color162 :: Attr
color162 =
  Attr 178 0

color163 :: Attr
color163 =
  Attr 179 0

color164 :: Attr
color164 =
  Attr 180 0

color165 :: Attr
color165 =
  Attr 181 0

color166 :: Attr
color166 =
  Attr 182 0

color167 :: Attr
color167 =
  Attr 183 0

color168 :: Attr
color168 =
  Attr 184 0

color169 :: Attr
color169 =
  Attr 185 0

color170 :: Attr
color170 =
  Attr 186 0

color171 :: Attr
color171 =
  Attr 187 0

color172 :: Attr
color172 =
  Attr 188 0

color173 :: Attr
color173 =
  Attr 189 0

color174 :: Attr
color174 =
  Attr 190 0

color175 :: Attr
color175 =
  Attr 191 0

color176 :: Attr
color176 =
  Attr 192 0

color177 :: Attr
color177 =
  Attr 193 0

color178 :: Attr
color178 =
  Attr 194 0

color179 :: Attr
color179 =
  Attr 195 0

color180 :: Attr
color180 =
  Attr 196 0

color181 :: Attr
color181 =
  Attr 197 0

color182 :: Attr
color182 =
  Attr 198 0

color183 :: Attr
color183 =
  Attr 199 0

color184 :: Attr
color184 =
  Attr 200 0

color185 :: Attr
color185 =
  Attr 201 0

color186 :: Attr
color186 =
  Attr 202 0

color187 :: Attr
color187 =
  Attr 203 0

color188 :: Attr
color188 =
  Attr 204 0

color189 :: Attr
color189 =
  Attr 205 0

color190 :: Attr
color190 =
  Attr 206 0

color191 :: Attr
color191 =
  Attr 207 0

color192 :: Attr
color192 =
  Attr 208 0

color193 :: Attr
color193 =
  Attr 209 0

color194 :: Attr
color194 =
  Attr 210 0

color195 :: Attr
color195 =
  Attr 211 0

color196 :: Attr
color196 =
  Attr 212 0

color197 :: Attr
color197 =
  Attr 213 0

color198 :: Attr
color198 =
  Attr 214 0

color199 :: Attr
color199 =
  Attr 215 0

color200 :: Attr
color200 =
  Attr 216 0

color201 :: Attr
color201 =
  Attr 217 0

color202 :: Attr
color202 =
  Attr 218 0

color203 :: Attr
color203 =
  Attr 219 0

color204 :: Attr
color204 =
  Attr 220 0

color205 :: Attr
color205 =
  Attr 221 0

color206 :: Attr
color206 =
  Attr 222 0

color207 :: Attr
color207 =
  Attr 223 0

color208 :: Attr
color208 =
  Attr 224 0

color209 :: Attr
color209 =
  Attr 225 0

color210 :: Attr
color210 =
  Attr 226 0

color211 :: Attr
color211 =
  Attr 227 0

color212 :: Attr
color212 =
  Attr 228 0

color213 :: Attr
color213 =
  Attr 229 0

color214 :: Attr
color214 =
  Attr 230 0

color215 :: Attr
color215 =
  Attr 231 0

gray0 :: Attr
gray0 =
  Attr 232 0

gray1 :: Attr
gray1 =
  Attr 233 0

gray2 :: Attr
gray2 =
  Attr 234 0

gray3 :: Attr
gray3 =
  Attr 235 0

gray4 :: Attr
gray4 =
  Attr 236 0

gray5 :: Attr
gray5 =
  Attr 237 0

gray6 :: Attr
gray6 =
  Attr 238 0

gray7 :: Attr
gray7 =
  Attr 239 0

gray8 :: Attr
gray8 =
  Attr 240 0

gray9 :: Attr
gray9 =
  Attr 241 0

gray10 :: Attr
gray10 =
  Attr 242 0

gray11 :: Attr
gray11 =
  Attr 243 0

gray12 :: Attr
gray12 =
  Attr 244 0

gray13 :: Attr
gray13 =
  Attr 245 0

gray14 :: Attr
gray14 =
  Attr 246 0

gray15 :: Attr
gray15 =
  Attr 247 0

gray16 :: Attr
gray16 =
  Attr 248 0

gray17 :: Attr
gray17 =
  Attr 249 0

gray18 :: Attr
gray18 =
  Attr 250 0

gray19 :: Attr
gray19 =
  Attr 251 0

gray20 :: Attr
gray20 =
  Attr 252 0

gray21 :: Attr
gray21 =
  Attr 253 0

gray22 :: Attr
gray22 =
  Attr 254 0

gray23 :: Attr
gray23 =
  Attr 255 0

-- | Bold modifier attribute.
bold :: Attr -> Attr
bold (Attr c s) =
  Attr c (s .|. C._TB_BOLD)

-- | Underline modifier attribute.
underline :: Attr -> Attr
underline (Attr c s) =
  Attr c (s .|. C._TB_UNDERLINE)

-- | Reverse modifier attribute.
reverse :: Attr -> Attr
reverse (Attr c s) =
  Attr c (s .|. C._TB_REVERSE)

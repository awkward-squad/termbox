module Termbox.Attr
  ( Attr (Attr),
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
  )
where

import qualified Termbox.Bindings
import Prelude hiding (reverse)

-- | A cell attribute, which includes its color, and whether or not it is
-- bold, underlined, and/or reversed.
--
-- A cell can only have one color, but may be (for example) bold /and/
-- underlined.
newtype Attr
  = Attr Termbox.Bindings.Tb_color
  deriving stock (Eq, Show)

black :: Attr
black =
  Attr 0

red :: Attr
red =
  Attr 1

green :: Attr
green =
  Attr 2

yellow :: Attr
yellow =
  Attr 3

blue :: Attr
blue =
  Attr 4

magenta :: Attr
magenta =
  Attr 5

cyan :: Attr
cyan =
  Attr 6

white :: Attr
white =
  Attr 7

brightBlack :: Attr
brightBlack =
  Attr 8

brightRed :: Attr
brightRed =
  Attr 9

brightGreen :: Attr
brightGreen =
  Attr 10

brightYellow :: Attr
brightYellow =
  Attr 11

brightBlue :: Attr
brightBlue =
  Attr 12

brightMagenta :: Attr
brightMagenta =
  Attr 13

brightCyan :: Attr
brightCyan =
  Attr 14

brightWhite :: Attr
brightWhite =
  Attr 15

color0 :: Attr
color0 =
  Attr 16

color1 :: Attr
color1 =
  Attr 17

color2 :: Attr
color2 =
  Attr 18

color3 :: Attr
color3 =
  Attr 19

color4 :: Attr
color4 =
  Attr 20

color5 :: Attr
color5 =
  Attr 21

color6 :: Attr
color6 =
  Attr 22

color7 :: Attr
color7 =
  Attr 23

color8 :: Attr
color8 =
  Attr 24

color9 :: Attr
color9 =
  Attr 25

color10 :: Attr
color10 =
  Attr 26

color11 :: Attr
color11 =
  Attr 27

color12 :: Attr
color12 =
  Attr 28

color13 :: Attr
color13 =
  Attr 29

color14 :: Attr
color14 =
  Attr 30

color15 :: Attr
color15 =
  Attr 31

color16 :: Attr
color16 =
  Attr 32

color17 :: Attr
color17 =
  Attr 33

color18 :: Attr
color18 =
  Attr 34

color19 :: Attr
color19 =
  Attr 35

color20 :: Attr
color20 =
  Attr 36

color21 :: Attr
color21 =
  Attr 37

color22 :: Attr
color22 =
  Attr 38

color23 :: Attr
color23 =
  Attr 39

color24 :: Attr
color24 =
  Attr 40

color25 :: Attr
color25 =
  Attr 41

color26 :: Attr
color26 =
  Attr 42

color27 :: Attr
color27 =
  Attr 43

color28 :: Attr
color28 =
  Attr 44

color29 :: Attr
color29 =
  Attr 45

color30 :: Attr
color30 =
  Attr 46

color31 :: Attr
color31 =
  Attr 47

color32 :: Attr
color32 =
  Attr 48

color33 :: Attr
color33 =
  Attr 49

color34 :: Attr
color34 =
  Attr 50

color35 :: Attr
color35 =
  Attr 51

color36 :: Attr
color36 =
  Attr 52

color37 :: Attr
color37 =
  Attr 53

color38 :: Attr
color38 =
  Attr 54

color39 :: Attr
color39 =
  Attr 55

color40 :: Attr
color40 =
  Attr 56

color41 :: Attr
color41 =
  Attr 57

color42 :: Attr
color42 =
  Attr 58

color43 :: Attr
color43 =
  Attr 59

color44 :: Attr
color44 =
  Attr 60

color45 :: Attr
color45 =
  Attr 61

color46 :: Attr
color46 =
  Attr 62

color47 :: Attr
color47 =
  Attr 63

color48 :: Attr
color48 =
  Attr 64

color49 :: Attr
color49 =
  Attr 65

color50 :: Attr
color50 =
  Attr 66

color51 :: Attr
color51 =
  Attr 67

color52 :: Attr
color52 =
  Attr 68

color53 :: Attr
color53 =
  Attr 69

color54 :: Attr
color54 =
  Attr 70

color55 :: Attr
color55 =
  Attr 71

color56 :: Attr
color56 =
  Attr 72

color57 :: Attr
color57 =
  Attr 73

color58 :: Attr
color58 =
  Attr 74

color59 :: Attr
color59 =
  Attr 75

color60 :: Attr
color60 =
  Attr 76

color61 :: Attr
color61 =
  Attr 77

color62 :: Attr
color62 =
  Attr 78

color63 :: Attr
color63 =
  Attr 79

color64 :: Attr
color64 =
  Attr 80

color65 :: Attr
color65 =
  Attr 81

color66 :: Attr
color66 =
  Attr 82

color67 :: Attr
color67 =
  Attr 83

color68 :: Attr
color68 =
  Attr 84

color69 :: Attr
color69 =
  Attr 85

color70 :: Attr
color70 =
  Attr 86

color71 :: Attr
color71 =
  Attr 87

color72 :: Attr
color72 =
  Attr 88

color73 :: Attr
color73 =
  Attr 89

color74 :: Attr
color74 =
  Attr 90

color75 :: Attr
color75 =
  Attr 91

color76 :: Attr
color76 =
  Attr 92

color77 :: Attr
color77 =
  Attr 93

color78 :: Attr
color78 =
  Attr 94

color79 :: Attr
color79 =
  Attr 95

color80 :: Attr
color80 =
  Attr 96

color81 :: Attr
color81 =
  Attr 97

color82 :: Attr
color82 =
  Attr 98

color83 :: Attr
color83 =
  Attr 99

color84 :: Attr
color84 =
  Attr 100

color85 :: Attr
color85 =
  Attr 101

color86 :: Attr
color86 =
  Attr 102

color87 :: Attr
color87 =
  Attr 103

color88 :: Attr
color88 =
  Attr 104

color89 :: Attr
color89 =
  Attr 105

color90 :: Attr
color90 =
  Attr 106

color91 :: Attr
color91 =
  Attr 107

color92 :: Attr
color92 =
  Attr 108

color93 :: Attr
color93 =
  Attr 109

color94 :: Attr
color94 =
  Attr 110

color95 :: Attr
color95 =
  Attr 111

color96 :: Attr
color96 =
  Attr 112

color97 :: Attr
color97 =
  Attr 113

color98 :: Attr
color98 =
  Attr 114

color99 :: Attr
color99 =
  Attr 115

color100 :: Attr
color100 =
  Attr 116

color101 :: Attr
color101 =
  Attr 117

color102 :: Attr
color102 =
  Attr 118

color103 :: Attr
color103 =
  Attr 119

color104 :: Attr
color104 =
  Attr 120

color105 :: Attr
color105 =
  Attr 121

color106 :: Attr
color106 =
  Attr 122

color107 :: Attr
color107 =
  Attr 123

color108 :: Attr
color108 =
  Attr 124

color109 :: Attr
color109 =
  Attr 125

color110 :: Attr
color110 =
  Attr 126

color111 :: Attr
color111 =
  Attr 127

color112 :: Attr
color112 =
  Attr 128

color113 :: Attr
color113 =
  Attr 129

color114 :: Attr
color114 =
  Attr 130

color115 :: Attr
color115 =
  Attr 131

color116 :: Attr
color116 =
  Attr 132

color117 :: Attr
color117 =
  Attr 133

color118 :: Attr
color118 =
  Attr 134

color119 :: Attr
color119 =
  Attr 135

color120 :: Attr
color120 =
  Attr 136

color121 :: Attr
color121 =
  Attr 137

color122 :: Attr
color122 =
  Attr 138

color123 :: Attr
color123 =
  Attr 139

color124 :: Attr
color124 =
  Attr 140

color125 :: Attr
color125 =
  Attr 141

color126 :: Attr
color126 =
  Attr 142

color127 :: Attr
color127 =
  Attr 143

color128 :: Attr
color128 =
  Attr 144

color129 :: Attr
color129 =
  Attr 145

color130 :: Attr
color130 =
  Attr 146

color131 :: Attr
color131 =
  Attr 147

color132 :: Attr
color132 =
  Attr 148

color133 :: Attr
color133 =
  Attr 149

color134 :: Attr
color134 =
  Attr 150

color135 :: Attr
color135 =
  Attr 151

color136 :: Attr
color136 =
  Attr 152

color137 :: Attr
color137 =
  Attr 153

color138 :: Attr
color138 =
  Attr 154

color139 :: Attr
color139 =
  Attr 155

color140 :: Attr
color140 =
  Attr 156

color141 :: Attr
color141 =
  Attr 157

color142 :: Attr
color142 =
  Attr 158

color143 :: Attr
color143 =
  Attr 159

color144 :: Attr
color144 =
  Attr 160

color145 :: Attr
color145 =
  Attr 161

color146 :: Attr
color146 =
  Attr 162

color147 :: Attr
color147 =
  Attr 163

color148 :: Attr
color148 =
  Attr 164

color149 :: Attr
color149 =
  Attr 165

color150 :: Attr
color150 =
  Attr 166

color151 :: Attr
color151 =
  Attr 167

color152 :: Attr
color152 =
  Attr 168

color153 :: Attr
color153 =
  Attr 169

color154 :: Attr
color154 =
  Attr 170

color155 :: Attr
color155 =
  Attr 171

color156 :: Attr
color156 =
  Attr 172

color157 :: Attr
color157 =
  Attr 173

color158 :: Attr
color158 =
  Attr 174

color159 :: Attr
color159 =
  Attr 175

color160 :: Attr
color160 =
  Attr 176

color161 :: Attr
color161 =
  Attr 177

color162 :: Attr
color162 =
  Attr 178

color163 :: Attr
color163 =
  Attr 179

color164 :: Attr
color164 =
  Attr 180

color165 :: Attr
color165 =
  Attr 181

color166 :: Attr
color166 =
  Attr 182

color167 :: Attr
color167 =
  Attr 183

color168 :: Attr
color168 =
  Attr 184

color169 :: Attr
color169 =
  Attr 185

color170 :: Attr
color170 =
  Attr 186

color171 :: Attr
color171 =
  Attr 187

color172 :: Attr
color172 =
  Attr 188

color173 :: Attr
color173 =
  Attr 189

color174 :: Attr
color174 =
  Attr 190

color175 :: Attr
color175 =
  Attr 191

color176 :: Attr
color176 =
  Attr 192

color177 :: Attr
color177 =
  Attr 193

color178 :: Attr
color178 =
  Attr 194

color179 :: Attr
color179 =
  Attr 195

color180 :: Attr
color180 =
  Attr 196

color181 :: Attr
color181 =
  Attr 197

color182 :: Attr
color182 =
  Attr 198

color183 :: Attr
color183 =
  Attr 199

color184 :: Attr
color184 =
  Attr 200

color185 :: Attr
color185 =
  Attr 201

color186 :: Attr
color186 =
  Attr 202

color187 :: Attr
color187 =
  Attr 203

color188 :: Attr
color188 =
  Attr 204

color189 :: Attr
color189 =
  Attr 205

color190 :: Attr
color190 =
  Attr 206

color191 :: Attr
color191 =
  Attr 207

color192 :: Attr
color192 =
  Attr 208

color193 :: Attr
color193 =
  Attr 209

color194 :: Attr
color194 =
  Attr 210

color195 :: Attr
color195 =
  Attr 211

color196 :: Attr
color196 =
  Attr 212

color197 :: Attr
color197 =
  Attr 213

color198 :: Attr
color198 =
  Attr 214

color199 :: Attr
color199 =
  Attr 215

color200 :: Attr
color200 =
  Attr 216

color201 :: Attr
color201 =
  Attr 217

color202 :: Attr
color202 =
  Attr 218

color203 :: Attr
color203 =
  Attr 219

color204 :: Attr
color204 =
  Attr 220

color205 :: Attr
color205 =
  Attr 221

color206 :: Attr
color206 =
  Attr 222

color207 :: Attr
color207 =
  Attr 223

color208 :: Attr
color208 =
  Attr 224

color209 :: Attr
color209 =
  Attr 225

color210 :: Attr
color210 =
  Attr 226

color211 :: Attr
color211 =
  Attr 227

color212 :: Attr
color212 =
  Attr 228

color213 :: Attr
color213 =
  Attr 229

color214 :: Attr
color214 =
  Attr 230

color215 :: Attr
color215 =
  Attr 231

gray0 :: Attr
gray0 =
  Attr 232

gray1 :: Attr
gray1 =
  Attr 233

gray2 :: Attr
gray2 =
  Attr 234

gray3 :: Attr
gray3 =
  Attr 235

gray4 :: Attr
gray4 =
  Attr 236

gray5 :: Attr
gray5 =
  Attr 237

gray6 :: Attr
gray6 =
  Attr 238

gray7 :: Attr
gray7 =
  Attr 239

gray8 :: Attr
gray8 =
  Attr 240

gray9 :: Attr
gray9 =
  Attr 241

gray10 :: Attr
gray10 =
  Attr 242

gray11 :: Attr
gray11 =
  Attr 243

gray12 :: Attr
gray12 =
  Attr 244

gray13 :: Attr
gray13 =
  Attr 245

gray14 :: Attr
gray14 =
  Attr 246

gray15 :: Attr
gray15 =
  Attr 247

gray16 :: Attr
gray16 =
  Attr 248

gray17 :: Attr
gray17 =
  Attr 249

gray18 :: Attr
gray18 =
  Attr 250

gray19 :: Attr
gray19 =
  Attr 251

gray20 :: Attr
gray20 =
  Attr 252

gray21 :: Attr
gray21 =
  Attr 253

gray22 :: Attr
gray22 =
  Attr 254

gray23 :: Attr
gray23 =
  Attr 255

-- | Bold modifier attribute.
bold :: Attr -> Attr
bold (Attr c) =
  Attr (Termbox.Bindings.tb_attr Termbox.Bindings.TB_BOLD c)

-- | Underline modifier attribute.
underline :: Attr -> Attr
underline (Attr c) =
  Attr (Termbox.Bindings.tb_attr Termbox.Bindings.TB_UNDERLINE c)

-- | Reverse modifier attribute.
reverse :: Attr -> Attr
reverse (Attr c) =
  Attr (Termbox.Bindings.tb_attr Termbox.Bindings.TB_REVERSE c)

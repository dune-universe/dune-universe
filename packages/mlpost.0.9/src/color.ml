(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Johannes Kanig, Stephane Lescuyer                       *)
(*  Jean-Christophe Filliatre, Romain Bardou and Francois Bobot           *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

open Concrete_types

type st = Concrete_types.scolor =
  | RGB of float * float * float
  | CMYK of float * float * float * float
  | Gray of float

type t = Concrete_types.color =
  | OPAQUE of scolor
  | TRANSPARENT of float * scolor

let rgb8 r g b =
  OPAQUE (RGB (float r /. 255., float g /. 255., float b /. 255.))

let rgb8a r g b a =
  TRANSPARENT
    (float a /. 255., RGB (float r /. 255., float g /. 255., float b /. 255.))

let rgb_from_int i =
  let b = i land 0xFF in
  let g = (i land 0xFF00) lsr 8 in
  let r = (i land 0xFF0000) lsr 16 in
  rgb8 r g b

(* http://en.wikipedia.org/wiki/HSL_and_HSV *)
let hsv h s v =
  assert (0. <= s && s <= 1.);
  assert (0. <= v && v <= 1.);
  let c = v *. s in
  let h' = h /. 60. in
  let x = c *. (1. -. abs_float (mod_float h' 2. -. 1.)) in
  let r_1, g_1, b_1 =
    if 0. <= h' && h' < 1. then (c, x, 0.)
    else if 1. <= h' && h' < 2. then (x, c, 0.)
    else if 2. <= h' && h' < 3. then (0., c, x)
    else if 3. <= h' && h' < 4. then (0., x, c)
    else if 4. <= h' && h' < 5. then (x, 0., c)
    else if 5. <= h' && h' < 6. then (c, 0., x)
    else (0., 0., 0.)
  in
  let m = v -. c in
  OPAQUE (RGB (r_1 +. m, g_1 +. m, b_1 +. m))

let color_gen s v =
  let choices = ref [] in
  let value = 180. in
  let next = ref 0. in
  fun () ->
    let rec aux acc value current = function
      | [] ->
          assert (current = 0.);
          next := value;
          List.rev_append acc [ true ]
      | true :: l -> aux (false :: acc) (value /. 2.) (current -. value) l
      | false :: l ->
          next := current +. value;
          List.rev_append acc (true :: l)
    in
    let res = !next in
    choices := aux [] value res !choices;
    hsv res s v

let red = OPAQUE (RGB (1.0, 0.0, 0.0))

let lightred = OPAQUE (RGB (1.0, 0.5, 0.5))

let blue = OPAQUE (RGB (0.0, 0.0, 1.0))

let lightblue = rgb_from_int 0xADD8E6

let green = rgb_from_int 0x008000

let lightgreen = rgb_from_int 0x90EE90

let orange = rgb_from_int 0xFFA500

let purple = rgb_from_int 0x7F007F

let magenta = OPAQUE (RGB (1.0, 0.0, 1.0))

let cyan = OPAQUE (RGB (0.0, 1.0, 1.0))

let lightcyan = rgb_from_int 0xE0FFFF

let yellow = OPAQUE (RGB (1.0, 1.0, 0.0))

let lightyellow = rgb_from_int 0xFFFFE0

(* these colors do not correspond to neither X11 nor HTML colors
   - commented out*)
(* let lightblue = OPAQUE (RGB (0.5,0.5,1.0)) *)
(* let green = OPAQUE (RGB (0.0,1.0,0.0)) *)
(* let lightgreen = OPAQUE (RGB (0.5,1.0,0.5)) *)
(* let orange = OPAQUE (RGB (1.0,0.4,0.0)) *)
(* let purple = OPAQUE (RGB (0.6,0.0,0.6)) *)
(* let lightcyan =  OPAQUE (RGB (0.5,1.0,1.0)) *)
(* let lightyellow = OPAQUE (RGB (1.0,1.0,0.5)) *)
let lightmagenta = OPAQUE (RGB (1.0, 0.5, 1.0))

let gray f = OPAQUE (Gray f)

let white = gray 1.0

let lightgray = gray 0.75

let mediumgray = gray 0.5

let darkgray = gray 0.25

let black = gray 0.0

let default = black

let rgb r g b = OPAQUE (RGB (r, g, b))

let rgba r g b a = TRANSPARENT (a, RGB (r, g, b))

let cmyk c m y k = OPAQUE (CMYK (c, m, y, k))

let cmyka c m y k a = TRANSPARENT (a, CMYK (c, m, y, k))

let is_opaque = function OPAQUE _ -> true | _ -> false

let opaque = function TRANSPARENT (_, c) -> OPAQUE c | c -> c

let transparent f = function
  | TRANSPARENT (f2, c) -> TRANSPARENT (f *. f2, c)
  | OPAQUE c -> TRANSPARENT (f, c)

let colors : (string, t) Hashtbl.t = Hashtbl.create 91

let color n = Hashtbl.find colors n

(** generated part *)
let _ = Hashtbl.add colors "snow" (rgb8 255 250 250)

let _ = Hashtbl.add colors "ghost" (rgb8 248 248 255)

let _ = Hashtbl.add colors "GhostWhite" (rgb8 248 248 255)

let _ = Hashtbl.add colors "white smoke" (rgb8 245 245 245)

let _ = Hashtbl.add colors "WhiteSmoke" (rgb8 245 245 245)

let _ = Hashtbl.add colors "gainsboro" (rgb8 220 220 220)

let _ = Hashtbl.add colors "floral white" (rgb8 255 250 240)

let _ = Hashtbl.add colors "FloralWhite" (rgb8 255 250 240)

let _ = Hashtbl.add colors "old lace" (rgb8 253 245 230)

let _ = Hashtbl.add colors "OldLace" (rgb8 253 245 230)

let _ = Hashtbl.add colors "linen" (rgb8 250 240 230)

let _ = Hashtbl.add colors "antique white" (rgb8 250 235 215)

let _ = Hashtbl.add colors "AntiqueWhite" (rgb8 250 235 215)

let _ = Hashtbl.add colors "papaya whip" (rgb8 255 239 213)

let _ = Hashtbl.add colors "PapayaWhip" (rgb8 255 239 213)

let _ = Hashtbl.add colors "blanched almond" (rgb8 255 235 205)

let _ = Hashtbl.add colors "BlanchedAlmond" (rgb8 255 235 205)

let _ = Hashtbl.add colors "bisque" (rgb8 255 228 196)

let _ = Hashtbl.add colors "peach puff" (rgb8 255 218 185)

let _ = Hashtbl.add colors "PeachPuff" (rgb8 255 218 185)

let _ = Hashtbl.add colors "navajo white" (rgb8 255 222 173)

let _ = Hashtbl.add colors "NavajoWhite" (rgb8 255 222 173)

let _ = Hashtbl.add colors "moccasin" (rgb8 255 228 181)

let _ = Hashtbl.add colors "cornsilk" (rgb8 255 248 220)

let _ = Hashtbl.add colors "ivory" (rgb8 255 255 240)

let _ = Hashtbl.add colors "lemon chiffon" (rgb8 255 250 205)

let _ = Hashtbl.add colors "LemonChiffon" (rgb8 255 250 205)

let _ = Hashtbl.add colors "seashell" (rgb8 255 245 238)

let _ = Hashtbl.add colors "honeydew" (rgb8 240 255 240)

let _ = Hashtbl.add colors "mint cream" (rgb8 245 255 250)

let _ = Hashtbl.add colors "MintCream" (rgb8 245 255 250)

let _ = Hashtbl.add colors "azure" (rgb8 240 255 255)

let _ = Hashtbl.add colors "alice blue" (rgb8 240 248 255)

let _ = Hashtbl.add colors "AliceBlue" (rgb8 240 248 255)

let _ = Hashtbl.add colors "lavender" (rgb8 230 230 250)

let _ = Hashtbl.add colors "lavender blush" (rgb8 255 240 245)

let _ = Hashtbl.add colors "LavenderBlush" (rgb8 255 240 245)

let _ = Hashtbl.add colors "misty rose" (rgb8 255 228 225)

let _ = Hashtbl.add colors "MistyRose" (rgb8 255 228 225)

let _ = Hashtbl.add colors "white" (rgb8 255 255 255)

let _ = Hashtbl.add colors "black" (rgb8 0 0 0)

let _ = Hashtbl.add colors "dark slate gray" (rgb8 47 79 79)

let _ = Hashtbl.add colors "DarkSlateGray" (rgb8 47 79 79)

let _ = Hashtbl.add colors "dark slate grey" (rgb8 47 79 79)

let _ = Hashtbl.add colors "DarkSlateGrey" (rgb8 47 79 79)

let _ = Hashtbl.add colors "dim gray" (rgb8 105 105 105)

let _ = Hashtbl.add colors "DimGray" (rgb8 105 105 105)

let _ = Hashtbl.add colors "dim grey" (rgb8 105 105 105)

let _ = Hashtbl.add colors "DimGrey" (rgb8 105 105 105)

let _ = Hashtbl.add colors "slate gray" (rgb8 112 128 144)

let _ = Hashtbl.add colors "SlateGray" (rgb8 112 128 144)

let _ = Hashtbl.add colors "slate grey" (rgb8 112 128 144)

let _ = Hashtbl.add colors "SlateGrey" (rgb8 112 128 144)

let _ = Hashtbl.add colors "light slate gray" (rgb8 119 136 153)

let _ = Hashtbl.add colors "LightSlateGray" (rgb8 119 136 153)

let _ = Hashtbl.add colors "light slate grey" (rgb8 119 136 153)

let _ = Hashtbl.add colors "LightSlateGrey" (rgb8 119 136 153)

let _ = Hashtbl.add colors "gray" (rgb8 190 190 190)

let _ = Hashtbl.add colors "grey" (rgb8 190 190 190)

let _ = Hashtbl.add colors "light grey" (rgb8 211 211 211)

let _ = Hashtbl.add colors "LightGrey" (rgb8 211 211 211)

let _ = Hashtbl.add colors "light gray" (rgb8 211 211 211)

let _ = Hashtbl.add colors "LightGray" (rgb8 211 211 211)

let _ = Hashtbl.add colors "midnight blue" (rgb8 25 25 112)

let _ = Hashtbl.add colors "MidnightBlue" (rgb8 25 25 112)

let _ = Hashtbl.add colors "navy" (rgb8 0 0 128)

let _ = Hashtbl.add colors "navy blue" (rgb8 0 0 128)

let _ = Hashtbl.add colors "NavyBlue" (rgb8 0 0 128)

let _ = Hashtbl.add colors "cornflower blue" (rgb8 100 149 237)

let _ = Hashtbl.add colors "CornflowerBlue" (rgb8 100 149 237)

let _ = Hashtbl.add colors "dark slate blue" (rgb8 72 61 139)

let _ = Hashtbl.add colors "DarkSlateBlue" (rgb8 72 61 139)

let _ = Hashtbl.add colors "slate blue" (rgb8 106 90 205)

let _ = Hashtbl.add colors "SlateBlue" (rgb8 106 90 205)

let _ = Hashtbl.add colors "medium slate blue" (rgb8 123 104 238)

let _ = Hashtbl.add colors "MediumSlateBlue" (rgb8 123 104 238)

let _ = Hashtbl.add colors "light slate blue" (rgb8 132 112 255)

let _ = Hashtbl.add colors "LightSlateBlue" (rgb8 132 112 255)

let _ = Hashtbl.add colors "medium blue" (rgb8 0 0 205)

let _ = Hashtbl.add colors "MediumBlue" (rgb8 0 0 205)

let _ = Hashtbl.add colors "royal blue" (rgb8 65 105 225)

let _ = Hashtbl.add colors "RoyalBlue" (rgb8 65 105 225)

let _ = Hashtbl.add colors "blue" (rgb8 0 0 255)

let _ = Hashtbl.add colors "dodger blue" (rgb8 30 144 255)

let _ = Hashtbl.add colors "DodgerBlue" (rgb8 30 144 255)

let _ = Hashtbl.add colors "deep sky blue" (rgb8 0 191 255)

let _ = Hashtbl.add colors "DeepSkyBlue" (rgb8 0 191 255)

let _ = Hashtbl.add colors "sky blue" (rgb8 135 206 235)

let _ = Hashtbl.add colors "SkyBlue" (rgb8 135 206 235)

let _ = Hashtbl.add colors "light sky blue" (rgb8 135 206 250)

let _ = Hashtbl.add colors "LightSkyBlue" (rgb8 135 206 250)

let _ = Hashtbl.add colors "steel blue" (rgb8 70 130 180)

let _ = Hashtbl.add colors "SteelBlue" (rgb8 70 130 180)

let _ = Hashtbl.add colors "light steel blue" (rgb8 176 196 222)

let _ = Hashtbl.add colors "LightSteelBlue" (rgb8 176 196 222)

let _ = Hashtbl.add colors "light blue" (rgb8 173 216 230)

let _ = Hashtbl.add colors "LightBlue" (rgb8 173 216 230)

let _ = Hashtbl.add colors "powder blue" (rgb8 176 224 230)

let _ = Hashtbl.add colors "PowderBlue" (rgb8 176 224 230)

let _ = Hashtbl.add colors "pale turquoise" (rgb8 175 238 238)

let _ = Hashtbl.add colors "PaleTurquoise" (rgb8 175 238 238)

let _ = Hashtbl.add colors "dark turquoise" (rgb8 0 206 209)

let _ = Hashtbl.add colors "DarkTurquoise" (rgb8 0 206 209)

let _ = Hashtbl.add colors "medium turquoise" (rgb8 72 209 204)

let _ = Hashtbl.add colors "MediumTurquoise" (rgb8 72 209 204)

let _ = Hashtbl.add colors "turquoise" (rgb8 64 224 208)

let _ = Hashtbl.add colors "cyan" (rgb8 0 255 255)

let _ = Hashtbl.add colors "light cyan" (rgb8 224 255 255)

let _ = Hashtbl.add colors "LightCyan" (rgb8 224 255 255)

let _ = Hashtbl.add colors "cadet blue" (rgb8 95 158 160)

let _ = Hashtbl.add colors "CadetBlue" (rgb8 95 158 160)

let _ = Hashtbl.add colors "medium aquamarine" (rgb8 102 205 170)

let _ = Hashtbl.add colors "MediumAquamarine" (rgb8 102 205 170)

let _ = Hashtbl.add colors "aquamarine" (rgb8 127 255 212)

let _ = Hashtbl.add colors "dark green" (rgb8 0 100 0)

let _ = Hashtbl.add colors "DarkGreen" (rgb8 0 100 0)

let _ = Hashtbl.add colors "dark olive green" (rgb8 85 107 47)

let _ = Hashtbl.add colors "DarkOliveGreen" (rgb8 85 107 47)

let _ = Hashtbl.add colors "dark sea green" (rgb8 143 188 143)

let _ = Hashtbl.add colors "DarkSeaGreen" (rgb8 143 188 143)

let _ = Hashtbl.add colors "sea green" (rgb8 46 139 87)

let _ = Hashtbl.add colors "SeaGreen" (rgb8 46 139 87)

let _ = Hashtbl.add colors "medium sea green" (rgb8 60 179 113)

let _ = Hashtbl.add colors "MediumSeaGreen" (rgb8 60 179 113)

let _ = Hashtbl.add colors "light sea green" (rgb8 32 178 170)

let _ = Hashtbl.add colors "LightSeaGreen" (rgb8 32 178 170)

let _ = Hashtbl.add colors "pale green" (rgb8 152 251 152)

let _ = Hashtbl.add colors "PaleGreen" (rgb8 152 251 152)

let _ = Hashtbl.add colors "spring green" (rgb8 0 255 127)

let _ = Hashtbl.add colors "SpringGreen" (rgb8 0 255 127)

let _ = Hashtbl.add colors "lawn green" (rgb8 124 252 0)

let _ = Hashtbl.add colors "LawnGreen" (rgb8 124 252 0)

let _ = Hashtbl.add colors "green" (rgb8 0 255 0)

let _ = Hashtbl.add colors "chartreuse" (rgb8 127 255 0)

let _ = Hashtbl.add colors "medium spring green" (rgb8 0 250 154)

let _ = Hashtbl.add colors "MediumSpringGreen" (rgb8 0 250 154)

let _ = Hashtbl.add colors "green yellow" (rgb8 173 255 47)

let _ = Hashtbl.add colors "GreenYellow" (rgb8 173 255 47)

let _ = Hashtbl.add colors "lime green" (rgb8 50 205 50)

let _ = Hashtbl.add colors "LimeGreen" (rgb8 50 205 50)

let _ = Hashtbl.add colors "yellow green" (rgb8 154 205 50)

let _ = Hashtbl.add colors "YellowGreen" (rgb8 154 205 50)

let _ = Hashtbl.add colors "forest green" (rgb8 34 139 34)

let _ = Hashtbl.add colors "ForestGreen" (rgb8 34 139 34)

let _ = Hashtbl.add colors "olive drab" (rgb8 107 142 35)

let _ = Hashtbl.add colors "OliveDrab" (rgb8 107 142 35)

let _ = Hashtbl.add colors "dark khaki" (rgb8 189 183 107)

let _ = Hashtbl.add colors "DarkKhaki" (rgb8 189 183 107)

let _ = Hashtbl.add colors "khaki" (rgb8 240 230 140)

let _ = Hashtbl.add colors "pale goldenrod" (rgb8 238 232 170)

let _ = Hashtbl.add colors "PaleGoldenrod" (rgb8 238 232 170)

let _ = Hashtbl.add colors "light goldenrod yellow" (rgb8 250 250 210)

let _ = Hashtbl.add colors "LightGoldenrodYellow" (rgb8 250 250 210)

let _ = Hashtbl.add colors "light yellow" (rgb8 255 255 224)

let _ = Hashtbl.add colors "LightYellow" (rgb8 255 255 224)

let _ = Hashtbl.add colors "yellow" (rgb8 255 255 0)

let _ = Hashtbl.add colors "gold" (rgb8 255 215 0)

let _ = Hashtbl.add colors "light goldenrod" (rgb8 238 221 130)

let _ = Hashtbl.add colors "LightGoldenrod" (rgb8 238 221 130)

let _ = Hashtbl.add colors "goldenrod" (rgb8 218 165 32)

let _ = Hashtbl.add colors "dark goldenrod" (rgb8 184 134 11)

let _ = Hashtbl.add colors "DarkGoldenrod" (rgb8 184 134 11)

let _ = Hashtbl.add colors "rosy brown" (rgb8 188 143 143)

let _ = Hashtbl.add colors "RosyBrown" (rgb8 188 143 143)

let _ = Hashtbl.add colors "indian red" (rgb8 205 92 92)

let _ = Hashtbl.add colors "IndianRed" (rgb8 205 92 92)

let _ = Hashtbl.add colors "saddle brown" (rgb8 139 69 19)

let _ = Hashtbl.add colors "SaddleBrown" (rgb8 139 69 19)

let _ = Hashtbl.add colors "sienna" (rgb8 160 82 45)

let _ = Hashtbl.add colors "peru" (rgb8 205 133 63)

let _ = Hashtbl.add colors "burlywood" (rgb8 222 184 135)

let _ = Hashtbl.add colors "beige" (rgb8 245 245 220)

let _ = Hashtbl.add colors "wheat" (rgb8 245 222 179)

let _ = Hashtbl.add colors "sandy brown" (rgb8 244 164 96)

let _ = Hashtbl.add colors "SandyBrown" (rgb8 244 164 96)

let _ = Hashtbl.add colors "tan" (rgb8 210 180 140)

let _ = Hashtbl.add colors "chocolate" (rgb8 210 105 30)

let _ = Hashtbl.add colors "firebrick" (rgb8 178 34 34)

let _ = Hashtbl.add colors "brown" (rgb8 165 42 42)

let _ = Hashtbl.add colors "dark salmon" (rgb8 233 150 122)

let _ = Hashtbl.add colors "DarkSalmon" (rgb8 233 150 122)

let _ = Hashtbl.add colors "salmon" (rgb8 250 128 114)

let _ = Hashtbl.add colors "light salmon" (rgb8 255 160 122)

let _ = Hashtbl.add colors "LightSalmon" (rgb8 255 160 122)

let _ = Hashtbl.add colors "orange" (rgb8 255 165 0)

let _ = Hashtbl.add colors "dark orange" (rgb8 255 140 0)

let _ = Hashtbl.add colors "DarkOrange" (rgb8 255 140 0)

let _ = Hashtbl.add colors "coral" (rgb8 255 127 80)

let _ = Hashtbl.add colors "light coral" (rgb8 240 128 128)

let _ = Hashtbl.add colors "LightCoral" (rgb8 240 128 128)

let _ = Hashtbl.add colors "tomato" (rgb8 255 99 71)

let _ = Hashtbl.add colors "orange red" (rgb8 255 69 0)

let _ = Hashtbl.add colors "OrangeRed" (rgb8 255 69 0)

let _ = Hashtbl.add colors "red" (rgb8 255 0 0)

let _ = Hashtbl.add colors "hot pink" (rgb8 255 105 180)

let _ = Hashtbl.add colors "HotPink" (rgb8 255 105 180)

let _ = Hashtbl.add colors "deep pink" (rgb8 255 20 147)

let _ = Hashtbl.add colors "DeepPink" (rgb8 255 20 147)

let _ = Hashtbl.add colors "pink" (rgb8 255 192 203)

let _ = Hashtbl.add colors "light pink" (rgb8 255 182 193)

let _ = Hashtbl.add colors "LightPink" (rgb8 255 182 193)

let _ = Hashtbl.add colors "pale violet red" (rgb8 219 112 147)

let _ = Hashtbl.add colors "PaleVioletRed" (rgb8 219 112 147)

let _ = Hashtbl.add colors "maroon" (rgb8 176 48 96)

let _ = Hashtbl.add colors "medium violet red" (rgb8 199 21 133)

let _ = Hashtbl.add colors "MediumVioletRed" (rgb8 199 21 133)

let _ = Hashtbl.add colors "violet red" (rgb8 208 32 144)

let _ = Hashtbl.add colors "VioletRed" (rgb8 208 32 144)

let _ = Hashtbl.add colors "magenta" (rgb8 255 0 255)

let _ = Hashtbl.add colors "violet" (rgb8 238 130 238)

let _ = Hashtbl.add colors "plum" (rgb8 221 160 221)

let _ = Hashtbl.add colors "orchid" (rgb8 218 112 214)

let _ = Hashtbl.add colors "medium orchid" (rgb8 186 85 211)

let _ = Hashtbl.add colors "MediumOrchid" (rgb8 186 85 211)

let _ = Hashtbl.add colors "dark orchid" (rgb8 153 50 204)

let _ = Hashtbl.add colors "DarkOrchid" (rgb8 153 50 204)

let _ = Hashtbl.add colors "dark violet" (rgb8 148 0 211)

let _ = Hashtbl.add colors "DarkViolet" (rgb8 148 0 211)

let _ = Hashtbl.add colors "blue violet" (rgb8 138 43 226)

let _ = Hashtbl.add colors "BlueViolet" (rgb8 138 43 226)

let _ = Hashtbl.add colors "purple" (rgb8 160 32 240)

let _ = Hashtbl.add colors "medium purple" (rgb8 147 112 219)

let _ = Hashtbl.add colors "MediumPurple" (rgb8 147 112 219)

let _ = Hashtbl.add colors "thistle" (rgb8 216 191 216)

let _ = Hashtbl.add colors "snow1" (rgb8 255 250 250)

let _ = Hashtbl.add colors "snow2" (rgb8 238 233 233)

let _ = Hashtbl.add colors "snow3" (rgb8 205 201 201)

let _ = Hashtbl.add colors "snow4" (rgb8 139 137 137)

let _ = Hashtbl.add colors "seashell1" (rgb8 255 245 238)

let _ = Hashtbl.add colors "seashell2" (rgb8 238 229 222)

let _ = Hashtbl.add colors "seashell3" (rgb8 205 197 191)

let _ = Hashtbl.add colors "seashell4" (rgb8 139 134 130)

let _ = Hashtbl.add colors "AntiqueWhite1" (rgb8 255 239 219)

let _ = Hashtbl.add colors "AntiqueWhite2" (rgb8 238 223 204)

let _ = Hashtbl.add colors "AntiqueWhite3" (rgb8 205 192 176)

let _ = Hashtbl.add colors "AntiqueWhite4" (rgb8 139 131 120)

let _ = Hashtbl.add colors "bisque1" (rgb8 255 228 196)

let _ = Hashtbl.add colors "bisque2" (rgb8 238 213 183)

let _ = Hashtbl.add colors "bisque3" (rgb8 205 183 158)

let _ = Hashtbl.add colors "bisque4" (rgb8 139 125 107)

let _ = Hashtbl.add colors "PeachPuff1" (rgb8 255 218 185)

let _ = Hashtbl.add colors "PeachPuff2" (rgb8 238 203 173)

let _ = Hashtbl.add colors "PeachPuff3" (rgb8 205 175 149)

let _ = Hashtbl.add colors "PeachPuff4" (rgb8 139 119 101)

let _ = Hashtbl.add colors "NavajoWhite1" (rgb8 255 222 173)

let _ = Hashtbl.add colors "NavajoWhite2" (rgb8 238 207 161)

let _ = Hashtbl.add colors "NavajoWhite3" (rgb8 205 179 139)

let _ = Hashtbl.add colors "NavajoWhite4" (rgb8 139 121 94)

let _ = Hashtbl.add colors "LemonChiffon1" (rgb8 255 250 205)

let _ = Hashtbl.add colors "LemonChiffon2" (rgb8 238 233 191)

let _ = Hashtbl.add colors "LemonChiffon3" (rgb8 205 201 165)

let _ = Hashtbl.add colors "LemonChiffon4" (rgb8 139 137 112)

let _ = Hashtbl.add colors "cornsilk1" (rgb8 255 248 220)

let _ = Hashtbl.add colors "cornsilk2" (rgb8 238 232 205)

let _ = Hashtbl.add colors "cornsilk3" (rgb8 205 200 177)

let _ = Hashtbl.add colors "cornsilk4" (rgb8 139 136 120)

let _ = Hashtbl.add colors "ivory1" (rgb8 255 255 240)

let _ = Hashtbl.add colors "ivory2" (rgb8 238 238 224)

let _ = Hashtbl.add colors "ivory3" (rgb8 205 205 193)

let _ = Hashtbl.add colors "ivory4" (rgb8 139 139 131)

let _ = Hashtbl.add colors "honeydew1" (rgb8 240 255 240)

let _ = Hashtbl.add colors "honeydew2" (rgb8 224 238 224)

let _ = Hashtbl.add colors "honeydew3" (rgb8 193 205 193)

let _ = Hashtbl.add colors "honeydew4" (rgb8 131 139 131)

let _ = Hashtbl.add colors "LavenderBlush1" (rgb8 255 240 245)

let _ = Hashtbl.add colors "LavenderBlush2" (rgb8 238 224 229)

let _ = Hashtbl.add colors "LavenderBlush3" (rgb8 205 193 197)

let _ = Hashtbl.add colors "LavenderBlush4" (rgb8 139 131 134)

let _ = Hashtbl.add colors "MistyRose1" (rgb8 255 228 225)

let _ = Hashtbl.add colors "MistyRose2" (rgb8 238 213 210)

let _ = Hashtbl.add colors "MistyRose3" (rgb8 205 183 181)

let _ = Hashtbl.add colors "MistyRose4" (rgb8 139 125 123)

let _ = Hashtbl.add colors "azure1" (rgb8 240 255 255)

let _ = Hashtbl.add colors "azure2" (rgb8 224 238 238)

let _ = Hashtbl.add colors "azure3" (rgb8 193 205 205)

let _ = Hashtbl.add colors "azure4" (rgb8 131 139 139)

let _ = Hashtbl.add colors "SlateBlue1" (rgb8 131 111 255)

let _ = Hashtbl.add colors "SlateBlue2" (rgb8 122 103 238)

let _ = Hashtbl.add colors "SlateBlue3" (rgb8 105 89 205)

let _ = Hashtbl.add colors "SlateBlue4" (rgb8 71 60 139)

let _ = Hashtbl.add colors "RoyalBlue1" (rgb8 72 118 255)

let _ = Hashtbl.add colors "RoyalBlue2" (rgb8 67 110 238)

let _ = Hashtbl.add colors "RoyalBlue3" (rgb8 58 95 205)

let _ = Hashtbl.add colors "RoyalBlue4" (rgb8 39 64 139)

let _ = Hashtbl.add colors "blue1" (rgb8 0 0 255)

let _ = Hashtbl.add colors "blue2" (rgb8 0 0 238)

let _ = Hashtbl.add colors "blue3" (rgb8 0 0 205)

let _ = Hashtbl.add colors "blue4" (rgb8 0 0 139)

let _ = Hashtbl.add colors "DodgerBlue1" (rgb8 30 144 255)

let _ = Hashtbl.add colors "DodgerBlue2" (rgb8 28 134 238)

let _ = Hashtbl.add colors "DodgerBlue3" (rgb8 24 116 205)

let _ = Hashtbl.add colors "DodgerBlue4" (rgb8 16 78 139)

let _ = Hashtbl.add colors "SteelBlue1" (rgb8 99 184 255)

let _ = Hashtbl.add colors "SteelBlue2" (rgb8 92 172 238)

let _ = Hashtbl.add colors "SteelBlue3" (rgb8 79 148 205)

let _ = Hashtbl.add colors "SteelBlue4" (rgb8 54 100 139)

let _ = Hashtbl.add colors "DeepSkyBlue1" (rgb8 0 191 255)

let _ = Hashtbl.add colors "DeepSkyBlue2" (rgb8 0 178 238)

let _ = Hashtbl.add colors "DeepSkyBlue3" (rgb8 0 154 205)

let _ = Hashtbl.add colors "DeepSkyBlue4" (rgb8 0 104 139)

let _ = Hashtbl.add colors "SkyBlue1" (rgb8 135 206 255)

let _ = Hashtbl.add colors "SkyBlue2" (rgb8 126 192 238)

let _ = Hashtbl.add colors "SkyBlue3" (rgb8 108 166 205)

let _ = Hashtbl.add colors "SkyBlue4" (rgb8 74 112 139)

let _ = Hashtbl.add colors "LightSkyBlue1" (rgb8 176 226 255)

let _ = Hashtbl.add colors "LightSkyBlue2" (rgb8 164 211 238)

let _ = Hashtbl.add colors "LightSkyBlue3" (rgb8 141 182 205)

let _ = Hashtbl.add colors "LightSkyBlue4" (rgb8 96 123 139)

let _ = Hashtbl.add colors "SlateGray1" (rgb8 198 226 255)

let _ = Hashtbl.add colors "SlateGray2" (rgb8 185 211 238)

let _ = Hashtbl.add colors "SlateGray3" (rgb8 159 182 205)

let _ = Hashtbl.add colors "SlateGray4" (rgb8 108 123 139)

let _ = Hashtbl.add colors "LightSteelBlue1" (rgb8 202 225 255)

let _ = Hashtbl.add colors "LightSteelBlue2" (rgb8 188 210 238)

let _ = Hashtbl.add colors "LightSteelBlue3" (rgb8 162 181 205)

let _ = Hashtbl.add colors "LightSteelBlue4" (rgb8 110 123 139)

let _ = Hashtbl.add colors "LightBlue1" (rgb8 191 239 255)

let _ = Hashtbl.add colors "LightBlue2" (rgb8 178 223 238)

let _ = Hashtbl.add colors "LightBlue3" (rgb8 154 192 205)

let _ = Hashtbl.add colors "LightBlue4" (rgb8 104 131 139)

let _ = Hashtbl.add colors "LightCyan1" (rgb8 224 255 255)

let _ = Hashtbl.add colors "LightCyan2" (rgb8 209 238 238)

let _ = Hashtbl.add colors "LightCyan3" (rgb8 180 205 205)

let _ = Hashtbl.add colors "LightCyan4" (rgb8 122 139 139)

let _ = Hashtbl.add colors "PaleTurquoise1" (rgb8 187 255 255)

let _ = Hashtbl.add colors "PaleTurquoise2" (rgb8 174 238 238)

let _ = Hashtbl.add colors "PaleTurquoise3" (rgb8 150 205 205)

let _ = Hashtbl.add colors "PaleTurquoise4" (rgb8 102 139 139)

let _ = Hashtbl.add colors "CadetBlue1" (rgb8 152 245 255)

let _ = Hashtbl.add colors "CadetBlue2" (rgb8 142 229 238)

let _ = Hashtbl.add colors "CadetBlue3" (rgb8 122 197 205)

let _ = Hashtbl.add colors "CadetBlue4" (rgb8 83 134 139)

let _ = Hashtbl.add colors "turquoise1" (rgb8 0 245 255)

let _ = Hashtbl.add colors "turquoise2" (rgb8 0 229 238)

let _ = Hashtbl.add colors "turquoise3" (rgb8 0 197 205)

let _ = Hashtbl.add colors "turquoise4" (rgb8 0 134 139)

let _ = Hashtbl.add colors "cyan1" (rgb8 0 255 255)

let _ = Hashtbl.add colors "cyan2" (rgb8 0 238 238)

let _ = Hashtbl.add colors "cyan3" (rgb8 0 205 205)

let _ = Hashtbl.add colors "cyan4" (rgb8 0 139 139)

let _ = Hashtbl.add colors "DarkSlateGray1" (rgb8 151 255 255)

let _ = Hashtbl.add colors "DarkSlateGray2" (rgb8 141 238 238)

let _ = Hashtbl.add colors "DarkSlateGray3" (rgb8 121 205 205)

let _ = Hashtbl.add colors "DarkSlateGray4" (rgb8 82 139 139)

let _ = Hashtbl.add colors "aquamarine1" (rgb8 127 255 212)

let _ = Hashtbl.add colors "aquamarine2" (rgb8 118 238 198)

let _ = Hashtbl.add colors "aquamarine3" (rgb8 102 205 170)

let _ = Hashtbl.add colors "aquamarine4" (rgb8 69 139 116)

let _ = Hashtbl.add colors "DarkSeaGreen1" (rgb8 193 255 193)

let _ = Hashtbl.add colors "DarkSeaGreen2" (rgb8 180 238 180)

let _ = Hashtbl.add colors "DarkSeaGreen3" (rgb8 155 205 155)

let _ = Hashtbl.add colors "DarkSeaGreen4" (rgb8 105 139 105)

let _ = Hashtbl.add colors "SeaGreen1" (rgb8 84 255 159)

let _ = Hashtbl.add colors "SeaGreen2" (rgb8 78 238 148)

let _ = Hashtbl.add colors "SeaGreen3" (rgb8 67 205 128)

let _ = Hashtbl.add colors "SeaGreen4" (rgb8 46 139 87)

let _ = Hashtbl.add colors "PaleGreen1" (rgb8 154 255 154)

let _ = Hashtbl.add colors "PaleGreen2" (rgb8 144 238 144)

let _ = Hashtbl.add colors "PaleGreen3" (rgb8 124 205 124)

let _ = Hashtbl.add colors "PaleGreen4" (rgb8 84 139 84)

let _ = Hashtbl.add colors "SpringGreen1" (rgb8 0 255 127)

let _ = Hashtbl.add colors "SpringGreen2" (rgb8 0 238 118)

let _ = Hashtbl.add colors "SpringGreen3" (rgb8 0 205 102)

let _ = Hashtbl.add colors "SpringGreen4" (rgb8 0 139 69)

let _ = Hashtbl.add colors "green1" (rgb8 0 255 0)

let _ = Hashtbl.add colors "green2" (rgb8 0 238 0)

let _ = Hashtbl.add colors "green3" (rgb8 0 205 0)

let _ = Hashtbl.add colors "green4" (rgb8 0 139 0)

let _ = Hashtbl.add colors "chartreuse1" (rgb8 127 255 0)

let _ = Hashtbl.add colors "chartreuse2" (rgb8 118 238 0)

let _ = Hashtbl.add colors "chartreuse3" (rgb8 102 205 0)

let _ = Hashtbl.add colors "chartreuse4" (rgb8 69 139 0)

let _ = Hashtbl.add colors "OliveDrab1" (rgb8 192 255 62)

let _ = Hashtbl.add colors "OliveDrab2" (rgb8 179 238 58)

let _ = Hashtbl.add colors "OliveDrab3" (rgb8 154 205 50)

let _ = Hashtbl.add colors "OliveDrab4" (rgb8 105 139 34)

let _ = Hashtbl.add colors "DarkOliveGreen1" (rgb8 202 255 112)

let _ = Hashtbl.add colors "DarkOliveGreen2" (rgb8 188 238 104)

let _ = Hashtbl.add colors "DarkOliveGreen3" (rgb8 162 205 90)

let _ = Hashtbl.add colors "DarkOliveGreen4" (rgb8 110 139 61)

let _ = Hashtbl.add colors "khaki1" (rgb8 255 246 143)

let _ = Hashtbl.add colors "khaki2" (rgb8 238 230 133)

let _ = Hashtbl.add colors "khaki3" (rgb8 205 198 115)

let _ = Hashtbl.add colors "khaki4" (rgb8 139 134 78)

let _ = Hashtbl.add colors "LightGoldenrod1" (rgb8 255 236 139)

let _ = Hashtbl.add colors "LightGoldenrod2" (rgb8 238 220 130)

let _ = Hashtbl.add colors "LightGoldenrod3" (rgb8 205 190 112)

let _ = Hashtbl.add colors "LightGoldenrod4" (rgb8 139 129 76)

let _ = Hashtbl.add colors "LightYellow1" (rgb8 255 255 224)

let _ = Hashtbl.add colors "LightYellow2" (rgb8 238 238 209)

let _ = Hashtbl.add colors "LightYellow3" (rgb8 205 205 180)

let _ = Hashtbl.add colors "LightYellow4" (rgb8 139 139 122)

let _ = Hashtbl.add colors "yellow1" (rgb8 255 255 0)

let _ = Hashtbl.add colors "yellow2" (rgb8 238 238 0)

let _ = Hashtbl.add colors "yellow3" (rgb8 205 205 0)

let _ = Hashtbl.add colors "yellow4" (rgb8 139 139 0)

let _ = Hashtbl.add colors "gold1" (rgb8 255 215 0)

let _ = Hashtbl.add colors "gold2" (rgb8 238 201 0)

let _ = Hashtbl.add colors "gold3" (rgb8 205 173 0)

let _ = Hashtbl.add colors "gold4" (rgb8 139 117 0)

let _ = Hashtbl.add colors "goldenrod1" (rgb8 255 193 37)

let _ = Hashtbl.add colors "goldenrod2" (rgb8 238 180 34)

let _ = Hashtbl.add colors "goldenrod3" (rgb8 205 155 29)

let _ = Hashtbl.add colors "goldenrod4" (rgb8 139 105 20)

let _ = Hashtbl.add colors "DarkGoldenrod1" (rgb8 255 185 15)

let _ = Hashtbl.add colors "DarkGoldenrod2" (rgb8 238 173 14)

let _ = Hashtbl.add colors "DarkGoldenrod3" (rgb8 205 149 12)

let _ = Hashtbl.add colors "DarkGoldenrod4" (rgb8 139 101 8)

let _ = Hashtbl.add colors "RosyBrown1" (rgb8 255 193 193)

let _ = Hashtbl.add colors "RosyBrown2" (rgb8 238 180 180)

let _ = Hashtbl.add colors "RosyBrown3" (rgb8 205 155 155)

let _ = Hashtbl.add colors "RosyBrown4" (rgb8 139 105 105)

let _ = Hashtbl.add colors "IndianRed1" (rgb8 255 106 106)

let _ = Hashtbl.add colors "IndianRed2" (rgb8 238 99 99)

let _ = Hashtbl.add colors "IndianRed3" (rgb8 205 85 85)

let _ = Hashtbl.add colors "IndianRed4" (rgb8 139 58 58)

let _ = Hashtbl.add colors "sienna1" (rgb8 255 130 71)

let _ = Hashtbl.add colors "sienna2" (rgb8 238 121 66)

let _ = Hashtbl.add colors "sienna3" (rgb8 205 104 57)

let _ = Hashtbl.add colors "sienna4" (rgb8 139 71 38)

let _ = Hashtbl.add colors "burlywood1" (rgb8 255 211 155)

let _ = Hashtbl.add colors "burlywood2" (rgb8 238 197 145)

let _ = Hashtbl.add colors "burlywood3" (rgb8 205 170 125)

let _ = Hashtbl.add colors "burlywood4" (rgb8 139 115 85)

let _ = Hashtbl.add colors "wheat1" (rgb8 255 231 186)

let _ = Hashtbl.add colors "wheat2" (rgb8 238 216 174)

let _ = Hashtbl.add colors "wheat3" (rgb8 205 186 150)

let _ = Hashtbl.add colors "wheat4" (rgb8 139 126 102)

let _ = Hashtbl.add colors "tan1" (rgb8 255 165 79)

let _ = Hashtbl.add colors "tan2" (rgb8 238 154 73)

let _ = Hashtbl.add colors "tan3" (rgb8 205 133 63)

let _ = Hashtbl.add colors "tan4" (rgb8 139 90 43)

let _ = Hashtbl.add colors "chocolate1" (rgb8 255 127 36)

let _ = Hashtbl.add colors "chocolate2" (rgb8 238 118 33)

let _ = Hashtbl.add colors "chocolate3" (rgb8 205 102 29)

let _ = Hashtbl.add colors "chocolate4" (rgb8 139 69 19)

let _ = Hashtbl.add colors "firebrick1" (rgb8 255 48 48)

let _ = Hashtbl.add colors "firebrick2" (rgb8 238 44 44)

let _ = Hashtbl.add colors "firebrick3" (rgb8 205 38 38)

let _ = Hashtbl.add colors "firebrick4" (rgb8 139 26 26)

let _ = Hashtbl.add colors "brown1" (rgb8 255 64 64)

let _ = Hashtbl.add colors "brown2" (rgb8 238 59 59)

let _ = Hashtbl.add colors "brown3" (rgb8 205 51 51)

let _ = Hashtbl.add colors "brown4" (rgb8 139 35 35)

let _ = Hashtbl.add colors "salmon1" (rgb8 255 140 105)

let _ = Hashtbl.add colors "salmon2" (rgb8 238 130 98)

let _ = Hashtbl.add colors "salmon3" (rgb8 205 112 84)

let _ = Hashtbl.add colors "salmon4" (rgb8 139 76 57)

let _ = Hashtbl.add colors "LightSalmon1" (rgb8 255 160 122)

let _ = Hashtbl.add colors "LightSalmon2" (rgb8 238 149 114)

let _ = Hashtbl.add colors "LightSalmon3" (rgb8 205 129 98)

let _ = Hashtbl.add colors "LightSalmon4" (rgb8 139 87 66)

let _ = Hashtbl.add colors "orange1" (rgb8 255 165 0)

let _ = Hashtbl.add colors "orange2" (rgb8 238 154 0)

let _ = Hashtbl.add colors "orange3" (rgb8 205 133 0)

let _ = Hashtbl.add colors "orange4" (rgb8 139 90 0)

let _ = Hashtbl.add colors "DarkOrange1" (rgb8 255 127 0)

let _ = Hashtbl.add colors "DarkOrange2" (rgb8 238 118 0)

let _ = Hashtbl.add colors "DarkOrange3" (rgb8 205 102 0)

let _ = Hashtbl.add colors "DarkOrange4" (rgb8 139 69 0)

let _ = Hashtbl.add colors "coral1" (rgb8 255 114 86)

let _ = Hashtbl.add colors "coral2" (rgb8 238 106 80)

let _ = Hashtbl.add colors "coral3" (rgb8 205 91 69)

let _ = Hashtbl.add colors "coral4" (rgb8 139 62 47)

let _ = Hashtbl.add colors "tomato1" (rgb8 255 99 71)

let _ = Hashtbl.add colors "tomato2" (rgb8 238 92 66)

let _ = Hashtbl.add colors "tomato3" (rgb8 205 79 57)

let _ = Hashtbl.add colors "tomato4" (rgb8 139 54 38)

let _ = Hashtbl.add colors "OrangeRed1" (rgb8 255 69 0)

let _ = Hashtbl.add colors "OrangeRed2" (rgb8 238 64 0)

let _ = Hashtbl.add colors "OrangeRed3" (rgb8 205 55 0)

let _ = Hashtbl.add colors "OrangeRed4" (rgb8 139 37 0)

let _ = Hashtbl.add colors "red1" (rgb8 255 0 0)

let _ = Hashtbl.add colors "red2" (rgb8 238 0 0)

let _ = Hashtbl.add colors "red3" (rgb8 205 0 0)

let _ = Hashtbl.add colors "red4" (rgb8 139 0 0)

let _ = Hashtbl.add colors "DebianRed" (rgb8 215 7 81)

let _ = Hashtbl.add colors "DeepPink1" (rgb8 255 20 147)

let _ = Hashtbl.add colors "DeepPink2" (rgb8 238 18 137)

let _ = Hashtbl.add colors "DeepPink3" (rgb8 205 16 118)

let _ = Hashtbl.add colors "DeepPink4" (rgb8 139 10 80)

let _ = Hashtbl.add colors "HotPink1" (rgb8 255 110 180)

let _ = Hashtbl.add colors "HotPink2" (rgb8 238 106 167)

let _ = Hashtbl.add colors "HotPink3" (rgb8 205 96 144)

let _ = Hashtbl.add colors "HotPink4" (rgb8 139 58 98)

let _ = Hashtbl.add colors "pink1" (rgb8 255 181 197)

let _ = Hashtbl.add colors "pink2" (rgb8 238 169 184)

let _ = Hashtbl.add colors "pink3" (rgb8 205 145 158)

let _ = Hashtbl.add colors "pink4" (rgb8 139 99 108)

let _ = Hashtbl.add colors "LightPink1" (rgb8 255 174 185)

let _ = Hashtbl.add colors "LightPink2" (rgb8 238 162 173)

let _ = Hashtbl.add colors "LightPink3" (rgb8 205 140 149)

let _ = Hashtbl.add colors "LightPink4" (rgb8 139 95 101)

let _ = Hashtbl.add colors "PaleVioletRed1" (rgb8 255 130 171)

let _ = Hashtbl.add colors "PaleVioletRed2" (rgb8 238 121 159)

let _ = Hashtbl.add colors "PaleVioletRed3" (rgb8 205 104 137)

let _ = Hashtbl.add colors "PaleVioletRed4" (rgb8 139 71 93)

let _ = Hashtbl.add colors "maroon1" (rgb8 255 52 179)

let _ = Hashtbl.add colors "maroon2" (rgb8 238 48 167)

let _ = Hashtbl.add colors "maroon3" (rgb8 205 41 144)

let _ = Hashtbl.add colors "maroon4" (rgb8 139 28 98)

let _ = Hashtbl.add colors "VioletRed1" (rgb8 255 62 150)

let _ = Hashtbl.add colors "VioletRed2" (rgb8 238 58 140)

let _ = Hashtbl.add colors "VioletRed3" (rgb8 205 50 120)

let _ = Hashtbl.add colors "VioletRed4" (rgb8 139 34 82)

let _ = Hashtbl.add colors "magenta1" (rgb8 255 0 255)

let _ = Hashtbl.add colors "magenta2" (rgb8 238 0 238)

let _ = Hashtbl.add colors "magenta3" (rgb8 205 0 205)

let _ = Hashtbl.add colors "magenta4" (rgb8 139 0 139)

let _ = Hashtbl.add colors "orchid1" (rgb8 255 131 250)

let _ = Hashtbl.add colors "orchid2" (rgb8 238 122 233)

let _ = Hashtbl.add colors "orchid3" (rgb8 205 105 201)

let _ = Hashtbl.add colors "orchid4" (rgb8 139 71 137)

let _ = Hashtbl.add colors "plum1" (rgb8 255 187 255)

let _ = Hashtbl.add colors "plum2" (rgb8 238 174 238)

let _ = Hashtbl.add colors "plum3" (rgb8 205 150 205)

let _ = Hashtbl.add colors "plum4" (rgb8 139 102 139)

let _ = Hashtbl.add colors "MediumOrchid1" (rgb8 224 102 255)

let _ = Hashtbl.add colors "MediumOrchid2" (rgb8 209 95 238)

let _ = Hashtbl.add colors "MediumOrchid3" (rgb8 180 82 205)

let _ = Hashtbl.add colors "MediumOrchid4" (rgb8 122 55 139)

let _ = Hashtbl.add colors "DarkOrchid1" (rgb8 191 62 255)

let _ = Hashtbl.add colors "DarkOrchid2" (rgb8 178 58 238)

let _ = Hashtbl.add colors "DarkOrchid3" (rgb8 154 50 205)

let _ = Hashtbl.add colors "DarkOrchid4" (rgb8 104 34 139)

let _ = Hashtbl.add colors "purple1" (rgb8 155 48 255)

let _ = Hashtbl.add colors "purple2" (rgb8 145 44 238)

let _ = Hashtbl.add colors "purple3" (rgb8 125 38 205)

let _ = Hashtbl.add colors "purple4" (rgb8 85 26 139)

let _ = Hashtbl.add colors "MediumPurple1" (rgb8 171 130 255)

let _ = Hashtbl.add colors "MediumPurple2" (rgb8 159 121 238)

let _ = Hashtbl.add colors "MediumPurple3" (rgb8 137 104 205)

let _ = Hashtbl.add colors "MediumPurple4" (rgb8 93 71 139)

let _ = Hashtbl.add colors "thistle1" (rgb8 255 225 255)

let _ = Hashtbl.add colors "thistle2" (rgb8 238 210 238)

let _ = Hashtbl.add colors "thistle3" (rgb8 205 181 205)

let _ = Hashtbl.add colors "thistle4" (rgb8 139 123 139)

let _ = Hashtbl.add colors "gray0" (rgb8 0 0 0)

let _ = Hashtbl.add colors "grey0" (rgb8 0 0 0)

let _ = Hashtbl.add colors "gray1" (rgb8 3 3 3)

let _ = Hashtbl.add colors "grey1" (rgb8 3 3 3)

let _ = Hashtbl.add colors "gray2" (rgb8 5 5 5)

let _ = Hashtbl.add colors "grey2" (rgb8 5 5 5)

let _ = Hashtbl.add colors "gray3" (rgb8 8 8 8)

let _ = Hashtbl.add colors "grey3" (rgb8 8 8 8)

let _ = Hashtbl.add colors "gray4" (rgb8 10 10 10)

let _ = Hashtbl.add colors "grey4" (rgb8 10 10 10)

let _ = Hashtbl.add colors "gray5" (rgb8 13 13 13)

let _ = Hashtbl.add colors "grey5" (rgb8 13 13 13)

let _ = Hashtbl.add colors "gray6" (rgb8 15 15 15)

let _ = Hashtbl.add colors "grey6" (rgb8 15 15 15)

let _ = Hashtbl.add colors "gray7" (rgb8 18 18 18)

let _ = Hashtbl.add colors "grey7" (rgb8 18 18 18)

let _ = Hashtbl.add colors "gray8" (rgb8 20 20 20)

let _ = Hashtbl.add colors "grey8" (rgb8 20 20 20)

let _ = Hashtbl.add colors "gray9" (rgb8 23 23 23)

let _ = Hashtbl.add colors "grey9" (rgb8 23 23 23)

let _ = Hashtbl.add colors "gray10" (rgb8 26 26 26)

let _ = Hashtbl.add colors "grey10" (rgb8 26 26 26)

let _ = Hashtbl.add colors "gray11" (rgb8 28 28 28)

let _ = Hashtbl.add colors "grey11" (rgb8 28 28 28)

let _ = Hashtbl.add colors "gray12" (rgb8 31 31 31)

let _ = Hashtbl.add colors "grey12" (rgb8 31 31 31)

let _ = Hashtbl.add colors "gray13" (rgb8 33 33 33)

let _ = Hashtbl.add colors "grey13" (rgb8 33 33 33)

let _ = Hashtbl.add colors "gray14" (rgb8 36 36 36)

let _ = Hashtbl.add colors "grey14" (rgb8 36 36 36)

let _ = Hashtbl.add colors "gray15" (rgb8 38 38 38)

let _ = Hashtbl.add colors "grey15" (rgb8 38 38 38)

let _ = Hashtbl.add colors "gray16" (rgb8 41 41 41)

let _ = Hashtbl.add colors "grey16" (rgb8 41 41 41)

let _ = Hashtbl.add colors "gray17" (rgb8 43 43 43)

let _ = Hashtbl.add colors "grey17" (rgb8 43 43 43)

let _ = Hashtbl.add colors "gray18" (rgb8 46 46 46)

let _ = Hashtbl.add colors "grey18" (rgb8 46 46 46)

let _ = Hashtbl.add colors "gray19" (rgb8 48 48 48)

let _ = Hashtbl.add colors "grey19" (rgb8 48 48 48)

let _ = Hashtbl.add colors "gray20" (rgb8 51 51 51)

let _ = Hashtbl.add colors "grey20" (rgb8 51 51 51)

let _ = Hashtbl.add colors "gray21" (rgb8 54 54 54)

let _ = Hashtbl.add colors "grey21" (rgb8 54 54 54)

let _ = Hashtbl.add colors "gray22" (rgb8 56 56 56)

let _ = Hashtbl.add colors "grey22" (rgb8 56 56 56)

let _ = Hashtbl.add colors "gray23" (rgb8 59 59 59)

let _ = Hashtbl.add colors "grey23" (rgb8 59 59 59)

let _ = Hashtbl.add colors "gray24" (rgb8 61 61 61)

let _ = Hashtbl.add colors "grey24" (rgb8 61 61 61)

let _ = Hashtbl.add colors "gray25" (rgb8 64 64 64)

let _ = Hashtbl.add colors "grey25" (rgb8 64 64 64)

let _ = Hashtbl.add colors "gray26" (rgb8 66 66 66)

let _ = Hashtbl.add colors "grey26" (rgb8 66 66 66)

let _ = Hashtbl.add colors "gray27" (rgb8 69 69 69)

let _ = Hashtbl.add colors "grey27" (rgb8 69 69 69)

let _ = Hashtbl.add colors "gray28" (rgb8 71 71 71)

let _ = Hashtbl.add colors "grey28" (rgb8 71 71 71)

let _ = Hashtbl.add colors "gray29" (rgb8 74 74 74)

let _ = Hashtbl.add colors "grey29" (rgb8 74 74 74)

let _ = Hashtbl.add colors "gray30" (rgb8 77 77 77)

let _ = Hashtbl.add colors "grey30" (rgb8 77 77 77)

let _ = Hashtbl.add colors "gray31" (rgb8 79 79 79)

let _ = Hashtbl.add colors "grey31" (rgb8 79 79 79)

let _ = Hashtbl.add colors "gray32" (rgb8 82 82 82)

let _ = Hashtbl.add colors "grey32" (rgb8 82 82 82)

let _ = Hashtbl.add colors "gray33" (rgb8 84 84 84)

let _ = Hashtbl.add colors "grey33" (rgb8 84 84 84)

let _ = Hashtbl.add colors "gray34" (rgb8 87 87 87)

let _ = Hashtbl.add colors "grey34" (rgb8 87 87 87)

let _ = Hashtbl.add colors "gray35" (rgb8 89 89 89)

let _ = Hashtbl.add colors "grey35" (rgb8 89 89 89)

let _ = Hashtbl.add colors "gray36" (rgb8 92 92 92)

let _ = Hashtbl.add colors "grey36" (rgb8 92 92 92)

let _ = Hashtbl.add colors "gray37" (rgb8 94 94 94)

let _ = Hashtbl.add colors "grey37" (rgb8 94 94 94)

let _ = Hashtbl.add colors "gray38" (rgb8 97 97 97)

let _ = Hashtbl.add colors "grey38" (rgb8 97 97 97)

let _ = Hashtbl.add colors "gray39" (rgb8 99 99 99)

let _ = Hashtbl.add colors "grey39" (rgb8 99 99 99)

let _ = Hashtbl.add colors "gray40" (rgb8 102 102 102)

let _ = Hashtbl.add colors "grey40" (rgb8 102 102 102)

let _ = Hashtbl.add colors "gray41" (rgb8 105 105 105)

let _ = Hashtbl.add colors "grey41" (rgb8 105 105 105)

let _ = Hashtbl.add colors "gray42" (rgb8 107 107 107)

let _ = Hashtbl.add colors "grey42" (rgb8 107 107 107)

let _ = Hashtbl.add colors "gray43" (rgb8 110 110 110)

let _ = Hashtbl.add colors "grey43" (rgb8 110 110 110)

let _ = Hashtbl.add colors "gray44" (rgb8 112 112 112)

let _ = Hashtbl.add colors "grey44" (rgb8 112 112 112)

let _ = Hashtbl.add colors "gray45" (rgb8 115 115 115)

let _ = Hashtbl.add colors "grey45" (rgb8 115 115 115)

let _ = Hashtbl.add colors "gray46" (rgb8 117 117 117)

let _ = Hashtbl.add colors "grey46" (rgb8 117 117 117)

let _ = Hashtbl.add colors "gray47" (rgb8 120 120 120)

let _ = Hashtbl.add colors "grey47" (rgb8 120 120 120)

let _ = Hashtbl.add colors "gray48" (rgb8 122 122 122)

let _ = Hashtbl.add colors "grey48" (rgb8 122 122 122)

let _ = Hashtbl.add colors "gray49" (rgb8 125 125 125)

let _ = Hashtbl.add colors "grey49" (rgb8 125 125 125)

let _ = Hashtbl.add colors "gray50" (rgb8 127 127 127)

let _ = Hashtbl.add colors "grey50" (rgb8 127 127 127)

let _ = Hashtbl.add colors "gray51" (rgb8 130 130 130)

let _ = Hashtbl.add colors "grey51" (rgb8 130 130 130)

let _ = Hashtbl.add colors "gray52" (rgb8 133 133 133)

let _ = Hashtbl.add colors "grey52" (rgb8 133 133 133)

let _ = Hashtbl.add colors "gray53" (rgb8 135 135 135)

let _ = Hashtbl.add colors "grey53" (rgb8 135 135 135)

let _ = Hashtbl.add colors "gray54" (rgb8 138 138 138)

let _ = Hashtbl.add colors "grey54" (rgb8 138 138 138)

let _ = Hashtbl.add colors "gray55" (rgb8 140 140 140)

let _ = Hashtbl.add colors "grey55" (rgb8 140 140 140)

let _ = Hashtbl.add colors "gray56" (rgb8 143 143 143)

let _ = Hashtbl.add colors "grey56" (rgb8 143 143 143)

let _ = Hashtbl.add colors "gray57" (rgb8 145 145 145)

let _ = Hashtbl.add colors "grey57" (rgb8 145 145 145)

let _ = Hashtbl.add colors "gray58" (rgb8 148 148 148)

let _ = Hashtbl.add colors "grey58" (rgb8 148 148 148)

let _ = Hashtbl.add colors "gray59" (rgb8 150 150 150)

let _ = Hashtbl.add colors "grey59" (rgb8 150 150 150)

let _ = Hashtbl.add colors "gray60" (rgb8 153 153 153)

let _ = Hashtbl.add colors "grey60" (rgb8 153 153 153)

let _ = Hashtbl.add colors "gray61" (rgb8 156 156 156)

let _ = Hashtbl.add colors "grey61" (rgb8 156 156 156)

let _ = Hashtbl.add colors "gray62" (rgb8 158 158 158)

let _ = Hashtbl.add colors "grey62" (rgb8 158 158 158)

let _ = Hashtbl.add colors "gray63" (rgb8 161 161 161)

let _ = Hashtbl.add colors "grey63" (rgb8 161 161 161)

let _ = Hashtbl.add colors "gray64" (rgb8 163 163 163)

let _ = Hashtbl.add colors "grey64" (rgb8 163 163 163)

let _ = Hashtbl.add colors "gray65" (rgb8 166 166 166)

let _ = Hashtbl.add colors "grey65" (rgb8 166 166 166)

let _ = Hashtbl.add colors "gray66" (rgb8 168 168 168)

let _ = Hashtbl.add colors "grey66" (rgb8 168 168 168)

let _ = Hashtbl.add colors "gray67" (rgb8 171 171 171)

let _ = Hashtbl.add colors "grey67" (rgb8 171 171 171)

let _ = Hashtbl.add colors "gray68" (rgb8 173 173 173)

let _ = Hashtbl.add colors "grey68" (rgb8 173 173 173)

let _ = Hashtbl.add colors "gray69" (rgb8 176 176 176)

let _ = Hashtbl.add colors "grey69" (rgb8 176 176 176)

let _ = Hashtbl.add colors "gray70" (rgb8 179 179 179)

let _ = Hashtbl.add colors "grey70" (rgb8 179 179 179)

let _ = Hashtbl.add colors "gray71" (rgb8 181 181 181)

let _ = Hashtbl.add colors "grey71" (rgb8 181 181 181)

let _ = Hashtbl.add colors "gray72" (rgb8 184 184 184)

let _ = Hashtbl.add colors "grey72" (rgb8 184 184 184)

let _ = Hashtbl.add colors "gray73" (rgb8 186 186 186)

let _ = Hashtbl.add colors "grey73" (rgb8 186 186 186)

let _ = Hashtbl.add colors "gray74" (rgb8 189 189 189)

let _ = Hashtbl.add colors "grey74" (rgb8 189 189 189)

let _ = Hashtbl.add colors "gray75" (rgb8 191 191 191)

let _ = Hashtbl.add colors "grey75" (rgb8 191 191 191)

let _ = Hashtbl.add colors "gray76" (rgb8 194 194 194)

let _ = Hashtbl.add colors "grey76" (rgb8 194 194 194)

let _ = Hashtbl.add colors "gray77" (rgb8 196 196 196)

let _ = Hashtbl.add colors "grey77" (rgb8 196 196 196)

let _ = Hashtbl.add colors "gray78" (rgb8 199 199 199)

let _ = Hashtbl.add colors "grey78" (rgb8 199 199 199)

let _ = Hashtbl.add colors "gray79" (rgb8 201 201 201)

let _ = Hashtbl.add colors "grey79" (rgb8 201 201 201)

let _ = Hashtbl.add colors "gray80" (rgb8 204 204 204)

let _ = Hashtbl.add colors "grey80" (rgb8 204 204 204)

let _ = Hashtbl.add colors "gray81" (rgb8 207 207 207)

let _ = Hashtbl.add colors "grey81" (rgb8 207 207 207)

let _ = Hashtbl.add colors "gray82" (rgb8 209 209 209)

let _ = Hashtbl.add colors "grey82" (rgb8 209 209 209)

let _ = Hashtbl.add colors "gray83" (rgb8 212 212 212)

let _ = Hashtbl.add colors "grey83" (rgb8 212 212 212)

let _ = Hashtbl.add colors "gray84" (rgb8 214 214 214)

let _ = Hashtbl.add colors "grey84" (rgb8 214 214 214)

let _ = Hashtbl.add colors "gray85" (rgb8 217 217 217)

let _ = Hashtbl.add colors "grey85" (rgb8 217 217 217)

let _ = Hashtbl.add colors "gray86" (rgb8 219 219 219)

let _ = Hashtbl.add colors "grey86" (rgb8 219 219 219)

let _ = Hashtbl.add colors "gray87" (rgb8 222 222 222)

let _ = Hashtbl.add colors "grey87" (rgb8 222 222 222)

let _ = Hashtbl.add colors "gray88" (rgb8 224 224 224)

let _ = Hashtbl.add colors "grey88" (rgb8 224 224 224)

let _ = Hashtbl.add colors "gray89" (rgb8 227 227 227)

let _ = Hashtbl.add colors "grey89" (rgb8 227 227 227)

let _ = Hashtbl.add colors "gray90" (rgb8 229 229 229)

let _ = Hashtbl.add colors "grey90" (rgb8 229 229 229)

let _ = Hashtbl.add colors "gray91" (rgb8 232 232 232)

let _ = Hashtbl.add colors "grey91" (rgb8 232 232 232)

let _ = Hashtbl.add colors "gray92" (rgb8 235 235 235)

let _ = Hashtbl.add colors "grey92" (rgb8 235 235 235)

let _ = Hashtbl.add colors "gray93" (rgb8 237 237 237)

let _ = Hashtbl.add colors "grey93" (rgb8 237 237 237)

let _ = Hashtbl.add colors "gray94" (rgb8 240 240 240)

let _ = Hashtbl.add colors "grey94" (rgb8 240 240 240)

let _ = Hashtbl.add colors "gray95" (rgb8 242 242 242)

let _ = Hashtbl.add colors "grey95" (rgb8 242 242 242)

let _ = Hashtbl.add colors "gray96" (rgb8 245 245 245)

let _ = Hashtbl.add colors "grey96" (rgb8 245 245 245)

let _ = Hashtbl.add colors "gray97" (rgb8 247 247 247)

let _ = Hashtbl.add colors "grey97" (rgb8 247 247 247)

let _ = Hashtbl.add colors "gray98" (rgb8 250 250 250)

let _ = Hashtbl.add colors "grey98" (rgb8 250 250 250)

let _ = Hashtbl.add colors "gray99" (rgb8 252 252 252)

let _ = Hashtbl.add colors "grey99" (rgb8 252 252 252)

let _ = Hashtbl.add colors "gray100" (rgb8 255 255 255)

let _ = Hashtbl.add colors "grey100" (rgb8 255 255 255)

let _ = Hashtbl.add colors "dark grey" (rgb8 169 169 169)

let _ = Hashtbl.add colors "DarkGrey" (rgb8 169 169 169)

let _ = Hashtbl.add colors "dark gray" (rgb8 169 169 169)

let _ = Hashtbl.add colors "DarkGray" (rgb8 169 169 169)

let _ = Hashtbl.add colors "dark blue" (rgb8 0 0 139)

let _ = Hashtbl.add colors "DarkBlue" (rgb8 0 0 139)

let _ = Hashtbl.add colors "dark cyan" (rgb8 0 139 139)

let _ = Hashtbl.add colors "DarkCyan" (rgb8 0 139 139)

let _ = Hashtbl.add colors "dark magenta" (rgb8 139 0 139)

let _ = Hashtbl.add colors "DarkMagenta" (rgb8 139 0 139)

let _ = Hashtbl.add colors "dark red" (rgb8 139 0 0)

let _ = Hashtbl.add colors "DarkRed" (rgb8 139 0 0)

let _ = Hashtbl.add colors "light green" (rgb8 144 238 144)

let _ = Hashtbl.add colors "LightGreen" (rgb8 144 238 144)

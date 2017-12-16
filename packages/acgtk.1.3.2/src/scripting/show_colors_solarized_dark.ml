let color_from_byte_rgb (r : int) (g : int) (b : int) =
  (float r /. 255., float g /. 255., float b /. 255., 1.)

let solarized_base03    = color_from_byte_rgb   0  43  54
let solarized_base02    = color_from_byte_rgb   7  54  66
let solarized_base01    = color_from_byte_rgb  88 110 117
let solarized_base00    = color_from_byte_rgb 101 123 131
let solarized_base0     = color_from_byte_rgb 131 148 150
let solarized_base1     = color_from_byte_rgb 147 161 161
let solarized_base2     = color_from_byte_rgb 238 232 213
let solarized_base3     = color_from_byte_rgb 253 246 227
let solarized_yellow    = color_from_byte_rgb 181 137   0
let solarized_orange    = color_from_byte_rgb 203  75  22
let solarized_red       = color_from_byte_rgb 220  50  47
let solarized_magenta   = color_from_byte_rgb 211  54 130
let solarized_violet    = color_from_byte_rgb 108 113 196
let solarized_blue      = color_from_byte_rgb  38 139 210
let solarized_cyan      = color_from_byte_rgb  42 161 152
let solarized_green     = color_from_byte_rgb 133 153   0
let solarized_white     = color_from_byte_rgb 255 255 255
let solarized_argile    = color_from_byte_rgb 239 239 239

let lines = [ solarized_blue;
              solarized_green;
              solarized_red;
              solarized_violet;
              solarized_orange;
              solarized_cyan;
              solarized_yellow;
              solarized_magenta ]
let tree = solarized_base0
(*let background = solarized_base03 *)
(*let background = solarized_white *)
(*let node_background = solarized_base02 *)
let background config = let r,g,b = Rendering_config.background_color config in color_from_byte_rgb r g b
(*let node_background = solarized_argile *)

let node_background config = let r,g,b = Rendering_config.node_color config in color_from_byte_rgb r g b

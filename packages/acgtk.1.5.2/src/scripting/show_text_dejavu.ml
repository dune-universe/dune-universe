open Cairo
open Diagram

let family = "DejaVu Sans"
let size = 15.

let font_extents = get_font_extents (font family) size
let textC ~face ~size contents =
  text_ ~face ~size contents
  |> translateY (-. font_extents.descent)
  |> translateY ((font_extents.ascent +. font_extents.descent) /. 2.)
  
let n = textC ~face:(font family) ~size
let b = textC ~face:(font family ~weight:Bold) ~size
let i = textC ~face:(font family ~slant:Italic) ~size


(** {1 Basic types and values} *)

(** {2 Types} *)

(** {3 Color types} *)

(** The type of 4-bits colors. They are informally rgb colors where each
    component is in a scale from 0 to 1, with an additionnal intensity bit. *)
type color4 =
  {
    r4 : bool;
    g4 : bool;
    b4 : bool;
    intensity4 : bool;
  }

(** The type of rgb colors with each component in \[0; 5\]. This is used to encode
    the 6*6*6 color cube of 8-bits colors? *)
type cube6 =
  {
    r6 : int;
    g6 : int;
    b6 : int;
  }

(** The type of a 8-bits color. Such a color is either a standard color, a 6*6*6
    rgb color or some kind of grey on a 24-steps grayscale. Thus, the paramter of
    {!Grayscale} should be in \[0; 23\] *)
type color8 =
  | Standard of color4
  | Cube6 of cube6
  | Grayscale of int (* [0; 23] *)

(** The type of 24-bits colors. Each component belongs to \[0; 255\]. *)
type color24 =
  {
    r24 : int;
    g24 : int;
    b24 : int;
  }

(** The type of a color that can be a 4-, 8- or 24-bits color. *)
type color =
  | C4 of color4
  | C8 of color8
  | C24 of color24

(** {3 Style types} *)

(** All style sequences that are implemented.
    - {!Reset} disables everything (returns to default)
    - {!Bold} enables bold font
    - {!Faint} decreases intensity
    - {!Faint_bold_off} revert the effect of {!Bold} and {!Faint}
    - {!Italic}: italic font (likely to not work)
    - {!Fraktur}: fraktur font (likely to not work)
    - {!Italic_fraktur_off}: disables italic and fraktur (likely to not work)
    - {!Underlined}: underlines text
    - {!Underlined_off}: disable underline
    - {!Blink} and {!Blink_off} enables and disables flow blink (fast blink is
      rarely supported)
    - {!Reverse_video} and {!Reverse_video_off} as expected
    - {!Conceal} and {!Conceal_off} as excpected (rarely supported)
    - {!Crossed_out} and {!Crossed_out_off} as excpected
    - {!Framed}, {!Encircled} and {!Framed_encircled_off} as excpected (rarely
      supported)
    - {!Overlined} and {!Overlined_off} as excpected
    - {!Default_font} set font to default (rarely supported)
    - {!Font} set font to specified number (in \[1;9\]) (rarely supported)
    - {!Fg} set foreground color to specified color
    - {!Default_fg} set foreground color to default color
    - {!Bg} set background color to specified color
    - {!Default_bg} set background color to default color
*)
type style = (* Comments for sequences that probably won't work *)
  | Reset
  | Bold
  | Faint         | Faint_bold_off
  | Italic        | Italic_fraktur_off (* Maybe this *)
  | Fraktur       (* This *)
  | DoubleUnderlined (* Maybe this *)
  | Underlined    | Underlined_off
  | Blink         | Blink_off
  | Reverse_video | Reverse_video_off
  | Conceal       | Conceal_off
  | Crossed_out   | Crossed_out_off
  | Framed        | Framed_encircled_off (* This *)
  | Encircled     (* This *)
  | Overlined     | Overlined_off
  | Default_font
  | Font of int (* [1-9] *) (* This *)
  | Fg of color
  | Default_fg
  | Bg of color
  | Default_bg

(** {2 Helpers and values} *)

(** {3 Iterators on finite (small) color space} *)

let fold_color4 (type acc) (f: color4 -> acc -> acc) (acc: acc) : acc =
  let rec aux (r4: bool) (g4: bool) (b4: bool) (intensity4: bool) (acc: acc) : acc =
    let acc = f {r4; g4; b4; intensity4} acc in
    match r4, g4, b4, intensity4 with
    | true , true , true , true  -> acc
    | false, true , true , true  -> aux true false false false acc
    | _    , false, true , true  -> aux r4   true  false false acc
    | _    , _    , false, true  -> aux r4   g4    true  false acc
    | _    , _    , _    , false -> aux r4   g4    b4    true  acc
  in
  aux false false false false acc

let fold_cube6 (type acc) (f: cube6 -> acc -> acc) (acc: acc) : acc =
  let rec aux (r6: int) (g6: int) (b6: int) (acc: acc) : acc =
    let acc = f {r6; g6; b6} acc in
    match r6, g6, b6 with
    | 5, 5, 5 -> acc
    | _, 5, 5 -> aux (r6 + 1) 0 0 acc
    | _, _, 5 -> aux r6 (g6 + 1) 0 acc
    | _, _, _ -> aux r6 g6 (b6 + 1) acc
  in
  aux 0 0 0 acc

let fold_greyscale (type acc) (f: int -> acc -> acc) (acc: acc) : acc =
  let rec aux (g: int) (acc: acc) =
    let acc = f g acc in
    match g with
    | 23 -> acc
    | _ -> aux (g + 1) acc
  in
  aux 0 acc


let fold_color8 (type acc) (f: color8 -> acc -> acc) (acc: acc) : acc =
  acc
  |> fold_color4 (fun c4 acc -> f (Standard c4) acc)
  |> fold_cube6 (fun c6 acc -> f (Cube6 c6) acc)
  |> fold_greyscale (fun g acc -> f (Grayscale g) acc)

(** {3 Useful names for 4-bits colors} *)

(** {4 Standard 3-bits colors} *)

let black   : color4 = {r4 = false; g4 = false; b4 = false; intensity4 = false;}
let red     : color4 = {black  with r4 = true;}
let green   : color4 = {black  with g4 = true;}
let blue    : color4 = {black  with b4 = true;}
let yellow  : color4 = {red    with g4 = true;}
let magenta : color4 = {blue   with r4 = true;}
let cyan    : color4 = {green  with b4 = true;}
let white   : color4 = {yellow with b4 = true;}

(** {4 High intensity 4-bits colors} *)

let hi_black   : color4 = {black   with intensity4 = true;}
let hi_red     : color4 = {red     with intensity4 = true;}
let hi_green   : color4 = {green   with intensity4 = true;}
let hi_blue    : color4 = {blue    with intensity4 = true;}
let hi_yellow  : color4 = {yellow  with intensity4 = true;}
let hi_magenta : color4 = {magenta with intensity4 = true;}
let hi_cyan    : color4 = {cyan    with intensity4 = true;}
let hi_white   : color4 = {white   with intensity4 = true;}

(** {2 Standard palettes types} *)

type rgb = int * int * int

module Color4Map = Map.Make(struct type t = color4 let compare = compare end)
module Color8Map = Map.Make(struct type t = color8 let compare = compare end)

(** The kind of palette the terminal uses. It is used to match a 4-bits color to
    a rgb color. This is useful to find the clostest 4-bits color to a rgb
    color, according to the actual colors. *)
type color_palette =
  | VGA
  | CMD
  | Terminal_app
  | PuTTY
  | MIRC
  | Xterm
  | X
  | Ubuntu
  | Custom_palette of rgb Color4Map.t

module ColorPaletteMap = Map.Make(struct type t = color_palette let compare = compare end)

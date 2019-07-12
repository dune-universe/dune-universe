open Ocolor_types

(** Until the last moment, it is more convenient to work with list of integers.
    It is easier to apply several styles at once, since a sequence can be the
    concatenation of several simple sequences. And, at the last moment, we
    ranslate it as the string sequence as it will be passed to the terminal.
*)
type seq = int list

(** Translate a list of integer to the corresponding escape sequence string.
    This is the list of integer separated by {!Ocolor_config.separator} (by
    default ";"), surrounded by the appropriate prefix and suffix (resp.
    ["\x1b\["] and  ["m"])
*)
let sgr_of_seq (l: seq) : string =
  let esc = "\x1b" in (* Escape character *)
  let csi = "[" in    (* Control Sequence Introducer *)
  let sgr = "m" in    (* Select Graphic Rendition *)
  let sep = Ocolor_config.get_separator () in
  let seq = List.map string_of_int l |> String.concat sep in
  esc^csi^seq^sgr

let code_of_color3 (c: color4) : int  =
  let int_of_bool : bool -> int = function true -> 1 | false -> 0 in
  int_of_bool c.r4 lsl 0 +
  int_of_bool c.g4 lsl 1 +
  int_of_bool c.b4 lsl 2

let fg_code_of_color4 (c: color4) : int  =
  code_of_color3 c + if c.intensity4 then 90 else 30

let code_of_color8 (c: color8) : int =
  match c with
  | Standard c -> code_of_color3 c + if c.intensity4 then 8 else 0
  | Cube6 {r6; g6; b6} ->
    if r6 < 0 || r6 > 5 then raise (Invalid_argument "Red parameter of a color8 should be in [0; 5].");
    if g6 < 0 || g6 > 5 then raise (Invalid_argument "Green parameter of a color8 should be in [0; 5].");
    if b6 < 0 || b6 > 5 then raise (Invalid_argument "Blue parameter of a color8 should be in [0; 5].");
    16 + 36*r6 + 6*g6 + b6
  | Grayscale c ->
    if c < 0 || c > 23 then raise (Invalid_argument "Grayscale parameter of a color8 should be in [0; 23].");
    16 + 36*6 + c

let bg_code_of_color4 (c: color4) : int = 10 + fg_code_of_color4 c

(** {2 Parametric style sequences} *)

(** {3 Foreground color} *)

(** {4 Integer sequences} *)

let default_fg_seq : seq = [39]

(** Legal values for parameters are \[0; 255\]. *)
let fg_rgb_seq (r: int) (g: int) (b: int) : seq =
  if r < 0 || r > 255 then raise (Invalid_argument "Red parameter should be in [0; 255].");
  if g < 0 || g > 255 then raise (Invalid_argument "Green parameter should be in [0; 255].");
  if b < 0 || b > 255 then raise (Invalid_argument "Blue parameter should be in [0; 255].");
  [38; 2; r; g; b]

let fg_color4_seq (c: color4) : seq = [fg_code_of_color4 c]
let fg_color8_seq (c: color8) : seq = [38; 5; code_of_color8 c]
let fg_color24_seq (c: color24) : seq = fg_rgb_seq c.r24 c.g24 c.b24

let fg_color_seq (c: color) : seq =
  let open Ocolor_converter in
  let open Ocolor_config in
  match c, get_color_capability () with
  | C4 c, (Color4|Color8|Color24) -> fg_color4_seq c
  | C8 c, Color4 -> c |> color4_of_color8 |> fg_color4_seq
  | C8 c, (Color8|Color24) -> fg_color8_seq c
  | C24 c, Color4 -> c |> color4_of_color24 |> fg_color4_seq
  | C24 c, Color8 -> c |> color8_of_color24 |> fg_color8_seq
  | C24 c, Color24 -> fg_color24_seq c

(** {4 Escape sequences} *)

let default_fg_sgr : string = sgr_of_seq default_fg_seq

(** Legal values for parameters are \[0; 255\]. *)
let fg_rgb_sgr (r: int) (g: int) (b: int) : string =
  if r < 0 || r > 255 then raise (Invalid_argument "Red parameter should be in [0; 255].");
  if g < 0 || g > 255 then raise (Invalid_argument "Green parameter should be in [0; 255].");
  if b < 0 || b > 255 then raise (Invalid_argument "Blue parameter should be in [0; 255].");
  fg_rgb_seq r g b |> sgr_of_seq

let fg_color4_sgr (c: color4) : string = c |> fg_color4_seq |> sgr_of_seq
let fg_color8_sgr (c: color8) : string = c |> fg_color8_seq |> sgr_of_seq
let fg_color24_sgr (c: color24) : string = c |> fg_color24_seq |> sgr_of_seq

(** {3 Background color} *)

(** {4 Integer sequences} *)

let default_bg_seq : seq = [49]

(** Legal values for parameters are \[0; 255\]. *)
let bg_rgb_seq (r: int) (g: int) (b: int) : seq =
  if r < 0 || r > 255 then raise (Invalid_argument "Red parameter should be in [0; 255].");
  if g < 0 || g > 255 then raise (Invalid_argument "Green parameter should be in [0; 255].");
  if b < 0 || b > 255 then raise (Invalid_argument "Blue parameter should be in [0; 255].");
  [48; 2; r; g; b]

let bg_color4_seq (c: color4) : seq = [bg_code_of_color4 c]
let bg_color8_seq (c: color8) : seq = [48; 5; code_of_color8 c]
let bg_color24_seq (c: color24) : seq = bg_rgb_seq c.r24 c.g24 c.b24

(** {4 Escape sequences} *)

let default_bg_sgr : string = sgr_of_seq default_bg_seq

(** Legal values for parameters are \[0; 255\]. *)
let bg_rgb_sgr (r: int) (g: int) (b: int) : string =
  if r < 0 || r > 255 then raise (Invalid_argument "Red parameter should be in [0; 255].");
  if g < 0 || g > 255 then raise (Invalid_argument "Green parameter should be in [0; 255].");
  if b < 0 || b > 255 then raise (Invalid_argument "Blue parameter should be in [0; 255].");
  bg_rgb_seq r g b |> sgr_of_seq

let bg_color4_sgr (c: color4) : string = c |> bg_color4_seq |> sgr_of_seq
let bg_color8_sgr (c: color8) : string = c |> bg_color8_seq |> sgr_of_seq
let bg_color24_sgr (c: color24) : string = c |> bg_color24_seq |> sgr_of_seq

let bg_color_seq (c: color) : seq =
  let open Ocolor_converter in
  let open Ocolor_config in
  match c, get_color_capability () with
  | C4 c, (Color4|Color8|Color24) -> bg_color4_seq c
  | C8 c, Color4 -> c |> color4_of_color8 |> bg_color4_seq
  | C8 c, (Color8|Color24) -> bg_color8_seq c
  | C24 c, Color4 -> c |> color4_of_color24 |> bg_color4_seq
  | C24 c, Color8 -> c |> color8_of_color24 |> bg_color8_seq
  | C24 c, Color24 -> bg_color24_seq c

(** {3 Font} *)

(** {4 Integer sequences} *)

(** Legal values for parameter are \[1; 9\] *)
let font_seq (i: int) : seq =
  if i < 1 || i > 9 then raise (Invalid_argument "Font parameter should be in [1; 9].");
  [10 + i]

let default_font_seq : seq = [10]

(** {4 Escape sequences} *)

(** Legal values for parameters are \[1; 9\] *)
let font_sgr (i: int) : string = i |> font_seq |> sgr_of_seq
let default_font_sgr : string = sgr_of_seq default_font_seq

(** {2 Non parametric style sequences} *)

(** {3 SGR sequences of common styles as [int list]} *)

let reset_seq                : seq = [0]
let bold_seq                 : seq = [1]
let faint_seq                : seq = [2]
let italic_seq               : seq = [3]
let underlined_seq           : seq = [4]
let blink_seq                : seq = [5]
let reverse_video_seq        : seq = [7]
let conceal_seq              : seq = [8]
let crossed_out_seq          : seq = [9]
let fraktur_seq              : seq = [20]
let double_underlined_seq    : seq = [21]
let faint_bold_off_seq       : seq = [22]
let italic_fraktur_off_seq   : seq = [23]
let underlined_off_seq       : seq = [24]
let blink_off_seq            : seq = [25]
let reverse_video_off_seq    : seq = [27]
let concel_off_seq           : seq = [28]
let crossed_out_off_seq      : seq = [29]
let framed_seq               : seq = [51]
let encircled_seq            : seq = [52]
let overlined_seq            : seq = [53]
let framed_encircled_off_seq : seq = [54]
let overlined_off_seq        : seq = [55]

(** {3 String SGR sequences of common styles} *)

let reset_sgr                : string = sgr_of_seq reset_seq
let bold_sgr                 : string = sgr_of_seq bold_seq
let double_underlined_sgr    : string = sgr_of_seq double_underlined_seq
let faint_sgr                : string = sgr_of_seq faint_seq
let faint_bold_off_sgr       : string = sgr_of_seq faint_bold_off_seq
let italic_sgr               : string = sgr_of_seq italic_seq
let italic_fraktur_off_sgr   : string = sgr_of_seq italic_fraktur_off_seq
let fraktur_sgr              : string = sgr_of_seq fraktur_seq
let underlined_sgr           : string = sgr_of_seq underlined_seq
let underlined_off_sgr       : string = sgr_of_seq underlined_off_seq
let blink_sgr                : string = sgr_of_seq blink_seq
let blink_off_sgr            : string = sgr_of_seq blink_off_seq
let reverse_video_sgr        : string = sgr_of_seq reverse_video_seq
let reverse_video_off_sgr    : string = sgr_of_seq reverse_video_off_seq
let conceal_sgr              : string = sgr_of_seq conceal_seq
let concel_off_sgr           : string = sgr_of_seq concel_off_seq
let crossed_out_sgr          : string = sgr_of_seq crossed_out_seq
let crossed_out_off_sgr      : string = sgr_of_seq crossed_out_off_seq
let framed_sgr               : string = sgr_of_seq framed_seq
let framed_encircled_off_sgr : string = sgr_of_seq framed_encircled_off_seq
let encircled_sgr            : string = sgr_of_seq encircled_seq
let overlined_sgr            : string = sgr_of_seq overlined_seq
let overlined_off_sgr        : string = sgr_of_seq overlined_off_seq

(** {2 Integer and escape sequence for arbitrary style} *)

let seq_of_style (s: style) : seq =
  match s with
  | Reset                -> reset_seq
  | Bold                 -> bold_seq
  | DoubleUnderlined     -> double_underlined_seq
  | Faint                -> faint_seq
  | Faint_bold_off       -> faint_bold_off_seq
  | Italic               -> italic_seq
  | Italic_fraktur_off   -> italic_fraktur_off_seq
  | Fraktur              -> fraktur_seq
  | Underlined           -> underlined_seq
  | Underlined_off       -> underlined_off_seq
  | Blink                -> blink_seq
  | Blink_off            -> blink_off_seq
  | Reverse_video        -> reverse_video_seq
  | Reverse_video_off    -> reverse_video_off_seq
  | Conceal              -> conceal_seq
  | Conceal_off          -> concel_off_seq
  | Crossed_out          -> crossed_out_seq
  | Crossed_out_off      -> crossed_out_off_seq
  | Framed               -> framed_seq
  | Framed_encircled_off -> framed_encircled_off_seq
  | Encircled            -> encircled_seq
  | Overlined            -> overlined_seq
  | Overlined_off        -> overlined_off_seq
  | Default_font         -> default_font_seq
  | Font i               -> font_seq i
  | Fg c                 -> fg_color_seq c
  | Default_fg           -> default_fg_seq
  | Bg c                 -> bg_color_seq c
  | Default_bg           -> default_bg_seq

let sgr_of_style (s: style) : string = s |> seq_of_style |> sgr_of_seq

let seq_of_styles (s: style list) : seq =
  s |> List.map seq_of_style |> List.flatten

let styles_sgr (s: style list) : string =
  s |> seq_of_styles |> sgr_of_seq

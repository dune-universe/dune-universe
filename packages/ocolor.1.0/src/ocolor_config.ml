type color_capability =
  | Color4
  | Color8
  | Color24

let separator : string ref = ref ";"
let set_separator : string -> unit = (:=) separator
let get_separator () : string = !separator

let palette : Ocolor_types.color_palette ref = ref Ocolor_types.Xterm
let set_palette : Ocolor_types.color_palette -> unit = (:=) palette
let get_palette () : Ocolor_types.color_palette = !palette

let color_capability : color_capability ref = ref Color24
let get_color_capability () : color_capability = !color_capability
let set_color_capability : color_capability -> unit = (:=) color_capability

let auto_reset : bool ref = ref true
let get_auto_reset () : bool = !auto_reset
let set_auto_reset : bool -> unit = (:=) auto_reset

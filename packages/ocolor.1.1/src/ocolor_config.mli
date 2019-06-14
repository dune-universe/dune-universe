(** {1 Configuration} *)

(** Sometimes, default configuration does not suit you. We go through setters
    and getters rather than just giving access to the underlying ref in order to
    allow some operations in the future when changing the configuration. For
    instance, if we cache color conversion, we might want to clear the cache
    when the configuration is changed.

    Please note that the configuration is not automatic. This library aims to be
    as pure as possible, and thus does not provide any mechanism, to detect the
    terminal settings.
*)

(** The kind of colors supported by the terminal. Except if done manually,
    every color will be encoded in the more expressible format possible. For
    instance, if the terminal support 8-bits colors, but the user try to print
    a 24-bits color, the result will be the clostest 8-bits color with respect
    to the distance in Lab color space *)
type color_capability =
  | Color4
  | Color8
  | Color24

(** The separator used between each integer in SGR sequences. By default, this
    is ";". It should work in any terminal, but since it is not the only legal
    separator, it is configurable. *)
val get_separator: unit -> string

(** Change {!separator}. *)
val set_separator: string -> unit


(** The palette of the terminal. By default, this is {!Xterm}. It should be ok. *)
val get_palette: unit -> Ocolor_types.color_palette

(** Change {!palette}.*)
val set_palette: Ocolor_types.color_palette -> unit


(** The kind of colors the terminal can display. By default, this is {!Color24}.
    Most of terminals can display 24-bits colors. Used to downgrade colors to
    8- or 4- bits colors, with respect to the capability of the terminal *)
val get_color_capability: unit -> color_capability

(** Change {!color_capability}.*)
val set_color_capability: color_capability -> unit


(** Whether [Printf]-like functions reset the style after the print or not.
    Default to [true]. But, prefer the [Format]-like functions of
    {!Ocolor_format}. *)
val get_auto_reset: unit -> bool

(** Change {!auto_reset}. *)
val set_auto_reset: bool -> unit

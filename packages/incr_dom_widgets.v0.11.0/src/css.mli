open Core_kernel
open Import

type css_global_values =
  [ `Inherit | `Initial ]
[@@deriving sexp, compare]

module Color : sig
  module RGBA : sig
    type t [@@deriving sexp, bin_io, compare]

    (** [create ~r ~g ~b ~a] creates a color that corresponds to rgba([r],[g],[b],[a])

        If [a] is omitted then it creates a color that corresponds to rgb([r],[g],[b])
    *)
    val create : r:int -> g:int -> b:int -> ?a:Percent.t -> unit -> t
  end

  type simple =
    [ `Black
    | `Blue
    | `Brown
    | `Yellow
    | `Cyan
    | `Green
    | `Grey
    | `Magenta
    | `Purple
    | `Red
    | `White
    | `SteelBlue
    | `Honeydew
    | `Whitesmoke
    | `LawnGreen
    | `GoldenRod
    | `DarkGreen
    | `DarkMagenta
    | `DarkRed
    ] [@@deriving sexp, bin_io, compare]

  type t =
    [ simple
    | `RGBA of RGBA.t
    | css_global_values
    ] [@@deriving sexp, bin_io, compare]

  val to_string_css : t -> string
end

module Length : sig
  type t =
    [ `Px of int
    | `Em of int
    | `Vh of Percent.t
    | `Vw of Percent.t
    | `Percent of Percent.t
    | css_global_values
    ] [@@deriving sexp, compare]

  val to_string_css : t -> string
end

module Auto_or_length : sig
  type t =
    [ `Auto
    | Length.t
    ]
  [@@deriving sexp, compare]
end

type t [@@deriving sexp, compare]

val create : field:string -> value:string -> t
val empty : t

(** Neither [combine] nor [concat] validate that each [t] is unique.
    For [combine x y], [y] will override [x] if they are the same attribute.
    For [concat l], the greatest index of an attribute will prevail. *)
val combine : t -> t -> t
val ( @> )  : t -> t -> t
val concat  : t list -> t

val to_attr : t -> Vdom.Attr.t
val to_string_css : t -> string

val box_sizing : [ `Content_box | `Border_box | css_global_values ] -> t

val display
  :  [ `Inline | `Block | `Inline_block | `List_item | `Table
     | `Inline_table | `None | css_global_values ]
  -> t

val visibility : [ `Visible | `Hidden | `Collapse | css_global_values ] -> t
val overflow : [ `Visible | `Hidden | `Scroll | `Auto | css_global_values ] -> t
val z_index : int -> t
val opacity : int -> t

type font_style = [ `Normal | `Italic | `Oblique | css_global_values ]
type font_weight = [ `Normal | `Bold | `Bolder | `Lighter | `Number of int | css_global_values ]
type font_variant = [ `Normal | `Small_caps | css_global_values ]

val font_size : Length.t -> t
val font_family : string list -> t
val font_style : font_style -> t
val font_weight : font_weight -> t
val font_variant : font_variant -> t
val font
  :  size:Length.t
  -> family:string list
  -> ?style:font_style
  -> ?weight:font_weight
  -> ?variant:font_variant
  -> unit
  -> t

val bold : t
val color : Color.t -> t
val background_color : Color.t -> t
val text_align : [ `Left | `Right | `Center | `Justify | css_global_values ] -> t
val horizontal_align : [ `Left | `Right | `Center | css_global_values ] -> t
val vertical_align : [ `Top | `Bottom | `Middle | css_global_values ] -> t

val width     : Length.t -> t
val min_width : Length.t -> t
val max_width : Length.t -> t

val height     : Length.t -> t
val min_height : Length.t -> t
val max_height : Length.t -> t

val padding_top    : Length.t -> t
val padding_bottom : Length.t -> t
val padding_left   : Length.t -> t
val padding_right  : Length.t -> t
val padding
  :  ?top:Length.t
  -> ?bottom:Length.t
  -> ?left:Length.t
  -> ?right:Length.t
  -> unit
  -> t

val margin_top    : Auto_or_length.t -> t
val margin_bottom : Auto_or_length.t -> t
val margin_left   : Auto_or_length.t -> t
val margin_right  : Auto_or_length.t -> t
val margin
  :  ?top:Auto_or_length.t
  -> ?bottom:Auto_or_length.t
  -> ?left:Auto_or_length.t
  -> ?right:Auto_or_length.t
  -> unit
  -> t

type border_style =
  [ `None | `Hidden | `Dotted | `Dashed | `Solid
  | `Double | `Groove | `Ridge | `Inset | `Outset
  | css_global_values ]

val border_top    : ?width:Length.t -> ?color:Color.t -> style:border_style -> unit -> t
val border_bottom : ?width:Length.t -> ?color:Color.t -> style:border_style -> unit -> t
val border_left   : ?width:Length.t -> ?color:Color.t -> style:border_style -> unit -> t
val border_right  : ?width:Length.t -> ?color:Color.t -> style:border_style -> unit -> t
val border        : ?width:Length.t -> ?color:Color.t -> style:border_style -> unit -> t

val border_collapse : [ `Separate | `Collapse | css_global_values ] -> t

val border_spacing : Length.t -> t

val flex_container
  :  ?inline:bool
  -> ?direction:[ `Row | `Row_reverse | `Column | `Column_reverse ]
  -> ?wrap:[ `Nowrap | `Wrap | `Wrap_reverse ]
  -> unit
  -> t

val flex_item
  :  ?order:int
  -> ?basis:Auto_or_length.t
  -> ?shrink:float
  -> grow:float
  -> unit
  -> t

(** Note: You must include the [name]s @keyframes in the stylesheet *)
val animation
  :  name:string
  -> duration:Time_ns.Span.t
  -> ?delay:Time_ns.Span.t
  -> ?direction:[ `Normal | `Reverse | `Alternate | `Alternate_reverse | css_global_values ]
  -> ?fill_mode:[ `None | `Forwards | `Backwards | `Both | css_global_values ]
  -> ?iter_count:int
  -> ?timing_function:string
  -> unit
  -> t

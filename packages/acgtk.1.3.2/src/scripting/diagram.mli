type toy_font = [ `Toy ] Cairo.Font_face.t
type diagram
type vector = float * float
type point = vector
type color = float * float * float * float

val (>>) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

val empty_diagram : diagram
val blend : diagram list -> diagram
val (|||) : diagram -> diagram -> diagram
val (===) : diagram -> diagram -> diagram
val hcat : diagram list -> diagram
val vcat : diagram list -> diagram

val draw : Cairo.context -> diagram -> unit
val extents : diagram -> Cairo.rectangle

val setup : (Cairo.context -> unit) -> diagram -> diagram
val reframe : (Cairo.rectangle -> Cairo.rectangle) -> diagram -> diagram

val transform : Cairo.Matrix.t -> diagram -> diagram
val translate : vector -> diagram -> diagram
val translateX : float -> diagram -> diagram
val translateY : float -> diagram -> diagram
val scale : vector -> diagram -> diagram
val scaleX : float -> diagram -> diagram
val scaleY : float -> diagram -> diagram
val uscale : float -> diagram -> diagram
val rotate : float -> diagram -> diagram

val centerX : diagram -> diagram
val centerY : diagram -> diagram
val center : diagram -> diagram
val alignL : diagram -> diagram
val alignR : diagram -> diagram
val alignT : diagram -> diagram
val alignB : diagram -> diagram

val font : ?slant:Cairo.slant -> ?weight:Cairo.weight -> string -> toy_font
val default_font : toy_font
val text : ?face:toy_font -> ?size:float -> string -> diagram
val text_ : ?face:toy_font -> ?size:float -> string -> diagram
val tighten_text : diagram -> diagram
val get_font_extents : toy_font -> float -> Cairo.font_extents

val line : point -> point -> diagram
val rectangle_outline : float -> float -> diagram
val rectangle_full : float -> float -> diagram
val circle_outline : float -> diagram
val circle_full : float -> diagram

val color : color -> diagram -> diagram
val bg_color : color -> diagram -> diagram
val empty_color : color
val black : color
val blue : color
val green : color
val lightgrey : color
val red : color

val hspace : float -> diagram
val vspace : float -> diagram
val pad_rel : ?all:float -> ?horizontal:float -> ?vertical:float ->
              ?left:float -> ?right:float -> ?top:float -> ?bottom:float ->
              diagram -> diagram
val pad_abs : ?all:float -> ?horizontal:float -> ?vertical:float ->
              ?left:float -> ?right:float -> ?top:float -> ?bottom:float ->
              diagram -> diagram

val trace_path : (Cairo.context -> unit) -> Cairo.Path.t
val stroke : Cairo.Path.t -> diagram
val fill : Cairo.Path.t -> diagram

val to_svg : string -> diagram -> unit
val frame : diagram -> diagram
val show_origin : diagram -> diagram
val show_extents : diagram -> diagram

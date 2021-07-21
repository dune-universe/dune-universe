open Gg

type point = float * float

module Scaling : sig
  type 'a t

  val id : float t
  val linear :
    domain:(float * float) ->
    range:(float * float) ->
    float t
end

module Viewport : sig
  type t

  val id : t

  val make :
    ?scale_x:float Scaling.t ->
    ?scale_y:float Scaling.t ->
    unit ->
    t

  val linear :
    xlim:float * float ->
    ylim:float * float ->
    size:float * float ->
    t

  val scale_x : t -> float -> float
  val scale_y : t -> float -> float
  val scale : t -> point -> point
end

module Font : sig
  type t

  val ascender : t -> float
  val descender : t -> float
  val xmin : t -> float
  val ymin : t -> float
  val xmax : t -> float
  val ymax : t -> float
  val default : t

  val dejavu_sans_mono : t
  val dejavu_sans_mono_bold : t
  val dejavu_sans_mono_oblique : t
  val dejavu_sans_mono_bold_oblique : t

  val liberation_sans : t
  val liberation_sans_bold : t
  val liberation_sans_italic : t
  val liberation_sans_bold_italic : t
end

type thickness = [
  | `normal
  | `thick
]

type point_shape = [
  | `bullet
  | `circle
]

module Picture : sig
  type t

  val void : t

  val points :
    ?col:Color.t ->
    ?shape:point_shape ->
    x:float array ->
    y:float array ->
    unit ->
    t

  val rect :
    ?draw:Color.t ->
    ?fill:Color.t ->
    ?thickness:thickness ->
    xmin:float ->
    xmax:float ->
    ymin:float ->
    ymax:float ->
    unit ->
    t

  val path :
    ?col:Color.t ->
    ?thickness:thickness ->
    ?arrow_head:bool ->
    ?cap:Vg.P.cap ->
    point list ->
    t

  val circle :
    ?draw:Color.t ->
    ?fill:Color.t ->
    ?thickness:thickness ->
    x:float ->
    y:float ->
    radius:float ->
    unit ->
    t

  val blend : t list -> t
  val blend2 : t -> t -> t

  val translate :
    ?dx:float ->
    ?dy:float ->
    t -> t

  val scale :
    ?center:[`bbox_center | `origin] ->
    ?sx:float ->
    ?sy:float ->
    t -> t

  val reshape :
    t ->
    bbox:box2 ->
    t

  val crop : t -> box2 -> t

  val frame : t -> t

  val pileup : t list -> t

  val vstack :
    ?align:[`none | `centered | `left | `right ] ->
    t list ->
    t

  val hstack :
    ?align:[`none | `centered | `top | `bottom ] ->
    t list ->
    t

  val text :
    ?col:Color.t ->
    ?size:float ->
    ?font:Font.t ->
    ?halign:[ `middle | `left | `right ] ->
    ?valign:[ `base | `top | `bottom ] ->
    x:float ->
    y:float ->
    string ->
    t

  val bbox : t -> Gg.box2
end

module Plot : sig
  type t

  val points :
    ?title:string ->
    ?col:Color.t ->
    ?shape:[`bullet | `circle] ->
    float array ->
    float array ->
    t

  val render :
    ?width:float ->
    ?height:float ->
    t list ->
    Picture.t
end

type target = [
  | `File of string
  | `Channel of Stdlib.out_channel
  | `Buffer of Buffer.t
]

module Layout : sig
  type t

  val simple : Picture.t -> t

  val render :
    ?width:float ->
    ?height:float ->
    [`pdf | `svg] ->
    t ->
    target ->
    unit
end

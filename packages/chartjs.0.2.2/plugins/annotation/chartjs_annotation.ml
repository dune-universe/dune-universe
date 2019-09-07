open Js_of_ocaml
open Chartjs

module Line_mode = struct
  type t = Js.js_string

  let vertical = Js.string "vertical"

  let horizontal = Js.string "horizontal"
end

module Draw_time = struct
  type t = Js.js_string

  let afterDraw = Js.string "afterDraw"

  let afterDatasetsDraw = Js.string "afterDatasetsDraw"

  let beforeDatasetsDraw = Js.string "beforeDatasetsDraw"
end

module Position = struct
  include Position

  let center = of_string "center"
end

class type baseAnnotation =
  object
    method _type : Js.js_string Js.t Js.prop

    method drawTime : Draw_time.t Js.t Js.optdef Js.prop

    method id : Js.js_string Js.t Js.prop
  end

class type label =
  object
    method backgroundColor : Color.t Js.t Js.prop

    method fontFamily : Js.js_string Js.t Js.prop

    method fontSize : int Js.prop

    method fontStyle : Js.js_string Js.t Js.prop

    method fontColor : Js.js_string Js.t Js.prop

    method xPadding : int Js.prop

    method yPadding : int Js.prop

    method cornerRadius : int Js.prop

    method position : Position.t Js.t Js.prop

    method xAdjust : int Js.prop

    method yAdjust : int Js.prop

    method enabled : bool Js.t Js.prop

    method content : Js.js_string Js.t Indexable.t Js.t Js.opt Js.prop
  end

class type ['a] lineAnnotation =
  object
    inherit baseAnnotation

    method mode : Line_mode.t Js.t Js.prop

    method scaleID : Js.js_string Js.t Js.prop

    method value : 'a Js.prop

    method endValue : 'a Js.prop

    method borderColor : Color.t Js.t Js.prop

    method borderWidth : int Js.prop

    method borderDash : line_dash Js.prop

    method borderDashOffset : line_dash_offset Js.prop

    method label : label Js.t Js.prop
  end

class type ['a, 'b] boxAnnotation =
  object
    inherit baseAnnotation

    method xScaleID : Js.js_string Js.t Js.prop

    method yScaleID : Js.js_string Js.t Js.prop

    method xMin : 'a Js.optdef Js.prop

    method xMax : 'a Js.optdef Js.prop

    method yMin : 'b Js.optdef Js.prop

    method yMax : 'b Js.optdef Js.prop

    method borderColor : Color.t Js.t Js.prop

    method borderWidth : int Js.prop

    method backgroundColor : Color.t Js.t Js.prop
  end

class type annotation =
  object
    method drawTime : Draw_time.t Js.t Js.prop

    method events : Js.js_string Js.t Js.js_array Js.t Js.prop

    method dblClickSpeed : float Js.prop

    method annotations : #baseAnnotation Js.t Js.js_array Js.t Js.prop
  end

module CoerceTo = struct
  let lineAnnotation (x : #baseAnnotation Js.t) : 'a lineAnnotation Js.t Js.opt =
    if Js.string "line" == x##._type then Js.some (Js.Unsafe.coerce x) else Js.null

  let boxAnnotation (x : #baseAnnotation Js.t) : ('a, 'b) boxAnnotation Js.t Js.opt =
    if Js.string "box" == x##._type then Js.some (Js.Unsafe.coerce x) else Js.null
end

let coerce_annotation a = (a :> baseAnnotation Js.t)

let empty_label () : label Js.t = Js.Unsafe.obj [||]

let empty_box_annotation () =
  let (obj : ('a, 'b) boxAnnotation Js.t) = Js.Unsafe.obj [||] in
  obj##._type := Js.string "box";
  obj

let empty_line_annotation () =
  let (obj : 'a lineAnnotation Js.t) = Js.Unsafe.obj [||] in
  obj##._type := Js.string "line";
  obj

let empty_annotation_config () : annotation Js.t = Js.Unsafe.obj [||]

let of_chart_options options = (Js.Unsafe.coerce options)##.annotation

let set_to_chart_options options plugin =
  (Js.Unsafe.coerce options)##.annotation := plugin

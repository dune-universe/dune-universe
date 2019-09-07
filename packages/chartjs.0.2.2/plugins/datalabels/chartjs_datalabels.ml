open Js_of_ocaml
open Chartjs

module Align = struct
  type t

  let center = Js.Unsafe.coerce @@ Js.string "center"

  let start = Js.Unsafe.coerce @@ Js.string "start"

  let _end = Js.Unsafe.coerce @@ Js.string "end"

  let right = Js.Unsafe.coerce @@ Js.string "right"

  let bottom = Js.Unsafe.coerce @@ Js.string "bottom"

  let left = Js.Unsafe.coerce @@ Js.string "left"

  let top = Js.Unsafe.coerce @@ Js.string "top"

  let degree x = Js.Unsafe.coerce @@ Js.number_of_float x
end

module Anchor = struct
  type t = Js.js_string

  let center = Js.string "center"

  let start = Js.string "start"

  let _end = Js.string "end"
end

module Visibility = struct
  type t

  let of_bool x = Js.Unsafe.coerce @@ Js.bool x

  let auto = Js.Unsafe.coerce @@ Js.string "auto"
end

module Text_align = struct
  type t

  let start = Js.Unsafe.coerce @@ Js.string "start"

  let center = Js.Unsafe.coerce @@ Js.string "center"

  let _end = Js.Unsafe.coerce @@ Js.string "end"

  let left = Js.Unsafe.coerce @@ Js.string "left"

  let right = Js.Unsafe.coerce @@ Js.string "right"
end

class type font =
  object
    method family : Js.js_string Js.t Js.optdef_prop

    method size : int Js.optdef_prop

    method style : Js.js_string Js.t Js.optdef_prop

    method weight : Js.js_string Js.t Js.optdef_prop

    method lineHeight : Line_height.t Js.t Js.optdef_prop
  end

class type optionContext =
  object
    method active : bool Js.t Js.readonly_prop

    method chart : chart Js.t Js.readonly_prop

    method dataIndex : int Js.readonly_prop

    method dataset : dataset Js.t Js.readonly_prop

    method datasetIndex : int Js.readonly_prop
  end

class type listeners =
  object
    method enter : (optionContext Js.t -> bool Js.t) Js.callback Js.optdef_prop

    method leave : (optionContext Js.t -> bool Js.t) Js.callback Js.optdef_prop

    method click : (optionContext Js.t -> bool Js.t) Js.callback Js.optdef_prop
  end

type 'a prop = (optionContext Js.t, 'a) Scriptable_indexable.t Js.t

type 'a formatter = ('a -> optionContext Js.t -> Js.js_string Js.t) Js.callback

class type datalabels =
  object
    method align : Align.t Js.t prop Js.optdef_prop

    method anchor : Anchor.t Js.t prop Js.optdef_prop

    method backgroundColor : Color.t Js.t Js.opt prop Js.optdef_prop

    method borderColor : Color.t Js.t Js.opt prop Js.optdef_prop

    method borderRadius : int prop Js.optdef_prop

    method borderWidth : int prop Js.optdef_prop

    method clamp : bool Js.t prop Js.optdef_prop

    method clip : bool Js.t prop Js.optdef_prop

    method color : Color.t Js.t prop Js.optdef_prop

    method display : Visibility.t Js.t prop Js.optdef_prop

    method font : font Js.t prop Js.optdef_prop

    method formatter : 'a formatter Js.opt Js.optdef_prop

    method listeners : listeners Js.t Js.optdef_prop

    method offset : float prop Js.optdef_prop

    method opacity : float prop Js.optdef_prop

    method padding : Padding.t Js.t prop Js.optdef_prop

    method rotation : float prop Js.optdef_prop

    method textAlign : Text_align.t Js.t prop Js.optdef_prop

    method textStrokeColor : Color.t Js.t prop Js.optdef_prop

    method textStrokeWidth : int prop Js.optdef_prop

    method textShadowBlur : float prop Js.optdef_prop

    method textShadowColor : Color.t Js.t prop Js.optdef_prop
  end

let empty_font () = Js.Unsafe.obj [||]

let empty_listeners () = Js.Unsafe.obj [||]

let empty_datalabels_config () = Js.Unsafe.obj [||]

let of_dataset dataset = (Js.Unsafe.coerce dataset)##.datalabels

let of_chart_options options = (Js.Unsafe.coerce options)##.plugins##.datalabels

let of_global () = Js.Unsafe.global##._Chart##.defaults##.global##.plugins##.datalabels

let set_to_dataset dataset plugin = (Js.Unsafe.coerce dataset)##.realtime := plugin

let set_to_chart_options chart plugin =
  (Js.Unsafe.coerce chart)##.options##.plugins##.datalabels := plugin

let set_globally plugin =
  Js.Unsafe.global##._Chart##.defaults##.global##.plugins##.datalabels := plugin

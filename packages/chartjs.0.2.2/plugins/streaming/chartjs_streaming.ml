open Js_of_ocaml

module Axis = struct
  let realtime = Chartjs.Axis.of_string "realtime"
end

class type updateConfig =
  object
    inherit Chartjs.updateConfig

    method preservation : bool Js.t Js.optdef_prop
  end

class type streaming =
  object
    method duration : int Js.optdef_prop

    method ttl : int Js.optdef Js.prop

    method delay : int Js.optdef_prop

    method refresh : int Js.optdef_prop

    method onRefresh : (Chartjs.chart Js.t -> unit) Js.callback Js.opt Js.optdef_prop

    method frameRate : float Js.optdef_prop

    method pause : bool Js.t Js.optdef_prop
  end

let empty_update_config ?preservation () =
  let (obj : updateConfig Js.t) = Js.Unsafe.obj [||] in
  match preservation with
  | None -> obj
  | Some p ->
      obj##.preservation := Js.bool p;
      obj

let empty_streaming_config () = Js.Unsafe.obj [||]

let of_axis axis = (Js.Unsafe.coerce axis)##.realtime

let of_chart_options options = (Js.Unsafe.coerce options)##.plugins##.streaming

let of_global () = Js.Unsafe.global##._Chart##.defaults##.global##.plugins##.streaming

let set_to_axis axis plugin = (Js.Unsafe.coerce axis)##.realtime := plugin

let set_to_chart_options options plugin =
  (Js.Unsafe.coerce options)##.plugins##.streaming := plugin

let set_globally plugin =
  Js.Unsafe.global##._Chart##.defaults##.global##.plugins##.streaming := plugin

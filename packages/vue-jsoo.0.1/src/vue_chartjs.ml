open Js_of_ocaml
open Js
open Mjs
open Vue_component

type vue_component = any

class type mixins = object
  method reactiveData : any readonly_prop
  method reactiveProp : any readonly_prop
end

class type vue_charts = object
  method _Bar : vue_component readonly_prop
  method _HorizontalBar : vue_component readonly_prop
  method _Doughnut : vue_component readonly_prop
  method _Line : vue_component readonly_prop
  method _Pie : vue_component readonly_prop
  method _PolarArea : vue_component readonly_prop
  method _Radar : vue_component readonly_prop
  method _Bubble : vue_component readonly_prop
  method _Scatter : vue_component readonly_prop
  method mixins : mixins t readonly_prop
  method generateChart : Unsafe.any readonly_prop
end

let vue_charts : vue_charts t = Unsafe.variable "VueChartJs"

let make typ id = make
    ~extends:typ
    ~props:(PrsArray ["data"; "options"])
    ~mounted:(fun this ->
        let this = Unsafe.coerce this in
        this##renderChart this##.data this##.options)
    id

let make_line () = make vue_charts##._Line "line-chart"
let make_bar () = make vue_charts##._Bar "bar-chart"

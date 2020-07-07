open Js_of_ocaml
open Js
open Mjs

let () =

  let _linechart = Vue_chartjs.make_line () in

  let labels = of_listf string
      ["January"; "February"; "March"; "April"; "May"; "June"; "July"] in
  let datasets = of_list [
      object%js
        val label = string "Data One";
        val backgroundColor = "#f87979"
        val data = of_list [ 40; 39; 10; 40; 39; 80; 40 ]
      end
    ] in
  let options = object%js
    val responsive = _true
    val maintainAspectRatio = _false
  end in

  let data = object%js
    val data = object%js
      val labels = labels
      val datasets = datasets
    end
    val options = options
  end in
  Js_of_ocaml.Js.export "app" @@ Vue_js.make ~data "app"

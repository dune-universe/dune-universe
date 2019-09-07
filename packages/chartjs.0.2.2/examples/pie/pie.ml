open Js_of_ocaml

let () =
  let border_color_fun context =
    Chartjs.Color.of_string
      (match context##.dataIndex with
      | 0 -> "red"
      | 1 -> "blue"
      | 2 -> "green"
      | _ -> "initial")
  in
  let background_color_fun context =
    Chartjs.Color.of_string
      (match context##.dataIndex with
      | 0 -> "pink"
      | 1 -> "lightblue"
      | 2 -> "lightgreen"
      | _ -> "initial")
  in
  (* Create dataset. *)
  let dataset = Chartjs.empty_pie_dataset () in
  dataset##.data := Js.array [|40; 15; 20|];
  dataset##.borderColor := Chartjs.Scriptable_indexable.of_fun border_color_fun;
  dataset##.backgroundColor := Chartjs.Scriptable_indexable.of_fun background_color_fun;
  dataset##.borderWidth := Chartjs.Scriptable_indexable.of_single 5;
  dataset##.label := Js.string "Dataset 1";
  (* Create chart data. *)
  let data = Chartjs.empty_data () in
  data##.datasets := Js.array [|dataset|];
  data##.labels := Js.array @@ Array.map Js.string [|"first"; "second"; "third"|];
  let legend = Chartjs.empty_legend () in
  let animation = Chartjs.empty_pie_animation () in
  let options = Chartjs.empty_pie_options () in
  animation##.animateScale := Js._true;
  animation##.animateRotate := Js._true;
  legend##.position := Chartjs.Position.left;
  options##.cutoutPercentage := 20.;
  options##.animation := animation;
  options##.legend := legend;
  let pie = Chartjs.chart_from_id Chartjs.Chart.pie data options "chart" in
  Js.Unsafe.global##.chart := pie

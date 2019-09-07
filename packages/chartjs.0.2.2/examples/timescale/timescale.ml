open Js_of_ocaml

let format = "MM/DD/YYYY HH:mm"

let new_date year = Chartjs.Time.of_array [|year|]

let random () = Random.int 10

let () =
  Random.init (Float.to_int (new%js Js.date_now)##getTime);
  let dataset1 = Chartjs.empty_line_dataset () in
  dataset1##.data := Js.array @@ Array.init 7 (fun _ -> random ());
  dataset1##.label := Js.string "My First Dataset";
  dataset1##.backgroundColor := Chartjs.Color.of_string "rgba(255, 0, 0, 0.5)";
  dataset1##.borderColor := Chartjs.Color.of_string "red";
  dataset1##.fill := Chartjs.Line_fill._false;
  let dataset2 = Chartjs.empty_line_dataset () in
  dataset2##.data := Js.array @@ Array.init 7 (fun _ -> random ());
  dataset2##.label := Js.string "My Second Dataset";
  dataset2##.backgroundColor := Chartjs.Color.of_string "rgba(0, 0, 255, 0.5)";
  dataset2##.borderColor := Chartjs.Color.of_string "blue";
  dataset2##.fill := Chartjs.Line_fill._false;
  let dataset3 = Chartjs.empty_line_dataset () in
  dataset3##.data :=
    Js.array
      Chartjs.
        [| create_data_point ~x:(new_date 1990) ~y:(random ())
         ; create_data_point ~x:(new_date 1992) ~y:(random ())
         ; create_data_point ~x:(new_date 1994) ~y:(random ())
         ; create_data_point ~x:(new_date 1996) ~y:(random ()) |];
  dataset3##.label := Js.string "Dataset with point data";
  dataset3##.backgroundColor := Chartjs.Color.of_string "rgba(0, 255, 0, 0.5)";
  dataset3##.borderColor := Chartjs.Color.of_string "green";
  dataset3##.fill := Chartjs.Line_fill._false;
  let data = Chartjs.empty_data () in
  data##.labels :=
    Js.array
      [| new_date 1990
       ; new_date 1991
       ; new_date 1992
       ; new_date 1993
       ; new_date 1994
       ; new_date 1995
       ; new_date 1996 |];
  data##.datasets :=
    Js.array
      Chartjs.
        [|coerce_dataset dataset1; coerce_dataset dataset2; coerce_dataset dataset3|];
  (* Initialize title *)
  let title = Chartjs.empty_title () in
  title##.display := Js._true;
  title##.text := Chartjs.Indexable.of_single @@ Js.string "Chart.js Time Scale";
  (* Initialize scales *)
  let time = Chartjs.empty_time_options () in
  let scaleLabel = Chartjs.empty_scale_label () in
  let xAxis = Chartjs.empty_time_axis () in
  time##._parser := Chartjs.Time_parser.of_string format;
  time##.tooltipFormat := Js.string "ll HH:mm";
  scaleLabel##.display := Js._true;
  scaleLabel##.labelString := Js.string "Date";
  xAxis##.time := time;
  xAxis##.scaleLabel := scaleLabel;
  let yAxis = Chartjs.empty_cartesian_axis () in
  let scaleLabel = Chartjs.empty_scale_label () in
  scaleLabel##.display := Js._true;
  scaleLabel##.labelString := Js.string "value";
  yAxis##.scaleLabel := scaleLabel;
  let scales = Chartjs.empty_line_scales () in
  scales##.xAxes := Js.array [|xAxis|];
  scales##.yAxes := Js.array [|yAxis|];
  (* Initialize other options *)
  let options = Chartjs.empty_line_options () in
  options##.scales := scales;
  options##.title := title;
  options##.maintainAspectRatio := Js._false;
  let chart = Chartjs.chart_from_id Chartjs.Chart.line data options "chart" in
  Js.Unsafe.global##.chart := chart

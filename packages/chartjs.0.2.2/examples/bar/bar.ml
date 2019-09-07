open Js_of_ocaml

let () =
  (* Create first dataset. *)
  let dataset1 = Chartjs.empty_bar_dataset () in
  dataset1##.data := Js.array [|10.; 15.; 30.; 20.; 25.; 10.; 7.|];
  dataset1##.label := Js.string "Dataset 1";
  dataset1##.borderColor :=
    Chartjs.Scriptable_indexable.of_single @@ Chartjs.Color.of_string "red";
  dataset1##.backgroundColor :=
    Chartjs.Scriptable_indexable.of_single
    @@ Chartjs.Color.of_string "rgba(255, 0, 0, 0.4)";
  (* Create second dataset. *)
  let dataset2 = Chartjs.empty_bar_dataset () in
  dataset2##.data := Js.array [|20.; 10.; nan; 15.; 5.; 7.; 30.|];
  dataset2##.label := Js.string "Dataset 2";
  dataset2##.borderColor :=
    Chartjs.Scriptable_indexable.of_single @@ Chartjs.Color.of_string "blue";
  dataset2##.backgroundColor :=
    Chartjs.Scriptable_indexable.of_single
    @@ Chartjs.Color.of_string "rgba(0, 0, 255, 0.4)";
  (* Create chart data. *)
  let labels =
    Array.map Js.string [|"January"; "February"; "March"; "April"; "May"; "June"; "July"|]
  in
  let data = Chartjs.empty_data () in
  data##.datasets := Js.array [|dataset1; dataset2|];
  data##.labels := Js.array labels;
  (* Initialize title *)
  let title = Chartjs.empty_title () in
  title##.display := Js._true;
  title##.text := Chartjs.Indexable.of_single @@ Js.string "Bar Chart";
  (* Initialize tooltips *)
  let tooltips = Chartjs.empty_tooltip () in
  tooltips##.mode := Chartjs.Interaction_mode.index;
  tooltips##.intersect := Js._false;
  (* Initialize other options *)
  let options = Chartjs.empty_bar_options () in
  options##.title := title;
  options##.tooltips := tooltips;
  options##.maintainAspectRatio := Js._false;
  let chart = Chartjs.chart_from_id Chartjs.Chart.bar data options "bar" in
  Js.Unsafe.global##.chart := chart

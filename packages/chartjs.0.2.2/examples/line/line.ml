open Js_of_ocaml

let () =
  (* Create first dataset. *)
  let dataset1 = Chartjs.empty_line_dataset () in
  dataset1##.data := Js.array [|10.; 15.; 30.; 20.; 25.; 10.; 7.|];
  dataset1##.borderColor := Chartjs.Color.of_string "red";
  dataset1##.backgroundColor := Chartjs.Color.of_string "rgba(255, 0, 0, 0.4)";
  dataset1##.label := Js.string "Dataset 1";
  dataset1##.pointStyle := Chartjs.Point_style.star;
  dataset1##.pointRadius := Chartjs.Scriptable_indexable.of_single 10;
  dataset1##.pointBorderWidth := Chartjs.Scriptable_indexable.of_single 2;
  (* Create second dataset. *)
  let dataset2 = Chartjs.empty_line_dataset () in
  dataset2##.data := Js.array [|20.; 10.; nan; 15.; 5.; 7.; 30.|];
  dataset2##.borderColor := Chartjs.Color.of_string "blue";
  dataset2##.backgroundColor := Chartjs.Color.of_string "rgba(0, 0, 255, 0.4)";
  dataset2##.label := Js.string "Dataset 2";
  dataset2##.spanGaps := Js._false;
  dataset2##.pointStyle := Chartjs.Point_style.rectRot;
  dataset2##.pointRadius := Chartjs.Scriptable_indexable.of_single 10;
  dataset2##.pointBorderWidth := Chartjs.Scriptable_indexable.of_single 2;
  (* Create chart data. *)
  let labels =
    Array.map Js.string [|"January"; "February"; "March"; "April"; "May"; "June"; "July"|]
  in
  let data = Chartjs.empty_data () in
  data##.datasets := Js.array [|dataset1; dataset2|];
  data##.labels := Js.array labels;
  (* Initialize legend *)
  let legend_labels = Chartjs.empty_legend_labels () in
  let legend = Chartjs.empty_legend () in
  legend_labels##.fontSize := 12;
  legend_labels##.fontColor := Chartjs.Color.of_string "blue";
  legend_labels##.fontStyle := Js.string "bold";
  legend_labels##.fontFamily := Js.string "monospace";
  legend_labels##.padding := 20;
  legend_labels##.usePointStyle := Js._true;
  legend##.fullWidth := Js._false;
  legend##.reverse := Js._true;
  legend##.labels := legend_labels;
  (* Initialize title *)
  let title = Chartjs.empty_title () in
  title##.display := Js._true;
  title##.fontFamily := Js.string "monospace";
  title##.fontColor := Js.string "indigo";
  title##.fontStyle := Js.string "italic";
  title##.fontSize := 15;
  title##.padding := 20;
  title##.lineHeight := Chartjs.Line_height.of_float 2.;
  title##.position := Chartjs.Position.left;
  title##.text := Chartjs.Indexable.of_list [Js.string "Title"; Js.string "subtitle"];
  (* Initialize tooltips *)
  let tooltips = Chartjs.empty_tooltip () in
  tooltips##.mode := Chartjs.Interaction_mode.index;
  tooltips##.intersect := Js._false;
  (* Initialize scales *)
  let axis = Chartjs.empty_category_axis () in
  let scales = Chartjs.empty_line_scales () in
  axis##.display := Chartjs.Axis_display.auto;
  scales##.xAxes := Js.array [|axis|];
  (* Initialize other options *)
  let options = Chartjs.empty_line_options () in
  options##.scales := scales;
  options##.legend := legend;
  options##.title := title;
  options##.tooltips := tooltips;
  options##.maintainAspectRatio := Js._false;
  let chart = Chartjs.chart_from_id Chartjs.Chart.line data options "chart" in
  Js.Unsafe.global##.chart := chart

open! Import
module Ui = Render.Make (Draw_notty)

let%expect_test "show a waveform through notty" =
  let ctx = Draw_notty.init ~rows:10 ~cols:60 in
  let waves = Test_data.create ~prefix:(fun _ -> "") ~length:10 ~num_signals:3 in
  Ui.draw_ui ~ctx waves;
  Draw_notty.to_image ctx |> Notty_unix.output_image;
  [%expect
    {|
    ┌Signals─┐┌Values──┐┌Waves─────────────────────────────────┐
    │clock   ││        ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐ │
    │        ││        ││    └───┘   └───┘   └───┘   └───┘   └─│
    │iinbezs ││       1││────────────────────────┐             │
    │        ││        ││                        └─────────────│
    │site    ││       1││────────────────────────┐             │
    │        ││        ││                        └─────────────│
    │suzfdbsn││       0││                        ┌─────────────│
    │        ││        ││────────────────────────┘             │
    └────────┘└────────┘└──────────────────────────────────────┘ |}]
;;

open! Core
open! Import

let%expect_test "default timing-wheel precision and level durations" =
  let module Config = Incremental.Config.Default () in
  let config = Config.timing_wheel_config in
  let durations = Timing_wheel_ns.Config.durations config in
  require [%here] (Time_ns.Span.( >= ) (List.last_exn durations) Time_ns.Span.day);
  print_s [%message
    ""
      ~alarm_precision:(Timing_wheel_ns.Config.alarm_precision config : Time_ns.Span.t)
      (durations : Time_ns.Span.t list)];
  [%expect {|
    ((alarm_precision 1.049ms) (durations (17.1799s 1.62891d 52.125d))) |}];
;;

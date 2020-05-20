let%expect_test _ =
  let logger = Loga.Logger.create_default () in
  Loga.Logger.set_timer logger Loga.Timer.zero_gen;

  let (module Loga) = Loga.with_logger logger in

  [%loga.info "loglog"];
  [%expect
    {| 1900-01-01T00:00:00+00:00 [     INFO] (example/log_example.ml:7) loglog |}];

  [%loga.info "param = %s" "a"];
  [%expect
    {| 1900-01-01T00:00:00+00:00 [     INFO] (example/log_example.ml:11) param = a |}];

  let n = 42 in
  [%loga.warning "N = %d" n];
  [%expect
    {| 1900-01-01T00:00:00+00:00 [  WARNING] (example/log_example.ml:16) N = 42 |}]

open Mkaudio_libs
open OUnit2

let test_seconds _ =
  assert_equal (Time.parse_duration "1s") (Result.Ok 1.0)

let test_minutes _ =
  assert_equal (Time.parse_duration "5m") (Result.Ok 300.0)

let test_hours _ =
  assert_equal (Time.parse_duration "2h") (Result.Ok 7200.0)

let test_bad_interval _ =
  assert_equal
    (Time.parse_duration "10q")
    (Result.Error "Unknown interval: q")

let test_missing_interval _ =
  assert_equal
    (Time.parse_duration "10")
    (Result.Error "Malformed duration: 10")

let test_nonsense_input _ =
  assert_equal
    (Time.parse_duration "rj3k4h34j")
    (Result.Error "Malformed duration: rj3k4h34j")

let parse_duration =
  "parse_duration" >::: [
    "test_seconds" >:: test_seconds;
    "test_minutes" >:: test_minutes;
    "test_hours" >:: test_hours;
    "test_bad_interval" >:: test_bad_interval;
    "test_missing_interval" >:: test_missing_interval;
    "test_nonsense_input" >:: test_nonsense_input;
  ]

let test_duration _ =
  assert_equal
    (Time.calculate_samples
      ~sample_rate:48000 ~duration:(Some 1.) ~tempo:None ~steps:None)
    (Result.Ok 48000)

let test_tempo_and_steps _ =
  assert_equal
    (Time.calculate_samples
      ~sample_rate:48000 ~duration:None
      ~tempo:(Some 120.) ~steps:(Some 16))
    (Result.Ok 96000)

let test_missing_args _ =
  assert_bool "check missing args produce an error" (
    match
      Time.calculate_samples
        ~sample_rate:48000 ~duration:None
        ~tempo:None ~steps:None
    with
    | Result.Ok _ -> false
    | Result.Error _ -> true
  )

let test_missing_tempo _ =
  assert_bool "check missing tempo produces an error" (
    match
      Time.calculate_samples
        ~sample_rate:48000 ~duration:None
        ~tempo:None ~steps:(Some 16)
    with
    | Result.Ok _ -> false
    | Result.Error _ -> true
  )

let test_missing_steps _ =
  assert_bool "check missing steps produces an error" (
    match
      Time.calculate_samples
        ~sample_rate:48000 ~duration:None
        ~tempo:(Some 240.) ~steps:None
    with
    | Result.Ok _ -> false
    | Result.Error _ -> true
  )

let calculate_samples =
  "calculate_samples" >::: [
    "test_duration" >:: test_duration;
    "test_tempo_and_steps" >:: test_tempo_and_steps;
    "test_missing_args" >:: test_missing_args;
    "test_missing_tempo" >:: test_missing_tempo;
    "test_missing_steps" >:: test_missing_steps;
  ]

let suite =
  "time" >::: [
    parse_duration;
    calculate_samples;
  ]

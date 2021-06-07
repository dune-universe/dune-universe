open Mkaudio_libs
open OUnit2

let test_parse_kick_pattern _ =
  assert_equal
    (Beat.parse_patterns ~kick:(Some "1") ~snare:None ~hihat:None)
    (Result.Ok [
      Beat.({
        kick = true;
        snare = false;
        hihat = false;
      });
    ])

let test_parse_snare_pattern _ =
  assert_equal
    (Beat.parse_patterns ~kick:None ~snare:(Some "1") ~hihat:None)
    (Result.Ok [
      Beat.({
        kick = false;
        snare = true;
        hihat = false;
      });
    ])

let test_parse_hihat_pattern _ =
  assert_equal
    (Beat.parse_patterns ~kick:None ~snare:None ~hihat:(Some "1"))
    (Result.Ok [
      Beat.({
        kick = false;
        snare = false;
        hihat = true;
      });
    ])

let suite =
  "beat" >::: [
    "test_parse_kick_pattern" >:: test_parse_kick_pattern;
    "test_parse_snare_pattern" >:: test_parse_snare_pattern;
    "test_parse_hihat_pattern" >:: test_parse_hihat_pattern;
  ]

open OUnit2

let (>:=) password test = password >:: test ~password

let overall_score_suite =
  let test ~password ~score _ctxt =
    let (real, _) = Zxcvbn.matches password in
    let diff = (real /. score) -. 1. |> abs_float in
    if diff > 0.01 then
      assert_failure @@ Printf.sprintf "Expected %.4f but got %.4f" score real
  in
  [ "zxcvbn" >:= test ~score:5.83
  ; "qwER43@!" >:= test ~score:26.44
  ; "Tr0ub4dour&3" >:= test ~score:30.87
  ; "archi" >:= test ~score:13.61
  ; "D0g.................." >:= test ~score:19.02
  ; "abcdefghijk987654321" >:= test ~score:8.53
  ; "neverforget13/3/1997" >:= test ~score:34.86
  ; "1qaz2wsx3edc" >:= test ~score:9.98
  ; "barbarbara" >:= test ~score:12.43
  ; "abarbarbara" >:= test ~score:16.18
  ; "temppass22" >:= test ~score:17.20
  ; "briansmith" >:= test ~score:5.32
  ; "htimsnairb" >:= test ~score:6.07
  ; "briansmith4mayor" >:= test ~score:21.63
  ; "password1" >:= test ~score:4.0
  ; "viking" >:= test ~score:7.93
  ; "thx1138" >:= test ~score:7.70
  ; "ScoRpi0ns" >:= test ~score:19.54
  ; "do you know" >:= test ~score:25.51
  ; "ryanhunter2000" >:= test ~score:20.8
  ; "rianhunter2000" >:= test ~score:28.25
  ; "asdfghju7654rewq" >:= test ~score:29.57
  ; "AOEUIDHG&*()LS_" >:= test ~score:33.33
  ; "12345678" >:= test ~score:1.59
  ; "defghi6789" >:= test ~score:13.61
  ; "02468" >:= test ~score:3.32
  ; "adgjmpsvy" >:= test ~score:4.17
  ; "rosebud" >:= test ~score:8.09
  ; "Rosebud" >:= test ~score:9.09
  ; "ROSEBUD" >:= test ~score:9.09
  ; "rosebuD" >:= test ~score:9.09
  ; "R0$38uD" >:= test ~score:12.09
  ; "ros3bud99" >:= test ~score:14.41
  ; "r0s3bud99" >:= test ~score:14.41
  ; "R0$38uD99" >:= test ~score:17.41
  ; "verlineVANDERMARK" >:= test ~score:27.24
  ; "eheuczkqyq" >:= test ~score:41.24
  ; "rWibMFACxAUGZmxhVncy" >:= test ~score:111.0
  ; "illness" >:= test ~score:11.26
  ; "1llness" >:= test ~score:12.26
  ; "i1lness" >:= test ~score:12.84
  ; "11lness" >:= test ~score:22.44
  ; "ssenl1i" >:= test ~score:12.84
  ; "Ba9ZyWABu99[BK#6MBgbH88Tofv)vs$w" >:= test ~score:171.63
  ; "correcthorsebatterystaple" >:= test ~score:47.98
  ; "elpatsyrettabesrohtcerroc" >:= test ~score:48.98
  ; "coRrecth0rseba++ery9.23.2007staple$" >:= test ~score:71.95
  ; "pass.word.pass.word.pass.word." >:= test ~score:60.41
  ; "passpasswordword" >:= test ~score:17.28
  ; "quvpzquvpz" >:= test ~score:24.50
  ; "magicfavoriteunclepromisedpublicbotherislandjimseriouslycellleadknowingbrokenadvicesomehowpaidblairlosingpushhelpedkillingusuallyearlierbosslaurabeginninglikedinnocentdocruleselizabethsabrinasummerexcoplearnedthirtyrisklettingphillipspeakingofficerridiculoussupportafternoonericwithsobutallwellareheohaboutrightyou're" >:= test ~score:545.9
  ]

module M = struct
  type t =
    { beginning: int
    ; length: int
    ; kind: Zxcvbn.Match.kind
    ; multipart: bool
    }
  [@@deriving eq,ord,show]

  let of_zxcvbn_match {Zxcvbn.Match.beginning; length; kind; multipart; _} =
    {beginning; length; kind; multipart}
end

let matches_suite =
  let test ~password ~matches ctxt =
    let module Match_set = Set.Make(M) in
    let cmp = Match_set.equal in
    let printer s = [%show: M.t list] @@ Match_set.elements s in
    let expected = Match_set.of_list matches in
    let real =
      Zxcvbn.matches password |> snd
      |> List.rev_map M.of_zxcvbn_match
      |> Match_set.of_list
    in
    assert_equal ~ctxt ~cmp ~printer expected real
  in
  let open Zxcvbn in
  [ "" >:=  test ~matches:[]
  ; "password" >:=
    test ~matches:[M.{ beginning=0; length=8; kind=Match.Dictionnary_match; multipart=false }]
  ; "passw0rd" >:=
    test ~matches:[M.{ beginning=0; length=8; kind=Match.Dict_leet_match; multipart=false }]
  ; "passwordpasswordpassword" >:=
    test ~matches:[M.{ beginning=0; length=24; kind=Match.Dictionnary_match; multipart=true }]
  ; "jkl;'" >:=
    test ~matches:[M.{ beginning=0; length=5; kind=Match.Spatial_match; multipart=false }]
  ; "defghij" >:=
    test ~matches:[M.{ beginning=0; length=7; kind=Match.Sequence_match; multipart=false }]
  ; "01/02/2003" >:=
    test ~matches:[M.{ beginning=0; length=10; kind=Match.Date_match; multipart=false }]
  ; "ccccccc" >:=
    test ~matches:[M.{ beginning=0; length=7; kind=Match.Repeats_match; multipart=false }]
  ; "lgjt" >:=
    test ~matches:[M.{ beginning=0; length=4; kind=Match.Brute_match; multipart=false }]
  ; "password1234" >:=
    test ~matches:[ M.{ beginning=0; length=8; kind=Match.Dictionnary_match; multipart=false }
                  ; M.{ beginning=8; length=4; kind=Match.Dictionnary_match; multipart=false }
                  ]
  ]

let all =
  "All" >:::
  [ "Overall score" >::: overall_score_suite
  ; "Matches" >::: matches_suite
  ]

let () = run_test_tt_main all

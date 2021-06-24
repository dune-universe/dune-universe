open Mula.Strings.Lev

(* Lev tests *)
let%test "simple delete abc ab" =
  get_distance ~k:2 "abc" "ab" = Some 1

let%test "swap (insert + delete) abc-acb" =
  get_distance ~k:2 "abc" "acb" = Some 2

let%test "simple insert abc-acbc" =
  get_distance ~k:2 "abc" "acbc" = Some 1

let%test "simple substitute abc-acc" =
  get_distance ~k:2 "abc" "acc" = Some 1

let%test "simple substitute insert abc-accc" =
  get_distance ~k:2 "abc" "accc" = Some 2

let%test "large diff abc-def" =
  get_distance ~k:2 "abc" "def" = None

let%test "large diff abcdef-g" =
  get_distance ~k:2 "abcdef" "g" = None

let%test "large diff abcdef-a" =
  get_distance ~k:2 "abcdef" "a" = None

let%test "large diff abcdef-ab" =
  get_distance ~k:2 "abcdef" "ab" = None

let%test "large diff abcdef-abc" =
  get_distance ~k:2 "abcdef" "abc" = None

let%test "large strings abcdef-abcd" =
  get_distance ~k:2 "abcdef" "abcd" = Some 2

let%test "large strings abcdef-abcde" =
  get_distance ~k:1 "abcdef" "abcde" = Some 1

let%test "current distance should be zero abcdef-abcde" =
  let nfa = start ~k:2 ~str:"abcdef" in
  (feed_str ~str:"abcde" nfa |> current_error)
  = Some 0

let%test "current distance should be zero abcdef-abcde" =
  let nfa = start ~k:2 ~str:"abcdef" in
  (feed_str ~str:"abcde" nfa |> end_input)
  = Some 1

let%test "current distance should be zero abcdef-ab" =
  let nfa = start ~k:2 ~str:"abcdef" in
  (feed_str ~str:"ab" nfa |> current_error)
  = Some 0

let%test "short input abcdef-ab" =
  let nfa = start ~k:2 ~str:"abcdef" in
  (feed_str ~str:"ab" nfa |> end_input)
  = None

let%test "current distance should be one abcdef-abd" =
  let nfa = start ~k:2 ~str:"abcdef" in
  (feed_str ~str:"abd" nfa |> current_error)
  = Some 1

(* empty *)
let%test "empty-empty" =
  get_distance ~k:1 "" "" = Some 0

let%test "empty-a" =
  get_distance ~k:1 "" "a" = Some 1

let%test "a-empty" =
  get_distance ~k:1 "a" "" = Some 1

let%test "ab-empty" =
  get_distance ~k:1 "ab" "" = None

let%test "empty-ab" =
  get_distance ~k:1 "" "ab" = None

(* k = 0 *)
let%test "k=0 abc-ab" =
  get_distance ~k:0 "abc" "ab" = None

let%test "k=0 abc-abc" =
  get_distance ~k:0 "abc" "abc" = Some 0

let%test "k=0 empty-empty" =
  get_distance ~k:0 "" "" = Some 0

let%test "k=0 empty-a" =
  get_distance ~k:0 "" "a" = None

let%test "k=0 a-empty" =
  get_distance ~k:0 "a" "" = None

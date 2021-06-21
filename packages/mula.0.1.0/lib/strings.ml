module UsualString : Match.S with type ch = char and type t = string = struct
  type ch = char
  type t = string

  let length = String.length
  let get = String.get

  let equal = Char.equal
end

include Match.Make(UsualString)

(* Lev tests *)
let%test "simple delete abc ab" =
  Lev.get_distance ~k:2 "abc" "ab" = Some 1

let%test "swap (insert + delete) abc-acb" =
  Lev.get_distance ~k:2 "abc" "acb" = Some 2

let%test "simple insert abc-acbc" =
  Lev.get_distance ~k:2 "abc" "acbc" = Some 1

let%test "simple substitute abc-acc" =
  Lev.get_distance ~k:2 "abc" "acc" = Some 1

let%test "simple substitute insert abc-accc" =
  Lev.get_distance ~k:2 "abc" "accc" = Some 2

let%test "large diff abc-def" =
  Lev.get_distance ~k:2 "abc" "def" = None

let%test "large diff abcdef-g" =
  Lev.get_distance ~k:2 "abcdef" "g" = None

let%test "large diff abcdef-a" =
  Lev.get_distance ~k:2 "abcdef" "a" = None

let%test "large diff abcdef-ab" =
  Lev.get_distance ~k:2 "abcdef" "ab" = None

let%test "large diff abcdef-abc" =
  Lev.get_distance ~k:2 "abcdef" "abc" = None

let%test "large strings abcdef-abcd" =
  Lev.get_distance ~k:2 "abcdef" "abcd" = Some 2

let%test "large strings abcdef-abcde" =
  Lev.get_distance ~k:1 "abcdef" "abcde" = Some 1

let%test "current distance should be zero abcdef-abcde" =
  let nfa = Lev.start ~k:2 ~str:"abcdef" in
  Lev.(feed_str ~str:"abcde" nfa |> current_error)
  = Some 0

let%test "current distance should be zero abcdef-abcde" =
  let nfa = Lev.start ~k:2 ~str:"abcdef" in
  Lev.(feed_str ~str:"abcde" nfa |> end_input)
  = Some 1

let%test "current distance should be zero abcdef-ab" =
  let open Lev in
  let nfa = Lev.start ~k:2 ~str:"abcdef" in
  (feed_str ~str:"ab" nfa |> current_error)
  = Some 0

let%test "short input abcdef-ab" =
  let nfa = Lev.start ~k:2 ~str:"abcdef" in
  Lev.(feed_str ~str:"ab" nfa |> end_input)
  = None

let%test "current distance should be one abcdef-abd" =
  let nfa = Lev.start ~k:2 ~str:"abcdef" in
  Lev.(feed_str ~str:"abd" nfa |> current_error)
  = Some 1

(* empty *)
let%test "empty-empty" =
  Lev.get_distance ~k:1 "" "" = Some 0

let%test "empty-a" =
  Lev.get_distance ~k:1 "" "a" = Some 1

let%test "a-empty" =
  Lev.get_distance ~k:1 "a" "" = Some 1

let%test "ab-empty" =
  Lev.get_distance ~k:1 "ab" "" = None

let%test "empty-ab" =
  Lev.get_distance ~k:1 "" "ab" = None

(* k = 0 *)
let%test "k=0 abc-ab" =
  Lev.get_distance ~k:0 "abc" "ab" = None

let%test "k=0 abc-abc" =
  Lev.get_distance ~k:0 "abc" "abc" = Some 0

let%test "k=0 empty-empty" =
  Lev.get_distance ~k:0 "" "" = Some 0

let%test "k=0 empty-a" =
  Lev.get_distance ~k:0 "" "a" = None

let%test "k=0 a-empty" =
  Lev.get_distance ~k:0 "a" "" = None


(* Dem tests *)
let%test "simple delete abc ab" =
  Dem.get_distance ~k:2 "abc" "ab" = Some 1

let%test "swap (insert + delete) abc-acb" =
  Dem.get_distance ~k:2 "abc" "acb" = Some 1

let%test "simple insert abc-acbc" =
  Dem.get_distance ~k:2 "abc" "acbc" = Some 1

let%test "simple substitute abc-acc" =
  Dem.get_distance ~k:2 "abc" "acc" = Some 1

let%test "simple substitute insert abc-accc" =
  Dem.get_distance ~k:2 "abc" "accc" = Some 2

let%test "large diff abc-def" =
  Dem.get_distance ~k:2 "abc" "def" = None

let%test "large diff abcdef-g" =
  Dem.get_distance ~k:2 "abcdef" "g" = None

let%test "large diff abcdef-a" =
  Dem.get_distance ~k:2 "abcdef" "a" = None

let%test "large diff abcdef-ab" =
  Dem.get_distance ~k:2 "abcdef" "ab" = None

let%test "large diff abcdef-abc" =
  Dem.get_distance ~k:2 "abcdef" "abc" = None

let%test "large strings abcdef-abcd" =
  Dem.get_distance ~k:2 "abcdef" "abcd" = Some 2

let%test "large strings abcdef-abcde" =
  Dem.get_distance ~k:1 "abcdef" "abcde" = Some 1

let%test "current distance should be zero abcdef-abcde" =
  let nfa = Dem.start ~k:2 ~str:"abcdef" in
  Dem.(feed_str ~str:"abcde" nfa |> current_error)
  = Some 0

let%test "current distance should be zero abcdef-abcde" =
  let nfa = Dem.start ~k:2 ~str:"abcdef" in
  Dem.(feed_str ~str:"abcde" nfa |> end_input)
  = Some 1

let%test "current distance should be zero abcdef-ab" =
  let open Dem in
  let nfa = Dem.start ~k:2 ~str:"abcdef" in
  (feed_str ~str:"ab" nfa |> current_error)
  = Some 0

let%test "current distance should be one abcdef-ba" =
  let open Dem in
  let nfa = Dem.start ~k:2 ~str:"abcdef" in
  (feed_str ~str:"ba" nfa |> current_error)
  = Some 1

let%test "short input abcdef-ab" =
  let nfa = Dem.start ~k:2 ~str:"abcdef" in
  Dem.(feed_str ~str:"ab" nfa |> end_input)
  = None

let%test "current distance should be one abcdef-abd" =
  let nfa = Dem.start ~k:2 ~str:"abcdef" in
  Dem.(feed_str ~str:"abd" nfa |> current_error)
  = Some 1

let%test "current distance should be one abcdef-abdc" =
  let nfa = Dem.start ~k:2 ~str:"abcdef" in
  Dem.(feed_str ~str:"abdc" nfa |> current_error)
  = Some 1

let%test "no triangle inequality" =
  Dem.get_distance ~k:4 "abcd" "abdc"
  = Some 1
  &&
  Dem.get_distance ~k:4 "abdc" "bdac"
  = Some 2
  &&
  Dem.get_distance ~k:4 "abcd" "bdac"
  = Some 4

(* empty *)
let%test "empty-empty" =
  Dem.get_distance ~k:1 "" "" = Some 0

let%test "empty-a" =
  Dem.get_distance ~k:1 "" "a" = Some 1

let%test "a-empty" =
  Dem.get_distance ~k:1 "a" "" = Some 1

let%test "ab-empty" =
  Dem.get_distance ~k:1 "ab" "" = None

let%test "empty-ab" =
  Dem.get_distance ~k:1 "" "ab" = None

(* k = 0 *)
let%test "k=0 abc-ab" =
  Dem.get_distance ~k:0 "abc" "ab" = None

let%test "k=0 abc-abc" =
  Dem.get_distance ~k:0 "abc" "abc" = Some 0

let%test "k=0 empty-empty" =
  Lev.get_distance ~k:0 "" "" = Some 0

let%test "k=0 empty-a" =
  Lev.get_distance ~k:0 "" "a" = None

let%test "k=0 a-empty" =
  Lev.get_distance ~k:0 "a" "" = None

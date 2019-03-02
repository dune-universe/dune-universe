open Core_kernel

type t =
  | Same of Sexp.t
  | Add of Sexp.t
  | Delete of Sexp.t
  | Replace of Sexp.t * Sexp.t
  | Enclose of t list
[@@deriving sexp, hash, compare]

let print_for_test t = print_s (sexp_of_t t)

let rec invert = function
  | Same x -> Same x
  | Add x -> Delete x
  | Delete x -> Add x
  | Replace (a, b) -> Replace (b, a)
  | Enclose xs -> Enclose (List.map xs ~f:invert)
;;

let require_equal ~expected ~found =
  if not (Sexp.equal expected found)
  then
    Or_error.error_s
      [%message
        "Can't apply the diff because the sexp is not equal to the original"
          (expected : Sexp.t)
          (found : Sexp.t)]
  else Ok ()
;;

let found_nothing ~expected =
  Or_error.error_s
    [%message
      "Can't apply the diff because the sexp ended unexpectedly" (expected : Sexp.t)]
;;

let expected_nothing ~found =
  Or_error.error_s
    [%message "Can't apply the diff: expected nothing" (found : Sexp.t list)]
;;

let unexpected_atom ~atom =
  Or_error.error_s
    [%message "Can't apply the diff: expected a list, found an atom" (atom : string)]
;;

let rec apply_list ~(diffs : t list) ~(sexps : Sexp.t list) : Sexp.t list Or_error.t =
  let open Or_error.Let_syntax in
  match diffs, sexps with
  | Same expected :: _, []
  | Delete expected :: _, []
  | Replace (expected, _) :: _, [] -> found_nothing ~expected
  | Enclose _ :: _, [] ->
    Or_error.error_string "Can't apply the diff: expected a list, found nothing"
  | Same expected :: diffs, found :: sexps ->
    let%bind () = require_equal ~expected ~found in
    let%map rest = apply_list ~diffs ~sexps in
    expected :: rest
  | Add x :: diffs, sexps ->
    let%map rest = apply_list ~diffs ~sexps in
    x :: rest
  | Delete expected :: diffs, found :: sexps ->
    let%bind () = require_equal ~expected ~found in
    apply_list ~diffs ~sexps
  | Replace (expected, new_sexp) :: diffs, found :: sexps ->
    let%bind () = require_equal ~expected ~found in
    let%map rest = apply_list ~diffs ~sexps in
    new_sexp :: rest
  | Enclose _ :: _, Sexp.Atom atom :: _ -> unexpected_atom ~atom
  | Enclose xs :: diffs, Sexp.List ys :: sexps ->
    let%bind first = apply_list ~diffs:xs ~sexps:ys in
    let%map rest = apply_list ~diffs ~sexps in
    Sexp.List first :: rest
  | [], [] -> Ok []
  | [], found -> expected_nothing ~found
;;

let apply diff sexp =
  let result = apply_list ~diffs:[ diff ] ~sexps:[ sexp ] in
  Or_error.map result ~f:(function
    | [ x ] -> x
    | _ ->
      raise_s
        [%message
          "internal error in [Sexp_diff] while applying diff" (diff : t) (sexp : Sexp.t)])
;;

let apply_exn diff sexp = apply diff sexp |> Or_error.ok_exn

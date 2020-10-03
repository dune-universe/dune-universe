open Core
module Incr = Incremental.Make ()
open Incr.Let_syntax

(* %pattern_bind allows arbitrary and nested patterns *)

[@@@ocaml.warning "-37"]

module M = struct
  type t =
    | A
    | B of int
end

type t =
  | Any of int
  | Var of int
  | Alias of int * bool
  | Constant of bool
  | Interval of char
  | Tuple of (int * bool)
  | Multi_arg_tuple of int * bool
  | Constructor of t * t
  | Variant of [ `A | `B of bool ]
  | Record of { i : int }
  | Array of int array
  | Or_a of int * bool
  | Or_b of bool * int
  | Constraint of int
  | Lazy of int lazy_t
  | Open of M.t

let%test "recursive as-pattern" =
  let (_ : unit Incr.t) =
    match%pattern_bind Incr.const () with
    | () as x as y ->
      let (_ : unit Incr.t) = x
      and (_ : unit Incr.t) = y in
      return ()
  in
  true
;;

let%test "patterns" =
  let node =
    match%pattern_bind return (Constructor (Record { i = 10 }, Interval 'a')) with
    | Any _ -> return 0
    | Var i -> i
    | Alias (_, _) as a ->
      (match%map a with
       | Alias (i, _) -> i
       | _ -> assert false)
    | Constant true -> return 1
    | Constant false -> return 0
    | Interval 'a' .. 'f' -> return 2
    | Interval _ -> return 3
    | Tuple y ->
      let%map a, _ = y in
      a
    | Multi_arg_tuple (a, _) -> a
    | Constructor (Multi_arg_tuple (i, _), Var j) ->
      let%map i = i
      and j = j in
      i + j
    | Constructor (_, _) -> return 10
    | Variant `A -> return 4
    | Variant (`B true) -> return 5
    | Variant y ->
      (match%map y with
       | `A -> 6
       | `B _ -> 7)
    | Record { i = 10 } -> return 10
    | Record { i = j } -> j
    | Array [| x |] -> x
    | Array _ -> return 8
    | Or_a (i, _) | Or_b (_, i) -> i
    | Constraint (i : int) -> i
    | Lazy i ->
      let%map i = i in
      force i
    | Open M.(A) -> return 9
    | Open M.(B x) -> x
  in
  let observer = Incr.observe node in
  Incr.stabilize ();
  match Incr.Observer.value_exn observer with
  | 10 -> true
  | _ -> false
;;

(* Polymorphic variants *)
let (_ : bool Incr.t) =
  let open Incr.Let_syntax in
  match%pattern_map return (`A (10, true, false)) with
  | `A (10, x, y) -> x && y
  | `A (_, x, _) -> x
  | `B _ -> true
;;

type onetwo =
  [ `One
  | `Two
  ]

type three =
  [ `One
  | `Two
  | `Three
  ]

let%test "type patterns" =
  let node =
    match%pattern_bind return `Three with
    | #onetwo -> return false
    | #three -> return true
  in
  let observer = Incr.observe node in
  Incr.stabilize ();
  Incr.Observer.value_exn observer
;;

let%test "map" =
  let node =
    let%pattern_map x, y, _ = return (10, 1, true) in
    x + y + 1 = 12
  in
  let observer = Incr.observe node in
  Incr.stabilize ();
  Incr.Observer.value_exn observer
;;

let%test "map multiple" =
  let node =
    let%pattern_map x, _ = return (10, true)
    and _, y = return (11, false) in
    if y then x + 1 else 0
  in
  let observer = Incr.observe node in
  Incr.stabilize ();
  Incr.Observer.value_exn observer = 0
;;

let%test "map none" =
  let node =
    let%pattern_map _ = return (10, true) in
    10
  in
  let observer = Incr.observe node in
  Incr.stabilize ();
  Incr.Observer.value_exn observer = 10
;;

let%expect_test "effects" =
  let node =
    let%pattern_map a, b, c, d, () = return (10, 10, 10, 10, printf "E") in
    a + b + c + d
  in
  let observer = Incr.observe node in
  Incr.stabilize ();
  Incr.Observer.disallow_future_use observer;
  [%expect {| E |}]
;;

let stabilize f =
  (* normalize the output to avoid depending on the specific evaluation
     order chosen by incremental *)
  Incr.stabilize ();
  f ()
  |> String.split_lines
  |> List.sort ~compare:String.compare
  |> String.concat ~sep:""
  |> print_string
;;

(* match%pattern_bind should not refire unless we change cases *)

type two =
  | X of int * bool
  | Y of bool

let%expect_test _ =
  let incr_v, incr =
    let pair = Incr.Var.create (X (0, true)) in
    pair, Incr.Var.watch pair
  in
  let pattern_node =
    match%pattern_bind incr with
    | X (_, y) ->
      printf "P1\n";
      let%map y = y in
      printf "P2\n";
      y
    | Y b -> b
  in
  let normal_node =
    match%bind incr with
    | X (_, y) ->
      printf "N\n";
      return y
    | Y b -> return b
  in
  let match_pattern = Incr.observe pattern_node in
  let match_normal = Incr.observe normal_node in
  let set x =
    Incr.Var.set incr_v x;
    stabilize (fun () -> [%expect.output])
  in
  stabilize (fun () -> [%expect.output]);
  [%expect {| NP1P2 |}];
  set (X (42, true));
  [%expect {| N |}];
  set (X (24, false));
  [%expect {| NP2 |}];
  set (Y false);
  [%expect {| |}];
  set (X (12, false));
  [%expect {| NP1P2 |}];
  set (X (12, Sys.opaque_identity false));
  [%expect {| N |}];
  Incr.Observer.disallow_future_use match_pattern;
  Incr.Observer.disallow_future_use match_normal
;;

(* let%pattern_bind should not refire unless the values of the
   variables in the pattern change *)

let%expect_test _ =
  let incr_v, incr =
    let pair = Incr.Var.create (0, true, 'a') in
    pair, Incr.Var.watch pair
  in
  let pattern_node =
    let%pattern_bind _, y, _z = incr in
    printf "P1\n";
    let%map y = y in
    printf "P2\n";
    not y
  in
  let normal_node =
    let%map _, y, _ = incr in
    printf "N\n";
    not y
  in
  let let_pattern = Incr.observe pattern_node in
  let let_normal = Incr.observe normal_node in
  let set x =
    Incr.Var.set incr_v x;
    stabilize (fun () -> [%expect.output])
  in
  stabilize (fun () -> [%expect.output]);
  [%expect {| NP1P2 |}];
  set (42, true, 'a');
  [%expect {| N |}];
  set (42, false, 'a');
  [%expect {| NP2 |}];
  set (42, false, 'b');
  [%expect {| N |}];
  set (42, false, Sys.opaque_identity 'b');
  [%expect {| N |}];
  Incr.Observer.disallow_future_use let_pattern;
  Incr.Observer.disallow_future_use let_normal
;;

(* let%pattern_map should not refire unless the values of the
   variables in the pattern change *)

let%expect_test _ =
  let incr_v, incr =
    let pair = Incr.Var.create (0, true) in
    pair, Incr.Var.watch pair
  in
  let pattern_node =
    let%pattern_map _, y = incr in
    printf "P\n";
    not y
  in
  let normal_node =
    let%map _, y = incr in
    printf "N\n";
    not y
  in
  let map_pattern = Incr.observe pattern_node in
  let map_normal = Incr.observe normal_node in
  let set x =
    Incr.Var.set incr_v x;
    stabilize (fun () -> [%expect.output])
  in
  stabilize (fun () -> [%expect.output]);
  [%expect {| NP |}];
  set (42, true);
  [%expect {| N |}];
  set (42, false);
  [%expect {| NP |}];
  set (42, Sys.opaque_identity false);
  [%expect {| N |}];
  Incr.Observer.disallow_future_use map_pattern;
  Incr.Observer.disallow_future_use map_normal
;;

let%expect_test "ppx should work with the defunctorized incremental" =
  let return = Incremental.return in
  let open Incremental.Let_syntax in
  ignore
    (let%pattern_bind a, b = return Incr.State.t (1, 2) in
     let%map a = a
     and b = b in
     a + b
     : int Incr.t)
;;

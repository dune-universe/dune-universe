(* Evaluating an arithmetic expression incrementally. *)

module String_map = Map.Make (String)

type expr = Var of string | Add of expr * expr | Neg of expr

let env l = String_map.of_seq (List.to_seq l)

let expr = Add (Add (Var "a", Var "b"), Neg (Add (Var "c", Var "d")))

let ( + ) x y =
  Format.printf "%d + %d@." x y ;
  x + y

let ( - ) x =
  Format.printf "- %d@." x ;
  -x

module Non_incremental = struct
  let () = Format.printf "Non incremental computation@."

  let rec eval (env : int String_map.t) expr =
    match expr with
    | Var s -> String_map.find s env
    | Add (l, r) -> eval env l + eval env r
    | Neg e -> -eval env e

  let () = Format.printf "First evaluation@."

  let () =
    assert (eval (env [("a", 1); ("b", 2); ("c", 3); ("d", 4)]) expr = -4)

  let () = Format.printf "Second evaluation@."

  let () =
    assert (eval (env [("a", 3); ("b", 2); ("c", 3); ("d", 4)]) expr = -2)
end

module Incremental = struct
  let () = Format.printf "Incremental computation@."

  open Cgraph

  let rec eval (env : int t String_map.t) expr =
    match expr with
    | Var s -> String_map.find s env
    | Add (l, r) -> map2 (eval env l) (eval env r) ( + )
    | Neg e -> map (eval env e) ( ~- )

  let (a, b, c, d) = (Var.create 1, Var.create 2, Var.create 3, Var.create 4)

  let graph =
    eval (env [("a", var a); ("b", var b); ("c", var c); ("d", var d)]) expr

  let () = Format.printf "First evaluation@."

  let () = assert (get graph = -4)

  let () = Var.set a 3

  let () = Format.printf "Second evaluation@."

  let () = assert (get graph = -2)
end

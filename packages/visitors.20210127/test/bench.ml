module Bench = Core_bench.Bench
module Command = Core.Command

let run tests =
  let tests =
    List.map (fun (name, test) -> Bench.Test.create ~name test) tests
  in
  Command.run (Bench.make_command tests)

type expr =
  | EConst of (int[@opaque])
  | EAdd of expr * expr
  | EList of expr list
  [@@deriving visitors { variety = "iter"; concrete = true },
              visitors { variety = "map"; concrete = true },
              visitors { variety = "reduce"; ancestors = ["VisitorsRuntime.addition_monoid"]; concrete = true }]

let iter : expr -> unit =
  new iter # visit_expr ()

let rec native_iter env e =
  match e with
  | EConst _ ->
      ()
  | EAdd (e1, e2) ->
      native_iter env e1;
      native_iter env e2
  | EList es ->
      List.iter (native_iter env) es

class size = object
  inherit [_] reduce as super
  method! visit_expr () e =
    1 + super # visit_expr () e
end

let size : expr -> int =
  new size # visit_expr ()

let rec native_size env e =
  match e with
  | EConst _ ->
      1
  | EAdd (e1, e2) ->
      native_size env e1 +
      native_size env e2
  | EList es ->
      List.fold_left (fun accu e -> accu + native_size env e) 0 es

let rec native_size_noenv e =
  match e with
  | EConst _ ->
      1
  | EAdd (e1, e2) ->
      native_size_noenv e1 +
      native_size_noenv e2
  | EList es ->
      List.fold_left (fun accu e -> accu + native_size_noenv e) 0 es

let rec native_size_noenv_accu accu e =
  match e with
  | EConst _ ->
      accu + 1
  | EAdd (e1, e2) ->
      let accu = native_size_noenv_accu accu e1 in
      native_size_noenv_accu accu e2
  | EList es ->
      List.fold_left native_size_noenv_accu accu es

let split n =
  assert (n >= 0);
  let n1 = Random.int (n + 1) in
  let n2 = n - n1 in
  assert (0 <= n1 && n1 <= n);
  assert (0 <= n2 && n2 <= n);
  n1, n2

let rec generate n =
  assert (n >= 0);
  if n = 0 then
    EConst (Random.int 100)
  else
    match Random.int 2 with
    | 0 ->
        let n1, n2 = split (n - 1) in
        EAdd (generate n1, generate n2)
    | 1 ->
        let n1, n2 = split (n - 1) in
        EList [ generate n1; generate n2 ]
    | _ ->
        assert false

let rec list_init i n f =
  if i = n then
    []
  else
    let x = f i in
    x :: list_init (i + 1) n f

let list_init n f =
  list_init 0 n f

let samples =
  list_init 100 (fun _ -> generate 100)

let test f () =
  List.iter (fun e -> ignore (f e)) samples

let () =
  run [
    "iter", test iter;
    "native_iter", test (native_iter ());
    ]

let () =
  run [
    "size", test size;
    "native_size", test (native_size ());
(*
    "native_size_noenv", test native_size_noenv;
    "native_size_noenv_accu", test (native_size_noenv_accu 0);
 *)
  ]

(* with hoisting, applied to self#visit_expr:
   iter allocates no memory, and is 46% slower than native_iter (which allocates).
   without hoisting:
   iter allocates memory and is 65% slower than native_iter
   So, in this case, hoisting helps a little.

   with hoisting, applied to self#visit_expr:
   size allocates no memory, and is 105% slower than native_size (which allocates).
   without hoisting:
   size allocates memory (same amount as native_size) and is 86% slower than native_size
   So, in this case, hoisting seems counter-productive.
 *)

open Client

(* -------------------------------------------------------------------------- *)

(* A random generator of pure lambda-terms. *)

let int2var k =
  "x" ^ string_of_int k

(* [split n] produces two numbers [n1] and [n2] comprised between [0] and [n]
   (inclusive) whose sum is [n]. *)

let split n =
  let n1 = Random.int (n + 1) in
  let n2 = n - n1 in
  n1, n2

(* The parameter [k] is the number of free variables; the parameter [n] is the
   size (i.e., the number of internal nodes). *)

let rec random_ml_term k n =
  if n = 0 then begin
    assert (k > 0);
    ML.Var (int2var (Random.int k))
  end
  else
    let c = Random.int 5 (* Abs, App, Pair, Let, LetProd *) in
    if k = 0 || c = 0 then
      (* The next available variable is [k]. *)
      let x, k = int2var k, k + 1 in
      ML.Abs (x, random_ml_term k (n - 1))
    else if c = 1 then
      let n1, n2 = split (n - 1) in
      ML.App (random_ml_term k n1, random_ml_term k n2)
    else if c = 2 then
      let n1, n2 = split (n - 1) in
      ML.Tuple [random_ml_term k n1; random_ml_term k n2]
    else if c = 3 then
      let n1, n2 = split (n - 1) in
      ML.Let (int2var k, random_ml_term k n1, random_ml_term (k + 1) n2)
    else if c = 4 then
      let n1, n2 = split (n - 1) in
      let x1, x2, k' = int2var k, int2var (k + 1), k + 2 in
      ML.LetProd ([x1; x2], random_ml_term k n1, random_ml_term k' n2)
    else
      assert false

let rec size accu = function
  | ML.Var _ ->
      accu
  | ML.Abs (_, t)
    -> size (accu + 1) t
  | ML.App (t1, t2)
  | ML.Let (_, t1, t2)
  | ML.LetProd (_, t1, t2)
    -> size (size (accu + 1) t1) t2
  | ML.Tuple ts
    -> List.fold_left size (accu + 1) ts

let size =
  size 0

(* -------------------------------------------------------------------------- *)

(* Random testing. *)

(* A list of pairs [m, n], where [m] is the number of tests and [n] is the
   size of the randomly generated terms. *)

let pairs =
  match Sys.getenv_opt "INFERNO_SLOW_TESTS" with
  | Some ("0" | "false" | "") | None -> [
      10_000, 10;
      10_000, 15;
    ]
  | Some _ -> [
      100_000, 5;
      100_000, 10;
      100_000, 15;
      100_000, 20;
      100_000, 25; (* at this size, about 1% of the terms are well-typed *)
      100_000, 30;
      (* At the following sizes, no terms are well-typed! *)
      10_000, 100;
      10_000, 500;
      1000, 1000;
      100, 10_000;
      10, 100_000;
      1, 1_000_000;
    ]

let () =
  Printf.printf "Preparing to type-check a bunch of randomly-generated terms...\n%!";
  Random.init 0;
  let c = ref 0 in
  let d = ref 0 in
  List.iter (fun (m, n) ->
    for i = 1 to m do
      if Test.Config.verbose then
        Printf.printf "Test number %d...\n%!" i;
      let t = random_ml_term 0 n in
      assert (size t = n);
      let success = Test.(Log.with_log CheckML.test t) in
      if success then incr c;
      incr d
    done
  ) pairs;
  Printf.printf "In total, %d out of %d terms were considered well-typed.\n%!" !c !d;
  Printf.printf "No problem detected.\n%!"

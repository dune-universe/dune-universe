let verbose =
  false

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
    let c = Random.int 5 (* Abs, App, Pair, Proj, Let *) in
    if k = 0 || c = 0 then
      (* The next available variable is [k]. *)
      let x, k = int2var k, k + 1 in
      ML.Abs (x, random_ml_term k (n - 1))
    else if c = 1 then
      let n1, n2 = split (n - 1) in
      ML.App (random_ml_term k n1, random_ml_term k n2)
    else if c = 2 then
      let n1, n2 = split (n - 1) in
      ML.Pair (random_ml_term k n1, random_ml_term k n2)
    else if c = 3 then
      ML.Proj (1 + Random.int 2, random_ml_term k (n - 1))
    else if c = 4 then
      let n1, n2 = split (n - 1) in
      ML.Let (int2var k, random_ml_term k n1, random_ml_term (k + 1) n2)
    else
      assert false

let rec size accu = function
  | ML.Var _ ->
      accu
  | ML.Abs (_, t)
  | ML.Proj (_, t)
    -> size (accu + 1) t
  | ML.App (t1, t2)
  | ML.Let (_, t1, t2)
  | ML.Pair (t1, t2)
    -> size (size (accu + 1) t1) t2

let size =
  size 0

(* -------------------------------------------------------------------------- *)

(* Facilities for dealing with exceptions. *)

(* A log is a mutable list of actions that produce output, stored in reverse
   order. It is used to suppress the printing of progress messages as long as
   everything goes well. If a problem occurs, then the printing actions are
   executed. *)

type log = {
  mutable actions: (unit -> unit) list
}

let create_log () =
  { actions = [] }

let log_action log action =
  log.actions <- action :: log.actions

let log_msg log msg =
  log_action log (fun () ->
    output_string stdout msg
  )

let print_log log =
  List.iter (fun action ->
    action();
    (* Flush after every action, as the action itself could raise an
       exception, and we will want to know which one failed. *)
    flush stdout
  ) (List.rev log.actions)

let attempt log msg f x =
  log_msg log msg;
  try
    f x
  with e ->
    print_log log;
    Printf.printf "%s\n" (Printexc.to_string e);
    Printexc.print_backtrace stdout;
    flush stdout;
    exit 1

(* -------------------------------------------------------------------------- *)

(* A wrapper over the client's [translate] function. We consider ill-typedness
   as normal, since our terms are randomly generated, so we translate the client
   exceptions to [None]. *)

let print_type ty =
  PPrint.(ToChannel.pretty 0.9 80 stdout (FPrinter.print_type ty ^^ hardline))

let translate t =
  try
    Some (Client.translate t)
  with
  | Client.Cycle ty ->
      if verbose then begin
        Printf.fprintf stdout "Type error: a cyclic type arose.\n";
        print_type ty
      end;
      None
  | Client.Unify (ty1, ty2) ->
      if verbose then begin
        Printf.fprintf stdout "Type error: type mismatch.\n";
        Printf.fprintf stdout "Type error: mismatch between the type:\n";
        print_type ty1;
        Printf.fprintf stdout "and the type:\n";
        print_type ty2
      end;
      None

(* -------------------------------------------------------------------------- *)

(* Running all passes over a single ML term. *)

let test (t : ML.term) : bool =
  let log = create_log() in
  let outcome =
    attempt log
      "Type inference and translation to System F...\n"
      translate t
  in
  match outcome with
  | None ->
      (* This term is ill-typed. This is considered a normal outcome, since
         our terms can be randomly generated. *)
      false
  | Some (t : F.nominal_term) ->
      log_action log (fun () ->
        Printf.printf "Formatting the System F term...\n%!";
        let doc = PPrint.(FPrinter.print_term t ^^ hardline) in
        Printf.printf "Pretty-printing the System F term...\n%!";
        PPrint.ToChannel.pretty 0.9 80 stdout doc
      );
      let t : F.debruijn_term =
        attempt log
          "Converting the System F term to de Bruijn style...\n"
          F.translate t
      in
      let _ty : F.debruijn_type =
        attempt log
          "Type-checking the System F term...\n"
          FTypeChecker.typeof t
      in
      (* Everything seems to be OK. *)
      if verbose then
        print_log log;
      true

(* -------------------------------------------------------------------------- *)

(* A few manually constructed terms. *)

let x =
  ML.Var "x"

let y =
  ML.Var "y"

let id =
  ML.Abs ("x", x)

let delta =
  ML.Abs ("x", ML.App (x, x))

let deltadelta =
  ML.App (delta, delta)

let idid =
  ML.App (id, id)

let k =
  ML.Abs ("x", ML.Abs ("y", x))

let genid =
  ML.Let ("x", id, x)

let genidid =
  ML.Let ("x", id, ML.App (x, x))

let genkidid =
  ML.Let ("x", ML.App (k, id), ML.App (x, id))

let genkidid2 =
  ML.Let ("x", ML.App (ML.App (k, id), id), x)

let app_pair = (* ill-typed *)
  ML.App (ML.Pair (id, id), id)

let () =
  assert (test idid);
  assert (test genid);
  assert (test genidid);
  assert (test genkidid);
  assert (test genkidid2)

(* -------------------------------------------------------------------------- *)

(* Random testing. *)

(* A list of pairs [m, n], where [m] is the number of tests and [n] is the
   size of the randomly generated terms. *)

let pairs = [
  100000, 5;
  100000, 10;
  100000, 15;
  100000, 20;
  100000, 25; (* at this size, about 1% of the terms are well-typed *)
  100000, 30;
  (* At the following sizes, no terms are well-typed! *)
   10000, 100;
   10000, 500;
    1000, 1000;
     100, 10000;
      10, 100000;
       1, 1000000;
]

let () =
  Printf.printf "Preparing to type-check a bunch of randomly-generated terms...\n%!";
  Random.init 0;
  let c = ref 0 in
  let d = ref 0 in
  List.iter (fun (m, n) ->
    for i = 1 to m do
      if verbose then
        Printf.printf "Test number %d...\n%!" i;
      let t = random_ml_term 0 n in
      assert (size t = n);
      let success = test t in
      if success then incr c;
      incr d
    done
  ) pairs;
  Printf.printf "In total, %d out of %d terms were considered well-typed.\n%!" !c !d;
  Printf.printf "No problem detected.\n%!"

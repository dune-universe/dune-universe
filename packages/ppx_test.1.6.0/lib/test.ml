module Misc = struct
  (* As usual, OCaml stdlib is poor and needed to extend *)

  external (&) : ('a -> 'b) -> 'a -> 'b = "%apply"
  (** (@@) but one less char *)

  let (!!%) = Format.eprintf

  let (+::=) r x = r := x :: !r
end

open Misc

(** test type *)

module Primitives = struct
  (* OUnit style test primitives *)
  type t =
    | Test     of (unit -> bool)
    | Labeled  of Longident.t * t
    | List     of t list
    | Location of Location.t * t
  
  let fun_ f = Test f
  let ident lid t = Labeled (lid, t)
  let label s t = Labeled (Lident s, t)
  let list ts = List ts
end

include Primitives

module Name = struct
  (* { label= None; location= None } is easy to be duped, but we do not care *)
  type t = {
    label : Longident.t option;
    location : Location.t option;
  }

  let null = { label = None; location = None }
    
  let add_label l t =
    match t.label with
    | None -> { t with label = Some l }
    | Some pl -> { t with label = Some (Longident.concat pl l) }

  let set_location l st = { st with location = Some l }

  let to_string n = match n.label with
    | None -> "_"
    | Some l -> Longident.to_string l

  let location n = match n.location with
    | None -> assert false
    | Some l -> l

end

module Config = struct
  type arg =
    | Do   of Re.re
    | Dont of Re.re

  type t = {
    args       : arg list;
    show_tests : bool;
    verbose    : bool;
    default_go : bool;
    print_failure_immediately : bool;
    stop_at_nth : int;
  }

  let rev_args = ref []
  let show_tests = ref false
  let test_verbose = ref false
  let print_failure_immediately = ref true
  let stop_at_nth = ref 0 (* never *)
    
  let from_args default_go = 
    { args = List.rev !rev_args;
      show_tests = !show_tests;
      verbose = !test_verbose;
      default_go = default_go;
      print_failure_immediately = !print_failure_immediately;
      stop_at_nth = !stop_at_nth
    }

  let is_go conf n =
    let s = Name.to_string n in
    List.fold_left (fun st -> function
      | Do rex when Re_pcre.pmatch ~rex s -> true
      | Dont rex when Re_pcre.pmatch ~rex s -> false
      | _ -> st) conf.default_go conf.args
end
  
open Config

let arg_specs = 
  let open Arg in
  let open Config in      
  [ "--test-go",   String (fun s -> rev_args +::= Do   (Re_pcre.regexp s)), "<string>: Perform tests match with the string";
    "--test-skip", String (fun s -> rev_args +::= Dont (Re_pcre.regexp s)), "<string>: Skip tests match with the string";
    "--test-verbose", Set test_verbose, ": Print out what is being tested.";
    "--test-show", Set show_tests, ": List all the tests. Do not perform any test.";
    "--test-print-immediately", Set print_failure_immediately, ": Show test errors immediately";
    "--test-max-failures", Int (fun i -> stop_at_nth := i), ": Stop at n-th test failure. 0 means never.";
  ]

module Error = struct
  type t =
    [ `Exn of exn * Printexc.raw_backtrace
    | `False
    ]

  open Format
      
  let format fmt = function
    | `Exn (exn, bt) ->
        fprintf fmt "@[<v>Exn: %s@ Backtrace: %s@]"
          (Printexc.to_string exn)
          (Printexc.raw_backtrace_to_string bt)
    | `False ->
        fprintf fmt "false"
end

module Result = struct
  type t = {
    time : float; (* in sec *)
    result : [ `Ok of unit | `Error of Error.t ]
  }
end
  
let rec fold st n f = function
  | Test t -> f st n t
  | Labeled (l, t) ->
      let n = Name.add_label l n in
      fold st n f t
  | Location (loc, t) ->
      let n = Name.set_location loc n in
      fold st n f t
  | List ts -> List.fold_left (fun st t -> fold st n f t) st ts

let show conf () n t = 
  let f () n _ =
    !!% "%a: %s: %s@."
      Location.format (Name.location n)
      (Name.to_string n)
      (if Config.is_go conf n then "go" else "skip")
  in
  fold () n f t

let report_one (n, { Result.time=_; result }) = 
  let open Format in
  let name = Name.to_string n in
  match result with
  | `Ok () ->
      !!% "Test succeeded: %s at %a@." name
        (fun ppf -> function
          | None -> fprintf ppf "<no location>"
          | Some l -> Location.format ppf l) n.Name.location;
  | `Error e ->
      !!% "Test failed: %s at %a@."
        name
        (fun ppf -> function
          | None -> fprintf ppf "<no location>"
          | Some l -> Location.format ppf l) n.Name.location;
      !!% "%a@.------@." Error.format e

module Report = struct
  type t = (Name.t * Result.t) list * int (** num of errors *)  

  let print (reslist, errors) =
    !!% "Test finished@.";
    let num_tests = List.length reslist in
    !!% "  Performed: %d@." num_tests;
    !!% "  Succeeded: %d@." (num_tests - errors); 
    !!% "  Failure:   %d@." errors;
    !!% "---@.";
    List.iter (function
      | (_n, { Result.result = `Ok () }) -> ()
      | x -> report_one x) reslist  

  let print_then_exit (_, errors as res) =
    print res;
    exit (if errors <> 0 then -1 else 0)
end

let abort conf (_reslist, errors as st) =
  !!% "Too many test failures %d >= %d@." errors conf.stop_at_nth;
  Report.print st;
  !!% "Too many test failures %d >= %d. Aborted.@." errors conf.stop_at_nth;
  exit (-1)

let run conf st n t = 
  let f (reslist, errors as st) n t = 
    if Config.is_go conf n then begin 
      let name = Name.to_string n in
      if conf.verbose then !!% "Test %s...@." name;
      let res =
        try
          if t () then `Ok ()
          else `Error `False
        with
        | e -> `Error (`Exn (e, Printexc.get_raw_backtrace ()))
      in
      let is_error = match res with `Ok _ -> false | _ -> true in
      if conf.verbose then !!% "Test %s %s@." name (if is_error then "failed" else "done");
      let x = (n, { Result.time = 0.0; result = res }) in
      if is_error && conf.print_failure_immediately then report_one x;
      let errors = match res with `Ok _ -> errors | _ -> errors + 1 in
      let st = ( x :: reslist, errors ) in
      if conf.stop_at_nth > 0 && errors >= conf.stop_at_nth then begin
        abort conf st
      end else st
    end else st
  in
  fold st n f t
(** Global test table *)

let rev_tests = ref []

let add t = rev_tests := t :: !rev_tests

let location loc t = Location (loc, t)

(* TEST_UNIT *)
let test_unit loc l f = rev_tests := (location loc & ident l & fun_ (fun () -> f (); true)) :: !rev_tests

(* TEST *)
let test loc l f =
  rev_tests := (location loc & ident l & fun_ f) :: !rev_tests

(* TEST_FAIL *)
let test_fail loc l f = rev_tests := (location loc & ident l & fun_ (fun () -> try f (); false with _ -> true)) :: !rev_tests

let fold_tests f init =
  let rec loop st = function
    | [] -> st
    | t::ts ->
        let st = f st Name.null t in
        loop st ts
  in
  loop init & List.rev !rev_tests

(* Running tests can add more tests, therefore we must loop things.
   Not sophisticated :-( *)
let run_tests default =
  let conf = Config.from_args default in
  if conf.show_tests then begin 
    fold_tests (show conf) ();
    exit 0
  end else 
    let ress, errors = fold_tests (run conf) ([],0) in
    List.rev ress, errors

let collect () = 
  Arg.parse arg_specs (fun s ->
    !!% "test command does not take anonymous arguments: %s@." s;
    exit 2
  ) "test";
  run_tests true |> Report.print_then_exit

(** {2 Library for test code} *)

module TestTool = struct
  let must_raise f = try ignore & f (); false with _ -> true
end

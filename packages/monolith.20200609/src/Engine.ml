(******************************************************************************)
(*                                                                            *)
(*                                  Monolith                                  *)
(*                                                                            *)
(*                              François Pottier                              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU Lesser General Public License as published by the Free   *)
(*  Software Foundation, either version 3 of the License, or (at your         *)
(*  option) any later version, as described in the file LICENSE.              *)
(*                                                                            *)
(******************************************************************************)

open Printf
open Code
open Type
open Spec
open Builtin

type document =
  PPrint.document

(* -------------------------------------------------------------------------- *)

(* This defines a function [log] which can be used for debugging. *)

include Debug.Make(struct let debug = false end)

(* -------------------------------------------------------------------------- *)

(* [o++]. *)

let[@inline] postincrement o =
  let x = !o in o := x + 1; x

(* -------------------------------------------------------------------------- *)

(* Turn on the recording of backtraces, so an uncaught exception causes a
   backtrace to be printed before the process is aborted. *)

let () =
  Printexc.record_backtrace true

(* -------------------------------------------------------------------------- *)

(* [abort()] aborts the process by sending a SIGABRT signal to itself. We
   seldom need to use it directly, as letting an uncaught exception escape has
   the same effect. We use it only when we wish to abort without displaying a
   backtrace. *)

let abort () =
  flush stdout;
  flush stderr;
  let self = Unix.getpid() in
  Unix.kill self Sys.sigabrt;
  (* This point should be unreachable. *)
  assert false

(* -------------------------------------------------------------------------- *)

(* The engine needs a description of the operations. *)

(* However, the engine doesn't have to be a functor. We expose a hook that
   allows the user to declare the existence of an operation; that's enough. *)

(* [declare name spec reference candidate] declares the existence of an
   operation. Its parameters are the operation's name (used for printing), the
   operation's type and specification (represented by a runtime value of type
   [('r, 'c) rty]), and the implementations of this operation in the reference
   and in the candidate (represented by values of type ['r] and ['c]). *)

let operations : (string * rany) list ref =
  ref []

let declare name rty reference candidate =
  operations := (name, RAny (rty, reference, candidate)) :: !operations

(* Once the user calls [main], declaring new operations is no longer permitted.
   At this point, we convert the list [!operations] to an array. This allows us
   to choose an operation much more efficiently. *)

(* This is also the time when we normalize every specification. For simple
   (non-dependent) specifications, this is done once and only once, but for
   dependent arrows, the normalization of the codomain is unfortunately
   repeated every time the normalized specification is used. *)

let operations : (string * rany) array Lazy.t =
  lazy (Array.map normalize_op (Array.of_list !operations))

let pick () =
  let operations = Lazy.force operations in
  let n = Array.length operations in
  operations.(Gen.int n ())

(* -------------------------------------------------------------------------- *)

(* The interpreter manipulates values of type [any]. *)

type value = any

(* -------------------------------------------------------------------------- *)

(* DSL syntax. *)

(* Variables. *)

type level =
  | Level of int [@@unboxed]
    (* a de Bruijn level: 0 is the most ancient variable *)

type var =
  level

(* An argument of an operation is one of the following: *)

type arg =

  (* A constant (of concrete type). *)
  | Constant of value code

  (* A variable (of abstract type). *)
  | Var of var

  (* A pair of arguments. *)
  | Pair of arg * arg

(* An operation is used in a context. A context is one of the following: *)

type context =

  (* The empty context [_]. *)
  | CEmpty

  (* An application of the hole to an argument [arg], inside a larger context
     [C]. On paper, such a context would be written [C[_ arg]]. *)
  | CAppLArg of arg * context

  (* An application of a constant [c] to the hole, inside a larger context [C].
     On paper, such a context would be written [C[c _]]. *)
  | CAppConstantR of rany code * context

(* An instruction that produces an observable result is annotated with
   an observation. This observation is enriched as the candidate runs
   (first) and the reference runs (second). *)

type observation =
  | NobodyHasRun
  | CandidateHasRun of value
  | Agreement of value
  | Disagreement of document
      (* A string representation of an OCaml instruction that explains the
         problem. A typical example is
         "assert (observed = 0);; (* candidate finds 1 *)". *)

(* A pattern indicates what to do with the result of an operation. If it has a
   concrete base type, we want to observe it. If it has an abstract type, we
   want to bind a new variable to it, so as to be able to use it in future
   instructions. If it has a pair type, we want to deconstruct it. If it has
   unit type, we could use [PatObserve], but making a special case [PatUnit]
   is more efficient and produces more readable printed code. *)

type ovar =
  int (* a number that must unique within a pattern *)

type pat =
  | PatBind of var * xspec
  | PatObserve of ovar * xspec * observation ref
  | PatUnit
  | PatPair of pat * pat

(* An instruction is an application of an operation to a list of arguments.
   It is possibly followed with an observation, that is, instructions that
   records an observable result or compare an observable result with a
   previously recorded result. *)

(* It takes (roughly) the following form:

      let pat = context[v];;
      assert (observed = ...);;
        (* where [observed] is an [ovar] bound by [pat] *)

 *)

(* In an instruction [LetOp (pat, name, v, context)],

   - [name] is the name of the operation. It is used when printing.

   - [v] is the operation itself. It is a triple [RAny (spec, reference,
     candidate)], where [spec] is a non-dependent specification for the
     operation (if this operation came with a dependent specification,
     this specification has been specialized for this particular context
     and therefore is no longer dependent). [reference] and [candidate]
     are the two implementations of this operation.

     Because [spec] is non-dependent, its projections [left spec] and
     [right spec] are well-defined. They represent the types of the
     values [reference] and [candidate].

   - [context] is the context in which this operation is used; e.g.,
     the operation is typically applied to a number of arguments.

   - the pattern [pat] indicates what should be done with the result.

 *)

type instruction =
  | LetOp of pat * string * rany * context

(* -------------------------------------------------------------------------- *)

(* Handling environments and variables. *)

(* For efficiency, an environment is represented as an array, indexed by de
   Bruijn levels. Environment lookup and environment extension are both
   constant-time operations. A downside of this approach is that creating a
   new (empty) environment requires supplying both a bound on the size of the
   environment and a dummy element. *)

(* For efficiency, we keep at hand an integer array whose size is the maximum
   size of the environment. This array is used as auxiliary storage in the
   implementation of the operation [var], which chooses an element of the
   environment that satisfies a predicate [p]. *)

(* We maintain the invariant that every value in the environment has an
   abstract base type. Indeed, a value of a concrete type is observed as soon
   as it is produced, and does not need to be bound to a variable. *)

module E = struct

  type 'value env =
    { data : 'value array; mutable n : int; storage: int array }

  let empty (bound : int) (dummy : 'value) =
    { data = Array.make bound dummy; n = 0; storage = Array.make bound 0 }

  let clear env =
    env.n <- 0

  let length { n; _ } =
    n

  let limit env : var =
    Level (length env)

  let succ (Level i) =
    Level (i + 1)

  let lookup { data; _ } (Level x) =
    Array.get data x

  let olookup env x =
    try
      Some (lookup env x)
    with Invalid_argument _ -> (* raised by [Array.get] *)
      None

  let bind ({ data; n; _ } as env) v =
    if n = Array.length data then
      failwith (sprintf "Argh -- reached max environment size (%d)" n)
    else begin
      Array.set data n v;
      env.n <- n + 1
    end

  let foreach3 env1 env2 env3 f =
    assert (env1.n = env2.n && env2.n = env3.n);
    let data1, data2, data3 = env1.data, env2.data, env3.data in
    for x = 0 to env1.n - 1 do
      f (Level x) data1.(x) data2.(x) data3.(x)
    done

  let var { data; n; storage } (p : 'value -> bool) : level =
    (* Construct an auxiliary array of the indices of the values that
       satisfy [p]. This information is stored in the array [storage],
       so we save the cost of allocating an initializing an array. *)
    let k = ref 0 in
    for i = 0 to n - 1 do
      if p data.(i) then
        storage.(postincrement k) <- i
    done;
    (* Pick an index among our [k] candidates. *)
    Level (storage.(Gen.int !k ()))

end (* E *)

(* A runtime environment contains values. *)

type env =
  value E.env

(* A type environment contains (relational) types. *)

type gamma =
  xspec E.env

(* This exception is used when an actual value does not match an expected
   value. *)

exception ObservationFailure

(* -------------------------------------------------------------------------- *)

(* Printing instructions. *)

(* The type [cause] indicates what kind of failure occurred, and is used to
   restrict what is printed. *)

type cause =

  (* Failure by the candidate in an instruction. *)
  | Instruction

  (* Failure by the candidate to produce a correct observable result. *)
  | Observation

  (* Failure in a well-formedness check. *)
  | Check of var

  (* Unexpected failure of the reference implementation. *)
  | Unexpected

module P = struct

  open Print

  (* [print_var x oxspec] prints the variable [x], whose type must be an
     abstract base type. *)

  let print_var x oxspec =
    match oxspec with
    | None ->
        (* It may happen that [x] is not bound in [gamma]; in that case, it is
           printed as [_]. This can happen if [x] denotes an output of the last
           instruction and this instruction has failed, so we do not have an
           extended [gamma] where [x] is defined. *)
        underscore
    | Some xspec ->
        (* Use a type-specific prefix, followed with the numeric level. *)
        match x with Level x ->
          utf8format "%s%d" (base xspec) x

  let print_ovar x =
    (* We could choose to name the variable [x] after its type [xspec], if we
       associated a [base] name with concrete base types. At the moment, we do
       not. *)
    if x = 0 then utf8format "observed"
    else utf8format "observed_%d" x

  let rec print_arg gamma a =
    match a with
    | Constant v ->
        Code.print v
    | Var x ->
        print_var x (E.olookup gamma x)
    | Pair (arg1, arg2) ->
        OCaml.tuple [ print_arg gamma arg1; print_arg gamma arg2 ]

  let rec print_fill_context gamma context body =
    match context with
    | CEmpty ->
        body
    | CAppLArg (arg, context) ->
        let body = apply body [ print_arg gamma arg ] in
        print_fill_context gamma context body
    | CAppConstantR (constant, context) ->
        let body = apply (Code.print constant) [ parens body ] in
        print_fill_context gamma context body

  (* [print_pat pat] prints the pattern [pat] in an environment that
     has already been extended with the variables bound by [pat]. *)

  let rec print_pat pat =
    match pat with
    | PatBind (x, xspec) ->
        print_var x (Some xspec)
    | PatObserve (x, _, _) ->
        (* We could decide to print this variable as a wildcard [_]
           if we notice that the observation is not [Disagreement]
           and therefore will give rise to no code. TODO *)
        print_ovar x
    | PatUnit ->
        unit
    | PatPair (pat1, pat2) ->
        OCaml.tuple [ print_pat pat1; print_pat pat2 ]

  (* [observe pat] prints the observation code that is implicit in the
     pattern [pat]. *)

  let rec observe pat =
    match pat with
    | PatBind _
    | PatUnit ->
        empty
    | PatPair (pat1, pat2) ->
        observe pat1 ^^
        observe pat2
    | PatObserve (_, _, o) ->
        match !o with
        | NobodyHasRun | CandidateHasRun _ | Agreement _ ->
            empty
        | Disagreement assertion ->
            hardline ^^ assertion

  (* [instruction gamma i] prints the instruction [i] to [stdout]. *)

  let instruction gamma i =
    match i with
    | LetOp (pat, name, _, context) ->
        (* Print the instruction itself. *)
        toplevel_let
          (print_pat pat)
          (print_fill_context gamma context (string name))
        ^^
        (* Print the observations that are implicit in the pattern [pat]. *)
        observe pat

  (* [instructions gamma renv cenv pc where is] prints the instructions [is]. *)

  (* [pc] is the instruction counter. *)

  (* [cause] indicates the cause of the failure that was observed. *)

  let rec instructions gamma renv pc cause is =
    match is with
    | [] ->
        empty
    | i :: is ->
        (* Print the instruction counter. *)
        hardline ^^
        utf8format "(* @%02d *) " pc ^^ align (
          (* Print the instruction [i]. *)
          instruction gamma i ^^
          (* If this is the last instruction and the parameter [cause] indicates
             that a well-formedness check caused a failure, print this check.
             Otherwise, continue. *)
          match is, cause with
          | [], Check x ->
              let xspec, rv = E.lookup gamma x, E.lookup renv x in
              hardline ^^
              apply
                (check xspec rv)
                [ print_var x (Some xspec) ]
              ^^ utf8format ";; (* this check fails! *)"
          | _, _ ->
              empty
        ) ^^
        let pc = pc + 1 in
        instructions gamma renv pc cause is

  let display_failure_and_abort gamma renv past e cause =
    let is = List.rev past in
    let pc = List.length is in
    printf "(* @%02d: %s! *)" pc (
      match cause with
      | Instruction -> "Failure in an instruction"
      | Observation -> "Failure in an observation"
      | Check _     -> "Failure in a well-formedness check"
      | Unexpected  -> "Failure in the reference implementation"
    );
    let pc = 1 in
    output (instructions gamma renv pc cause is ^^ hardline ^^ hardline);
    match e with
    | ObservationFailure ->
        (* If the problem is an observation failure,
           no backtrace is required. A single line suffices. *)
        printf "Fatal error: disagreement between candidate and reference.\n";
        abort()
    | e ->
        (* Otherwise, print a full backtrace. *)
        print_endline (Printexc.to_string e);
        Printexc.print_backtrace stdout;
        abort()

  (* [print_equal x1 v2] prints an equality between the variable [x1] and the
     value [v2]. The name of the equality function depends on the runtime type
     of the value [v2]. *)

  let print_equal xspec (x1 : document) (v2 : value) =
    let equal = equal xspec in
    (* TODO recognize the default equality (if possible) and print it infix *)
    apply (Code.print equal) [ x1; print xspec v2 ]

  (* [print_equality_assertion x xspec reference candidate] produces an
     equality assertion that explains why the candidate value [candidate] is
     incorrect. *)

  let print_equality_assertion x xspec reference candidate =
    assert_ (print_equal xspec (print_ovar x) reference)
    ^^ string ";;"
    ^^ candidate_finds (print xspec candidate)

end (* P *)

(* -------------------------------------------------------------------------- *)

(* The interpreter. *)

module I = struct

  (* [eval_arg env a] evaluates the argument [a] to a value. *)

  let rec eval_arg env (a : arg) : value =
    match a with
    | Constant v ->
        Code.value v
    | Var x ->
        E.lookup env x
    | Pair (a1, a2) ->
        pair (eval_arg env a1) (eval_arg env a2)

  (* [eval_fill_context selector env v context] places the value [v] in the
     context [context] and evaluates this code in the implementation determined
     by [selector]. *)

  let rec eval_fill_context selector env (v : value) context : value =
    match context with

    | CAppLArg (arg, context) ->
        (* We want to evaluate the application of [v] to [arg], then place
           the result in the context [context]. *)
        let v = apply v (eval_arg env arg) in
        eval_fill_context selector env v context

    | CAppConstantR (constant, context) ->
        (* We want to evaluate the application of [constant] to [v], then
           place the result in the context [context]. *)
        let v = apply (choose selector (Code.value constant)) v in
        eval_fill_context selector env v context

    | CEmpty ->
        v

  (* [disagree o assertion] records a disagreement between the reference and
     candidate implementations, then signals a failure. *)

  let disagree o assertion =
    o := Disagreement assertion;
    raise ObservationFailure

  (* [observe x xspec o w] compares the previous observation [o] with the newly
     observed value [w]. *)

  let observe x xspec (o : observation ref) (w : value) =
    match !o with

    | NobodyHasRun ->
        (* The candidate implementation runs first. [w] is its result.
           Record it for future comparison. *)
        o := CandidateHasRun w

    | CandidateHasRun candidate ->
        (* The candidate implementation runs first. [candidate] is its
           recorded result, and [w] is the reference result. *)
        let reference = w in
        (* [equal xspec] yields a user-defined notion of equality on the values
           [candidate] and [reference], which have a concrete base type. *)
        if Code.value (equal xspec) candidate reference then
          o := Agreement w
        else
          disagree o (P.print_equality_assertion x xspec reference candidate)

    | Agreement _
    | Disagreement _ ->
        (* There is no third run. *)
        assert false

  (* [interpret_pat env pat w] matches the value [w] against the pattern
     [pat] and (as a side effect) extends the environment [env]. *)

  let rec interpret_pat env pat w : unit =
    match pat, w with
    | PatBind (x, _), _ ->
        (* Extend the environment with a new variable, bound to [w]. *)
        assert (x = E.limit env);
        E.bind env w
    | PatObserve (x, _, o), Any (TyNondet ty, postcondition) ->
        (* The value [w] is tagged with [TyNondet]. In this special case,
           [w] is a reference-side postcondition, which expects to receive
           a candidate-side value as an argument. The candidate has run
           first, so the observation [o] contains the candidate's result. *)
        begin match !o with
        | NobodyHasRun | Agreement _ | Disagreement _ ->
            assert false
        | CandidateHasRun cv ->
            (* Apply the reference-side postcondition to the candidate's
               result. By convention, if it returns [Valid], this means that
               the reference is satisfied; if it returns [Invalid assertion],
               this means that it is not satisfied and [assertion] is an OCaml
               instruction that explains the problem. This assertion is
               parameterized over a variable that denotes the candidate's
               result. *)
            match postcondition (unwrap ty cv) with
            | Valid ->
                o := Agreement cv
            | Invalid assertion ->
                disagree o (assertion (P.print_ovar x))
        end
    | _, Any (TyNondet _, _) ->
        (* The combinator [nondet] can be applied to concrete types only. *)
        assert false
    | PatObserve (x, xspec, o), _ ->
        (* Check the result [w] against our previous observation [o].
           Do not extend the environment. *)
        observe x xspec o w
    | PatUnit, _ ->
        ()
    | PatPair (pat1, pat2), _ ->
        (* The value [w] must be a pair, which we deconstruct. *)
        let w1, w2 = unpair w in
        interpret_pat env pat1 w1;
        interpret_pat env pat2 w2

  (* [static_interpret_pat gamma pat] extends the type environment [gamma]
     with the pattern [pat]. Its structure is the same as that of
     [interpret_pat] above. *)

  let rec static_interpret_pat gamma pat : unit =
    match pat with
    | PatBind (x, xspec) ->
        (* Extend the environment with a new variable. *)
        assert (x = E.limit gamma);
        E.bind gamma xspec
    | PatObserve _
    | PatUnit ->
        ()
    | PatPair (pat1, pat2) ->
        static_interpret_pat gamma pat1;
        static_interpret_pat gamma pat2

  (* [interpret selector env i] interprets the instruction [i], under the
     environment [env], in the implementation dictated by [selector] --
     either the reference implementation or the candidate implementation.
     As a side effect, it extends the environment [env]. *)

  let interpret selector env i : unit=
    match i with

    | LetOp (pat, name, v, context) ->
        log "Interpreting a \"%s\" instruction (%s).\n%!" name (show selector);
        section begin fun () ->
          (* By construction, the value [v] is the implementation of some
             operation [uop], wrapped in a non-dependent type, whose left
             and right projections can therefore be computed. *)
          let v = choose selector v in
          (* Apply the operation to its arguments, yielding a value [w]. *)
          let w = eval_fill_context selector env v context in
          (* Obey the pattern on the left-hand side. *)
          interpret_pat env pat w
        end

  (* [static_interpret gamma i] computes the effect of the instruction [i] on
     the type environment [gamma]. As a side effect, [gamma] is extended. The
     structure of this function is the same as that of [interpret] above. *)

  let static_interpret gamma i : unit =
    match i with
    | LetOp (pat, _, _, _) ->
        static_interpret_pat gamma pat

end (* I *)

(* -------------------------------------------------------------------------- *)

(* Well-formedness checks. *)

(* The user-provided function [check] is expected to perform some kind of
   internal well-formedness check. It can fail by raising an arbitrary
   exception [e], perhaps an [Assert_failure]. It has access to both the
   reference value and the candidate value, so (if desired) it can check that
   the candidate data structure conforms to its logical model. *)

exception CheckFailed of var * exn

let performs_checks gamma renv cenv =
  E.foreach3 gamma renv cenv (fun x xspec rv cv ->
    (* We have a variable [x], its relational type [xspec],
       its value [rv] in the reference implementation, and
       its value [cv] in the candidate implementation. *)
    match xspec with
    | XSpec (SpecBaseAbstract (ty1, ty2, properties)) ->
        let rv = unwrap ty1 rv
        and cv = unwrap ty2 cv in
        let check = Code.value (properties.aty_check rv) in
        begin try
          check cv
        with e ->
          raise (CheckFailed (x, e))
        end
    | XSpec _ ->
        (* Every variable in the environment has abstract base type. *)
        assert false
  )

(* -------------------------------------------------------------------------- *)

(* An instruction generator. *)

(* During generation, we have access to the runtime environment [renv] of the
   reference implementation. *)

module G = struct

  (* [generate_arg renv spec p] generates an argument that has type [spec] and
     satisfies the precondition [p]. It returns a pair of a raw value and an
     argument. [renv] is the runtime environment of the reference
     implementation. *)

  let rec generate_arg
  : type r c . env -> (r, c) spec -> (r -> bool) -> r * arg
  = fun renv spec p ->
    match spec with

    | SpecBaseConcrete (ty, properties) ->
        begin match properties.cty_generate with
        | Some generate ->
            (* We have a generation function, which we can use to obtain a
               value [v]. We must check that this value satisfies [p]; if it
               doesn't, then generation fails and will be retried. *)
            let code : r code = generate() in
            let arg = Constant (Code.map (wrap ty) code) in
            let v : r = Code.value code in
            if p v then v, arg else Gen.reject()
        | None ->
            (* [normalize] guarantees that a concrete type that appears in a
               negative position is equipped with a generator, so this cannot
               happen. *)
            assert false
        end

    | SpecBaseAbstract (ty, _, _) ->
        (* Pick a variable in the environment that has type [ty], the left
           projection of [spec], and that satisfies the predicate [p]. *)
        let x = E.var renv (fun v ->
          match unwrap ty v with
          | exception RuntimeTypeError ->
              false
          | v ->
              p v
        ) in
        let v = E.lookup renv x in
        unwrap ty v, Var x

    | SpecPair (spec1, spec2) ->
        (* Generate a value for each pair component. *)
        let v1, arg1 = generate_arg renv spec1 (fun _ -> true)
        and v2, arg2 = generate_arg renv spec2 (fun _ -> true) in
        let v, arg = (v1, v2), Pair (arg1, arg2) in
        (* Check that [p] is satisfied. *)
        if p v then v, arg else Gen.reject()

    | SpecSubset (spec, q) ->
        (* This argument must satisfy the precondition [q]. Compute the
           conjunction of our argument [p] with [q], and go down. We do
           not adopt the naive solution of 1- making a recursive call to
           generate a value, and 2- checking a posteriori that this value
           satisfies [q]. Indeed, in the case where there is an abstract
           type below, we can do better -- we can test which values in
           the environment satisfy [q], and select a value from them.
           This eliminates the risk of choosing a value that does not
           satisfy [q]. *)
        generate_arg renv spec (fun v -> p v && q v)

    (* We do not expect any of the following cases to arise. [normalize]
       guarantees that arrows, dependent arrows, [nondet] and [map] cannot
       occur in a negative position. *)
    | SpecArrow _ ->
        assert false
    | SpecDependentArrow _ ->
        assert false
    | SpecNondet _ ->
        assert false
    | SpecMap _ ->
        assert false
    | SpecGenerate _ ->
        assert false
    | SpecGenerateSimplified _ ->
        assert false

  let generate_arg renv spec =
    log "Generating an argument.\n%!";
    section begin fun () ->
      generate_arg renv spec (fun _ -> true)
    end

  (* This name generator is used for observations. These variables are
     short-lived; they are used only when we print OCaml code for an
     observation. They are not bound in the runtime environment of our
     interpreter. *)

  let next_ovar, reset_ovars =
    let o = ref 0 in
    (fun () -> postincrement o), (fun () -> o := 0)

  (* [generate_pat limit spec] generates a left-hand side for an application of an
     operation whose result type is [spec]. The parameter [limit] is the de Bruijn
     level of the next available variable. *)

  let rec generate_pat
  : type r c . var -> (r, c) spec -> var * pat
  = fun limit spec ->
    match spec with

    | SpecBaseConcrete (TyBase TyUnit.T.Tag, _) ->
        log "generate_pat: unit type.\n%!";
        limit, PatUnit

    | SpecBaseConcrete _ ->
        log "generate_pat: concrete base type.\n%!";
        limit, PatObserve (next_ovar(), XSpec spec, ref NobodyHasRun)

    | SpecBaseAbstract (_, _, _) ->
        log "generate_pat: abstract base type.\n%!";
        let x = limit in
        let limit = E.succ limit in
        limit, PatBind (x, XSpec spec)
          (* We maintain the invariant that every variable in the
             environment has an abstract base type. *)

    | SpecPair (spec1, spec2) ->
        log "generate_pat: pair type.\n%!";
        section begin fun () ->
          let limit, pat1 = generate_pat limit spec1 in
          let limit, pat2 = generate_pat limit spec2 in
          limit, PatPair (pat1, pat2)
        end

    (* We do not expect the following cases to arise. *)
    | SpecArrow _ ->
        assert false
    | SpecDependentArrow _ ->
        assert false
    | SpecMap _ ->
        assert false
    | SpecGenerate _ ->
        assert false
    | SpecGenerateSimplified _ ->
        assert false

    | SpecSubset (spec, _) ->
        (* At the moment, if a result type is annotated with a postcondition,
           we ignore this postcondition. We could conceivably test whether it
           is satisfied and signal a problem if it isn't. *)
        log "generate_pat: SpecSubset.\n%!";
        generate_pat limit spec

    | SpecNondet spec ->
        log "generate_pat: SpecNondet.\n%!";
        generate_pat limit spec

  (* [generate_pat_context renv spec] generates a context and a left-hand side
     for an operation of type [spec]. It also returns a non-dependent version
     of the specification [spec], obtained by specializing [spec] for the
     arguments that have been generated. *)

  let rec generate_pat_context
  : type r c . env -> (r, c) spec -> pat * context * (r, c) spec
  = fun renv spec ->
    match spec with

    | SpecMap (name, rc, cc, domain, codomain) ->
        (* In this case, the user has provided an operation of type [domain]
           but wants to wrap it so that it appears to have type [codomain].
           The user provides a constant of type [domain -> codomain], whose
           name is [name] and whose implementations on the reference side
           and candidate side are [rc] and [cc]. *)
        (* We want to generate a context of the form [context[c _]]. *)
        (* First, generate the outer context, based on the type [codomain],
           and obtain a non-dependent version of this type. *)
        let pat, context, codomain = generate_pat_context renv codomain in
        (* Then, build a description of the constant [c]. It is worth noting
           that [domain] and [codomain] can be function types [TyArrow], in
           which case [c] is a higher-order constant. *)
        let arrow = SpecArrow (domain, codomain) in
        let c = code (lazy (PPrint.string name)) (RAny (arrow, rc, cc)) in
        (* Finally, construct the context [context[c _]]. *)
        pat, CAppConstantR (c, context), domain

    | SpecArrow (domain, codomain) ->
        (* An ordinary arrow. *)
        (* Generate an argument [arg] of type [domain]. *)
        let _, arg = generate_arg renv domain in
        (* Generate more arguments, and obtain a non-dependent version of
           [codomain]. *)
        let pat, context, codomain = generate_pat_context renv codomain in
        (* Put the results together. *)
        let spec = SpecArrow (domain, codomain) in
        pat, CAppLArg (arg, context), spec

    | SpecDependentArrow (domain, codomain) ->
        (* A dependent arrow. *)
        (* Generate an argument [arg] of type [domain]. *)
        let v, arg = generate_arg renv domain in
        (* [codomain] is a function of the raw value [v] to a specification.
           Apply it. *)
        let codomain = codomain v in
        (* Continue as in the non-dependent case. *)
        let pat, context, codomain = generate_pat_context renv codomain in
        let spec = SpecArrow (domain, codomain) in
        pat, CAppLArg (arg, context), spec

    | codomain ->
        (* Done. Generate a left-hand side. *)
        log "Generating a left-hand side.\n%!";
        section begin fun () ->
          reset_ovars();
          let _, pat = generate_pat (E.limit renv) codomain in
          let context = CEmpty in
          pat, context, codomain
        end

  (* [generate renv] generates an instruction. *)

  let generate renv : instruction =
    log "Generating an instruction.\n%!";
    section begin fun () ->
      (* Pick an operation. *)
      let name, RAny (spec, reference, candidate) = pick() in
      log "Picked operation \"%s\".\n%!" name;
      (* Generate appropriate arguments, and compute a non-dependent type [spec]
         by specializing [spec] to these arguments. *)
      let pat, context, spec = generate_pat_context renv spec in
      let v = RAny (spec, reference, candidate) in
      LetOp (pat, name, v, context)
    end

end (* G *)

(* -------------------------------------------------------------------------- *)

(* The engine's main loop. *)

(* [fuel] is the number of instructions that we are still allowed to generate.
   [past] is a reversed list of the instructions generated and executed so
   far. [gamma] is a type environment. [renv] and [cenv] are the runtime
   environments of the reference and candidate implementations. *)

let rec test fuel past (gamma : gamma) (renv : env) (cenv : env) : unit =

  (* If the maximum number of instructions has been reached, stop. Otherwise,
     generate an instruction. *)

  if fuel > 0 then
  match G.generate renv with

  | exception End_of_file ->
      (* The generation of a new instruction involves making choices, thus
         consuming input data. This can fail if there is no more input data.
         In that case, the test is over. We do not abort; we terminate
         normally, so another test can run. *)
      let pc = List.length past in
      printf "@%02d: Input data exhausted; end of this test.\n" pc

  | exception Gen.Reject ->
      (* The generation of a new instruction involves making choices, thus
         consuming input data. This can fail if the input data does not suit
         us. In that case, try generating another instruction. *)
      log "Generation of an instruction failed; retrying.\n";
      test fuel past gamma renv cenv

  | exception IllFormedSpec (op, msg) ->
      (* The generation of an instruction involves a call to [normalize],
         which can fail if a specification is ill-formed. *)
      printf "Error: in the specification of operation `%s`:\n%s\n" op msg;
      exit 13

  | exception e ->
      (* Another exception indicates an abnormal condition, perhaps a mistake
         in the generation code. It should be reported, so it can be fixed. *)
      printf "An exception was raised during generation!\n";
      raise e

  | (i : instruction) ->

  (* Record this instruction. *)
  let fuel = fuel - 1
  and past = i :: past in

  (* Execute [i] under the candidate implementation first. This can fail
     if the candidate implementation raises an exception. If it succeeds,
     then we get an instruction [i] annotated with an actual result. *)

  match I.interpret Candidate cenv i with
  | exception e ->
      P.display_failure_and_abort gamma renv past e Instruction
  | () ->

  (* Execute the instruction [i] under the reference implementation.
     Executing the reference implementation *after* the candidate
     implementation allows us to tolerate situations where the
     candidate is allowed to make nondeterministic choices; the
     reference implementation can then follow these choices. This
     execution can fail if it turns out that an observable result
     produced by the candidate is not acceptable. *)
  match I.interpret Reference renv i with

  | exception (ObservationFailure as e) ->
      P.display_failure_and_abort gamma renv past e Observation

  | exception e ->
      (* Another exception indicates an abnormal condition, perhaps
         a mistake in the reference implementation. It should be
         reported, so it can be fixed. *)
      P.display_failure_and_abort gamma renv past e Unexpected

  | () ->

  (* Both the reference and the candidate implementations have run
     successfully. We may now extend the type environment. *)

  I.static_interpret gamma i;

  (* Make sure that every data structure is well-formed. *)
  match performs_checks gamma renv cenv with

  | exception CheckFailed (x, e) ->
      P.display_failure_and_abort gamma renv past e (Check x)

  | () ->
      (* Continue. *)
      test fuel past gamma renv cenv

(* -------------------------------------------------------------------------- *)

(* In order to save the cost of allocating fresh (empty) environments for
   every run, we re-use the same environment over and over. The operation
   [E.clear] is extremely cheap. *)

let envs : (gamma * env * env) option ref =
  ref None

let envs fuel : gamma * env * env =
  match !envs with
  | None ->
      (* Compute a bound on the size of the environment. This should ideally
         be computed by multiplying [fuel] by the maximum number of variables
         that a single operation can bind, that is, the maximum number of
         results of abstract type that an operation returns. For the moment,
         let's be lazy and estimate this maximum number to be 5. *)
      let bound = 5 * fuel in
      (* Create three empty initial environments. *)
      let dummy_xspec = XSpec unit
      and dummy_value = unit_value in
      let gamma = E.empty bound dummy_xspec
      and renv = E.empty bound dummy_value
      and cenv = E.empty bound dummy_value in
      (* Store them for later re-use. *)
      let result = gamma, renv, cenv in
      envs := Some result;
      (* Return them. *)
      result
  | Some ((gamma, renv, cenv) as result) ->
      (* We assume that every run uses the same value of [fuel], so the
         existing environments have an appropriate maximum size. *)
      E.clear gamma;
      E.clear renv;
      E.clear cenv;
      result

(* -------------------------------------------------------------------------- *)

(* Initialization. *)

let test fuel =
  log "Beginning one test run.\n%!";
  section begin fun () ->
    (* No past. *)
    let past = [] in
    (* Run. *)
    let gamma, renv, cenv = envs fuel in
    test fuel past gamma renv cenv;
    (* Done. *)
    log "This test run is finished.\n%!"
  end

(* -------------------------------------------------------------------------- *)

(* Find the name of our input file on the command line. *)

let source : string option =
  let usage = sprintf "Usage: %s <input file>" Sys.argv.(0) in
  let source = ref None in
  Arg.parse [] (fun s -> source := Some s) usage;
  !source

(* -------------------------------------------------------------------------- *)

(* Reading input data from the source file whose name was given on the command
   line, perform a number of test runs. [AflPersistent] allows performing many
   test runs without creating a new process every time. *)

(* The function [prologue] is executed after the source channel has
   been opened, so it is allowed to call the data generation functions
   in the module [Gen]. *)

let none () =
  ()

let main ?prologue:(prologue=none) fuel =
  AflPersistent.run (fun () ->
    Gen.with_source source (fun () ->
      prologue();
      test fuel
    )
  )

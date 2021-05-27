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
open Misc
open Error
open Eq
open Spec
open Env
open Ops

type document =
  PPrint.document

(* -------------------------------------------------------------------------- *)

(* This defines a function [log] which can be used for debugging. *)

include Debug.Make(struct let debug = false end)

(* -------------------------------------------------------------------------- *)

(* Turn on the recording of backtraces, so an uncaught exception causes a
   backtrace to be printed before the process is aborted. *)

let () =
  Printexc.record_backtrace true

(* -------------------------------------------------------------------------- *)

(* DSL syntax. *)

(* It is worth noting that we do not need an *interpreter* for this syntax;
   we only need a *printer*. Indeed, the engine generates a program and
   executes it on the fly. Since generation and execution are not separate,
   there is no need for communication between them. Instead, the syntax is
   used by the engine as a way of recording its actions. Thus, when a fault
   is discovered, a scenario can be printed. *)

(* An argument of an operation is one of the following: *)

type arg =

  (* A constant (of an arbitrary and unknown type). *)
  | ArgConstant of document

  (* A variable (of abstract type). *)
  | ArgVar of var

  (* A unit argument. *)
  | ArgUnit

  (* A pair of arguments. *)
  | ArgPair of arg * arg

  (* An argument of type [option]. *)
  | ArgOption of arg option

  (* An argument of type [result]. *)
  | ArgResult of (arg, arg) result

  (* An argument of type [list]. *)
  | ArgList of arg list

  (* An application of a constant to an argument. *)
  | ArgAppConstant of document * arg

(* An operation is used inside an expression. An expression is one of the
   following. (Note the linear structure of this type, as opposed to a
   tree-like structure which would be more standard.) *)

type expr =

  (* At the heart of an expression lies an operation that we wish to use. *)
  | EOp of string

  (* An application of an expression to an argument. *)
  | EAppL of expr * arg

  (* An application of some constant to an expression. *)
  (* At the time of writing, this constant must originate in a use of the
     combinator [map_into]. *)
  (* This constant is represented as a function of type [document list ->
     document]. Thus, an application of this constant to a sufficient number
     of arguments can possibly be beta-reduced on the fly and printed in
     a simpler way. *)
  | EAppR of (document list -> document) * expr

(* A pattern indicates what to do with the result of an operation. We construct
   a pattern only after the two implementations (candidate and reference) have
   run. We use this pattern to explain the disagreement when there is one and
   to deconstruct the result value when there is agreement. *)

type pat =
  | PatWildcard
      (* [PatWildcard] can be used at an arbitrary type. *)
  | PatBind of var
      (* [PatBind] can be used only at an abstract base type. It indicates
         that a variable should be bound to this value, so that this value
         can be used in future instructions. *)
  | PatObserveAgreement
      (* [PatObserveAgreement] is printed like a wildcard pattern. It is
         used only at a concrete base type. It indicates that both sides
         agree on this value. *)
  | PatObserveDisagreement of document
      (* [PatObserveDisagreement] is used only at a concrete base type.
         It indicates that there is a disagreement, and carries a string
         representation of an OCaml instruction that explains the problem.
         An example is "assert (observed = 0);; (* candidate finds 1 *)".
         This pattern plays the double role of binding the observation
         variable [ovar] and of carrying this OCaml instruction, which
         refers to the variable [ovar]. *)
  | PatUnit
      (* [PatUnit] is used at the [unit] type. *)
  | PatPair of pat * pat
      (* [PatPair] is used at a pair type. *)
  | PatOption of pat option
      (* [PatOption is used at an option type. *)
  | PatResult of (pat, pat) result
      (* [PatResult] is used at a result type. *)
  | PatNil
  | PatCons of pat
      (* [PatNil] and [PatCons] are used at a list type. [PatCons] carries
         a single pattern, which matches a pair. *)

(* An instruction takes (roughly) the following form:

      let pat = expr;;
      assert (observed = ...);;
        (* where  *)

   The expression [expr] is always a use of an operation [op] within a certain
   context (e.g., an application of this operation to a list of arguments).

   The line [assert (observed = ...);;;] is an (optional) observation. If it
   is present, then [observed] is an observation variable bound by [pat], and
   [pat] binds no other variables. This assertion compares an observed result
   with an expected result. It is present only if a mismatch has been
   detected. *)

type instruction =
  | I of pat * expr

(* When an exception is caught and turned into a piece of data, the backtrace
   must be recorded now, as it could otherwise be clobbered by raising another
   exception. *)

type frozen_exn =
  { e: exn; backtrace: string }

let freeze e =
  let backtrace = Printexc.get_backtrace() in
  { e; backtrace }

(* A [failure] records what kind of failure occurred. *)

type failure =

  (* The reference implementation raised an exception. *)
  | ReferenceFailure of frozen_exn

  (* The candidate implementation raised an exception. *)
  | CandidateFailure of frozen_exn

  (* A wrapper inserted by [map_into] or [map_outof] raised an exception. *)
  | ReferenceWrapperFailure of frozen_exn
  | CandidateWrapperFailure of frozen_exn

  (* The candidate produced an incorrect observable result. *)
  | ObservationFailure

  (* A well-formedness check raised an exception. *)
  | CheckFailure of (* x: *) var * frozen_exn

(* -------------------------------------------------------------------------- *)

(* Environments. *)

(* We maintain an environment that maps each variable to a value, that is, a
   triple of a relational specification, a reference value, and a candidate
   value. *)

(* We maintain the invariant that every value in the environment has an
   abstract base type. Thus, every spec in the environment is of the form
   [SpecBaseAbstract _]. Indeed, a value of a concrete type is observed as
   soon as it is produced, and does not need to be bound to a variable.
   A value of a structural type (product, sum, etc.) is deconstructed as
   soon as it is produced. *)

type env =
  value Env.env

(* -------------------------------------------------------------------------- *)

(* Exceptions. *)

(** The exception [PleaseBackOff] can be raised by the reference
    implementation to indicate that this operation (or this particular choice
    of arguments for this operation) should not be exercised. The reason could
    be that it is not permitted, or that this has not been implemented yet.
    This exception causes Monolith to back off and try something else. For this
    reason, the reference implementation must not perform any side effect
    before raising this exception. *)
exception PleaseBackOff

(** The exception [Unimplemented] can be raised by the candidate
    implementation to indicate that this operation (or this particular choice
    of arguments for this operation) should not be exercised. This exception
    causes Monolith to abandon the current scenario and investigate other
    scenarios. *)
exception Unimplemented

(* -------------------------------------------------------------------------- *)

(* Printing instructions. *)

include struct

  open Print

  (* [print_var env x] prints the variable [x], which must be bound in [env]. *)

  let print_var env x =
    (* Use a type-specific prefix, followed with the numeric level. *)
    (* We do not need distinct types to be associated with distinct
       prefixes; the presence of the numeric level alone ensures that
       we produce distinct names for distinct variables. *)
    match Env.lookup env x with
    | Value (SpecBaseAbstract (_, properties), _, _) ->
        (* Extract the base name associated with this abstract base type. *)
        let base = properties.aty_var in
        utf8format "%s%d" base (level x)
    | Value _ ->
        assert false (* every variable has abstract type *)

  (* [ovar] is the name of the single observation variable that is needed
     when we wish to name an observed value and indicate that it does not
     meet our expectation. *)

  let ovar =
    utf8format "observed"

  (* [print_arg env a] prints the argument [a], whose free variables must
     be bound in [env]. The resulting string is parenthesized. *)

  let rec print_arg env arg =
    match arg with
    | ArgConstant constant ->
        constant
    | ArgVar x ->
        print_var env x
    | ArgUnit ->
        unit
    | ArgPair (arg1, arg2) ->
        OCaml.tuple [ print_arg env arg1; print_arg env arg2 ]
    | ArgOption oarg ->
        option (print_arg env) oarg
    | ArgResult rarg ->
        result (print_arg env) (print_arg env) rarg
    | ArgList args ->
        list (print_arg env) args
    | ArgAppConstant (constant, arg) ->
        parens (apply constant [ print_arg env arg ])

  (* [print_expr env expr] prints the expression [expr]. The result string
     is not parenthesized. *)

  (* [print_expr] uses an auxiliary function [print_app], which descends into
     the left-hand side of applications and accumulates a list of actual
     arguments. This allows us to print an n-ary application in one swoop, and
     possibly to simplify it on the fly. *)

  let rec print_expr env expr =
    print_app env expr []

  and print_app env expr (actuals : document list) =
    match expr with

    | EOp op ->
        (* We have reached the head of this n-ary application. *)
        apply
          (!^ op)
          actuals

    | EAppL (expr, arg) ->
        (* Descend into the left-hand side, accumulating one more actual
           argument. No parentheses are needed because [print_arg] prints
           a parenthesized string already. *)
        print_app env expr (print_arg env arg :: actuals)

    | EAppR (constant, expr) ->
        (* Descend into the left-hand side and immediately remark that we have
           reached the head of this n-ary application. Parentheses are needed
           because [print_expr] does not produce a parenthesized string. *)
        (* Here, [constant] is a function that expects a list of documents. *)
        constant
          (parens (print_expr env expr) :: actuals)

  (* [print_pat env pat] prints the pattern [pat], whose variables must
     be bound in [env]. The resulting string is parenthesized. *)

  let cons =
    !^ " ::" ^^ break 1

  let rec print_pat env pat =
    match pat with
    | PatWildcard ->
        underscore
    | PatBind x ->
        print_var env x
    | PatObserveAgreement ->
        underscore
    | PatObserveDisagreement _ ->
        ovar
    | PatUnit ->
        unit
    | PatPair (pat1, pat2) ->
        OCaml.tuple [ print_pat env pat1; print_pat env pat2 ]
    | PatOption opat ->
        option (print_pat env) opat
    | PatResult rpat ->
        result (print_pat env) (print_pat env) rpat
    | PatNil
    | PatCons _ ->
        print_pat_list env [] pat

  and print_pat_list env accu pat =
    match pat with
    | PatCons (PatPair (pat1, pat2)) ->
        print_pat_list env (pat1 :: accu) pat2
    | PatCons _ ->
        assert false
    | PatNil ->
        (* We have a closed list pattern. We can use list syntax. *)
        let pats = List.rev accu in
        list (print_pat env) pats
    | _ ->
        assert (pat = PatWildcard);
        (* We have an unclosed list pattern. We must use cons syntax. *)
        let pats = List.rev (pat :: accu) in
        group (lparen ^^ nest 2 (
          flow_map cons (print_pat env) pats
        ) ^^ rparen)

  (* [print_observation pat] prints the observation code that is implicit in
     the pattern [pat]. *)

  (* In principle, a pattern contains at most one subpattern of the form
     [PatObserveDisagreement _]. This implies that [print_observation]
     produces at most one assertion. *)

  let rec print_observation pat =
    match pat with
    | PatWildcard ->
        empty
    | PatBind _ ->
        empty
    | PatObserveAgreement ->
        empty
    | PatObserveDisagreement assertion ->
        hardline ^^ assertion
    | PatUnit ->
        empty
    | PatPair (pat1, pat2) ->
        print_observation pat1 ^^
        print_observation pat2
    | PatOption None ->
        empty
    | PatOption (Some pat) ->
        print_observation pat
    | PatResult (Ok pat)
    | PatResult (Error pat) ->
        print_observation pat
    | PatNil ->
        empty
    | PatCons pat ->
        print_observation pat

  (* [print_instruction env i] prints the instruction [i]. *)

  let print_instruction env i =
    match i with
    | I (pat, expr) ->
        (* We expect that the environment [env] has already been extended with
           the variables bound by the pattern [pat]. *)
        toplevel_let (print_pat env pat) (print_expr env expr) ^^
        (* Print the observations that are implicit in the pattern [pat]. *)
        print_observation pat

  (* [print_instructions env pc failure is] prints the instruction sequence
     [is]. [pc] is the instruction counter. [failure] indicates the cause of
     the failure that was observed. *)

  let rec print_instructions env pc failure is =
    match is with
    | [] ->
        empty
    | i :: is ->
        (* Print the instruction counter. *)
        utf8format "(* @%02d *) " pc ^^ align (
          (* Print the instruction [i]. *)
          print_instruction env i ^^
          (* If this is the last instruction and if the parameter [failure]
             shows that a well-formedness check caused a failure, print this
             check. Otherwise, continue. *)
          match is, failure with
          | [], CheckFailure (x, _) ->
              begin match Env.lookup env x with
                | Value (SpecBaseAbstract (_, properties), rv, _) ->
                    let check = properties.aty_check rv in
                    hardline ^^
                    Code.print check [ print_var env x ]
                    ^^ utf8format ";; (* this check fails! *)"
                | _ ->
                    assert false (* every variable has abstract type *)
              end
          | _, _ ->
              empty
        ) ^^
        hardline ^^
        let pc = pc + 1 in
        print_instructions env pc failure is

  (* [failure_type failure] is a one-line summary of the failure. *)

  let failure_type failure =
    match failure with
    | ReferenceFailure _ ->
        "Failure in the reference implementation"
    | CandidateFailure _ ->
        "Failure in an instruction"
    | CandidateWrapperFailure _ ->
        sprintf "Failure in a wrapper on the candidate side"
    | ReferenceWrapperFailure _ ->
        sprintf "Failure in a wrapper on the reference side"
    | ObservationFailure ->
        "Failure in an observation: candidate and reference disagree"
    | CheckFailure _ ->
        "Failure in a well-formedness check"

  (* [print_failure_exception failure] returns a string that describes
     the exception associated with the failure, if there is one. *)

  let print_failure_exception pc failure : string =
    match failure with
    | ObservationFailure ->
        (* If the problem is an observation failure, no exception was raised. *)
        ""
    | ReferenceFailure e
    | CandidateFailure e
    | ReferenceWrapperFailure e
    | CandidateWrapperFailure e
    | CheckFailure (_, e) ->
        (* If an exception was raised, print a full backtrace. *)
        sprintf "(* @%02d: The exception %s was raised. *)\n\n%s"
          pc
          (Printexc.to_string e.e)
          e.backtrace

  (* [print_failure env past failure] returns a failure message. (This is done
     immediately before aborting.) [past] is the list of past instructions.
     [failure] is the cause of the failure. *)

  let print_failure env past failure : string =
    let b = Buffer.create 1024 in
    let is = List.rev past in
    let pc = List.length is in
    bprintf b "(* @%02d: %s. *)\n" pc (failure_type failure);
    DelayedOutput.dump b;
    output b (print_instructions env 1 failure is);
    Buffer.add_string b (print_failure_exception pc failure);
    Buffer.contents b

  (* [print_equality_assertion equal rv cv] produces an equality assertion
     that explains why the candidate value [cv] is incorrect. It is an
     equality between the variable [ovar] (which denotes a candidate value)
     and the reference value [rv]. A comment is also printed, in which the
     incorrect candidate value appears. *)

  let print_equality_assertion equal rv cv =
    assert_ (Code.print equal [ ovar; rv ])
    ^^ !^ ";;"
    ^^ candidate_finds cv

end

(* -------------------------------------------------------------------------- *)

(* This exception is used to abort a run. It is handled in different ways
   when running under AFL and when running in purely random mode. *)

(* Its integer argument is the fuel that was necessary to discover a bug. *)

(* Its string argument is the scenario. *)

exception Abort of string * int

let raise_abort env past failure =
  let scenario = print_failure env past failure
  and fuel = List.length past in
  raise (Abort (scenario, fuel))

(* -------------------------------------------------------------------------- *)

(* Constructing an argument for an operation. *)

(* The [true] precondition. *)

let no_requirement _ = true

(* [construct_arg env spec p] constructs an argument that has type [spec] and
   satisfies the precondition [p]. It returns an argument as well as the
   result of evaluating this argument, a pair of a reference value [rv] and a
   candidate value [cv]. [spec] must be constructible. *)

(* [construct_arg] fails if a wrapper raises an exception: see [SpecMapOutof].
   This is an abnormal situation, a fatal error, most likely a programming
   error in the wrapper. We deal with it by wrapping this exception in a
   [ConstructArg] exception. An earlier version of the code used a [result]
   return type, but that was too painful. *)

(* [construct_arg] can also raise [Gen.Reject], causing the generator to
   backtrack and try some other instruction. *)

exception ConstructArg of arg * failure

let rec construct_arg
: type r c . env -> (r, c) spec -> (r -> bool) -> arg * r * c
= fun env spec p ->
  match spec with

  | SpecBaseAbstract (tag, _) ->
      (* Pick a variable in the environment that has the desired type
         and that satisfies the predicate [p]. *)
      Env.choose env (fun x (Value (spec', rv, cv)) ->
        match spec' with
        | SpecBaseAbstract (tag', _) ->
            begin match Tag.equal tag tag' with
            | Eq ->
                (* The success of the tag equality test allows the
                   following two type casts. *)
                let rv : r = rv
                and cv : c = cv in
                if p rv then Some (ArgVar x, rv, cv) else None
            | exception Tag.RuntimeTagError ->
                None
            end
        | _ ->
            assert false (* every variable has abstract type *)
      )

  | SpecSubset (spec, q) ->
      (* This argument must satisfy the precondition [q]. Compute the
         conjunction of our argument [p] with [q], and go down. We do
         not adopt the naive solution of 1- making a recursive call to
         construct a value, and 2- checking a posteriori that this value
         satisfies [q]. Indeed, in the case where there is an abstract
         type below, we can do better -- we can test which values in
         the environment satisfy [q], and select a value from them.
         This eliminates the risk of choosing a value that does not
         satisfy [q]. *)
      construct_arg env spec (fun v -> p v && q v)

  | _ ->
      (* In all other cases, we construct an argument and verify a posteriori
         that the predicate [p] is satisfied. *)
      let (_, rv, _) as outcome = construct_arg_no_requirement env spec in
      if p rv then outcome else Gen.reject()

(* [construct_arg_no_requirement] specializes [construct_arg] to the case
   where the precondition [p] is trivial (always true). This special case
   is common and can be handled more efficiently. *)

and construct_arg_no_requirement
: type r c . env -> (r, c) spec -> arg * r * c
= fun env spec ->
  match spec with

  | SpecBaseAbstract _ ->
      (* To avoid code duplication, jump back to [construct_arg]. *)
      construct_arg env spec no_requirement

  | SpecSubset (spec, q) ->
      (* Easy. *)
      construct_arg env spec q

  | SpecConstructible { generate } ->
      (* We have a generation function, which we can use to obtain a
         value [v]. Because this is a concrete type, the same value
         works on both sides. *)
      let code = generate() in
      let arg = ArgConstant (Code.print code []) in
      let v = Code.value code in
      arg, v, v

  | SpecUnit ->
      ArgUnit, (), ()

  | SpecPair (spec1, spec2) ->
      (* Construct a value for each pair component. *)
      let arg1, rv1, cv1 = construct_arg_no_requirement env spec1
      and arg2, rv2, cv2 = construct_arg_no_requirement env spec2 in
      ArgPair (arg1, arg2), (rv1, rv2), (cv1, cv2)

  | SpecOption spec ->
      (* Choose between constructing [None] and constructing [Some _]. *)
      if Gen.bool() then
        ArgOption None, None, None
      else
        let arg, rv, cv = construct_arg_no_requirement env spec in
        ArgOption (Some arg), Some rv, Some cv

  | SpecResult (spec1, spec2) ->
      if Gen.bool() then
        let arg, rv, cv = construct_arg_no_requirement env spec1 in
        ArgResult (Ok arg), Ok rv, Ok cv
      else
        let arg, rv, cv = construct_arg_no_requirement env spec2 in
        ArgResult (Error arg), Error rv, Error cv

  | SpecList (length, spec) ->
      (* If we view [SpecList] as strictly synonymous with a recursive
         specification that involves a sum type [Nil + Cons], then we should
         flip a coin so as to choose between [Nil] and [Cons] and continue.
         But this doesn't seem very smart, as it amounts to encoding the
         length of the list in unary notation, and yields a small probability
         of generating long lists. For this reason, we prefer to use a
         different generation strategy, where we pick the length of the list
         up front in the semi-open interval [0, n). *)
      let n = length() in
      let args, rvs, cvs = construct_arg_list env spec n [] [] [] in
      ArgList args, rvs, cvs

  | SpecMapOutof (rwrap, cwrap, spec) ->
      begin
        let arg, rv, cv = construct_arg_no_requirement env spec in
        let arg = ArgAppConstant (Code.print cwrap [], arg) in
        match rwrap rv with
        | exception e ->
            raise (ConstructArg (arg, ReferenceWrapperFailure (freeze e)))
        | rv ->
        match Code.value cwrap cv with
        | exception e ->
            raise (ConstructArg (arg, CandidateWrapperFailure (freeze e)))
        | cv ->
        arg, rv, cv
      end

  (* Recursive specifications are unfolded on the fly. *)
  | SpecDeferred spec ->
      construct_arg_no_requirement env (Lazy.force spec)

  (* [SpecIfPol] is interpreted on the fly. Its first argument represents
     the construction side. *)
  | SpecIfPol (spec, _) ->
      construct_arg_no_requirement env spec

  (* We do not expect any of the following cases to arise. *)
  | SpecDeconstructible _ ->
      assert false
  | SpecTop ->
      assert false
  | SpecArrow _ ->
      assert false
  | SpecDependentArrow _ ->
      assert false
  | SpecNondet _ ->
      assert false
  | SpecMapInto _ ->
      assert false

and construct_arg_list
: type r c . env -> (r, c) spec -> int ->
             arg list -> r list -> c list -> arg list * r list * c list
= fun env spec n args rvs cvs ->
  if n = 0 then
    args, rvs, cvs
  else
    let arg, rv, cv = construct_arg_no_requirement env spec in
    construct_arg_list env spec (n-1) (arg :: args) (rv :: rvs) (cv :: cvs)

(* [construct_arg_no_requirement env spec] generates an argument of
   type [spec]. It returns a triple of a reference value, a candidate
   value, and an argument. *)

let construct_arg_no_requirement env spec =
  log "Constructing an argument.\n%!";
  section begin fun () ->
    construct_arg_no_requirement env spec
  end

(* -------------------------------------------------------------------------- *)

(* Using an operation (by placing it in a context). *)

(* We do this by starting with the trivial expression [EOp op] and growing
   it into a larger expression. *)

(* [use env spec e rv cv] uses the expression [e], whose (dual) value is [rv]
   / [cv], and which conforms to the specification [spec]. Based on [spec], we
   determine how this expression should be used, that is, in what context it
   should be placed: e.g., if it is a function, then it should be applied to
   an appropriate argument. Thus, we build a larger expression, which we
   evaluate, and the process continues.

   This process goes on as long as [spec] is a "negative" type, that is, an
   opaque type, typically a function type. Once a "positive" type is reached,
   this process stops and we move on to a different phase, where the value is
   deconstructed.

   This process can fail if either the reference implementation or the
   candidate implementation raises an exception.

   Regardless of success or failure, [use] always returns the expression that
   was generated. In case of success, it also returns the residual value [v']
   obtained by evaluating the original value in this context. In case of
   failure, it returns the failure observed by evaluating the original value
   in this context. *)

let rec use
: type r c .
  env -> (r, c) spec -> expr -> r -> c -> expr * (value, failure) result
= fun env spec expr rv cv ->
  match spec with

  | SpecArrow (spec1, spec2) ->
      (* Remap this on the fly to a trivial dependent arrow, so as to
         avoid redundancy with the next case. *)
      use env (SpecDependentArrow (spec1, fun _ -> spec2)) expr rv cv

  | SpecDependentArrow (spec1, spec2) ->
      begin
        (* Generate an argument [arg1] of type [spec1]. *)
        match construct_arg_no_requirement env spec1 with
        | exception ConstructArg (arg1, failure) ->
            EAppL (expr, arg1), Error failure
        | arg1, rv1, cv1 ->
        (* Construct the application of [expr] to [arg1]. *)
        let expr = EAppL (expr, arg1) in
        (* [spec2] is a function of [rv1] to a specification. Apply it. *)
        let spec2 = spec2 rv1 in
        (* Evaluate the application [expr arg1] on each side. In the absence
           of failures, it would not matter in which order we evaluate them.
           However, if the reference implementation raises [PleaseBackOff],
           then we wish to back off and try another operation (within the same
           scenario). This is safe only if the candidate implementation has
           not had an opportunity to perform side effects; so the reference
           implementation must run first. The candidate implementation is
           allowed to raise [Unimplemented]; in that case, because the side
           effects of the reference implementation cannot be undone, we start
           afresh. *)
        match rv rv1 with
        | exception e ->
            expr, Error (ReferenceFailure (freeze e))
        | rv2 ->
        match cv cv1 with
        | exception e ->
            expr, Error (CandidateFailure (freeze e))
        | cv2 ->
        (* Continue. *)
        use env spec2 expr rv2 cv2
      end

  | SpecMapInto (rwrap, cwrap, spec) ->
      begin
        (* The user has provided an operation of some unknown type [foo] and
           wants to wrap it so that it appears to have type [spec]. The user
           provides a wrapper of type [foo -> spec], whose implementations on
           the reference side and candidate side are [rwrap] and [cwrap]. *)
        (* Generate the expression [freeze expr]. *)
        let expr = EAppR (Code.print cwrap, expr) in
        (* Evaluate this application on each side. *)
        match rwrap rv with
        | exception e ->
            expr, Error (CandidateWrapperFailure (freeze e))
        | rv ->
        match Code.value cwrap cv with
        | exception e ->
            expr, Error (ReferenceWrapperFailure (freeze e))
        | cv ->
        (* Continue. *)
        use env spec expr rv cv
      end

  (* Recursive specifications are unfolded on the fly. *)
  | SpecDeferred spec ->
      use env (Lazy.force spec) expr rv cv

  (* [SpecIfPol] is interpreted on the fly. Its first second represents
     the deconstruction side. *)
  | SpecIfPol (_, spec) ->
      use env spec expr rv cv

  | spec ->
      (* Done. *)
      log "Done generating a context.\n%!";
      expr, Ok (Value (spec, rv, cv))

(* [generate_instruction env] generates an instruction of the form
   [let _ = context[op]] and evaluates its right-hand side. (The wildcard
   pattern on the left-hand side is intended to be later replaced with
   a more interesting pattern [pat].) Like [use], this operation always
   produces an instruction; furthermore, it produces a result which tells
   whether evaluation resulted in a value or in a failure. *)

let generate_instruction env : instruction * (value, failure) result =
  log "Generating an instruction.\n%!";
  section begin fun () ->
    (* Pick an operation. *)
    let op, Value (spec, rv, cv) = pick() in
    log "Picked operation \"%s\".\n%!" op;
    (* Generate an expression that uses this operation,
       and evaluate this expression. *)
    let expr, result = use env spec (EOp op) rv cv in
    (* Generate an instruction. At this point, we have not yet decided
       how to deconstruct the result, so we temporarily use a wildcard
       pattern. *)
    I (PatWildcard, expr), result
  end

(* -------------------------------------------------------------------------- *)

(* Deconstructing a result. *)

(* [deconstruct env rv cv spec] simultaneously deconstructs the reference
   value [rv] and the candidate value [cv], whose relational type is [spec].

   It returns a pair of a Boolean outcome [ok] and a pattern [pat]. In [ok] is
   [true], the values [rv] and [cv] agree, and the pattern [pat] matches both
   of them. If [ok] is [false], the values [rv] and [cv] disagree, and the
   pattern [pat] matches [rv] but not [cv].

   In the case of a success, the environment [env] is appropriately extended.
   In the case of a failure, it is partially extended. That should not be a
   problem; if it turned out to be a problem, we could easily reset it to its
   initial height. *)

(* [deconstruct] fails if the reference implementation of a nondeterministic
   operation raises an exception. This is an abnormal situation. We deal with
   it by wrapping this exception in a [Deconstruct] exception. We lose some
   information as this exception is propagated back up (we forget where we
   were in the tree), but it seems difficult to do better. *)

exception Deconstruct of failure

let rec deconstruct : type r c .
  env -> r -> c -> (r, c) spec -> bool * pat
= fun env rv cv spec ->
  match spec, rv, cv with

  | SpecTop, _, _ ->
      log "deconstruct: top.\n%!";
      true, PatWildcard

  | SpecDeconstructible { equal; print }, _, _ ->
      log "deconstruct: deconstructible type.\n%!";
      (* [equal] is a user-defined notion of equality at this type. *)
      if Code.value equal rv cv then
        (* Agreement. *)
        true, PatObserveAgreement
      else
        (* Disagreement. *)
        let assertion = print_equality_assertion equal (print rv) (print cv) in
        false, PatObserveDisagreement assertion

  | SpecBaseAbstract _, _, _ ->
      log "deconstruct: abstract base type.\n%!";
      let x = Env.limit env in
      Env.bind env (Value (spec, rv, cv));
      true, PatBind x

  | SpecUnit, (), () ->
      log "deconstruct: unit type.\n%!";
      true, PatUnit

  | SpecPair (spec1, spec2), (rv1, rv2), (cv1, cv2) ->
      log "deconstruct: pair type.\n%!";
      section begin fun () ->
        let ok1, pat1 = deconstruct env rv1 cv1 spec1 in
        if ok1 then
          let ok2, pat2 = deconstruct env rv2 cv2 spec2 in
          if ok2 then
            true, PatPair (pat1, pat2)
          else
            false, PatPair (PatWildcard, pat2)
        else
            false, PatPair (pat1, PatWildcard)
      end

  | SpecOption _, None, None ->
      log "deconstruct: option type (None/None).\n%!";
      true, PatOption None

  | SpecOption spec, Some rv, Some cv ->
      log "deconstruct: option type (Some/Some).\n%!";
      let ok, pat = deconstruct env rv cv spec in
      ok, PatOption (Some pat)

  | SpecOption _, None, Some _ ->
      log "deconstruct: option type (None/Some).\n%!";
      false, PatOption None

  | SpecOption _, Some _, None ->
      log "deconstruct: option type (Some/None).\n%!";
      false, PatOption (Some PatWildcard)

  | SpecResult (spec, _), Ok rv, Ok cv ->
      log "deconstruct: result type (Ok/Ok).\n%!";
      let ok, pat = deconstruct env rv cv spec in
      ok, PatResult (Ok pat)

  | SpecResult (_, spec), Error rv, Error cv ->
      log "deconstruct: result type (Error/Error).\n%!";
      let ok, pat = deconstruct env rv cv spec in
      ok, PatResult (Error pat)

  | SpecResult _, Error _, Ok _ ->
      log "deconstruct: option type (Error/Ok).\n%!";
      false, PatResult (Error PatWildcard)

  | SpecResult _, Ok _, Error _ ->
      log "deconstruct: option type (Ok/Error).\n%!";
      false, PatResult (Ok PatWildcard)

  | SpecList _, [], [] ->
      log "deconstruct: list type (Nil/Nil).\n%!";
      true, PatNil

  | SpecList (_, element), rv :: rvs, cv :: cvs ->
      log "deconstruct: list type (Cons/Cons).\n%!";
      (* Unfold [SpecList] on the fly. A nonempty list is viewed as an
         application of a unary [Cons] data constructor to a pair of an
         element and a list. *)
      let ok, pat =
        deconstruct env (rv, rvs) (cv, cvs) (SpecPair (element, spec)) in
      ok, PatCons pat

  | SpecList _, [], _ :: _ ->
      log "deconstruct: list type (Nil/Cons).\n%!";
      false, PatNil

  | SpecList _, _ :: _, [] ->
      log "deconstruct: list type (Cons/Nil).\n%!";
      false, PatCons PatWildcard

  | SpecNondet spec, _, _ ->
      log "deconstruct: SpecNondet.\n%!";
      (* We expect the reference value [rv] to be a function, which expects
         the candidate value [cv] as an argument, and returns a diagnostic. *)
      begin match rv cv with
      | Valid rv ->
          (* The reference implementation is happy with the candidate value
             [cv] and produces its own value [rv]. Continue. *)
          deconstruct env rv cv spec
      | Invalid assertion ->
          (* The reference implementation is unhappy with [cv] and produces
             an assertion that explains the problem. *)
          false, PatObserveDisagreement (assertion ovar)
      | exception e ->
          (* The reference implementation inexplicably fails. *)
          raise (Deconstruct (ReferenceFailure (freeze e)))
      end

  (* Recursive specifications are unfolded on the fly. *)
  | SpecDeferred spec, _, _ ->
      deconstruct env rv cv (Lazy.force spec)

  (* [SpecIfPol] is interpreted on the fly. Its first second represents
     the deconstruction side. *)
  | SpecIfPol (_, spec), _, _ ->
      deconstruct env rv cv spec

  (* We do not expect the following cases to arise. *)
  | SpecConstructible _, _, _ ->
      assert false
  | SpecSubset _, _, _ ->
      assert false
  | SpecArrow _, _, _ ->
      assert false
  | SpecDependentArrow _, _, _ ->
      assert false
  | SpecMapInto _, _, _ ->
      assert false
  | SpecMapOutof _, _, _ ->
      assert false

(* -------------------------------------------------------------------------- *)

(* Well-formedness checks. *)

(* The user-provided function [check] is expected to perform some kind of
   internal well-formedness check. It can fail by raising an arbitrary
   exception [e], perhaps an [Assert_failure]. It has access to both the
   reference value and the candidate value, so (if desired) it can check that
   the candidate data structure conforms to its logical model. *)

let perform_checks env past =
  Env.foreach env (fun x (Value (spec, rv, cv)) ->
    (* We have a variable [x], its relational type [spec],
       its value [rv] in the reference implementation, and
       its value [cv] in the candidate implementation. *)
    match spec with
    | SpecBaseAbstract (_, properties) ->
        let check = Code.value (properties.aty_check rv) in
        begin try
          check cv
        with e ->
          raise_abort env past (CheckFailure (x, (freeze e)))
        end
    | _ ->
        assert false (* every variable has abstract type *)
  )

(* -------------------------------------------------------------------------- *)

(* The engine's main loop. *)

(* [fuel] is the number of instructions that we are still allowed to generate.
   [past] is a reversed list of the instructions generated and executed so
   far. *)

let rec test fuel past env : unit =

  (* If the maximum number of instructions has been reached, stop. Otherwise,
     generate an instruction. *)

  if fuel > 0 then match generate_instruction env with

  | exception Gen.OutOfInputData ->
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
      test fuel past env

  | exception IllFormedSpec (op, msg) ->
      (* The generation of an instruction involves a call to [normalize],
         which can fail if a specification is ill-formed. *)
      error "in the specification of operation `%s`:\n%s" op msg

  | exception e ->
      (* Another exception indicates an abnormal condition, perhaps a mistake
         in the generation code. It should be reported, so it can be fixed. *)
      printf "An exception was raised during generation!\n";
      raise e

  | _, Error (ReferenceFailure { e = PleaseBackOff; _ }) ->
      (* The reference implementation does not allow the operation that was
         chosen, or some particular use of this operation. Abandon this
         attempt and try some other operation within the current scenario. The
         reference implementation runs first and is not allowed to perform a
         side effect before raising [PleaseBackOff], so this is safe. *)
      (* We do not decrease any parameter in the recursive call: although this
         could cause nontermination, I don't think it can be a problem, as
         afl-fuzz will abort every run at some point anyway. *)
      test fuel past env

  | _, Error (CandidateFailure { e = Unimplemented; _ }) ->
      (* The reference implementation does not allow the operation that was
         chosen, or some particular use of this operation. Abandon this
         attempt entirely and start a new scenario. *)
      ()

  | i, Error failure ->
      let past = i :: past in
      raise_abort env past failure

  | I (pat, expr), Ok v ->
      assert (pat = PatWildcard);

      (* Both the reference implementation and the candidate implementation
         have run successfully. An instruction [i] has just been constructed
         and executed, and a value [v] has been obtained as a result. *)

      (* Out of [v], we extract a reference value [rv] and a candidate value
         [cv], described by [spec]. *)

      let Value (spec, rv, cv) = v in

      (* We now deconstruct these values. While doing so, we check that they
         are in agreement; if so, we construct a pattern and extend the
         environment; if not, we also construct a pattern (one that explains
         the disagreement) and stop. *)

      match deconstruct env rv cv spec with

      | exception Deconstruct failure ->
          (* We do not have an exact context, so we use a wildcard pattern.
             The scenario that we print will not quite explain the problem,
             but it cannot explain the problem anyway, as the scenario is
             phrased in terms of the candidate implementation, and the error
             here lies in the reference implementation, which has a different
             type and takes one more argument, as [nondet] is used. *)
          let pat = PatWildcard in
          let past = I (pat, expr) :: past in
          raise_abort env past failure

      | ok, pat ->

      (* Record this instruction. *)
      let past = I (pat, expr) :: past in

      if not ok then begin

          (* The values [rv] and [cv] disagree in a concrete way, e.g., at
             a concrete type, or at a sum type. The pattern [pat] exhibits
             the problem; it is a pattern that matches [rv] but does not
             match [cv]. *)

          (* Report the problem. *)
          raise_abort env past ObservationFailure

        end
        else begin

          (* [pat] is a pattern that matches both [rv] and [cv]. *)

          (* Consume one unit of fuel. *)
          let fuel = fuel - 1 in

          (* Make sure that every data structure is well-formed. *)
          perform_checks env past;

          (* Continue. *)
          test fuel past env

        end

(* -------------------------------------------------------------------------- *)

(* In order to save the cost of allocating a fresh (empty) environment for
   every run, we re-use the same environment over and over. The operation
   [Env.clear] is extremely cheap. *)

let stored : env option ref =
  ref None

let env fuel : env =
  match !stored with
  | None ->
      (* Compute a bound on the size of the environment. This should ideally
         be computed by multiplying [fuel] by the maximum number of variables
         that a single operation can bind, that is, the maximum number of
         results of abstract type that an operation returns. For the moment,
         let's estimate this maximum number to be 5. *)
      let bound = 5 * fuel in
      (* Create an empty initial environment. *)
      let dummy_value = Value (SpecUnit, (), ()) in
      let env = Env.empty bound dummy_value in
      (* Store it for later re-use. *)
      stored := Some env;
      (* Return it. *)
      env
  | Some env ->
      (* We assume that every run uses the same value of [fuel], so the
         existing environments have an appropriate maximum size. *)
      Env.clear env;
      env

(* -------------------------------------------------------------------------- *)

(* Initialization. *)

let test fuel =
  log "Beginning one test run.\n%!";
  section begin fun () ->
    (* No past. *)
    let past = [] in
    (* Run. *)
    let env = env fuel in
    test fuel past env;
    (* Done. *)
    log "This test run is finished.\n%!"
  end

(* -------------------------------------------------------------------------- *)

(* [run prologue fuel] performs one test run. *)

(* The function [prologue] is executed after the source channel has been
   opened, so it is allowed to call the data generation functions in the
   module [Gen]. It is also allowed to call [declare], [override_exn_eq],
   [dprintf], etc. Thus, it can modify our global state in several ways. *)

let run prologue fuel =
  (* Restore a correct initial state for this iteration. *)
  GlobalState.reset();
  (* Execute [prologue()], which can declare new operations, etc. *)
  match prologue() with
  | exception Gen.OutOfInputData ->
      log "Prologue: input data exhausted; abandoning.\n"
  | exception Gen.Reject ->
      log "Prologue: generation failure; abandoning.\n"
  | exception e ->
      printf "Prologue: an exception was raised!\n";
      raise e
  | _ ->
      (* Run the test. *)
      test fuel

(* -------------------------------------------------------------------------- *)

(* [main_afl source prologue fuel] performs many test runs in AFL mode. *)

(* [source] is the name of the input file that AFL has created for us. *)

(* We use [AflPersistent.run] to perform many test runs, say 1001. Each run
   must open and close the source file, as it is recreated afresh by AFL for
   each run. *)

let main_afl source prologue fuel =
  AflPersistent.run (fun () ->
    Gen.with_source source (fun () ->
      try
        run prologue fuel
      with Abort (scenario, _fuel) ->
        (* Print the scenario to [stdout]. This is necessary for the user
           to be able to find out what happened. *)
        output_string stdout scenario;
        flush stdout;
        abort()
    )
  )

(* -------------------------------------------------------------------------- *)

(* [main_random source prologue fuel] performs an unbounded number of test
   runs in random mode. *)

(* We use an explicit infinite loop to perform an unbounded number of runs.
   The source channel (/dev/urandom) is opened just once, as it would be
   pointless to close it and reopen it many times. *)

(* We arrange to print information roughly every second. We print how many
   tests have been performed so far, our overall average speed, and our
   current speed. *)

let rec main_random source prologue fuel =
  try
    assert (source = None);
    Gen.with_source source begin fun () ->
      let granularity = 1000 in
      let clock = Clock.make granularity in
      (* An infinite loop. *)
      while true do
        (* Perform one run. *)
        run prologue fuel;
        (* Roughly every second, display statistics. *)
        Clock.tick clock begin fun () ->
          printf "%s tests run so far (%s/s overall, %s/s now) (fuel = %d).\n%!"
            (summarize (Clock.ticks clock))
            (summarize (Clock.overall_ticks_per_second clock))
            (summarize (Clock.current_ticks_per_second clock))
            fuel
        end
      done
    end
  with Abort (scenario, fuel) ->
    (* Print the scenario to [stdout]. This is necessary for the user
       to be able to find out what happened. Also, log it to a file
       in the directory ./output/crashes. *)
    output_string stdout scenario;
    print_newline();
    flush stdout;

    let temp_dir = "./output/crashes" in
    mkdirp temp_dir;
    let prefix = sprintf "scenario.%03d." fuel
    and suffix = "" in
    let _, oc = Filename.open_temp_file ~temp_dir prefix suffix in
    output_string oc scenario;
    close_out_noerr oc;

    (* We have been able to find a problem with a certain amount of
       fuel. Try again, with this amount. This restricts our search
       space, and (with luck) we might now be able to find an even
       shorter scenario. *)
    main_random source prologue fuel

(* -------------------------------------------------------------------------- *)

(* Find the name of our input file on the command line. *)

let source : string option =
  let usage = sprintf "Usage: %s <input file>" Sys.argv.(0) in
  let source = ref None in
  Arg.parse [] (fun s -> source := Some s) usage;
  !source

(* -------------------------------------------------------------------------- *)

(* Reading input data from the source file whose name was given on the command
   line, perform a number of test runs. *)

let none () =
  ()

let main ?prologue:(prologue=none) fuel =
  (* Printing the following two lines at the beginning of every scenario
     allows us to use [Monolith.Support], under the name [Sup], in our
     scenarios. *)
  DelayedOutput.dprintf "          #require \"monolith\";;\n";
  DelayedOutput.dprintf "          module Sup = Monolith.Support;;\n";
  (* We are about to perform many runs, and each run begins with a call to
     [prologue] that can modify our global state by calling functions such
     as [declare], [override_exn_eq], [dprintf], etc. Therefore, we must
     save our global state now, just once, and reset it at the beginning
     of every run, before calling [prologue]. *)
  GlobalState.save();
  match source with
  | Some _ ->
      main_afl source prologue fuel
  | None ->
      main_random source prologue fuel

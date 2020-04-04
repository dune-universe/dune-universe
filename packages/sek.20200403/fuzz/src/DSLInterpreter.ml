(******************************************************************************)
(*                                                                            *)
(*                                    Sek                                     *)
(*                                                                            *)
(*          Arthur Charguéraud, Émilie Guermeur and François Pottier          *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU Lesser General Public License as published by the Free   *)
(*  Software Foundation, either version 3 of the License, or (at your         *)
(*  option) any later version, as described in the file LICENSE.              *)
(*                                                                            *)
(******************************************************************************)

open Misc
open Printing
open Signature
open DSLSyntax
let sp_element = sp_int

module Make (S : SEQUENCES) = struct

  open S

  type eseq = element E.t
  type pseq = element P.t

  (* A runtime environment maps variables to values. We have several types of
     variables: a variable may denote an ephemeral sequences or a persistent
     sequences. We choose to write the interpreter in an untyped style (with
     tags and runtime tests) because this is more convenient and flexible.
     Provided the generator produces well-typed code, no runtime tag test will
     ever fail. *)

  type value =
    | VESeq of eseq
    | VPSeq of pseq

  let is_eseq = function
    | VESeq _ -> true
    | VPSeq _ -> false

  let is_pseq = function
    | VESeq _ -> false
    | VPSeq _ -> true

  let extract_eseq = function
    | VESeq s -> s
    | VPSeq _ -> assert false (* dynamic tag test *)

  let extract_pseq = function
    | VESeq _ -> assert false (* dynamic tag test *)
    | VPSeq s -> s

  let check_wf = function
    | VESeq s -> E.check s
    | VPSeq s -> P.check s

  let length = function
    | VESeq s -> E.length s
    | VPSeq s -> P.length s

  let is_empty = function
    | VESeq s -> E.is_empty s
    | VPSeq s -> P.is_empty s

  let get = function
    | VESeq s -> E.get s
    | VPSeq s -> P.get s

  let peek_opt side = function
    | VESeq s -> E.peek_opt side s
    | VPSeq s -> P.peek_opt side s

  let concat v1 v2 =
    match v1, v2 with
    | VESeq s1, VESeq s2 ->
        VESeq (E.concat s1 s2)
    | VPSeq s1, VPSeq s2 ->
        VPSeq (P.concat s1 s2)
    | VPSeq _, VESeq _
    | VESeq _, VPSeq _ ->
        assert false (* type error *)

  let split s i =
    match s with
    | VESeq s ->
        let s1, s2 = E.split s i in
        VESeq s1, VESeq s2
    | VPSeq s ->
        let s1, s2 = P.split s i in
        VPSeq s1, VPSeq s2

  let to_array = function
    | VESeq s -> E.to_array s
    | VPSeq s -> P.to_array s

  let iteri direction f = function
    | VESeq s -> E.iteri direction f s
    | VPSeq s -> P.iteri direction f s

  type env =
    value list

  let check_wf env =
    List.iter check_wf env

  (* The empty environment. *)

  let empty =
    []

  (* Environment lookup. *)

  let lookup : env -> var -> value =
    lookup

  let elookup env x =
    extract_eseq (lookup env x)

  let plookup env x =
    extract_pseq (lookup env x)

  (* Environment inspection functions. *)

  (* TODO can be optimized *)

  let nevars env =
    List.length (List.filter is_eseq env)

  let npvars env =
    List.length (List.filter is_pseq env)

  let nseqvars env =
    List.length env

  let evars env : var list =
    index 0 env
      |> List.filter (fun (_i, v) -> is_eseq v)
      |> List.map fst

  let pvars env : var list =
    index 0 env
      |> List.filter (fun (_i, v) -> is_pseq v)
      |> List.map fst

  let seqvars env : var list =
    index 0 env
      |> List.map fst

  (* Environment extension. *)

  let bind env (v : value) : env =
    v :: env

  let ebind env s =
    bind env (VESeq s)

  let pbind env s =
    bind env (VPSeq s)

  (* Checking that an actual observation matches an expected observation. *)

  (* We assume that polymorphic equality can be used. *)

  let check sp (expected : 'a observation) (actual : 'a) =
    match expected with
    | None ->
        ()
    | Some expected ->
        if expected <> actual then
          Fuzz.fail
            "Fatal error: discrepancy: \
             reference finds %a, candidate finds %a"
            sp expected
            sp actual

  (* Interpreting a syntactic side as a semantic side. *)

  let interpret_side side =
    match side with
    | Front ->
        front
    | Back ->
        back

  let interpret_direction direction =
    match direction with
    | Forward ->
        forward
    | Backward ->
        backward

  (* The interpreter maps an unannotated program to an annotated program:
     it records the result of every [pop] operation. If the program is
     annotated already, then the interpreter checks that every [pop]
     operation produces the expected result. *)

  let interpret env (i : instruction) : instruction * env =
    (* Then, execute this instruction. *)
    match i with
    | Nop ->
        Nop, env
    | Push (side, x, e) ->
        (* Push [e] onto sequence [x]. *)
        E.push (interpret_side side) (elookup env x) e;
        i, env
    | Pop (side, expected, x) ->
        (* Pop [eo2] off sequence [x]; compare observables. *)
        let actual = E.pop_opt (interpret_side side) (elookup env x) in
        check (sp_option sp_element) expected actual;
        (* Annotate the instruction with an observation. *)
        Pop (side, Some actual, x), env
    | Clear x ->
        (* Clear sequence [x]. *)
        E.clear (elookup env x);
        i, env
    | Assign (x1, x2) ->
        E.assign (elookup env x1) (elookup env x2);
        i, env
    | LetCopy x ->
        (* Copy sequence [x]; bind ephemeral variable 0. *)
        let s = E.copy (elookup env x) in
        i, ebind env s
    | LetCarve (side, x, k) ->
        let s = elookup env x in
        (* Carve sequence [x]; bind ephemeral variable 0. *)
        let s = E.carve (interpret_side side) s k in
        i, ebind env s
    | Set (x, k, e) ->
        let s = elookup env x in
        E.set s k e;
        i, env
    | Append (side, x1, x2) ->
        assert (x1 <> x2); (* by construction; should never fail *)
        E.append (interpret_side side) (elookup env x1) (elookup env x2);
        i, env
    | LetCreate (kind, e) ->
        (* Create new sequence; bind variable 0. *)
        let v = match kind with
          | E -> VESeq (E.create e)
          | P -> VPSeq (P.create e)
        in
        i, bind env v
    | LetMake (kind, d, n, e) ->
        let v = match kind with
          | E -> VESeq (E.make d n e)
          | P -> VPSeq (P.make d n e)
        in
        i, bind env v
    | LetInit (kind, d, n) ->
        (* As an argument to [init], we use [delay (ref 0) (fun i -> i)], a
           function that does not commute with itself. This allows us to
           detect an [init] function that issues calls to [f] in an incorrect
           order. *)
        let delay r f y = let x = !r in r := f y; x in
        let v = match kind with
          | E -> VESeq (E.init d n (delay (ref 0) (fun i -> i)))
          | P -> VPSeq (P.init d n (delay (ref 0) (fun i -> i)))
        in
        i, bind env v
    | LetOfArray (kind, e, n) ->
        let a = Array.init n (fun x -> x) in
        let v = match kind with
          | E -> VESeq (E.of_array e a)
          | P -> VPSeq (P.of_array e a)
        in
        i, bind env v
    | LetOfArraySegment (kind, e, n, h, k) ->
        let a = Array.init n (fun x -> x) in
        let v = match kind with
          | E -> VESeq (E.of_array_segment e a h k)
          | P -> VPSeq (P.of_array_segment e a h k)
        in
        i, bind env v
    | LetPPush (side, x, e) ->
        (* Push [e] onto sequence [x]. *)
        let s = P.push (interpret_side side) (plookup env x) e in
        (* Bind persistent variable 0. *)
        i, pbind env s
    | LetPPop (side, expected, x) ->
        (* Pop an observation off sequence [x]; compare observables. *)
        let actual, s = P.pop_opt (interpret_side side) (plookup env x) in
        check (sp_option sp_element) expected actual;
        (* Annotate the instruction with an observation. *)
        (* Bind persistent variable 0. *)
        LetPPop (side, Some actual, x), pbind env s
    | LetConcat (kind, x1, x2) ->
        if kind = E then assert (x1 <> x2); (* by construction; should never fail *)
        (* Concatenate the sequences [x1] and [x2]. Bind variable 0. *)
        let v = concat (lookup env x1) (lookup env x2) in
        i, bind env v
    | LetSplit (_, x, k) ->
        let v = lookup env x in
        (* Split sequence [x]; bind variables 0 and 1. I think it does not
           matter which variable is bound to which sequence, as long as the
           DSL interpreter and the DSL printer agree. *)
        let v1, v2 = split v k in
        i, bind (bind env v1) v2
    | LetPSet (x, k, e) ->
        let s = plookup env x in
        let s' = P.set s k e in
        i, pbind env s'
    | Length (expected, x) ->
        let actual = length (lookup env x) in
        check sp_int expected actual;
        (* Annotate the instruction with an observation. *)
        Length (Some actual, x), env
    | IsEmpty (expected, x) ->
        let actual = is_empty (lookup env x) in
        check sp_bool expected actual;
        (* Annotate the instruction with an observation. *)
        IsEmpty (Some actual, x), env
    | Peek (side, expected, x) ->
        (* Pop an observation off sequence [x]; compare observables. *)
        let actual = peek_opt (interpret_side side) (lookup env x) in
        check (sp_option sp_element) expected actual;
        (* Annotate the instruction with an observation. *)
        Peek (side, Some actual, x), env
    | Get (expected, x, k) ->
        let s = lookup env x in
        let actual = get s k in
        check sp_element expected actual;
        (* Annotate the instruction with an observation. *)
        Get (Some actual, x, k), env
    | ToArray (expected, x) ->
        let actual = to_array (lookup env x) in
        check (sp_array sp_element) expected actual;
        ToArray (Some actual, x), env
    | Iteri (expected, direction, x) ->
        let ixs = ref [] in
        let f i x = ixs := (i, x) :: !ixs in
        iteri (interpret_direction direction) f (lookup env x);
        let actual = List.rev !ixs in
        check (sp_list (sp_pair sp_int sp_element)) expected actual;
        Iteri (Some actual, direction, x), env
    | LetSnapshot x ->
        (* Take a snapshot of sequence [x]; bind persistent variable 0. *)
        let s = snapshot (elookup env x) in
        i, pbind env s
    | LetEdit x ->
        (* Edit sequence [x]; bind ephemeral variable 0. *)
        let s = edit (plookup env x) in
        i, ebind env s
    | LetSnapshotAndClear x ->
        (* Freeze sequence [x]; bind persistent variable 0. *)
        let s = snapshot_and_clear (elookup env x) in
        i, pbind env s

  let length env x =
    length (lookup env x)

end

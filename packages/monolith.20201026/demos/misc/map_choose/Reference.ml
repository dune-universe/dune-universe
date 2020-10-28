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

(* We use OCaml's Map module, specialized to integer keys and integer values,
   as a reference implementation. *)

include Map.Make(Int)

(* Define the type [map]. *)

type map =
  int t

(* Some boilerplate is required in order to satisfy the signature [S]. *)

let union f m1 m2 =
  union (fun _key v1 v2 -> Some (f v1 v2)) m1 m2

(* [choose_opt] is nondeterministic. *)

open Monolith
open Monolith.Print
open PPrint

let expected_some_result () =
  (* The map is nonempty, yet the candidate returned nothing. *)
  Invalid (fun observed ->
    assert_ (observed ^^ string " <> None") ^^
    comment (string "The map is supposed to be nonempty.")
      (* If desired, we could print a list of the keys which
         we expect to exist in the map. *)
  )

let nonexistent_key k =
  (* The key [k] does not exist in the map. *)
  Invalid (fun _ -> comment (utf8format
    "The key %d is not supposed to exist in the map." k))

let incorrect_value k rv cv =
  Invalid (fun _ -> comment (utf8format
    "The key %d is supposed to be associated with %d, not %d."
    k rv cv))

let candidate_has_returned candidate m k cv =
  match find_opt k m with
  | None ->
      nonexistent_key k
  | Some rv ->
      if rv = cv then
        Valid candidate
      else
        incorrect_value k rv cv

let choose_opt (m : int t) (candidate : (key * int) option)
: (key * int) option diagnostic =
  match is_empty m, candidate with
  | false, None ->
      expected_some_result()
  | true, None ->
      Valid candidate
  | _, Some (k, cv) ->
      candidate_has_returned candidate m k cv

(* [choose] is nondeterministic. *)

(* We could avoid code duplication simply by testing [C.choose] indirectly:
   wrap it so that it has the same type as [C.choose_opt], then test it
   exactly like [C.choose_opt]. Here, as an exercise, we give a direct
   reference implementation for [choose_opt]. *)

let choose (m : int t) (candidate : (key * int, exn) result)
: (key * int, exn) result diagnostic =
  match is_empty m, candidate with
  | false, Error Not_found ->
      expected_some_result()
  | true, Error Not_found ->
      Valid candidate
  | _, Error e ->
      (* The candidate has raised some exception other than [Not_found]. *)
      Invalid (fun _ -> comment (utf8format
        "Candidate has raised %s." (Printexc.to_string e)))
  | _, Ok (k, cv) ->
      candidate_has_returned candidate m k cv

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

let choose_opt (m : int t) (candidate : (key * int) option)
: (key * int) option diagnostic =
  match is_empty m, candidate with
  | false, None ->
      (* The map is nonempty, yet the candidate returned nothing. *)
      Invalid (fun observed ->
        assert_ (observed ^^ string " <> None") ^^
        comment (string "The map is supposed to be nonempty.")
          (* If desired, we could print a list of the keys which
             we expect to exist in the map. *)
      )
  | true, None ->
      Valid candidate
  | _, Some (k, v) ->
      match find_opt k m with
      | None ->
          (* The key [k] does not exist in the map. *)
          Invalid (fun _ -> comment (utf8format
            "The key %d is not supposed to exist in the map." k))
      | Some v' ->
          if v = v' then
            Valid candidate
          else
            Invalid (fun _ -> comment (utf8format
              "The key %d is supposed to be associated with %d, not %d."
              k v' v))

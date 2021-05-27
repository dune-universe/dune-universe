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

(* A reference implementation of a bag, based on a reference to a
   linked list. *)

open PPrint
open Monolith
open Monolith.Print

type 'a t =
  'a list ref

let create () =
  ref []

let add x bag =
  bag := x :: !bag

let elements bag =
  !bag

let mem x bag =
  List.mem x !bag

let rec remove x xs =
  match xs with
  | [] ->
      assert false
  | x' :: xs ->
      if x = x' then xs else x' :: remove x xs

let remove x bag =
  bag := remove x !bag

(* [print_mem o bag] produces code for an OCaml expression of type [bool]
   which expresses the fact that the variable [o] of type [int option] is
   equal to [Some c], where [c] ranges over the elements of the bag [bag].
   E.g., if the bag contains the elements 1 and 2, then [print_mem "observed"
   bag] produces the string ["observed = Some 1 || observed = Some 2"]. *)

let print_mem (o : document) (xs : int list) : document =
  flow_map
    (break 1 ^^ string "||" ^^ space)
    (fun x -> o ^^ utf8format " = Some %d" x)
    xs

let print_mem o bag =
  print_mem o !bag

let extract (bag : 'a t) (ox : 'a option) : 'a option diagnostic =
  (* Our mission is to determine whether [ox] is a valid result
     for the operation [extract bag]. *)
  match !bag, ox with
  | [], Some x ->
      (* The bag is empty, yet the candidate is able to extract an element.
         Declare that this is invalid, and construct an assertion that states
         that we expected the candidate to return [None]. *)
      Invalid (fun o ->
        assert_ (o ^^ string " = None") ^^
        candidate_finds (option int (Some x))
      )
  | _ :: _, None ->
      (* The bag is nonempty, yet the candidate returns [None]. *)
      Invalid (fun o ->
        assert_ (o ^^ string " <> None") ^^
        candidate_finds (string "None")
      )
  | [], None ->
      Valid ox
  | _, Some x ->
      (* The bag is nonempty and the candidate extracts [x]. We must check
         that [x] is indeed an element of the bag, and remove it, so that the
         reference and the candidate remain in sync. *)
      (* To implement this logic, we need the operations [mem] and [remove]
         to exist in the reference implementation. They need not exist in
         the candidate implementation. *)
      if mem x bag then begin
        remove x bag;
        Valid ox
      end
      else
        Invalid (fun o ->
          assert_ (print_mem o bag) ^^
          candidate_finds (option int (Some x))
        )

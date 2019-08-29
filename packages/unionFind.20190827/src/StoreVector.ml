(***************************************************************************)
(*                                                                         *)
(*                                 UnionFind                               *)
(*                                                                         *)
(*                       François Pottier, Inria Paris                     *)
(*                                                                         *)
(*  Copyright Inria. All rights reserved. This file is distributed under   *)
(*  the terms of the GNU Library General Public License version 2, with a  *)
(*  special exception on linking, as described in the file LICENSE.        *)
(***************************************************************************)

(* A store is implemented as an extensible array, that is, a pair of an
   integer address and an array. We maintain the invariant that the length of
   the array is at least [limit]. The area of the array at index [limit] and
   beyond is considered uninitialized. *)

(* In the current implementation, this area is filled with arbitrary value(s)
   provided by the user in calls to [make] or [set]. This is not ideal, as it
   can cause a memory leak. *)

type 'a store = {
  (* The logical size of the array; also, the next available address. *)
  mutable limit:   int;
  (* The array, whose length is at least [limit]. *)
  mutable content: 'a array
}

(* The array is created with a size and length of zero. We have no other
   choice, since we do not have a value of type ['a] at hand. *)

let new_store () : 'a store = {
  limit = 0;
  content = [||]
}

(* A reference is an index into the array. *)

type 'a rref =
  int

(* The array jumps from length zero to length [default_initial_length] as soon
   as a call to [make] is made. *)

let default_initial_length =
  256

(* [enlarge s v] increases the length of the array (if necessary) so as to
   ensure that [s.limit] becomes a valid index. The argument [v] is used as a
   default value to fill the uninitialized area. *)

let enlarge (s : 'a store) (v : 'a) : unit =
  let content = s.content in
  let length = Array.length content in
  if s.limit = length then begin
    let length' =
      if length = 0 then
        default_initial_length
      else
        2 * length
    in
    assert (s.limit < length');
    let content' = Array.make length' v in
    Array.blit content 0 content' 0 length;
    s.content <- content'
  end

(* Note that we cannot use [Array.unsafe_set] and [Array.unsafe_get] without
   any precautions, since the OCaml type-checker cannot guarantee that the
   indices are in range. A confused user could pass references into some
   other store. We choose to explicitly check that the index is within the
   logical bounds of the array -- this is a more precise check. *)

exception InvalidRef

let check (s : 'a store) (x : 'a rref) : unit =
  (* We do not check that [x] is nonnegative. An overflow cannot occur,
     since that would imply that we have filled the memory with a huge
     array. *)
  if x >= s.limit then
    raise InvalidRef

let make (s : 'a store) (v : 'a) : 'a store * 'a rref =
  enlarge s v;
  let x = s.limit in
  s.limit <- x + 1;
  Array.unsafe_set s.content x v;
  s, x

let get (s : 'a store) (x : 'a rref) : 'a store * 'a =
  check s x;
  s,
  Array.unsafe_get s.content x

let set (s : 'a store) (x : 'a rref) (v : 'a) : 'a store =
  check s x;
  Array.unsafe_set s.content x v;
  s

let eq  (s : 'a store) (x : 'a rref) (y : 'a rref) : 'a store * bool =
  check s x;
  check s y;
  s, x = y

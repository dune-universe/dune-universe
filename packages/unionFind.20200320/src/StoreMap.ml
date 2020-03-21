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

module type INTMAP = sig
  type 'a t
  val empty: 'a t
  val find: int -> 'a t -> 'a
  val add: int -> 'a -> 'a t -> 'a t
end

module Make (IntMap : INTMAP) = struct

(* A store is implemented as a pair of an integer address and a map of
   integer addresses to values. We maintain the invariant that the
   domain of the map [content] is the semi-open interval of zero
   (included) up to [limit] (excluded). *)

type 'a store = {
  (* The next available address. *)
  limit:   int;
  (* The content of the store. *)
  content: 'a IntMap.t
}

let new_store () = {
  limit = 0;
  content = IntMap.empty
}

(* A reference is just an integer address. *)

type 'a rref =
  int

let make s v =
  let x = s.limit in
  {
    limit = x + 1;
    content = IntMap.add x v s.content
  },
  x

(* [check s x] checks that the address [x] is in range for the store [s]. If
   this dynamic check fails, then the user is confused and has passed us an
   address that is associated with some other store. (If the check succeeds,
   the user may be confused too! but we cannot detect it.) *)

exception InvalidRef

let check (s : 'a store) (x : 'a rref) =
  (* We do not check that [x] is nonnegative. An overflow cannot occur,
     since that would imply that we have filled the memory with a huge
     array. *)
   if x >= s.limit then
     raise InvalidRef

let get s x =
  (* Failure of this assertion would indicate that the user has passed us
     an address that is associated with some other store. *)
  check s x;
  s,
  IntMap.find x s.content

let set s x v =
  (* Failure of this assertion would indicate that the user has passed us
     an address that is associated with some other store. *)
  check s x;
  {
    limit = s.limit;
    content = IntMap.add x v s.content
  }

let eq s (x : int) (y : int) =
  (* Failure of this assertion would indicate that the user has passed us
     an address that is associated with some other store. *)
  check s x;
  check s y;
  s,
  x = y

end

(* Include the most common instance of the above functor. *)

include Make(Map.Make(struct
  type t = int
  let compare = (-) (* ok since the arguments are nonnegative integers *)
end))

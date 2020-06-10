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

open Eq

(* -------------------------------------------------------------------------- *)

(* An extensible type of tags. *)

(* A tag of type ['a tag] is a runtime representation of the base type ['a]. *)

type _ tag = ..

(* -------------------------------------------------------------------------- *)

(* An extensible function [equal]. *)

(* [equal tag1 tag2] compares the tags [tag1] and [tag2] for equality. If the
   comparison fails, the exception [RuntimeTagError] is raised. If it
   succeeds, a runtime witness of type equality is returned. *)

exception RuntimeTagError

type equal =
  { mutable equal_hook : 'a1 'a2. 'a1 tag -> 'a2 tag -> ('a1, 'a2) eq }

let equal_hook =
  { equal_hook = (fun _tag1 _tag2 -> assert false) }

let equal tag1 tag2 =
  equal_hook.equal_hook tag1 tag2

(* -------------------------------------------------------------------------- *)

(* The functor [NewTag] extends the type [_ tag] with a new data constructor. *)

module NewTag (X : sig
  type t
end) = struct

  (* Extend the type [tag] with a new data constructor [Tag]. *)

  type _ tag +=
  | Tag : X.t tag

  (* Extend the function [equal] with a new case for [Tag]. *)

  let () =
    let next = equal_hook.equal_hook in
    let equal (type a1 a2) (tag1 : a1 tag) (tag2 : a2 tag) : (a1, a2) eq =
      match tag1 with
      | Tag ->
          begin match tag2 with
          | Tag ->
              Eq
          | _ ->
              raise RuntimeTagError
          end
      | _ ->
          next tag1 tag2
    in
    equal_hook.equal_hook <- equal

end

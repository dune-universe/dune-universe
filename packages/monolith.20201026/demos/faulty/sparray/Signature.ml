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

(* The signature of persistent arrays. *)

module type S = sig

  type 'a t

  val make : int -> 'a -> 'a t

  val length : 'a t -> int

  val get : 'a t -> int -> 'a

  val set : 'a t -> int -> 'a -> 'a t

  val to_list : 'a t -> 'a list

end

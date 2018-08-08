(***********************************************************************)
(*                                                                     *)
(*                            OCamlSpotter                             *)
(*                                                                     *)
(*                             Jun FURUSE                              *)
(*                                                                     *)
(*   Copyright 2008-2014 Jun Furuse. All rights reserved.              *)
(*   This file is distributed under the terms of the GNU Library       *)
(*   General Public License, with the special exception on linking     *)
(*   described in file LICENSE.                                        *)
(*                                                                     *)
(***********************************************************************)

(* Set with binary custom search *)

module type OrderedType = sig
  include Set.OrderedType
  val format : Format.formatter -> t -> unit
end

module type S = sig
  include Set.S

  (* They use Obj.magic, so unsafe! *)
  val unsafe_binary : t -> (t * elt * t) option
  val unsafe_middle : t -> elt option
  val unsafe_find : elt -> t -> elt option
  val unsafe_dump : Format.formatter -> t -> unit
end

module Make(Ord : OrderedType) : S 
  with type elt = Ord.t

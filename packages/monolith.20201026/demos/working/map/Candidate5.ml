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

(* The diet trees provided by Batteries Included. *)

include BatIMap

let empty = empty ~eq:(=)

let singleton k v = singleton ~eq:(=) k v

let update = modify_opt (* batteries >= 2.1 *)

let compare cmp m1 m2 =
  ignore (cmp, m1, m2);
  raise Monolith.Unimplemented

let equal eq m1 m2 =
  ignore (eq, m1, m2);
  raise Monolith.Unimplemented

let exists f m =
  ignore (f, m);
  raise Monolith.Unimplemented

let filter f m =
  ignore (f, m);
  raise Monolith.Unimplemented

let filter_map f m =
  ignore (f, m);
  raise Monolith.Unimplemented

let cardinal m =
  fold (fun _k _v sum -> sum + 1) m 0

let bindings m =
  fold (fun k v bindings -> (k, v) :: bindings) m []

let choose m =
  ignore m;
  raise Monolith.Unimplemented

let choose_opt m =
  ignore m;
  raise Monolith.Unimplemented

let find_opt k m =
  ignore (k, m);
  raise Monolith.Unimplemented

let map f m =
  map f m

let mapi f m =
  mapi f m

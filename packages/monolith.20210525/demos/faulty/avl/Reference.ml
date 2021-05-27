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

(* We provide a trivial reference implementation. Indeed, in this case, the
   signature of the unit-under-test is so restricted ([empty], [add], [union],
   [check]) that there is no way of observing the contents of a set. *)

type elt = int
type t = unit
let empty = ()
let add _ _ = ()
let union _ _ = ()

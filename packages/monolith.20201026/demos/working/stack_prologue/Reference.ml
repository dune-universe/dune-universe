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

(* A reference implementation of a bounded stack, based on OCaml's [Stack]
   module. *)

(* The bound [n] chosen by the client is a functor parameter. *)

module Make (X : sig
  val n : int
end) = struct
  open X

  include Stack

  let create _d =
    Stack.create()

  let is_full s =
    Stack.length s = n

end

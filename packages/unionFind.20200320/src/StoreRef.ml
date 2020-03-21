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

(* When OCaml's built-in store is used, no explicit store is needed. *)

type 'a store =
  unit

let new_store () =
  ()

(* A reference is a primitive reference. *)

type 'a rref =
  'a ref

let make () v =
  (), ref v

let get () x =
  (), !x

let set () x v =
  x := v

let eq () x y =
  (), x == y


(******************************************************************************)
(*                                                                            *)
(*                                  Inferno                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the MIT License, as described in the file LICENSE.               *)
(*                                                                            *)
(******************************************************************************)

(* This module implements a transactional variant of the union-find
   algorithm. It uses transactional references instead of ordinary
   references, so that a series of operations performed within a
   transaction can be either committed or rolled back. *)

(* See [UnionFind] for a description of the operations. As far as the
   user is concerned, the only difference in the API is that [union]
   requires a transaction parameter. A transaction can be created by
   [TRef.tentatively]. *)

type 'a point

type 'a link

val fresh: 'a -> 'a point

val find: 'a point -> 'a

val union: 'a link TRef.transaction -> ('a -> 'a -> 'a) -> 'a point -> 'a point -> unit

val equivalent: 'a point -> 'a point -> bool

val is_representative: 'a point -> bool


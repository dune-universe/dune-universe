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

open UnifierSig

(* The unifier is parameterized over the type structure. *)

module Make (S : STRUCTURE) : UNIFIER with type 'a structure = 'a S.structure


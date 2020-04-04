(******************************************************************************)
(*                                                                            *)
(*                                    Sek                                     *)
(*                                                                            *)
(*          Arthur Charguéraud, Émilie Guermeur and François Pottier          *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU Lesser General Public License as published by the Free   *)
(*  Software Foundation, either version 3 of the License, or (at your         *)
(*  option) any later version, as described in the file LICENSE.              *)
(*                                                                            *)
(******************************************************************************)

open DSLSyntax

(* [print f pwf is] prints the instructions [is] on the output channel [f].
   If the flag [pwf] is [true], then the well-formedness checks that follow
   the last instruction are printed; otherwise no well-formedness checks are
   printed. *)

val print: out_channel -> bool -> instructions -> unit

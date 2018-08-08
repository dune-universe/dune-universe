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

(* Name is an identifier name with its stamp number. For example, 
   an ident of name "string" with a stamp 3 has the name "string__3".

   With these names, OCamlSpotter distinguishes textual
   representations of idents with the same name but with different
   stamps.
*)
type t = string (* CR jfuruse: should be abstracted? *)
val create : string -> int -> t
val parse : t -> string * int


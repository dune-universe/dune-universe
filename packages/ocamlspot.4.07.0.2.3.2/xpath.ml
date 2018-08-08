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

(* extend the original module *)
open Path
module Ident0 = Ident
module Ident = struct
  include Ident
  include Xident
end

let rec name = function
  | Pident id -> Ident.name id
  | Pdot(p, s, pos) -> Name.create (name p ^ "." ^ s) pos
  | Papply(p1, p2) -> name p1 ^ "(" ^ name p2 ^ ")"
        
let rec local = function
  | Pident id -> not (Ident.global id)
  | Pdot (p, _, _) -> local p
  | Papply(p1, _p2) -> local p1 (* ? *) 

let parse s =
  let rec to_path = function
    | Longident.Lident s -> Pident (Xident.parse s)
    | Longident.Ldot (lid, s) ->
        let s, pos = Name.parse s in
        let path = to_path lid in
        Pdot (path, s, pos)
    | Longident.Lapply (lid1, lid2) ->
        let path1 = to_path lid1 in
        let path2 = to_path lid2 in
        Papply(path1, path2)
  in
  to_path (Longident.parse s)

open Format
let format ppf p = pp_print_string ppf (name p)

let rec to_longident = 
  let open Longident in
  function
  | Pident id -> Lident (Ident0.name id )
  | Pdot (p, name, _) -> Ldot (to_longident p, name)
  | Papply (p1, p2) -> Lapply (to_longident p1, to_longident p2)

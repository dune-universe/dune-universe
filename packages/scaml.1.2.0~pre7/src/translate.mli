(**************************************************************************)
(*                                                                        *)
(*                                 SCaml                                  *)
(*                                                                        *)
(*                       Jun Furuse, DaiLambda, Inc.                      *)
(*                                                                        *)
(*                   Copyright 2019,2020  DaiLambda, Inc.                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

val contract_self_id : Ident.t
  
val implementation 
  : bool (* compile only *)
  -> string (* source file name *)
  -> string (* output prefix *)
  -> Typedtree.structure 
  -> (Michelson.Type.t * Michelson.Type.t * IML.t) option (* global entry *)
     * (IML.PatVar.t * IML.t * bool (* defined at the top *)) list (* definitions *)

val link 
  : (Michelson.Type.t * Michelson.Type.t * IML.t) (* global entry *)
  -> (IML.PatVar.t * IML.t) list (* definitions *)
  -> Michelson.Type.t * Michelson.Type.t * IML.t

val connect 
  : (IML.PatVar.t * IML.t) list (* definitions *)
  -> IML.t

val convert
  : Typedtree.structure 
  -> [> `Type of Ident.t * Michelson.Type.t | `Value of Ident.t option * IML.t ] list

val with_flags_in_code : Typedtree.structure -> (unit -> 'a) -> 'a

val reject_SCaml_attribute_in_complex_structure : Typedtree.structure -> unit

val filter_by_SCaml_attribute : Typedtree.structure -> Typedtree.structure

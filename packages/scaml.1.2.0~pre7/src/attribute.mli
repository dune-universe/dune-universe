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

val parse_options_in_payload : 
  loc:Tools.Location.t 
  -> string 
  -> Ppxlib.Parsetree.payload 
  -> (Ppxlib.Longident.t Asttypes.loc * [> `Bool of bool | `Constant of Ppxlib.Parsetree.constant ]) list

val get_scaml_toplevel_attributes 
  : Typedtree.structure 
  -> (Location.t * (Ppxlib.Longident.t Asttypes.loc * [`Bool of bool | `Constant of Ppxlib.Parsetree.constant ]) list) list

(**************************************************************************)
(*                                                                        *)
(*                                 SCaml                                  *)
(*                                                                        *)
(*                       Jun Furuse, DaiLambda, Inc.                      *)
(*                                                                        *)
(*                     Copyright 2020  DaiLambda, Inc.                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Untyped OCaml AST *)

(* Our compiler is 4.09.1, but Ppx_lib.Ast_builder of ppxlib.0.13.0 uses 4.08 *)

module Longident = Ppxlib.Longident
module Parsetree = Ppxlib.Parsetree
module Pprintast = Ppxlib.Pprintast
module Ast_builder = Ppxlib.Ast_builder.Make(struct let loc = Location.none end)
module Migrate = Migrate_parsetree__.Migrate_parsetree_409_408_migrate



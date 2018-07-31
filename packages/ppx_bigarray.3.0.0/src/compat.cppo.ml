(* ppx_bigarray --- A PPX extension for providing big array literals in OCaml

   Copyright (C) 2015 Akinori ABE
   This software is distributed under MIT License
   See LICENSE.txt for details. *)

#if OCAML_VERSION >= (4, 08, 0)
#error "OCaml 4.08 or above is unsupported."
#elif OCAML_VERSION >= (4, 07, 0)
#define AST_VNUM 407
#elif OCAML_VERSION >= (4, 06, 0)
#define AST_VNUM 406
#elif OCAML_VERSION >= (4, 05, 0)
#define AST_VNUM 405
#elif OCAML_VERSION >= (4, 04, 0)
#define AST_VNUM 404
#elif OCAML_VERSION >= (4, 03, 0)
#define AST_VNUM 403
#elif OCAML_VERSION >= (4, 02, 0)
#define AST_VNUM 402
#endif

open Migrate_parsetree

include CONCAT(Ast_, AST_VNUM)
include CONCAT(Ast_, AST_VNUM).Parsetree

let ocaml_version = Versions.CONCAT(ocaml_, AST_VNUM)


open Asttypes
open Docstrings
open Parsetree
open Astextra

let default_loc = Ast_helper.default_loc
let with_default_loc = Ast_helper.with_default_loc

type object_field = str * attributes * core_type

module Const = Ast_helper.Const
module Typ   = Ast_helper.Typ

module Pat   =
  struct
    include Ast_helper.Pat

    let open_ : ?loc:loc -> ?attrs:attrs  -> lid -> pattern -> pattern =
      fun ?loc ?attrs _ _ ->
        (* TODO probably difficult to backport. *)
        assert false
  end

module Exp   = Ast_helper.Exp
module Val  = Ast_helper.Val
module Type = Ast_helper.Type
module Te   = Ast_helper.Te
module Mty  = Ast_helper.Mty
module Mod  = Ast_helper.Mod
module Sig  = Ast_helper.Sig
module Str  = Ast_helper.Str
module Md   = Ast_helper.Md
module Mtd  = Ast_helper.Mtd
module Mb   = Ast_helper.Mb
module Opn  = Ast_helper.Opn
module Incl = Ast_helper.Incl
module Vb   = Ast_helper.Vb

module Cty  =
  struct
    include Ast_helper.Cty

    let open_ : ?loc:loc -> ?attrs:attrs  -> override_flag ->
                lid -> class_type -> class_type =
      fun ?loc ?attrs _ _ ->
        (* TODO probably difficult to backport. *)
        assert false
  end

module Ctf  = Ast_helper.Ctf
module Cf   = Ast_helper.Cf

module Cl   =
  struct
    include Ast_helper.Cl

    let open_ : ?loc:loc -> ?attrs:attrs -> override_flag -> lid -> class_expr
                -> class_expr =
      fun ?loc ?attrs _ _ ->
        (* TODO probably difficult to backport. *)
        assert false
  end

module Ci   = Ast_helper.Ci
module Csig = Ast_helper.Csig
module Cstr = Ast_helper.Cstr

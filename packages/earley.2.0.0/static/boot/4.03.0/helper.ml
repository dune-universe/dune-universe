open Asttypes
open Docstrings
open Parsetree
open Astextra

let default_loc = Ast_helper.default_loc
let with_default_loc = Ast_helper.with_default_loc

let build_loc loc =
  match loc with
  | None     -> !default_loc
  | Some loc -> loc

let build_attrs attrs =
  match attrs with
  | None       -> []
  | Some attrs -> attrs

let str_opt_to_string_opt so =
  match so with
  | None   -> None
  | Some s -> Some s.txt

module Const = Ast_helper.Const

module Typ  =
  struct
    include Ast_helper.Typ

    let varify_constructors : str list -> core_type -> core_type =
      fun var_names t ->
        let var_names = List.map (fun x -> x.txt) var_names in
        let check_variable vl loc v =
          if List.mem v vl then
            raise Syntaxerr.(Error(Variable_in_scope(loc,v)))
        in
        let rec loop t =
          let desc =
            match t.ptyp_desc with
            | Ptyp_any -> Ptyp_any
            | Ptyp_var x ->
               check_variable var_names t.ptyp_loc x;
               Ptyp_var x
            | Ptyp_arrow (label,core_type,core_type') ->
               Ptyp_arrow(label, loop core_type, loop core_type')
            | Ptyp_tuple lst -> Ptyp_tuple (List.map loop lst)
            | Ptyp_constr( { txt = Longident.Lident s }, [])
                 when List.mem s var_names ->
               Ptyp_var s
            | Ptyp_constr(longident, lst) ->
               Ptyp_constr(longident, List.map loop lst)
            | Ptyp_object (lst, cl) ->
               Ptyp_object (List.map loop_core_field lst, cl)
            | Ptyp_class (longident, lst) ->
               Ptyp_class (longident, List.map loop lst)
            | Ptyp_extension(_) as ty -> ty
            | Ptyp_alias(core_type, string) ->
               check_variable var_names t.ptyp_loc string;
               Ptyp_alias(loop core_type, string)
            | Ptyp_variant(row_field_list, flag, lbl_lst_option) ->
               Ptyp_variant(List.map loop_row_field row_field_list,
                            flag, lbl_lst_option)
            | Ptyp_poly(string_lst, core_type) ->
               List.iter (check_variable var_names t.ptyp_loc) string_lst;
               Ptyp_poly(string_lst, loop core_type)
            | Ptyp_package(longident,lst) ->
               Ptyp_package(longident,List.map (fun (n,typ) -> (n,loop typ) ) lst)
          in
          {t with ptyp_desc = desc}
        and loop_core_field (str, attr, ty) = (str, attr, loop ty)
        and loop_row_field  =
          function
          | Rtag(label,attr,flag,lst) ->
             Rtag(label,attr,flag,List.map loop lst)
          | Rinherit t ->
             Rinherit (loop t)
        in loop t

    let object_ : ?loc:loc -> ?attrs:attrs
        -> (str * Parsetree.attributes * Parsetree.core_type) list
        -> Asttypes.closed_flag -> Parsetree.core_type =
      fun ?loc ?attrs l ->
        let l = List.map (fun (s,a,ct) -> (s.txt,a,ct)) l in
        object_ ~loc:(build_loc loc) ~attrs:(build_attrs attrs) l

    let poly : ?loc:loc -> ?attrs:attrs -> str list -> core_type
        -> core_type =
      fun ?loc ?attrs ss ->
        let ss = List.map (fun s -> s.txt) ss in
        poly ~loc:(build_loc loc) ~attrs:(build_attrs attrs) ss
  end

module Pat  =
  struct
    include Ast_helper.Pat

    let open_ : ?loc:loc -> ?attrs:attrs  -> lid -> pattern -> pattern =
      fun ?loc ?attrs _ _ ->
        (* TODO probably difficult to backport. *)
        assert false
  end

module Exp  =
  struct
    include Ast_helper.Exp

    let send: ?loc:loc -> ?attrs:attrs -> expression -> str -> expression =
      fun ?loc ?attrs e s ->
        send ~loc:(build_loc loc) ~attrs:(build_attrs attrs) e s.txt

    let letexception : ?loc:loc -> ?attrs:attrs -> extension_constructor
        -> expression -> expression =
      fun ?loc ?attrs _ _ ->
        (* TODO can be backported using first-class module. *)
        assert false

    let newtype: ?loc:loc -> ?attrs:attrs -> str -> expression -> expression =
      fun ?loc ?attrs s ->
        newtype ~loc:(build_loc loc) ~attrs:(build_attrs attrs) s.txt
  end

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

module Ctf  =
  struct
    include Ast_helper.Ctf

    let val_ : ?loc:loc -> ?attrs:attrs -> str -> mutable_flag
        -> virtual_flag -> core_type -> class_type_field =
      fun ?loc ?attrs s ->
        val_ ~loc:(build_loc loc) ~attrs:(build_attrs attrs) s.txt

    let method_: ?loc:loc -> ?attrs:attrs -> str -> private_flag
        -> virtual_flag -> core_type -> class_type_field =
      fun ?loc ?attrs s ->
        method_ ~loc:(build_loc loc) ~attrs:(build_attrs attrs) s.txt
  end

module Cf   =
  struct
    include Ast_helper.Cf

    let inherit_: ?loc:loc -> ?attrs:attrs -> override_flag -> class_expr
        -> str option -> class_field =
      fun ?loc ?attrs flag ce so ->
        let so = str_opt_to_string_opt so in
        inherit_ ~loc:(build_loc loc) ~attrs:(build_attrs attrs) flag ce so
  end

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

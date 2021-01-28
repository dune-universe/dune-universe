open Ppxlib
open Result
open Asttypes
open Parsetree
open Ast_helper
open Ppx_deriving
open VisitorsPlugin

(* This module offers helper functions for abstract syntax tree analysis. *)

(* -------------------------------------------------------------------------- *)

type tycon = string
type tyvar = string
type tyvars = tyvar list

(* -------------------------------------------------------------------------- *)

(* Testing whether an identifier is valid. *)

(* We use OCaml's lexer to analyze the string and check if it is a valid
   identifier. This method is slightly unorthodox, as the lexer can have
   undesired side effects, such as raising an [Error] exception or printing
   warnings. We do our best to hide these effects. The strength of this
   approach is to give us (at little cost) a correct criterion for deciding if
   an identifier is valid. *)

(* Note: [Location.formatter_for_warnings] appeared in OCaml 4.02.2. *)

type classification =
  | LIDENT
  | UIDENT
  | OTHER

let classify (s : string) : classification =
  let lexbuf = Lexing.from_string s in
  let backup = !Ocaml_common.Location.formatter_for_warnings in
  let null = Format.formatter_of_buffer (Buffer.create 0) in
  Ocaml_common.Location.formatter_for_warnings := null;
  let result = try
      let token1 = Lexer.token lexbuf in
      let token2 = Lexer.token lexbuf in
      match token1, token2 with
      | Parser.LIDENT _, Parser.EOF ->
         LIDENT
      | Parser.UIDENT _, Parser.EOF ->
         UIDENT
      | _, _ ->
         OTHER
    with Lexer.Error _ ->
      OTHER
  in
  Ocaml_common.Location.formatter_for_warnings := backup;
  result

(* -------------------------------------------------------------------------- *)

(* Testing if a string is a valid [mod_longident], i.e., a possibly-qualified
   module identifier. *)

(* We might wish to use OCaml's parser for this purpose, but [mod_longident] is
   not declared as a start symbol. Furthermore, that would be perhaps slightly
   too lenient, e.g., allowing whitespace and comments inside. Our solution is
   to split at the dots and check that every piece is a valid module name. *)

(* We used to use [Longident.parse] to do the splitting, but this function has
   been deprecated as of 4.11.0, and its suggested replacements do not go as
   far back in time as we need. So, we use our own variant of this code. *)

let rec parse s n =
  (* Parse the substring that extends from offset 0 to offset [n] excluded. *)
  try
    let i = String.rindex_from s (n - 1) '.' in
    let segment = String.sub s (i + 1) (n - (i + 1)) in
    Ldot (parse s i, segment)
  with Not_found ->
    Lident (String.sub s 0 n)

let parse s =
  parse s (String.length s)

let is_valid_mod_longident (m : string) : bool =
  String.length m > 0 &&
  let ms = Longident.flatten_exn (parse m) in
  List.for_all (fun m -> classify m = UIDENT) ms

(* -------------------------------------------------------------------------- *)

(* Testing if a string is a valid [class_longident], i.e., a possibly-qualified
   class identifier. *)

let is_valid_class_longident (m : string) : bool =
  String.length m > 0 &&
  match parse m with
  | Lident c ->
      classify c = LIDENT
  | Ldot (m, c) ->
      List.for_all (fun m -> classify m = UIDENT) (Longident.flatten_exn m) &&
      classify c = LIDENT
  | Lapply _ ->
      assert false (* this cannot happen *)

(* -------------------------------------------------------------------------- *)

(* Testing if a string is a valid method name prefix. *)

let is_valid_method_name_prefix (m : string) : bool =
  String.length m > 0 &&
  classify m = LIDENT

(* -------------------------------------------------------------------------- *)

(* Testing for the presence of attributes. *)

(* We use [ppx_deriving] to extract a specific attribute from an attribute
   list. By convention, an attribute named [foo] can also be referred to as
   [visitors.foo] or as [deriving.visitors.foo]. *)

(* [select foo attrs] extracts the attribute named [foo] from the attribute
   list [attrs]. *)

let select (foo : string) (attrs : attributes) : attribute option =
  attr ~deriver:plugin foo attrs

(* [present foo attrs] tests whether an attribute named [foo] is present
   (with no argument) in the list [attrs]. *)

let present (foo : string) (attrs : attributes) : bool =
  Arg.get_flag ~deriver:plugin (select foo attrs)

(* [opacity attrs] tests for the presence of an [@opaque] attribute. *)

type opacity =
  | Opaque
  | NonOpaque

let opacity (attrs : attributes) : opacity =
  if present "opaque" attrs then Opaque else NonOpaque

(* [name attrs] tests for the presence of a [@name] attribute, carrying a
   payload of type [string]. We check that the payload is a valid (lowercase
   or uppercase) identifier, because we intend to use it as the basis of a
   method name. *)

let identifier : string Arg.conv =
  fun e ->
    match Arg.string e with
    | Error msg ->
        Error msg
    | Ok s ->
        match classify s with
        | LIDENT | UIDENT ->
            Ok s
        | OTHER ->
            Error "identifier"

let name (attrs : attributes) : string option =
  Arg.get_attr ~deriver:plugin identifier (select "name" attrs)

(* [build attrs] tests for the presence of a [@build] attribute,
   carrying a payload that is an arbitrary OCaml expression. *)

let build (attrs : attributes) : expression option =
  Arg.get_attr ~deriver:plugin Arg.expr (select "build" attrs)

(* [maybe ox y] returns [x] if present, otherwise [y]. *)

let maybe (ox : 'a option) (y : 'a) : 'a =
  match ox with Some x -> x | None -> y

(* -------------------------------------------------------------------------- *)

(* When parsing a record declaration, the OCaml parser attaches attributes
   with field labels, whereas the user might naturally expect them to be
   attached with the type. We rectify this situation by copying all attributes
   from the label to the type. This might seem dangerous, but we use it only
   to test for the presence of an [@opaque] attribute. *)

let paste (ty : core_type) (attrs : attributes) : core_type =
  { ty with ptyp_attributes = attrs @ ty.ptyp_attributes }

let fix (ld : label_declaration) : label_declaration =
  { ld with pld_type = paste ld.pld_type ld.pld_attributes }

let fix =
  List.map fix

(* -------------------------------------------------------------------------- *)

(* [type_param_to_tyvar] expects a type parameter as found in the field
   [ptype_params] of a type definition, and returns the underlying type
   variable. *)

let type_param_to_tyvar ((ty, _) : core_type * variance) : tyvar =
  match ty.ptyp_desc with
  | Ptyp_var tv ->
      tv
  | Ptyp_any ->
      (* This error occurs if a formal type parameter is a wildcard [_].
         We could support this form, but it makes life slightly simpler
         to disallow it. It is usually used only in GADTs anyway. *)
      raise_errorf ~loc:ty.ptyp_loc
        "%s: every formal type parameter should be named." plugin
  | _ ->
      assert false

let type_params_to_tyvars =
  List.map type_param_to_tyvar

(* [decl_params decl] returns the type parameters of the declaration [decl]. *)

let decl_params (decl : type_declaration) : tyvars =
  type_params_to_tyvars decl.ptype_params

(* [is_local decls tycon] tests whether the type constructor [tycon] is
   declared by the type declarations [decls]. If so, it returns the
   corresponding declaration. *)

let rec is_local (decls : type_declaration list) (tycon : tycon)
: type_declaration option =
  match decls with
  | [] ->
      None
  | decl :: decls ->
      if decl.ptype_name.txt = tycon then
        Some decl
      else
        is_local decls tycon

let is_local (decls : type_declaration list) (tycon : Longident.t)
: type_declaration option =
  match tycon with
  | Lident tycon ->
      is_local decls tycon
  | Ldot _
  | Lapply _ ->
      None

(* -------------------------------------------------------------------------- *)

(* [occurs_type alpha ty] tests whether the type variable [alpha] occurs in
   the type [ty]. This function goes down into all OCaml types, even those
   that are not supported by [visitors]. *)

exception Occurs of loc

let rec occurs_type (alpha : tyvar) (ty : core_type) : unit =
  match ty.ptyp_desc with
  | Ptyp_any ->
      ()
  | Ptyp_var beta ->
      if alpha = beta then
        raise (Occurs ty.ptyp_loc)
  | Ptyp_alias (ty, _) ->
      (* This is not a binder; just go down into it. *)
      occurs_type alpha ty
  | Ptyp_arrow (_, ty1, ty2) ->
      occurs_types alpha [ ty1; ty2 ]
  | Ptyp_tuple tys
  | Ptyp_constr (_, tys)
  | Ptyp_class (_, tys) ->
      occurs_types alpha tys
  | Ptyp_object (fields, _) ->
      fields
      |> List.map VisitorsCompatibility.object_field_to_core_type
      |> occurs_types alpha
  | Ptyp_variant (fields, _, _) ->
      List.iter (occurs_row_field alpha) fields
  | Ptyp_poly (qs, ty) ->
      let qs : string list = VisitorsCompatibility.quantifiers qs in
      (* The type variables in [qs] are bound. *)
      if not (occurs_quantifiers alpha qs) then
        occurs_type alpha ty
  | Ptyp_package (_, ltys) ->
      List.iter (fun (_, ty) -> occurs_type alpha ty) ltys
  | Ptyp_extension (_, payload) ->
      occurs_payload alpha payload

and occurs_types alpha tys =
  List.iter (occurs_type alpha) tys

and occurs_row_field alpha field =
  field
  |> VisitorsCompatibility.row_field_to_core_types
  |> occurs_types alpha

and occurs_quantifiers alpha (qs : string list) =
  List.mem alpha qs

and occurs_payload alpha = function
  | PTyp ty ->
      occurs_type alpha ty
  (* | PStr _ | PPat _ *)
  (* | PSig _ (* >= 4.03 *) *)
  | _ ->
      (* We assume that these cases won't arise or won't have any free type
         variables in them. *)
      ()

(* -------------------------------------------------------------------------- *)

(* An error message about an unsupported type. *)

let unsupported ty =
  let loc = ty.ptyp_loc in
  raise_errorf ~loc
    "%s: cannot deal with the type %s.\n\
     Consider annotating it with [@opaque]."
    plugin
    (string_of_core_type ty)

(* -------------------------------------------------------------------------- *)

(* [at_opaque f ty] applies the function [f] to every [@opaque] component of
   the type [ty]. *)

let rec at_opaque (f : core_type -> unit) (ty : core_type) : unit =
  match opacity ty.ptyp_attributes, ty.ptyp_desc with
  | NonOpaque, Ptyp_any
  | NonOpaque, Ptyp_var _ ->
      ()
  | NonOpaque, Ptyp_tuple tys
  | NonOpaque, Ptyp_constr (_, tys) ->
      List.iter (at_opaque f) tys
  | Opaque, _ ->
      f ty
  | NonOpaque, Ptyp_arrow _
  | NonOpaque, Ptyp_object _
  | NonOpaque, Ptyp_class _
  | NonOpaque, Ptyp_alias _
  | NonOpaque, Ptyp_variant _
  | NonOpaque, Ptyp_poly _
  | NonOpaque, Ptyp_package _
  | NonOpaque, Ptyp_extension _ ->
      unsupported ty

(* -------------------------------------------------------------------------- *)

(* [check_poly_under_opaque alphas tys] checks that none of the type variables
   [alphas] appears under [@opaque] in the types [tys]. *)

let check_poly_under_opaque alphas tys =
  List.iter (fun alpha ->
    List.iter (fun ty ->
      at_opaque (fun ty ->
        try
          occurs_type alpha ty
        with Occurs loc ->
          raise_errorf ~loc
            "%s: a [polymorphic] type variable must not appear under @opaque."
            plugin
      ) ty
    ) tys
  ) alphas

(* -------------------------------------------------------------------------- *)

(* [subst_type sigma ty] applies [sigma], a substitution of types for type
   variables, to the type [ty].

   [rename_type rho ty] applies [rho], a renaming of type variables, to the
   type [ty]. *)

(* We do not go down into [@opaque] types. We replace every opaque type with a
   wildcard [_]. Because we have checked that [poly] variables do not appear
   under [@opaque], this is good enough: there is never a need for an
   explicitly named/quantified type variable to describe an opaque
   component. *)

type substitution =
  tyvar -> core_type

type renaming =
  tyvar -> tyvar

let rec subst_type (sigma : substitution) (ty : core_type) : core_type =
  match opacity ty.ptyp_attributes, ty.ptyp_desc with
  | NonOpaque, Ptyp_any ->
      ty
  | NonOpaque, Ptyp_var alpha ->
      sigma alpha
  | NonOpaque, Ptyp_tuple tys ->
      { ty with ptyp_desc = Ptyp_tuple (subst_types sigma tys) }
  | NonOpaque, Ptyp_constr (tycon, tys) ->
      { ty with ptyp_desc = Ptyp_constr (tycon, subst_types sigma tys) }
  | Opaque, _ ->
      Typ.any()
  | NonOpaque, Ptyp_arrow _
  | NonOpaque, Ptyp_object _
  | NonOpaque, Ptyp_class _
  | NonOpaque, Ptyp_alias _
  | NonOpaque, Ptyp_variant _
  | NonOpaque, Ptyp_poly _
  | NonOpaque, Ptyp_package _
  | NonOpaque, Ptyp_extension _ ->
      unsupported ty

and subst_types sigma tys =
  List.map (subst_type sigma) tys

let rename_type (rho : renaming) (ty : core_type) : core_type =
  subst_type (fun alpha -> Typ.var (rho alpha)) ty

let mknoloc = Location.mknoloc
open Ppxlib
open Asttypes
open Parsetree
open Ast_helper

(* OCaml's abstract syntax tree evolves with time. We depend on this tree
   because we analyze it (that is, we analyze type definitions) and because we
   construct it (that is, we generate code). This module was initially created
   to gather the ugly bits whose definition varies depending on the version of
   OCaml that we are working with. That said, since we have switched to using
   ppxlib, my understanding is that we always work with the latest version of
   the AST. More precisely, the version of the AST that we get no longer
   depends on which OCaml compiler we are using; it depends on which version
   of ppxlib we are using. At the time of writing, we depend on ppxlib 0.9.0
   or newer, which implies that we get an AST for OCaml 4.08 or newer. See
   https://github.com/ocaml-ppx/ppxlib/blob/master/CHANGES.md *)

(* Constructing an arrow type. *)

let ty_arrow (a : core_type) (b : core_type) : core_type =
  Typ.arrow Nolabel a b

(* Constructing a function. *)

let plambda (p : pattern) (e : expression) : expression =
  Exp.fun_ Nolabel None p e

(* [ld_label] and [ld_ty] extract a label and type out of an OCaml record label
   declaration. *)

let ld_label (ld : label_declaration) : label =
  ld.pld_name.txt

let ld_labels =
  List.map ld_label

let ld_ty (ld : label_declaration) : core_type =
  ld.pld_type

let ld_tys =
  List.map ld_ty

(* Between OCaml 4.04 and OCaml 4.05, the types of several functions in [Ast_helper]
   have changed. They used to take arguments of type [string], and now take arguments
   of type [str], thus requiring a conversion. These functions include [Typ.object_],
   [Typ.poly], [Exp.send], [Exp.newtype], [Ctf.val_], [Ctf.method_], [Cf.inherit_].  *)

type str =
  string Location.loc

let string2str (s : string) : str =
  mknoloc s

let str2string (s : str) : string =
  s.txt

let typ_poly (tyvars : string list) (cty : core_type) : core_type =
  Typ.poly (List.map string2str tyvars) cty

let exp_send (e : expression) (m : string) : expression =
  Exp.send e (string2str m)

(* In the data constructor [Ptyp_poly (qs, ty)], the type of [qs] has changed from
   [string list] to [string loc list] between OCaml 4.04 and 4.05.
   See commit b0e880c448c78ed0cedff28356fcaf88f1436eef.
   The function [quantifiers] compensates for this. *)

let quantifiers qs : string list =
  List.map str2string qs

(* In the data constructor [Ptyp_object (methods, _)], the type of [methods] has
   changed from [(string loc * attributes * core_type) list] in OCaml 4.05 to
                [object_field                          list] in OCaml 4.06. *)

let object_field_to_core_type (field : object_field) : core_type =
  match field.pof_desc with
  | Otag (_, ty)  -> ty
  | Oinherit ty   -> ty
    (* this may seem nonsensical, but (so far) is used only in the
       function [occurs_type], where we do not care what the types
       mean *)

let row_field_to_core_types (field : row_field) : core_type list =
  match field.prf_desc with
  | Rtag (_, _, tys) ->
      tys
  | Rinherit ty ->
      [ ty ]

(* -------------------------------------------------------------------------- *)

(* [floating s items] produces a floating attribute whose name is [s] and
   whose payload is the list of structure items [items]. *)

(* The type [attribute] is defined in 4.07 as [string loc * payload], but in
   4.08 its definition changes to a record type and the function [Attr.mk]
   appears. *)

let floating (s : string) (items : structure) : structure_item =
  let name = mknoloc s
  and payload = PStr items in
  Str.attribute (Attr.mk name payload)

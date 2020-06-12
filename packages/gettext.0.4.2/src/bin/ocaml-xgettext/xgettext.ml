(**************************************************************************)
(*  ocaml-gettext: a library to translate messages                        *)
(*                                                                        *)
(*  Copyright (C) 2003-2008 Sylvain Le Gall <sylvain@le-gall.net>         *)
(*                                                                        *)
(*  This library is free software; you can redistribute it and/or         *)
(*  modify it under the terms of the GNU Lesser General Public            *)
(*  License as published by the Free Software Foundation; either          *)
(*  version 2.1 of the License, or (at your option) any later version;    *)
(*  with the OCaml static compilation exception.                          *)
(*                                                                        *)
(*  This library is distributed in the hope that it will be useful,       *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *)
(*  Lesser General Public License for more details.                       *)
(*                                                                        *)
(*  You should have received a copy of the GNU Lesser General Public      *)
(*  License along with this library; if not, write to the Free Software   *)
(*  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *)
(*  USA                                                                   *)
(**************************************************************************)

(** PPX dumper to extract strings.
    @author Richard W.M. Jones
    @author Sylvain Le Gall
  *)

(* Extract the string which should be used for a gettext translation. Output a
   po_content list through the function Marshal.to_channel
   Functions that are looked for :

Functions     Arg 1      Arg 2      Arg 3      Arg 4      Arg 5      Arg 6   ...
s_            singular
f_            singular
sn_           singular   plural     _
fn_           singular   plural     _
gettext       _          singular
fgettext      _          singular
dgettext      _          domain     singular
fdgettext     _          domain     singular
dcgettext     _          domain     singular   _
fdcgettext    _          domain     singular   _
ngettext      _          singular   plural     _
fngettext     _          singular   plural     _
dngettext     _          domain     singular   plural     _
fdngettext    _          domain     singular   plural     _
dcngettext    _          domain     singular   plural     _          _
fdcngettext   _          domain     singular   plural     _          _

All this function name should also be matched when they are called using a
module.

*)

open GettextTypes
open GettextPo
open Parsetree
open Longident

type t = { po_content : po_content; translated : SetString.t }

let translations = ref { po_content = empty_po; translated = SetString.empty }

let default_textdomain = ref None

let current_file = ref ""

let add_translation loc singular plural_opt domain =
  let t = !translations in
  let filepos =
    let start = loc.Location.loc_start in
    let fname =
      match start.Lexing.pos_fname with "" -> !current_file | fname -> fname
    in
    (fname, start.Lexing.pos_lnum)
  in
  let translated = SetString.add singular t.translated in
  let translated, translation =
    match plural_opt with
    | Some plural ->
        ( SetString.add plural translated,
          {
            po_comment_special = [];
            po_comment_filepos = [ filepos ];
            po_comment_translation =
              PoPlural ([ singular ], [ plural ], [ [ "" ]; [ "" ] ]);
          } )
    | None ->
        ( translated,
          {
            po_comment_special = [];
            po_comment_filepos = [ filepos ];
            po_comment_translation = PoSingular ([ singular ], [ "" ]);
          } )
  in
  let po_content =
    match (domain, !default_textdomain) with
    | Some domain, _ ->
        add_po_translation_domain domain t.po_content translation
    | None, Some domain ->
        add_po_translation_domain domain t.po_content translation
    | None, None -> add_po_translation_no_domain t.po_content translation
  in
  translations := { po_content; translated }

let output_translations ?output_file t =
  let fd = match output_file with Some f -> open_out f | None -> stdout in
  set_binary_mode_out fd true;
  Marshal.to_channel fd t.po_content []

let rec is_like lid = function
  | [] -> false
  | func :: functions -> (
      match lid with
      | (Lident f | Ldot (_, f)) when f = func -> true
      | _ -> is_like lid functions )

let visit_expr (iterator : Ast_iterator.iterator) expr =
  let loc = expr.pexp_loc in
  match expr.pexp_desc with
  | Pexp_apply
      ( { pexp_desc = Pexp_ident { Asttypes.txt = lid; _ }; _ },
        ( Asttypes.Nolabel,
#if OCAML_VERSION >= (4, 11, 0)
          { pexp_desc = Pexp_constant (Pconst_string (singular, _, _)); _ } )
#else
          { pexp_desc = Pexp_constant (Pconst_string (singular, _)); _ } )
#endif
        :: _ )
    when is_like lid [ "s_"; "f_" ] ->
      (* Add a singular / default domain string *)
      add_translation loc singular None None
  | Pexp_apply
      ( { pexp_desc = Pexp_ident { Asttypes.txt = lid; _ }; _ },
        ( Asttypes.Nolabel,
#if OCAML_VERSION >= (4, 11, 0)
          { pexp_desc = Pexp_constant (Pconst_string (singular, _, _)); _ } )
#else
          { pexp_desc = Pexp_constant (Pconst_string (singular, _)); _ } )
#endif
        :: ( Asttypes.Nolabel,
#if OCAML_VERSION >= (4, 11, 0)
             { pexp_desc = Pexp_constant (Pconst_string (plural, _, _)); _ } )
#else
             { pexp_desc = Pexp_constant (Pconst_string (plural, _)); _ } )
#endif
           :: _ )
    when is_like lid [ "sn_"; "fn_" ] ->
      (* Add a plural / default domain string *)
      add_translation loc singular (Some plural) None
  | Pexp_apply
      ( { pexp_desc = Pexp_ident { Asttypes.txt = lid; _ }; _ },
        _
        :: ( Asttypes.Nolabel,
#if OCAML_VERSION >= (4, 11, 0)
             { pexp_desc = Pexp_constant (Pconst_string (singular, _, _)); _ } )
#else
             { pexp_desc = Pexp_constant (Pconst_string (singular, _)); _ } )
#endif
           :: _ )
    when is_like lid [ "gettext"; "fgettext" ] ->
      (* Add a singular / default domain string *)
      add_translation loc singular None None
  | Pexp_apply
      ( { pexp_desc = Pexp_ident { Asttypes.txt = lid; _ }; _ },
        _
        :: ( Asttypes.Nolabel,
#if OCAML_VERSION >= (4, 11, 0)
             { pexp_desc = Pexp_constant (Pconst_string (domain, _, _)); _ } )
#else
             { pexp_desc = Pexp_constant (Pconst_string (domain, _)); _ } )
#endif
           :: ( Asttypes.Nolabel,
#if OCAML_VERSION >= (4, 11, 0)
                { pexp_desc = Pexp_constant (Pconst_string (singular, _, _)); _ }
#else
                { pexp_desc = Pexp_constant (Pconst_string (singular, _)); _ }
#endif
              )
              :: _ )
    when is_like lid [ "dgettext"; "fdgettext"; "dcgettext"; "fdcgettext" ] ->
      (* Add a singular / defined domain string *)
      add_translation loc singular None (Some domain)
  | Pexp_apply
      ( { pexp_desc = Pexp_ident { Asttypes.txt = lid; _ }; _ },
        _
        :: ( Asttypes.Nolabel,
#if OCAML_VERSION >= (4, 11, 0)
             { pexp_desc = Pexp_constant (Pconst_string (singular, _, _)); _ } )
#else
             { pexp_desc = Pexp_constant (Pconst_string (singular, _)); _ } )
#endif
           :: ( Asttypes.Nolabel,
#if OCAML_VERSION >= (4, 11, 0)
                { pexp_desc = Pexp_constant (Pconst_string (plural, _, _)); _ } )
#else
                { pexp_desc = Pexp_constant (Pconst_string (plural, _)); _ } )
#endif
              :: _ )
    when is_like lid [ "ngettext"; "fngettext" ] ->
      (* Add a plural / default domain string *)
      add_translation loc singular (Some plural) None
  | Pexp_apply
      ( { pexp_desc = Pexp_ident { Asttypes.txt = lid; _ }; _ },
        _
        :: ( Asttypes.Nolabel,
#if OCAML_VERSION >= (4, 11, 0)
             { pexp_desc = Pexp_constant (Pconst_string (domain, _, _)); _ } )
#else
             { pexp_desc = Pexp_constant (Pconst_string (domain, _)); _ } )
#endif
           :: ( Asttypes.Nolabel,
#if OCAML_VERSION >= (4, 11, 0)
                { pexp_desc = Pexp_constant (Pconst_string (singular, _, _)); _ }
#else
                { pexp_desc = Pexp_constant (Pconst_string (singular, _)); _ }
#endif
              )
              :: ( Asttypes.Nolabel,
#if OCAML_VERSION >= (4, 11, 0)
                   { pexp_desc = Pexp_constant (Pconst_string (plural, _, _)); _ }
#else
                   { pexp_desc = Pexp_constant (Pconst_string (plural, _)); _ }
#endif
                 )
                 :: _ )
    when is_like lid [ "dngettext"; "fdngettext"; "dcngettext"; "fdcngettext" ]
    ->
      (* Add a plural / defined domain string *)
      add_translation loc singular (Some plural) (Some domain)
  | _ -> Ast_iterator.default_iterator.expr iterator expr

let ast_iterator = { Ast_iterator.default_iterator with expr = visit_expr }

let go fn =
  current_file := fn;
  try
    let lexbuf = Lexing.from_channel (open_in fn) in
    let structure = Parse.implementation lexbuf in
    ast_iterator.Ast_iterator.structure ast_iterator structure
  with exn -> failwith (fn ^ ": " ^ Printexc.to_string exn)

let () =
  (* XXX Add -default-textdomain option which sets default_textdomain. *)
  Arg.parse [] go "";
  output_translations !translations

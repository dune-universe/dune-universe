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

(**
    @author Sylvain Le Gall
  *)

open GettextTypes
open GettextCategory

(** Function for manipulation the type t *)

let upgrade_textdomain t k value =
  let current_codeset, current_dir =
    try MapTextdomain.find k t.textdomains with Not_found -> (None, None)
  in
  let new_value =
    match value with
    | None, None -> (current_codeset, current_dir)
    | None, new_dir -> (current_codeset, new_dir)
    | new_codeset, None -> (new_codeset, current_dir)
    | new_codeset, new_dir -> (new_codeset, new_dir)
  in
  { t with textdomains = MapTextdomain.add k new_value t.textdomains }

let create ?(failsafe = Ignore) ?(categories = []) ?(codesets = [])
    ?(dirs = []) ?(textdomains = []) ?(codeset = GettextConfig.default_codeset)
    ?(path = GettextConfig.default_path) ?language textdomain =
  let map_categories =
    List.fold_left
      (fun map (category, locale) -> MapCategory.add category locale map)
      MapCategory.empty categories
  in
  let result =
    {
      failsafe;
      textdomains = MapTextdomain.empty;
      categories = map_categories;
      language;
      codeset;
      path;
      default = textdomain;
    }
  in
  (* Apply any upgrade required by the different settings provided *)
  let apply_upgrade t lst =
    List.fold_left
      (fun t (textdomain, changes) -> upgrade_textdomain t textdomain changes)
      t lst
  in
  (* All changes from the setting of textdomains *)
  let textdomains_changes =
    List.map
      (fun textdomain -> (textdomain, (None, None)))
      (textdomain :: textdomains)
  in
  (* All changes from the setting of codesets *)
  let codesets_changes =
    List.map
      (fun (textdomain, codeset) -> (textdomain, (Some codeset, None)))
      codesets
  in
  (* All changes from the setting of dirs *)
  let dirs_changes =
    List.map (fun (textdomain, dir) -> (textdomain, (None, Some dir))) dirs
  in
  apply_upgrade result (textdomains_changes @ codesets_changes @ dirs_changes)

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

let empty_po = { no_domain = MapString.empty; domain = MapTextdomain.empty }

(* See GettextPo for details concerning merge of the translation *)
let add_po_translation_aux map commented_translation =
  let translation = commented_translation.po_comment_translation in
  let is_lst_empty lst =
    List.for_all (fun lst -> String.concat "" lst = "") lst
  in
  let is_lst_same lst1 lst2 =
    try not (List.exists2 (fun a b -> a <> b) lst1 lst2)
    with Invalid_argument _ -> false
  in
  let string_of_list lst =
    let lst_escaped =
      List.map (fun s -> Printf.sprintf "%S" (String.concat "" s)) lst
    in
    Printf.sprintf "[ %a ]" (fun () lst -> String.concat ";" lst) lst_escaped
  in
  let str_id =
    match translation with
    | PoSingular (str_lst, _) | PoPlural (str_lst, _, _) -> str_lst
  in
  let new_commented_translation =
    try
      let previous_commented_translation =
        MapString.find (String.concat "" str_id) map
      in
      let previous_location_lst =
        previous_commented_translation.po_comment_filepos
      in
      let previous_translation =
        previous_commented_translation.po_comment_translation
      in
      let location_lst = commented_translation.po_comment_filepos in
      let merged_translation =
        match (previous_translation, translation) with
        | PoSingular (_, str1), PoSingular (_, str2) when is_lst_same str1 str2
          ->
            PoSingular (str_id, str1)
        | PoSingular (_, [ "" ]), PoSingular (_, str2) ->
            PoSingular (str_id, str2)
        | PoSingular (_, str1), PoSingular (_, [ "" ]) ->
            PoSingular (str_id, str1)
        | PoSingular (_, str1), PoSingular (_, str2) ->
            raise
              (PoInconsistentMerge
                 (String.concat "" str1, String.concat "" str2))
        | PoPlural (_, str1, lst1), PoPlural (_, str2, lst2)
          when is_lst_same str1 str2 && is_lst_empty lst1 ->
            PoPlural (str_id, str2, lst2)
        | PoPlural (_, str1, lst1), PoPlural (_, str2, lst2)
          when is_lst_same str1 str2 && is_lst_empty lst2 ->
            PoPlural (str_id, str1, lst1)
        | PoPlural (_, str1, lst1), PoPlural (_, str2, lst2)
          when is_lst_same str1 str2 && is_lst_same lst1 lst2 ->
            PoPlural (str_id, str1, lst1)
        | PoPlural (_, str1, lst1), PoPlural (_, str2, lst2)
          when is_lst_same str1 str2 ->
            raise
              (PoInconsistentMerge (string_of_list lst1, string_of_list lst2))
        | PoPlural (_, str1, _), PoPlural (_, str2, _) ->
            raise
              (PoInconsistentMerge
                 (String.concat "" str1, String.concat "" str2))
        | PoSingular (_, str), PoPlural (_, str_plural, lst)
        | PoPlural (_, str_plural, lst), PoSingular (_, str) -> (
            match lst with
            | x :: tl when String.concat "" x = "" ->
                PoPlural (str_id, str_plural, str :: tl)
            | [] -> PoPlural (str_id, str_plural, [ str ])
            | _ ->
                raise
                  (PoInconsistentMerge
                     (String.concat "" str, string_of_list lst)) )
      in
      (* TODO: merge po_comment_special and use fuzzy when merging *)
      {
        po_comment_special =
          previous_commented_translation.po_comment_special
          @ commented_translation.po_comment_special;
        po_comment_filepos = location_lst @ previous_location_lst;
        po_comment_translation = merged_translation;
      }
    with Not_found -> commented_translation
  in
  MapString.add (String.concat "" str_id) new_commented_translation map

let add_po_translation_no_domain po po_translation =
  { po with no_domain = add_po_translation_aux po.no_domain po_translation }

let add_po_translation_domain domain po po_translation =
  {
    po with
    domain =
      (let map_domain =
         try MapTextdomain.find domain po.domain
         with Not_found -> MapString.empty
       in
       let map_domain = add_po_translation_aux map_domain po_translation in
       MapTextdomain.add domain map_domain po.domain);
  }

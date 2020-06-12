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

(** empty_po : value representing an empty PO *)
let empty_po = GettextPo_utils.empty_po

(** add_po_translation_no_domain po (comment_lst,location_lst,translation) :
    add a translation to a corpus of already defined translation with no domain
    defined. If the translation already exist, they are merged concerning
    location, and follow these rules for the translation itself :
      - singular and singular : if there is an empty string ( "" ) in one
        of the translation, use the other translation,
      - plural and plural : if there is an empty string list ( [ "" ; "" ] ) in
        one of the translaiton, use the other translation,
      - singular and plural : merge into a plural form.
    There is checks during the merge that can raise PoInconsistentMerge :
      - for one singular string if the two plural strings differs
      - if there is some elements that differs (considering the special case of
        the empty string ) in the translation
*)
let add_po_translation_no_domain po po_translation =
  try GettextPo_utils.add_po_translation_no_domain po po_translation
  with PoInconsistentMerge (str1, str2) ->
    raise (PoInconsistentMerge (str1, str2))

(** add_po_translation_domain po domain (comment_lst,location_lst,translation):
   add a translation to the already defined translation with the domain
   defined. See add_translation_no_domain for details.
*)
let add_po_translation_domain po domain po_translation =
  try GettextPo_utils.add_po_translation_domain po domain po_translation
  with PoInconsistentMerge (str1, str2) ->
    raise (PoInconsistentMerge (str1, str2))

(** merge_po po1 po2 : merge two PO. The rule for merging are the same as
    defined in add_po_translation_no_domain. Can raise PoInconsistentMerge
*)
let merge_po po1 po2 =
  (* We take po2 as the initial set, we merge po1 into po2 beginning with
    po1.no_domain and then po1.domain *)
  let merge_no_domain =
    MapString.fold
      (fun _ translation po -> add_po_translation_no_domain po translation)
      po1.no_domain po2
  in
  let merge_one_domain domain map_domain po =
    MapString.fold
      (fun _ translation po -> add_po_translation_domain domain po translation)
      map_domain po
  in
  MapTextdomain.fold merge_one_domain po1.domain merge_no_domain

(** merge_pot po pot : merge a PO with a POT. Only consider strings that
    exists in the pot. Always use location as defined in the POT. If a string
    is not found, use the translation provided in the POT. If a plural is found
    and a singular should be used, downgrade the plural to singular. If a
    singular is found and a plural should be used, upgrade singular to plural,
    using the strings provided in the POT for ending the translation.
  *)
let merge_pot pot po =
  let order_po_map ?domain () =
    match domain with
    | None ->
        po.no_domain
        :: MapTextdomain.fold (fun _ x lst -> x :: lst) po.domain []
    | Some domain -> (
        let tl =
          po.no_domain
          :: MapTextdomain.fold
               (fun key x lst -> if key = domain then lst else x :: lst)
               po.domain []
        in
        try MapTextdomain.find domain po.domain :: tl with Not_found -> tl )
  in
  let merge_translation map_lst key commented_translation_pot =
    let translation_pot = commented_translation_pot.po_comment_translation in
    let translation_merged =
      try
        let commented_translation_po =
          let map_po = List.find (MapString.mem key) map_lst in
          MapString.find key map_po
        in
        let translation_po = commented_translation_po.po_comment_translation in
        (* Implementation of the rule given above *)
        match (translation_pot, translation_po) with
        | PoSingular (str_id, _), PoPlural (_, _, str :: _) ->
            PoSingular (str_id, str)
        | PoPlural (str_id, str_plural, _ :: tl), PoSingular (_, str) ->
            PoPlural (str_id, str_plural, str :: tl)
        | PoPlural (str_id, str_plural, []), PoSingular (_, str) ->
            PoPlural (str_id, str_plural, [ str ])
        | _, translation -> translation
      with Not_found ->
        (* Fallback to the translation provided in the POT *)
        translation_pot
    in
    {
      commented_translation_pot with
      po_comment_translation = translation_merged;
    }
  in
  (* We begin with an empty po, and merge everything according to the rule
     above. *)
  let merge_no_domain =
    MapString.fold
      (fun key pot_translation po ->
        add_po_translation_no_domain po
          (merge_translation (order_po_map ()) key pot_translation))
      pot.no_domain empty_po
  in
  let merge_one_domain domain map_domain po =
    MapString.fold
      (fun key pot_translation po ->
        add_po_translation_domain domain po
          (merge_translation (order_po_map ~domain ()) key pot_translation))
      map_domain po
  in
  MapTextdomain.fold merge_one_domain pot.domain merge_no_domain

let input_po chn =
  let lexbuf = Lexing.from_channel chn in
  try GettextPo_parser.msgfmt GettextPo_lexer.token lexbuf with
  | Parsing.Parse_error -> raise (PoInvalidFile ("parse error", lexbuf, chn))
  | Failure s -> raise (PoInvalidFile (s, lexbuf, chn))
  | PoInconsistentMerge (str1, str2) ->
      raise (PoInconsistentMerge (str1, str2))

let output_po chn po =
  let () = set_binary_mode_out chn true in
  let comment_max_length = 80 in
  let fpf x = Printf.fprintf chn x in
  let escape_string str =
    let rec escape_string_aux buff i =
      if i < String.length str then
        let () =
          match str.[i] with
          | '\n' -> Buffer.add_string buff "\\n"
          | '\t' -> Buffer.add_string buff "\\t"
          | '\b' -> Buffer.add_string buff "\\b"
          | '\r' -> Buffer.add_string buff "\\r"
          | '\012' -> Buffer.add_string buff "\\f"
          | '\011' -> Buffer.add_string buff "\\v"
          | '\007' -> Buffer.add_string buff "\\a"
          | '"' -> Buffer.add_string buff "\\\""
          | '\\' -> Buffer.add_string buff "\\\\"
          | e -> Buffer.add_char buff e
        in
        escape_string_aux buff (i + 1)
      else ()
    in
    let buff = Buffer.create (String.length str + 2) in
    Buffer.add_char buff '"';
    escape_string_aux buff 0;
    Buffer.add_char buff '"';
    Buffer.contents buff
  in
  let hyphens chn lst =
    match lst with
    | [] -> ()
    | lst ->
        Printf.fprintf chn "%s"
          (String.concat "\n" (List.map escape_string lst))
  in
  let comment_line str_hyphen str_sep line_max_length token_lst =
    let str_len =
      List.fold_left (fun acc str -> acc + String.length str) 0 token_lst
      + (List.length token_lst * String.length str_sep)
    in
    let buff =
      Buffer.create
        (str_len + (String.length str_hyphen * (str_len / line_max_length)))
    in
    let rec comment_line_aux first_token line_length lst =
      match lst with
      | str :: tl ->
          let sep_length =
            if first_token then 0
            else if String.length str + line_length > line_max_length then (
              Buffer.add_char buff '\n';
              Buffer.add_string buff str_hyphen;
              Buffer.add_string buff str_sep;
              String.length str_hyphen + String.length str_sep )
            else (
              Buffer.add_string buff str_sep;
              String.length str_sep )
          in
          Buffer.add_string buff str;
          comment_line_aux false
            (sep_length + String.length str + line_length)
            tl
      | [] -> Buffer.contents buff
    in
    comment_line_aux true 0 token_lst
  in
  let output_po_translation_aux _ commented_translation =
    ( match commented_translation.po_comment_filepos with
    | [] -> ()
    | lst ->
        fpf "%s\n"
          (comment_line "#." " " comment_max_length
             ( "#:"
             :: List.map
                  (fun (str, line) -> Printf.sprintf "%s:%d" str line)
                  lst )) );
    ( match commented_translation.po_comment_special with
    | [] -> ()
    | lst ->
        fpf "%s\n" (comment_line "#." " " comment_max_length ("#," :: lst)) );
    ( match commented_translation.po_comment_translation with
    | PoSingular (id, str) ->
        fpf "msgid %a\n" hyphens id;
        fpf "msgstr %a\n" hyphens str
    | PoPlural (id, id_plural, lst) ->
        fpf "msgid %a\n" hyphens id;
        fpf "msgid_plural %a\n" hyphens id_plural;
        let _ =
          List.fold_left
            (fun i s ->
              fpf "msgstr[%i] %a\n" i hyphens s;
              i + 1)
            0 lst
        in
        () );
    fpf "\n"
  in
  MapString.iter output_po_translation_aux po.no_domain;
  MapTextdomain.iter
    (fun domain map ->
      fpf "domain %S\n\n" domain;
      MapString.iter output_po_translation_aux map)
    po.domain

let translation_of_po_translation po_translation =
  match po_translation with
  | PoSingular (id, str) -> Singular (String.concat "" id, String.concat "" str)
  | PoPlural (id, id_plural, lst) ->
      Plural
        ( String.concat "" id,
          String.concat "" id_plural,
          List.map (String.concat "") lst )

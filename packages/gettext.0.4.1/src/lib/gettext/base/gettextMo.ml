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

open GettextUtils
(**
    @author Sylvain Le Gall
  *)

open GettextTypes
open GettextMo_int32

let mo_sig_be = int32_of_byte (0x95, 0x04, 0x12, 0xde)

let mo_sig_le = int32_of_byte (0xde, 0x12, 0x04, 0x95)

let check_mo_header chn hdr =
  let offset_min = Int32.of_int 28 in
  let offset_max = Int32.of_int (in_channel_length chn) in
  let range_offset start_bound =
    let end_bound =
      Int32.add start_bound
        (Int32.mul (Int32.pred hdr.number_of_strings) (Int32.of_int 8))
    in
    ((offset_min, offset_max), (start_bound, end_bound))
  in
  let val_in_range (start_bound, end_bound) value =
    Int32.compare start_bound value <= 0 && Int32.compare value end_bound <= 0
  in
  (* check_* function return true in case of problem *)
  let check_overlap start_bound1 start_bound2 =
    let _, (_, end_bound1) = range_offset start_bound1 in
    let _, (_, end_bound2) = range_offset start_bound2 in
    val_in_range (start_bound1, end_bound1) start_bound2
    || val_in_range (start_bound1, end_bound1) end_bound2
    || val_in_range (start_bound2, end_bound2) start_bound1
    || val_in_range (start_bound2, end_bound2) end_bound1
  in
  let check_range_offset start_bound =
    let file, (start_bound, end_bound) = range_offset start_bound in
    not (val_in_range file start_bound && val_in_range file end_bound)
  in
  if Int32.compare hdr.number_of_strings Int32.zero < 0 then
    raise MoInvalidHeaderNegativeStrings
  else if check_range_offset hdr.offset_table_strings then
    raise
      (MoInvalidHeaderTableStringOutOfBound
         ( fst (range_offset hdr.offset_table_strings),
           snd (range_offset hdr.offset_table_strings) ))
  else if check_range_offset hdr.offset_table_translation then
    raise
      (MoInvalidHeaderTableTranslationOutOfBound
         ( fst (range_offset hdr.offset_table_translation),
           snd (range_offset hdr.offset_table_translation) ))
  else if check_overlap hdr.offset_table_translation hdr.offset_table_strings
  then
    raise
      (MoInvalidHeaderTableTranslationStringOverlap
         ( snd (range_offset hdr.offset_table_translation),
           snd (range_offset hdr.offset_table_strings) ))
    (* We don't care of hashing table, since we don't use it *)
  else hdr

let input_mo_header chn =
  let endianess =
    let magic_number =
      seek_in chn 0;
      input_int32 chn BigEndian
    in
    if magic_number = mo_sig_be then BigEndian
    else if magic_number = mo_sig_le then LittleEndian
    else raise MoInvalidFile
  in
  let seek_and_input x =
    seek_in chn x;
    input_int32 chn endianess
  in
  check_mo_header chn
    {
      endianess;
      file_format_revision = seek_and_input 4;
      number_of_strings = seek_and_input 8;
      offset_table_strings = seek_and_input 12;
      offset_table_translation = seek_and_input 16;
      size_of_hashing_table = seek_and_input 20;
      offset_of_hashing_table = seek_and_input 24;
    }

let output_mo_header chn hdr =
  let output = output_int32 chn hdr.endianess in
  (* magic_number : be is the native way to
  * specify it, it will be translated through
  * the output_int32*)
  output mo_sig_be;
  output hdr.file_format_revision;
  output hdr.number_of_strings;
  output hdr.offset_table_strings;
  output hdr.offset_table_translation;
  output hdr.size_of_hashing_table;
  output hdr.offset_of_hashing_table

let string_of_mo_header mo_header =
  let buff = Buffer.create 256 in
  Printf.bprintf buff "File format revision                     : %ld\n"
    mo_header.file_format_revision;
  Printf.bprintf buff "Number of string                         : %ld\n"
    mo_header.number_of_strings;
  Printf.bprintf buff "Offset of table with original strings    : %lx\n"
    mo_header.offset_table_strings;
  Printf.bprintf buff "Offset of table with translation strings : %lx\n"
    mo_header.offset_table_translation;
  Printf.bprintf buff "Size of hashing table                    : %lx\n"
    mo_header.size_of_hashing_table;
  Printf.bprintf buff "Offset of hashing table                  : %lx\n"
    mo_header.offset_of_hashing_table;
  Buffer.contents buff

let input_mo_untranslated _failsafe chn mo_header number =
  if number < Int32.to_int mo_header.number_of_strings then
    let offset_pair =
      Int32.to_int mo_header.offset_table_strings + (number * 8)
    in
    let str =
      try
        seek_in chn offset_pair;
        input_int32_pair_string chn mo_header.endianess
      with End_of_file ->
        raise (MoInvalidStringOutOfBound (in_channel_length chn, offset_pair))
    in
    split_plural str
  else
    raise
      (MoInvalidStringOutOfBound
         (Int32.to_int mo_header.number_of_strings, number))

let input_mo_translated _failsafe chn mo_header number =
  if number < Int32.to_int mo_header.number_of_strings then
    let offset_pair =
      Int32.to_int mo_header.offset_table_translation + (number * 8)
    in
    let str =
      try
        seek_in chn offset_pair;
        input_int32_pair_string chn mo_header.endianess
      with End_of_file ->
        raise
          (MoInvalidTranslationOutOfBound (in_channel_length chn, offset_pair))
    in
    split_plural str
  else
    raise
      (MoInvalidStringOutOfBound
         (Int32.to_int mo_header.number_of_strings, number))

let input_mo_translation failsafe chn mo_header number =
  let untranslated = input_mo_untranslated failsafe chn mo_header number in
  let translated = input_mo_translated failsafe chn mo_header number in
  match untranslated with
  | [ id ] -> Singular (id, String.concat "\000" translated)
  | [ id; id_plural ] -> Plural (id, id_plural, translated)
  | id :: id_plural :: tl ->
      fail_or_continue failsafe
        (MoJunk (id, tl))
        (Plural (id, id_plural, translated))
  | [] -> fail_or_continue failsafe MoEmptyEntry (Singular ("", ""))

let get_translated_value failsafe translation plural_number =
  match (translation, plural_number) with
  | Singular (_, str), 0 -> str
  | Singular (_, str), x ->
      fail_or_continue failsafe (MoInvalidTranslationSingular (str, x)) str
  | Plural (str, str_plural, []), x -> if x = 0 then str else str_plural
  | Plural (_, _, lst), x when x < List.length lst -> List.nth lst x
  | Plural (_, _, lst), x ->
      fail_or_continue failsafe
        (MoInvalidTranslationPlural (lst, x))
        List.nth lst 0

let germanic_plural (* The germanic default *) n = if n = 1 then 1 else 0

let input_mo_informations failsafe chn mo_header =
  (* La position de "" est forcément 0 *)
  let empty_translation =
    get_translated_value failsafe
      (input_mo_translation failsafe chn mo_header 0)
      0
  in
  let field_value =
    let lexbuf = Lexing.from_string empty_translation in
    try GettextMo_parser.main GettextMo_lexer.token_field_name lexbuf
    with Parsing.Parse_error | Failure _ ->
      fail_or_continue failsafe
        (MoInvalidOptions (lexbuf, empty_translation))
        []
  in
  let nplurals, fun_plural_forms =
    try
      let field_plural_forms = List.assoc "Plural-Forms" field_value in
      let lexbuf = Lexing.from_string field_plural_forms in
      try
        GettextMo_parser.plural_forms GettextMo_lexer.token_field_plural_value
          lexbuf
      with Parsing.Parse_error | Failure _ ->
        fail_or_continue failsafe
          (MoInvalidPlurals (lexbuf, field_plural_forms))
          (2, germanic_plural)
    with Not_found -> (2, germanic_plural)
  in
  let _content_type, content_type_charset =
    let gettext_content = ("text/plain", GettextConfig.default_codeset) in
    try
      let field_content_type = List.assoc "Content-Type" field_value in
      let lexbuf = Lexing.from_string field_content_type in
      try
        GettextMo_parser.content_type GettextMo_lexer.token_field_content_type
          lexbuf
      with Parsing.Parse_error | Failure _ ->
        fail_or_continue failsafe
          (MoInvalidContentType (lexbuf, field_content_type))
          gettext_content
    with Not_found -> gettext_content
  in
  let extract_field_string name =
    try Some (List.assoc name field_value) with Not_found -> None
  in
  {
    project_id_version = extract_field_string "Project-Id-Version";
    report_msgid_bugs_to = extract_field_string "Report-Msgid-Bugs-To";
    pot_creation_date = extract_field_string "POT-Creation-Date";
    po_revision_date = extract_field_string "PO-Revision-Date";
    last_translator = extract_field_string "Last-Translator";
    language_tream = extract_field_string "Language-Team";
    mime_version = extract_field_string "MIME-Version";
    content_type = extract_field_string "Content-Type";
    content_transfer_encoding =
      extract_field_string "Content-Transfer-Encoding";
    plural_forms = extract_field_string "Plural-Forms";
    content_type_charset;
    nplurals;
    fun_plural_forms;
  }

let string_of_mo_informations ?(compute_plurals = (0, 3)) mo_translation =
  let buff = Buffer.create 1024 in
  let p = Printf.bprintf in
  let extract_string x = match x with Some s -> s | None -> "" in
  p buff "Project-Id-Version        : %s\n"
    (extract_string mo_translation.project_id_version);
  p buff "Report-Msgid-Bugs-To      : %s\n"
    (extract_string mo_translation.report_msgid_bugs_to);
  p buff "POT-Creation-Date         : %s\n"
    (extract_string mo_translation.pot_creation_date);
  p buff "PO-Revision-Date          : %s\n"
    (extract_string mo_translation.po_revision_date);
  p buff "Last-Translator           : %s\n"
    (extract_string mo_translation.last_translator);
  p buff "Language-Team             : %s\n"
    (extract_string mo_translation.language_tream);
  p buff "MIME-Version              : %s\n"
    (extract_string mo_translation.mime_version);
  p buff "Content-Type              : %s\n"
    (extract_string mo_translation.content_type);
  p buff "Plurals-Forms             : %s\n"
    (extract_string mo_translation.plural_forms);
  p buff "Content-Transfer-Encoding : %s\n"
    (extract_string mo_translation.content_transfer_encoding);
  p buff "Content-Type-Charset      : %s\n" mo_translation.content_type_charset;
  p buff "NPlurals                  : %d\n" mo_translation.nplurals;
  p buff "Fun plural                : ";
  (let a, b = compute_plurals in
   for i = a to b do
     p buff "%d -> %d ; " i (mo_translation.fun_plural_forms i)
   done);
  p buff "\n";
  Buffer.contents buff

let output_mo ?(endianess = LittleEndian) chn lst =
  (* There could have potential issue with alignment, but it seems to be fixed
  * at 1 in gettext-0.14.1/gettext-tools/configure.ac, so there is no probleme
  * *)
  let null_terminated lst = List.map (fun str -> str ^ "\000") lst in
  let compute_table start_pos lst =
    let compute_length lst = List.map String.length lst in
    let compute_offset (current_pos, lst_pos) length =
      (* Remove 1 since we have NULL terminated strings *)
      (current_pos + length, (length - 1, current_pos) :: lst_pos)
    in
    let final_pos, lst_rev =
      List.fold_left compute_offset (start_pos, []) (compute_length lst)
    in
    (final_pos, List.rev lst_rev)
  in
  let no_empty_lst =
    (* Avoid using empty translated string *)
    List.filter
      (function
        | Singular (_, "") -> false
        | Plural (_, _, lst) when String.concat "" lst = "" -> false
        | _ -> true)
      lst
  in
  let sorted_lst =
    let compare_entry entry1 entry2 =
      let value_of_entry entry =
        match entry with Singular (id, _) -> id | Plural (id, _, _) -> id
      in
      String.compare (value_of_entry entry1) (value_of_entry entry2)
    in
    List.sort compare_entry no_empty_lst
  in
  let untranslated =
    let to_string entry =
      match entry with
      | Singular (id, _) -> id
      | Plural (id, id_plural, _) -> id ^ "\000" ^ id_plural
    in
    null_terminated (List.map to_string sorted_lst)
  in
  let translated =
    let to_string entry =
      match entry with
      | Singular (_, str) -> str
      | Plural (_, _, lst) -> String.concat "\000" lst
    in
    null_terminated (List.map to_string sorted_lst)
  in
  let gN = List.length sorted_lst in
  let gO = 28 (* Size of the header *) in
  let gT = gO + (8 * gN) in
  let gS =
    0
    (* Hashtable is not implemented, since algorithm is not public -- documented *)
  in
  let gH = gT + (8 * gN) in
  let final_untranslated, untranslated_table =
    compute_table (gH + (gS * 4)) untranslated
  in
  let _, translated_table = compute_table final_untranslated translated in
  let header =
    {
      endianess;
      file_format_revision = Int32.zero;
      number_of_strings = Int32.of_int gN;
      offset_table_strings = Int32.of_int gO;
      offset_table_translation = Int32.of_int gT;
      size_of_hashing_table = Int32.of_int gS;
      offset_of_hashing_table = Int32.of_int gH;
    }
  in
  output_mo_header chn header;
  List.iter
    (List.iter (fun (a, b) ->
         output_int32_pair chn endianess (Int32.of_int a, Int32.of_int b)))
    [ untranslated_table; translated_table ];
  List.iter (output_string chn) untranslated;
  List.iter (output_string chn) translated

let fold_mo failsafe f init fl_mo =
  let chn = open_in_bin fl_mo in
  let res =
    try
      (* Processing of the file *)
      let mo_header = input_mo_header chn in
      let informations = input_mo_informations failsafe chn mo_header in
      let fun_plural_forms = informations.GettextTypes.fun_plural_forms in
      let rec fold_mo_aux accu i =
        if i < Int32.to_int mo_header.number_of_strings then
          let new_translation =
            input_mo_translation failsafe chn mo_header i
          in
          let new_accu = f new_translation accu in
          fold_mo_aux new_accu (i + 1)
        else accu
      in
      let translations = fold_mo_aux init 0 in
      (translations, fun_plural_forms)
    with Sys_error _ ->
      fail_or_continue failsafe (MoCannotOpenFile fl_mo) (init, germanic_plural)
  in
  close_in chn;
  res

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

(** Signature of module for translation storage / access.
    @author Sylvain Le Gall
  *)

open GettextTypes
open GettextUtils
open GettextMo
open GettextFormat

module type TRANSLATE_TYPE = sig
  type u

  val create : t -> filename -> (string -> string) -> u
  (** create t filename recode : Create a translation
        table using filename as the mo file and recode as the encoding
        converter.
    *)

  (* BUG : need update *)

  val translate : u -> bool -> string -> (string * int) option -> string
  (** translate str (plural_form,number) tbl : translate the string
        str using tbl. It is possible that the operation modify tbl,
        so it is returned also. It is also possible to get the plural
        form of the translated string using plural_form and number.
    *)
end

module Dummy : TRANSLATE_TYPE = struct
  type u = string -> string

  let create _t _filename charset = charset

  let translate charset printf_format str plural_form =
    match plural_form with
    | None -> charset str
    | Some (str_plural, x) ->
        let check =
          if printf_format then check_format Ignore else fun x -> x
        in
        charset
          (get_translated_value Ignore
             (check (Plural (str, str_plural, [])))
             (germanic_plural x))
end

module Map : TRANSLATE_TYPE = struct
  type u = {
    dummy : Dummy.u;
    map : translation MapString.t;
    failsafe : failsafe;
    fun_plural_forms : int -> int;
  }

  let create t filename charset =
    let map, fun_plural_forms =
      fold_mo t.GettextTypes.failsafe
        (fun translation accu ->
          match translation with
          | Singular (str_id, str) ->
              MapString.add str_id (Singular (str_id, charset str)) accu
          | Plural (str_id, str_plural, lst) ->
              MapString.add str_id
                (Plural (str_id, str_plural, List.map charset lst))
                accu)
        MapString.empty filename
    in
    {
      dummy = Dummy.create t filename charset;
      map;
      failsafe = t.GettextTypes.failsafe;
      fun_plural_forms;
    }

  let translate u printf_format str plural_form =
    try
      let plural_number =
        u.fun_plural_forms
          (match plural_form with Some (_, x) -> x | None -> 0)
      in
      let check =
        if printf_format then check_format u.failsafe else fun x -> x
      in
      get_translated_value u.failsafe
        (check (MapString.find str u.map))
        plural_number
    with Not_found ->
      fail_or_continue u.failsafe (TranslateStringNotFound str)
        (Dummy.translate u.dummy printf_format str plural_form)
end

module Hashtbl : TRANSLATE_TYPE = struct
  type u = {
    dummy : Dummy.u;
    hashtbl : (string, translation) Hashtbl.t;
    failsafe : failsafe;
    fun_plural_forms : int -> int;
  }

  let create t filename charset =
    let hashtbl, fun_plural_forms =
      fold_mo t.GettextTypes.failsafe
        (fun translation accu ->
          match translation with
          | Singular (str_id, str) ->
              Hashtbl.add accu str_id (Singular (str_id, charset str));
              accu
          | Plural (str_id, str_plural, lst) ->
              Hashtbl.add accu str_id
                (Plural (str_id, str_plural, List.map charset lst));
              accu)
        (* 32 is only a guest on the number of string contains in the
           future table *)
        (Hashtbl.create 32)
        filename
    in
    {
      dummy = Dummy.create t filename charset;
      hashtbl;
      failsafe = t.GettextTypes.failsafe;
      fun_plural_forms;
    }

  let translate u printf_format str plural_form =
    try
      let plural_number =
        u.fun_plural_forms
          (match plural_form with Some (_, x) -> x | None -> 0)
      in
      let check =
        if printf_format then check_format u.failsafe else fun x -> x
      in
      get_translated_value u.failsafe
        (check (Hashtbl.find u.hashtbl str))
        plural_number
    with Not_found ->
      fail_or_continue u.failsafe (TranslateStringNotFound str)
        (Dummy.translate u.dummy printf_format str plural_form)
end

module Open : TRANSLATE_TYPE = struct
  type u = {
    dummy : Dummy.u;
    filename : filename;
    charset : string -> string;
    failsafe : failsafe;
    fun_plural_forms : int -> int;
    number_of_strings : int;
  }

  let create t filename charset =
    (* Processing of the file *)
    let chn = open_in_bin filename in
    let header = input_mo_header chn in
    let informations =
      input_mo_informations t.GettextTypes.failsafe chn header
    in
    close_in chn;
    {
      dummy = Dummy.create t filename charset;
      filename;
      charset;
      failsafe = t.GettextTypes.failsafe;
      fun_plural_forms = informations.GettextTypes.fun_plural_forms;
      number_of_strings = Int32.to_int header.GettextTypes.number_of_strings;
    }

  let translate u printf_format str plural_form =
    let chn = open_in_bin u.filename in
    let res =
      try
        let plural_number =
          u.fun_plural_forms
            (match plural_form with Some (_, x) -> x | None -> 0)
        in
        let header = input_mo_header chn in
        let rec find_str_id (start_index, end_index) =
          let middle_index = (start_index + end_index) / 2 in
          let str_id =
            let lst_str_id =
              input_mo_untranslated u.failsafe chn header middle_index
            in
            match lst_str_id with
            | str_id :: _ -> str_id
            | [] ->
                (* BUG : should be a real exception *)
                raise Not_found
          in
          match String.compare str str_id with
          | x when x < 0 && start_index <= middle_index - 1 ->
              find_str_id (start_index, middle_index - 1)
          | x when x > 0 && middle_index + 1 <= end_index ->
              find_str_id (middle_index + 1, end_index)
          | x when x = 0 -> middle_index
          | _ -> raise Not_found
        in
        let translation =
          let translation =
            input_mo_translation u.failsafe chn header
              (find_str_id (0, u.number_of_strings - 1))
          in
          match translation with
          | Singular (str_id, str) -> Singular (str_id, u.charset str)
          | Plural (str_id, str_plural, lst) ->
              Plural (str_id, str_plural, List.map u.charset lst)
        in
        get_translated_value u.failsafe translation plural_number
      with Not_found ->
        fail_or_continue u.failsafe (TranslateStringNotFound str)
          (Dummy.translate u.dummy printf_format str plural_form)
    in
    close_in chn;
    res
end

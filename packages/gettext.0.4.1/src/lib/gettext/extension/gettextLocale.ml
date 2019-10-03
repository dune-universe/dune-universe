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

(** Implements different operation over locale/category.
    @author Sylvain Le Gall
  *)

open GettextTypes
open GettextCategory
open GettextUtils

module type LOCALE_TYPE = sig
  val get_locale : t -> category -> locale list * codeset
  (** get_locale t cat : Return the value of locale and encoding for cat.
        The value returned is in ASCII. Priority should be given to the
        values language/codeset provided in variable t.
    *)
end

(** Return the best value of environnement variable, that can be found according to the
    priority defined in gettext. The choice take into account t and category,
    but may ignore it, if a variable with a best priority is set.
    This function can be used to get a value for a LOCALE_TYPE implementation.
    Raise Not_found if nothing appropriate.
*)
let posix_getenv t category =
  (* http://www.gnu.org/software/gettext/manual/html_mono/gettext.html#SEC155
         In the function dcgettext at every call the current setting of the
         highest priority environment variable is determined and used.
         Highest priority means here the following list with decreasing priority:
          1. LANGUAGE
          2. LC_ALL
          3. LC_xxx, according to selected locale
          4. LANG
      *)
  match t.language with
  | Some str -> str
  | None -> (
      try
        let best_env =
          List.find
            (fun s ->
              try
                ignore (Sys.getenv s);
                true
              with Not_found -> false)
            [
              "LANGUAGE";
              string_of_category LC_ALL;
              string_of_category category;
              "LANG";
            ]
        in
        Sys.getenv best_env
      with Not_found -> "C" )

module Posix : LOCALE_TYPE = struct
  (* Extract from "man setlocale"
       A locale name is typically of  the  form  language[_territory][.codeset][@modi-
       fier],  where  language  is  an ISO 639 language code, territory is an ISO 3166
       country code, and codeset is  a  character  set  or  encoding  identifier  like
       ISO-8859-1 or UTF-8.  For a list of all supported locales, try "locale -a", cf.
       locale(1).
     *)

  let get_locale t category =
    let posix_lang = posix_getenv t category in
    let locale =
      try
        let lexbuf = Lexing.from_string posix_lang in
        GettextLocale_parser.main GettextLocale_lexer.token lexbuf
      with x ->
        fail_or_continue t.failsafe
          (LocalePosixUnparseable (posix_lang ^ " " ^ Printexc.to_string x))
          (GettextLocale_types.create_locale posix_lang)
    in
    let locales =
      match
        ( locale.GettextLocale_types.territory,
          locale.GettextLocale_types.modifier )
      with
      | Some territory, Some modifier ->
          [
            locale.GettextLocale_types.language ^ "_" ^ territory ^ "@"
            ^ modifier;
            locale.GettextLocale_types.language ^ "_" ^ territory;
            locale.GettextLocale_types.language;
          ]
      | None, Some modifier ->
          [
            locale.GettextLocale_types.language ^ "@" ^ modifier;
            locale.GettextLocale_types.language;
          ]
      | Some territory, None ->
          [
            locale.GettextLocale_types.language ^ "_" ^ territory;
            locale.GettextLocale_types.language;
          ]
      | None, None -> [ locale.GettextLocale_types.language ]
    in
    let codeset =
      match locale.GettextLocale_types.codeset with
      | Some codeset -> codeset
      | None -> t.codeset
    in
    (locales, codeset)
end

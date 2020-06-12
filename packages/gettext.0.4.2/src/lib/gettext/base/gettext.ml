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

open GettextTypes
open GettextCompat
open GettextUtils
open GettextModules
open Lexing

(* Function the main global variable of gettext with/without thread *)

type global_type = { t : t option; realize : t -> t'; t' : t' option }

(* Default value *)

let dummy_realize _t _printf_format _textdomain str _str_plural _category = str

let default_realize = dummy_realize

(* Referenced function used to manage access to global variable,
   in other word to be fullfiled with mutex locking/unlocking if needed
 *)

let global_lock = ref (fun () -> ())

let global_unlock = ref (fun () -> ())

let global = ref { t = None; realize = default_realize; t' = None }

let get_global_t () =
  let t =
    !global_lock ();
    !global.t
  in
  !global_unlock ();
  match t with Some t -> t | None -> raise GettextUninitialized

let set_global_t t =
  let () =
    !global_lock ();
    global := { !global with t = Some t; t' = None }
  in
  !global_unlock ()

let set_global_realize realize =
  let () =
    !global_lock ();
    global := { !global with realize; t' = None }
  in
  !global_unlock ()

let get_global_t' () =
  let t' =
    !global_lock ();
    match !global.t' with
    | None ->
        (* Try to build it out of the other value provided *)
        let t =
          match !global.t with
          | Some t -> t
          | None -> raise GettextUninitialized
        in
        let t' = !global.realize t in
        global := { !global with t' = Some t' };
        t'
    | Some t' -> t'
  in
  !global_unlock ();
  t'

(* High level functions *)

module Library (Init : INIT_TYPE) = struct
  let init = (Init.textdomain, Init.codeset, Init.dir) :: Init.dependencies

  let s_ str = dgettext (get_global_t' ()) Init.textdomain str

  let f_ str = fdgettext (get_global_t' ()) Init.textdomain str

  let sn_ str = dngettext (get_global_t' ()) Init.textdomain str

  let fn_ str = fdngettext (get_global_t' ()) Init.textdomain str
end

(* i18n/l10n of gettext it self *)
module GettextGettext = Library (struct
  let textdomain = "ocaml-gettext"

  let codeset = None

  let dir = None

  let dependencies = []

  (* Off course, we don't depend on anything because
     we are the root of translation *)
end)

(* Initialization of gettext library *)

let init = GettextGettext.init

(* Exception *)

let string_of_exception exc =
  (* It is important to keep the name f_ and s_, in order to allow ocaml-gettext
     program to extract the string *)
  let f_ x = GettextGettext.f_ x in
  let s_ x = GettextGettext.s_ x in
  let spf x = Printf.sprintf x in
  let string_of_pos lexbuf =
    let char_pos = lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol in
    let line_pos = lexbuf.lex_curr_p.pos_lnum in
    spf (f_ "line %d character %d") line_pos char_pos
  in
  match exc with
  | CompileProblemReadingFile (fln, error) ->
      spf (f_ "Problem reading file %s: %s.") fln error
  | CompileExtractionFailed (fln, cmd, status) ->
      spf
        (f_ "Problem while extracting %s: command %S exits with code %d.")
        fln cmd status
  | CompileExtractionInterrupted (fln, cmd, signal) ->
      spf
        (f_ "Problem while extracting %s: command %S killed by signal %d.")
        fln cmd signal
  | DomainFileDoesntExist lst ->
      spf
        (f_ "Cannot find an approriate ocaml-gettext compiled file ( %s ).")
        (string_of_list lst)
  | GettextUninitialized -> s_ "Ocaml-gettext library is not initialized"
  | MoInvalidOptions (lexbuf, text) ->
      spf
        (f_ "Error while processing parsing of options at %s: %S.")
        (string_of_pos lexbuf) text
  | MoInvalidPlurals (lexbuf, text) ->
      spf
        (f_ "Error while processing parsing of plural at %s: %S.")
        (string_of_pos lexbuf) text
  | MoInvalidContentType (lexbuf, text) ->
      spf
        (f_ "Error while processing parsing of content-type at %s: %S.")
        (string_of_pos lexbuf) text
  | MoInvalidFile ->
      s_ "MO file provided is not encoded following ocaml-gettext convention."
  | MoInvalidTranslationSingular (str, x) ->
      spf
        (f_ "Trying to fetch the plural form %d of a singular form %S.")
        x str
  | MoInvalidTranslationPlural (lst, x) ->
      spf
        (f_ "Trying to fetch the plural form %d of plural form %s.")
        x (string_of_list lst)
  | MoJunk (id, lst) ->
      spf
        (f_ "Junk at the end of the plural form id %S: %s.")
        id (string_of_list lst)
  | MoEmptyEntry -> s_ "An empty entry has been encounter."
  | MoInvalidHeaderNegativeStrings -> s_ "Number of strings is negative."
  | MoInvalidHeaderTableStringOutOfBound ((b1, e1), (b2, e2)) ->
      spf
        (f_
           "Offset of string table is out of bound ([%ld,%ld] should be in \
            [%ld,%ld]).")
        b1 e1 b2 e2
  | MoInvalidHeaderTableTranslationOutOfBound ((b1, e1), (b2, e2)) ->
      spf
        (f_
           "Offset of translation table is out of bound ([%ld,%ld] should be \
            in [%ld,%ld]).")
        b1 e1 b2 e2
  | MoInvalidHeaderTableTranslationStringOverlap ((b1, e1), (b2, e2)) ->
      spf
        (f_
           "Translation table and string table overlap ([%ld,%ld] and \
            [%ld,%ld] have a non empty intersection).")
        b1 e1 b2 e2
  | MoInvalidStringOutOfBound (max, cur) ->
      spf
        (f_ "Out of bound access when trying to find a string (%d < %d).")
        max cur
  | MoInvalidTranslationOutOfBound (max, cur) ->
      spf
        (f_ "Out of bound access when trying to find a translation (%d < %d).")
        max cur
  | MoCannotOpenFile fln -> spf (f_ "Could not open file %s.") fln
  | PoInvalidFile (s, lexbuf, _chn) ->
      spf
        (f_ "Error while processing parsing of PO file: %S at %s.")
        s (string_of_pos lexbuf)
  | PoFileInvalidIndex (id, i) ->
      spf
        (f_
           "Error while processing parsing of PO file, in msgid %S, %d index \
            is out of bound.")
        id i
  | PoFileDoesntExist fl ->
      spf (f_ "Error while trying to load PO file %s, file doesn't exist.") fl
  | PoInconsistentMerge (str1, str2) ->
      spf
        (f_ "Error while merging two PO files: %S and %S cannot be merged.")
        str1 str2
  | TranslateStringNotFound str -> spf (f_ "Cannot find string %S.") str
  | LocalePosixUnparseable str ->
      spf (f_ "Unable to parse the POSIX language environment variable %s") str
  | _ -> Printexc.to_string exc

module Program (Init : INIT_TYPE) (Realize : REALIZE_TYPE) = struct
  let textdomain = Init.textdomain

  let dependencies =
    (Init.textdomain, Init.codeset, Init.dir) :: Init.dependencies

  let init =
    (* Initialization from all the known textdomain/codeset/dir provided
         by library linked with the program *)
    (* It is important to keep f_ and s_, for the same reason as in
         string_of_exception *)
    let f_ x = GettextGettext.f_ x in
    let s_ x = GettextGettext.s_ x in
    let spf x = Printf.sprintf x in
    let () = set_global_t (GettextModules.create textdomain) in
    let () =
      set_global_t
        (List.fold_left
           (fun t (textdomain, codeset_opt, dir_opt) ->
             upgrade_textdomain t textdomain (codeset_opt, dir_opt))
           (get_global_t ()) dependencies)
    in
    let () = set_global_realize Realize.realize in
    ( [
        ( "--gettext-failsafe",
          Arg.Symbol
            ( [ "ignore"; "inform-stderr"; "raise-exception" ],
              fun x ->
                match x with
                | "ignore" ->
                    set_global_t { (get_global_t ()) with failsafe = Ignore }
                | "inform-stderr" ->
                    set_global_t
                      {
                        (get_global_t ()) with
                        failsafe = InformStderr string_of_exception;
                      }
                | "raise-exception" ->
                    set_global_t
                      { (get_global_t ()) with failsafe = RaiseException }
                | _ -> () ),
          spf
            (f_ " Choose how to handle failure in ocaml-gettext. Default: %s.")
            ( match (get_global_t ()).failsafe with
            | Ignore -> "ignore"
            | InformStderr _ -> "inform-stderr"
            | RaiseException -> "raise-exception" ) );
        ( "--gettext-disable",
          Arg.Unit (fun () -> set_global_realize dummy_realize),
          s_
            " Disable the translation perform by ocaml-gettext. Default: \
             enable." );
        ( "--gettext-domain-dir",
          (let current_textdomain = ref textdomain in
           Arg.Tuple
             [
               Arg.String (fun textdomain -> current_textdomain := textdomain);
               Arg.String
                 (fun dir ->
                   set_global_t
                     (bindtextdomain !current_textdomain dir (get_global_t ())));
             ]),
          spf
            (f_
               "textdomain dir Set a dir to search ocaml-gettext files for \
                the specified domain. Default: %s.")
            (string_of_list
               (MapTextdomain.fold
                  (fun textdomain (_, dir_opt) lst ->
                    match dir_opt with
                    | Some dir -> spf "%s: %s" textdomain dir :: lst
                    | None -> lst)
                  (get_global_t ()).textdomains [])) );
        ( "--gettext-dir",
          Arg.String
            (fun s ->
              set_global_t
                { (get_global_t ()) with path = s :: (get_global_t ()).path }),
          spf
            (f_ "dir Add a search dir for ocaml-gettext files. Default: %s.")
            (string_of_list (get_global_t ()).path) );
        ( "--gettext-language",
          Arg.String
            (fun s ->
              set_global_t { (get_global_t ()) with language = Some s }),
          spf
            (f_
               "language Set the default language for ocaml-gettext. Default: \
                %s.")
            ( match (get_global_t ()).language with
            | Some s -> s
            | None -> "(none)" ) );
        ( "--gettext-codeset",
          Arg.String
            (fun s -> set_global_t { (get_global_t ()) with codeset = s }),
          spf
            (f_
               "codeset Set the default codeset for outputting string with \
                ocaml-gettext. Default: %s.")
            (get_global_t ()).codeset );
      ],
      GettextConfig.copyright )

  let s_ str = dgettext (get_global_t' ()) textdomain str

  let f_ str = fdgettext (get_global_t' ()) textdomain str

  let sn_ str = dngettext (get_global_t' ()) textdomain str

  let fn_ str = fdngettext (get_global_t' ()) textdomain str
end

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

(** Misc. utilities.
    @author Sylvain Le Gall
  *)

open GettextTypes

let string_of_list lst =
  "[ "
  ^ String.concat "; " (List.map (fun str -> Printf.sprintf "%S" str) lst)
  ^ " ]"

let split_plural str =
  let rec split_plural_one start =
    let next_sep =
      try String.index_from str start '\000'
      with Not_found -> String.length str
    in
    let new_plural = String.sub str start (next_sep - start) in
    if next_sep + 1 >= String.length str then [ new_plural ]
    else new_plural :: split_plural_one (next_sep + 1)
  in
  split_plural_one 0

let fail_or_continue failsafe exc cont_value =
  match failsafe with
  | Ignore -> cont_value
  | InformStderr exc_printer ->
      prerr_string (exc_printer exc);
      prerr_newline ();
      cont_value
  | RaiseException -> raise exc

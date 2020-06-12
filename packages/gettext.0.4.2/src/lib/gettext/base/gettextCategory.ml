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

(** Functions to manipulate category.
    @author Sylvain Le Gall
  *)

type category =
  | LC_CTYPE
  | LC_NUMERIC
  | LC_TIME
  | LC_COLLATE
  | LC_MONETARY
  | LC_MESSAGES
  | LC_ALL

let string_of_category cat =
  match cat with
  | LC_CTYPE -> "LC_CTYPE"
  | LC_NUMERIC -> "LC_NUMERIC"
  | LC_TIME -> "LC_TIME"
  | LC_COLLATE -> "LC_COLLATE"
  | LC_MONETARY -> "LC_MONETARY"
  | LC_MESSAGES -> "LC_MESSAGES"
  | LC_ALL -> "LC_ALL"

let category_of_string str =
  match str with
  | "LC_CTYPE" -> LC_CTYPE
  | "LC_NUMERIC" -> LC_NUMERIC
  | "LC_TIME" -> LC_TIME
  | "LC_COLLATE" -> LC_COLLATE
  | "LC_MONETARY" -> LC_MONETARY
  | "LC_MESSAGES" -> LC_MESSAGES
  | "LC_ALL" -> LC_ALL
  | _ -> raise (Invalid_argument "category_of_string")

let categories =
  [
    LC_CTYPE;
    LC_NUMERIC;
    LC_TIME;
    LC_COLLATE;
    LC_MONETARY;
    LC_MESSAGES;
    LC_ALL;
  ]

let compare c1 c2 =
  let val_category x =
    match x with
    | LC_CTYPE -> 0
    | LC_NUMERIC -> 1
    | LC_TIME -> 2
    | LC_COLLATE -> 3
    | LC_MONETARY -> 4
    | LC_MESSAGES -> 5
    | LC_ALL -> 6
  in
  compare (val_category c1) (val_category c2)

module MapCategory = Map.Make (struct
  type t = category

  let compare = compare
end)

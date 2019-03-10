(***************************************************************************)
(*                                 Morsmall                                *)
(*                      A concise AST for POSIX shell                      *)
(*                                                                         *)
(*  Copyright (C) 2017,2018,2019 Yann Régis-Gianas, Ralf Treinen,          *)
(*  Nicolas Jeannerod                                                      *)
(*                                                                         *)
(*  This program is free software: you can redistribute it and/or modify   *)
(*  it under the terms of the GNU General Public License as published by   *)
(*  the Free Software Foundation, either version 3 of the License, or      *)
(*  (at your option) any later version.                                    *)
(*                                                                         *)
(*  This program is distributed in the hope that it will be useful,        *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of         *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          *)
(*  GNU General Public License for more details.                           *)
(*                                                                         *)
(*  You should have received a copy of the GNU General Public License      *)
(*  along with this program.  If not, see <http://www.gnu.org/licenses/>.  *)
(***************************************************************************)

(** abstract syntax of test expressions *)

type expression =
  | And of expression * expression
  | Or  of expression * expression
  | Not of expression
  | Binary of string * string * string   (* (op,arg_left,arg_right) *)
  | Unary  of string * string            (* (op,arg) *)
  | Single of string                     (* arg *)

exception Parse_error

(** [parse ~bracket wl] parses the list of words [wl] as a test
   expression (or [None] if [wl] is empty). If [bracket] is [true]
   then the last word of [wl] must be a right bracket.  *)
val parse: ?bracket:bool -> string list -> expression option

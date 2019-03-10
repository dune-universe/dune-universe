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

module AST = AST
(** Shell AST. *)

(** {2 Parsers} *)

exception SyntaxError of Location.lexing_position

val parse_file : string -> AST.program
(** Parses a whole Shell file into a list of {!AST.command}. The list
   can be empty. Can raise {!SyntaxError}. *)

(** {2 Printers} *)

val pp_print_safe : Format.formatter -> AST.program -> unit
(** Prints a Shell from its AST. *)

val pp_print_debug : Format.formatter -> AST.program -> unit
(** Prints a representation of the AST in OCaml-style. *)

(** {2 Other Modules} *)

module Location = Location
module SafePrinter = SafePrinter
module CST_to_AST = CST_to_AST
module Utilities = Morsmall_utilities

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

open LibraryGettext.Gettext

(* Give access to the init of LibraryGettext *)
let init = Gettext.init

(* Example function *)
let library_only_function () =
  (* Two simple examples : singular translation *)
  print_endline (s_ "Hello world !");
  Printf.printf (f_ "Hello %s !\n") "world";

  (* More complicated : plural translation, using strings *)
  print_endline
    ( sn_ "There is " "There are " 2
    ^ string_of_int 2 ^ sn_ "plate." "plates." 2 );

  (* More simple forms of plural translation, using printf *)
  Printf.printf (fn_ "There is %d plate.\n" "There are %d plates.\n" 2) 2

(* Another example function : used by program.ml *)
let hello_you name = Printf.printf (f_ "Hello %s\n") name

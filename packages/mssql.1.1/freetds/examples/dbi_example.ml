(*
  This file is part of ocaml-freetds - An OCaml binding to the FreeTDS library
  Copyright (C) 2004 Kenneth Knowles

  ocaml-freetds is free software; you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as published by
  the Free Software Foundation; either version 2.1 of the License, or
  (at your option) any later version.

  ocaml-freetds is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public License
  along with ocaml-freetds; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)
open Dbi_freetds
open Printf

let server = ref "localhost"
let database = ref ""
let username = ref (try Sys.getenv "USER" with Not_found -> "")
let password = ref ""
let sql = ref ""
let debug = ref false

let debug_print s =
  if !debug then print_string s else ();
  flush stdout

let print_row row =
  Array.iter (function
      | `String s -> printf "%-20s" s
      | `Int i -> printf "%-20i" i
      | _ -> printf "%-20s" "?")
    row;
  printf "\n%!"

let () =
  try
    Arg.parse [
        "-server", Arg.Set_string server, "Server to connect to";
        "-db", Arg.Set_string database, "Database to connect to";
        "-user", Arg.Set_string username, "Username to connect with";
        "-pwd", Arg.Set_string password, "Password to connect with";
        "-debug", Arg.Set debug, "Enable debugging output";
      ]
      (fun s -> sql := s)
      "Usage: dbi_example [options] sql";

    let conn = new connection ~host:!server ~user:!username
                 ~password:!password !database in

    if !debug then conn # set_debug true;
    (* Cant just conn#set_debug !debug because it has output *)

    debug_print "Created connection\n";

    let sth = conn # ex_multi !sql [] in

    debug_print "Executed SQL\n";

    List.iter (fun rs ->
        Array.iter (fun name -> printf "%-20s" name) (rs.rs_names);
        printf "\n";
        Array.iter print_row rs.rs_rows;
        printf "\n")
      sth # rs_fetch_all;

    conn # close ()
  with
  | Dbi.SQL_error err -> printf "Error:\n%s\n" err

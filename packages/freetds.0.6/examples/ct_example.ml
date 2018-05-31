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

open Freetds.Ct
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

let raise_messages conn =
  List.iter (fun (sev, err) ->
      if sev <> Inform then printf "Client: %s\n" err)
    (get_messages conn ~client:true);

  List.iter (fun (sev, err) ->
      if sev <> Inform then printf "Server: %s\n" err)
    (get_messages conn ~server:true)

let () =
  Arg.parse [
      "-server", Arg.Set_string server, "Server to connect to";
      "-db", Arg.Set_string database, "Database to connect to";
      "-user", Arg.Set_string username, "Username to connect with";
      "-pwd", Arg.Set_string password, "Password to connect with";
      "-debug", Arg.Set debug, "Enable debugging output";
    ]
    (fun s -> sql := s)
    "Usage: ct_example [options] sql";

  let context = ctx_create () in
  let conn = con_alloc context in
  debug_print "Allocated connection\n";

  con_setstring conn `Username !username;
  debug_print (sprintf "Set username to %s\n" !username);

  con_setstring conn `Password !password;
  debug_print (sprintf "Set password to %s\n" !password);

  debug_print (sprintf "Connecting to %s ..." !server);
  connect conn !server;
  debug_print "Connected\n";

  if !database <> "" then
    (try
       let cmd = cmd_alloc conn in
       command cmd `Lang ("USE " ^ !database);
       send cmd;
       ignore (results cmd)
     with
     | Cmd_fail
       | Failure _ -> raise_messages conn);

  debug_print (sprintf "Set database to %s\n" !database);

  let cmd = cmd_alloc conn in
  debug_print "Allocated command\n";

  command cmd `Lang !sql;
  debug_print (sprintf "Set command text to '%s'\n" !sql);

  send cmd;
  debug_print "Sent command\n";
  (try
     while true do
       match (results cmd) with
       | `Cmd_fail -> failwith "Command Failed!"
       | `Cmd_succeed -> debug_print "Command Succeeded\n"
       | `Cmd_done -> ()
       | `Status -> ()
       | `Param -> failwith "Don't handle param results"
       | `Row ->
          debug_print (sprintf "Got row results.\n");
          let cols = Array.init (res_info cmd `Numdata)
                       (fun i -> bind cmd (i+1)) in
          Array.iter (fun col -> printf "%-20s" col.col_name) cols;
          printf "\n%!";
          try
            while true do
              ignore (fetch cmd);
              Array.iter
                (fun col ->
                  match buffer_contents col.col_buffer with
                  | `Datetime s
                    | `Decimal s
                    | `Text s
                    | `String s -> printf "%-20s" s
                  | `Smallint i
                    | `Tinyint i -> printf "%-20i" i
                  | `Float f -> printf "%-20f" f
                  | `Int i -> printf "%-20li" i
                  | `Binary _ -> printf "%-20s" "<binary>"
                  | `Bit b -> printf "%B" b
                  | `Null -> printf "%-20s" "<null>"
                )
                cols;
              printf "\n%!";
            done
          with
          (* We can only get the number of rows after we process them
                                  all *)
          | End_data ->
             match (res_info cmd `Row_count) with
             | -1 -> ()
             | i -> printf "(%i rows affected)\n\n" i
     done;
   with
   | End_results -> ()
   | Cmd_fail -> raise_messages conn
   | Failure s -> printf "%s\n%!" s; raise_messages conn
  );

  close conn

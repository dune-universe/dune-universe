(**************************************************************************)
(*                                                                        *)
(*    Copyright 2018-2021 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

let main ?(downgrades=[]) ~upgrades database =
  let database = ref database in
  let verbose = ref false in
  let witness = ref None in
  let max_version = List.length upgrades in
  let target = ref max_version in
  let old_info = ref false in
  let allow_downgrade = ref false in
  let use_current = ref false in
  let host = ref None in
  let port = ref None in
  let user = ref None in
  let password = ref None in
  let unix_domain_socket_dir = ref None in
  let set_string_opt r = Arg.String (fun s -> r := Some s) in
  let set_int_opt r = Arg.Int (fun s -> r := Some s) in
  Arg.parse [
      "--verbose", Arg.Set verbose, " Set verbose mode";
      "--witness", set_string_opt witness,
      "FILE Touch FILE if database is modified";
      "--target", Arg.Int (fun n ->
                      if n > max_version then begin
                          Printf.eprintf "Cannot target version > %d\n%!"
                                         max_version;
                          exit 2
                        end;
                      target := n),
      "VERSION Target version VERSION";
      "--old-info", Arg.Set old_info, " Use old 'info' table name";
      "--allow-downgrade", Arg.Set allow_downgrade, " Allow downgrade";
      "--use-current", Arg.Set use_current, " Use current downgrades instead of DB";
      "--database", Arg.Set_string database, "Set database name";
      "--host", set_string_opt host, "Set database host";
      "--port", set_int_opt port, "Set database port";
      "--user", set_string_opt user, "Set database user";
      "--password", set_string_opt password, "Set database password";
      "--socket-dir", set_string_opt unix_domain_socket_dir, "Set database unix domain socket directory";
      "--dropdb", Arg.Unit (fun () ->
          EzPG.dropdb
            ?host:!host ?port:!port ?unix_domain_socket_dir:!unix_domain_socket_dir
            !database;
          exit 0), " Drop database";
      "--createdb", Arg.Unit (fun () ->
          EzPG.createdb
            ?host:!host ?port:!port ?unix_domain_socket_dir:!unix_domain_socket_dir
            !database;
          exit 0), " Create database";

    ] (fun _s -> ()) "database-updater [OPTIONS]";
  let database = !database in
  let verbose = !verbose in
  let witness = !witness in
  let target = !target in
  let host = !host in
  let port = !port in
  let user = !user in
  let password = !password in
  let unix_domain_socket_dir = !unix_domain_socket_dir in
  let dbh =
    try
      EzPG.connect ?host ?port ?user ?password ?unix_domain_socket_dir database
    with _ ->
      EzPG.createdb ?host ?port ?unix_domain_socket_dir database;
      let dbh = EzPG.connect ?host ?port ?user ?password ?unix_domain_socket_dir database in
      EzPG.init ?witness dbh;
      dbh
  in
  let downgrades = if !use_current then Some downgrades else None in
  Printf.eprintf "Use current %b %b\n%!" !use_current (downgrades = None);
  if !old_info then EzPG.may_upgrade_old_info ~verbose dbh;
  EzPG.upgrade_database
    ~allow_downgrade: !allow_downgrade
    ~target ~verbose ?witness dbh ~upgrades ?downgrades;
  EzPG.close dbh

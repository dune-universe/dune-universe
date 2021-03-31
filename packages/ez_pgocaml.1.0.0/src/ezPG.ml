(**************************************************************************)
(*                                                                        *)
(*    Copyright 2018-2021 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

exception ExecFailed of string

let connect ?host ?port ?user ?password ?unix_domain_socket_dir database =
  let (dbh : 'a PGOCaml.t PGOCaml.monad) =
    PGOCaml.connect ?host ?port ?user ?password ?unix_domain_socket_dir ~database ()
  in
  dbh

let close dbh = PGOCaml.close dbh

let exec ?(verbose=true) dbh ?callback query =
  let res =
    try
      if verbose then
        Printf.eprintf "exec: %s\n%!" query;
      ignore (() = PGOCaml.prepare dbh ~query ());
      let (rows : PGOCaml.result list list) =
        PGOCaml.execute dbh ~params:[] () in
      Some rows
    with
    | exn ->
       if not verbose then
         Printf.eprintf "exec: %s\n%!" query;
       Printf.eprintf "EzPG error: %s\n%!"
                      (Printexc.to_string exn);
       match callback with
       | None -> raise (ExecFailed query)
       | Some _ -> None
  in
  match callback with
  | None -> ()
  | Some f ->
     f (match res with
        | None -> None
        | Some rows ->
           let rows =
             List.map (fun cols ->
               List.map (fun res ->
                   match res with
                   | None -> ""
                   | Some s -> s
                 ) cols
               ) rows
           in
           Some rows)

let execs ?verbose dbh queries =
  List.iter (fun query ->
      exec ?verbose dbh query) queries

let printf ?verbose ?callback dbh fmt =
  Printf.kprintf (fun s -> exec ?verbose ?callback dbh s) fmt

let createdb ?(verbose=true) ?host ?port ?unix_domain_socket_dir database =
  let dbh = connect ?host ?port ?unix_domain_socket_dir "postgres" in
  printf ~verbose dbh "CREATE DATABASE %s" database;
  close dbh

let dropdb ?(verbose=true) ?host ?port ?unix_domain_socket_dir database =
  let dbh = connect ?host ?port ?unix_domain_socket_dir "postgres" in
  printf ~verbose dbh "DROP DATABASE %s" database;
  close dbh

let begin_tr dbh = exec dbh "BEGIN"
let end_tr dbh = exec dbh "COMMIT"
let abort_tr dbh = exec dbh "ABORT"

let in_tr dbh f =
  let should_abort = ref true in
  try
    begin_tr dbh;
    f dbh;
    should_abort := false;
    end_tr dbh
  with exn ->
       if !should_abort then
         abort_tr dbh;
       raise exn

let touch_witness ?witness version =
    match witness with
    | None -> ()
    | Some file ->
       let oc = open_out file in
       Printf.fprintf oc "%d\n" version;
       close_out oc

let ezpg_to_version_1 ?verbose dbh =
  exec ?verbose dbh {| CREATE TABLE ezpg_upgrades (version INTEGER, command TEXT NOT NULL) |};
  exec ?verbose dbh {| CREATE TABLE ezpg_downgrades (version INTEGER, command TEXT NOT NULL) |};
  printf ?verbose dbh {| INSERT INTO ezpg_info VALUES ('ezpg_version',0) |};
  ()

let init ?verbose ?witness ?searchpath dbh =
  begin match searchpath with
    | None -> ()
    | Some db ->
      printf ?verbose dbh "CREATE SCHEMA IF NOT EXISTS %s" db;
      printf ?verbose dbh "SET search_path TO %s,public" db;
  end;
  printf ?verbose dbh {| CREATE TABLE ezpg_info (name VARCHAR PRIMARY KEY, value INTEGER) |};
  printf ?verbose dbh {| INSERT INTO ezpg_info VALUES ('version',0) |};
  ezpg_to_version_1 ?verbose dbh;
  touch_witness ?witness 0;
  ()

let set_version dbh version =
  printf dbh
    "UPDATE ezpg_info SET value = %d WHERE name = 'version'" version

let escape_string_content s =
  if String.contains s '\'' then
    let len = String.length s in
    let b = Buffer.create len in
    Buffer.add_string b "E'";
    for i = 0 to len-1 do
      match s.[i] with
        ( '\'' | '\\') as c
        -> Buffer.add_char b '\\'; Buffer.add_char b c
      | '\r' -> ()
      | '\n' -> Buffer.add_char b ' '
      | c -> Buffer.add_char b c
    done;
    Buffer.add_char b '\'';
    Buffer.contents b
  else
    Printf.sprintf "'%s'" s

let upgrade ?verbose ~version ?(downgrade=[]) ~dbh cmds =
  List.iter (fun query ->
      exec ?verbose dbh query;
    ) cmds;
  List.iter (fun cmd ->
      printf ?verbose dbh
        "INSERT INTO ezpg_upgrades VALUES (%d, %s)"
        version (escape_string_content cmd)) cmds;
  List.iter (fun cmd ->
      printf ?verbose dbh
        "INSERT INTO ezpg_downgrades (version, command) VALUES (%d, %s)"
        version
        (escape_string_content cmd)) downgrade;
  ()

(* Note: version is the target version, after the downgrade *)
let downgrade ?verbose ~version ~dbh cmds =
  List.iter (fun query ->
      exec ?verbose dbh query;
    ) cmds;
  printf ?verbose dbh
    "DELETE FROM ezpg_upgrades WHERE version = %d"
    version;
  printf ?verbose dbh
    "DELETE FROM ezpg_downgrades WHERE version = %d"
    version;
  ()

let check_upgrade dbh ~target =
  printf ~verbose:false dbh ~callback:(fun res ->
      let version =
        match res with
        | Some [[ version ]] -> (try int_of_string version with _ -> -1)
        | _ -> -1
      in
      if version <> target then begin
        Printf.eprintf "Error: database update failed.\n%!";
        Printf.eprintf "  Cannot run on this database schema.\n%!";
        exit 2
      end;
      Printf.printf "EzPG: database is up-to-date at version %d\n%!"
        target)
    {| SELECT value FROM ezpg_info WHERE name = 'version' |};
  ()

let upgrade_version ~target ~allow_downgrade ~upgrades ?downgrades
    ?witness dbh version =
  if version = target then
    Printf.printf "EzPG: database is up-to-date at version %d\n%!"
      target
  else

  if version < target then begin

    let version = ref version in
    while !version < target do
      Printf.eprintf "version = %d\n%!" !version;
      begin
        try
          let f = List.assoc !version upgrades in
          begin_tr dbh;
          f dbh !version;
          set_version dbh (!version+1);
          end_tr dbh;
          touch_witness ?witness !version;
          version := !version +1;
        with Not_found ->
          Printf.eprintf "Your database version %d is unsupported.\n" !version;
          Printf.eprintf "Maximal supported version is %d.\n%!" target;
          Printf.eprintf "Aborting.\n%!";
          exit 2
      end;
    done;
    check_upgrade dbh ~target
  end else begin
    if not allow_downgrade then begin
      Printf.eprintf "Error: your database version is %d, we need %d.\n%!"
        version target;
      Printf.eprintf "   Downgrade is disabled. You may try to run the updater with:\n";
      Printf.eprintf "      --allow-downgrade.\n%!";
      Printf.eprintf "Aborting.\n%!";
      exit 2
    end;

    let rec iter version =
      if version > target then
        begin
          Printf.eprintf "version = %d\n%!" version;
          match downgrades with
          | None ->
            printf dbh "SELECT command FROM ezpg_downgrades WHERE version=%d" (version-1) ~callback:(fun res ->
                match res with
                | None -> assert false
                | Some rows ->
                  Printf.eprintf "From DB\n%!";
                  let cmds = List.map List.hd rows in
                  iter2 version cmds
              )
          | Some downgrades ->
            Printf.eprintf "Using current commands\n%!";
            match List.assoc version downgrades with
            | exception Not_found ->
              Printf.eprintf "Your database version %d is unsupported.\n" version;
              Printf.eprintf "Maximal supported version is %d.\n%!" target;
              Printf.eprintf "Aborting.\n%!";
              exit 2
            | cmds ->
              iter2 version cmds
        end
    and iter2 version cmds =
      begin_tr dbh;
      downgrade ~dbh cmds ~version:(version-1);
      set_version dbh (version-1);
      end_tr dbh;
      touch_witness ?witness version;
      iter (version -1)
    in
    iter version;
    check_upgrade dbh ~target

  end

let upgrade_database ?(verbose=false)
    ?downgrades
    ?(allow_downgrade = false)
    ~upgrades
    ?(target = List.length upgrades) ?witness dbh =

  printf ~verbose dbh
    ~callback:(fun res ->
         let version =
           match res with
           | Some [[ "" ]] -> 0
           | Some [[ version ]] -> int_of_string version
           | Some [] -> 0
           | Some _ -> 0
           | None -> (* table ezpg_info not found => init *)
             init ?witness dbh;
             0
         in
         upgrade_version ~target ~allow_downgrade
           ?witness dbh version ~upgrades ?downgrades
       )
    {| SELECT value FROM ezpg_info WHERE name = 'version'  |}

let may_upgrade_old_info ?(verbose=true) dbh =

  Printf.eprintf "may_upgrade_old_info...\n%!";
  exec ~verbose dbh
    ~callback:(fun res ->
        match res with
        | Some [[ _version ]] ->
          Printf.eprintf "Must upgrade_old_info...\n%!";
          exec ~verbose dbh
            {| ALTER TABLE info RENAME TO ezpg_info |};
          ezpg_to_version_1 dbh
        | _ -> ()
      )
    {| SELECT value FROM info WHERE name = 'version'  |}


module Mtimes = struct

  let upgrade_init =
    [
  {|CREATE OR REPLACE FUNCTION update_row_modified_function_()
    RETURNS TRIGGER
    AS
    $$
    BEGIN
    -- ASSUMES the table has a column named exactly "row_modified_".
    -- Fetch date-time of actual current moment from clock, rather than start of statement or start of transaction.
    NEW.row_modified_ = clock_timestamp();
    RETURN NEW;
    END;
    $$
    language 'plpgsql'
|}
]

  let downgrade_init =
    [ {| DROP FUNCTION update_row_modified_function_ |} ]

  let upgrade_table table =
    [
      Printf.sprintf {|
ALTER TABLE %s
   ADD COLUMN row_modified_ TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT clock_timestamp()
      |} table;
      Printf.sprintf {|
ALTER TABLE %s
   ADD COLUMN row_created_ TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT clock_timestamp()
|} table;

      Printf.sprintf {|
CREATE TRIGGER row_mod_on_%s_trigger_
BEFORE UPDATE
ON %s
FOR EACH ROW
EXECUTE PROCEDURE update_row_modified_function_();
|} table table
    ]

  let downgrade_table table =
    [
      Printf.sprintf {| DROP TRIGGER row_mod_on_%s_trigger_|} table;
      Printf.sprintf {| ALTER TABLE %s DROP COLUMN row_created_ |} table;
      Printf.sprintf {| ALTER TABLE %s DROP COLUMN row_modified_ |} table;
    ]


end

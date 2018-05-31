open Freetds
open OUnit2
open Printf

let string_of_data : Ct.sql_t -> string = function
  | `Bit b -> if b then "Bit(1)" else "Bit(0)"
  | `Tinyint i -> sprintf "Tinyint %i" i
  | `Smallint i -> sprintf "Smallint %i" i
  | `Int i -> sprintf "Int %li" i
  | `Text s -> sprintf "Text %S" s
  | `String s -> sprintf "String %S" s
  | `Binary s -> sprintf "Binary %S" s
  | `Float f -> sprintf "Float %g" f
  | `Datetime s -> sprintf "Datetime %s" s
  | `Decimal s -> sprintf "Decimal %s" s
  | `Null -> "Null"

let string_of_row row =
  let row = List.map string_of_data row in
  sprintf "[%s]" (String.concat ", " row)

let string_of_rows rows =
  let rows = List.map string_of_row rows in
  sprintf "[ %s ]" (String.concat ", " rows)

let get_params () =
  [ "USER" ; "PASSWORD" ; "SERVER" ; "DATABASE" ]
  |> List.map (fun suffix ->
         try  Some (Sys.getenv(sprintf "MSSQL_TEST_%s" suffix))
         with Not_found -> None)
  |> function
    | [Some user; Some password; Some server; Some database] ->
       Some (user, password, server, database)
    | _ ->
       None

let with_conn (user, password, server, database) f =
  let ctx = Ct.ctx_create () in
  let conn = Ct.con_alloc ctx in
  Ct.con_setstring conn `Username user;
  Ct.con_setstring conn `Password password;
  Ct.connect conn server;
  let cmd = Ct.cmd_alloc conn in
  Ct.command cmd `Lang ("USE " ^ database);
  Ct.send cmd;
  ignore(Ct.results cmd);
  try
    f conn;
    Ct.close conn
  with e ->
    Ct.close conn;
    raise e

let sql_results conn sql =
  let cmd = Ct.cmd_alloc conn in
  Ct.command cmd `Lang sql;
  Ct.send cmd;
  let rows = ref [] in
  try
    while true do
      match Ct.results cmd with
      | `Row ->
     let ncols = Ct.res_info cmd `Numdata in
     let cols = Array.init ncols (fun i -> Ct.bind cmd (i+1)) in
     let cols = Array.to_list cols in
     (try
       while true do
         ignore(Ct.fetch cmd);
         let row = List.map (fun c ->
                       Ct.buffer_contents c.Ct.col_buffer) cols in
         rows := row :: !rows
       done;
       assert false
      with Ct.End_data -> ())
      | `Cmd_succeed | `Cmd_done | `Status -> ()
      | _ -> assert_failure(sprintf "`Row results expected")
    done;
    assert false
  with Ct.End_results ->
    List.rev !rows

let test_basic params _ =
  with_conn params (fun conn ->
      let rows = sql_results conn "SELECT CAST(1 AS INT) AS test" in
      assert_equal [ [`Int 1l] ] rows ~printer:string_of_rows
    )

let test_empty_strings params _ =
  with_conn params (fun conn ->
      let rows = sql_results conn
                   "SELECT \
                    CAST('' AS VARCHAR(10)) AS vc,
                    CAST('' AS TEXT) AS txt,
                    CAST('' AS VARBINARY(10)) AS vb,
                    CAST(NULL AS VARCHAR(1)) AS nvc,
                    CAST(NULL AS TEXT) AS ntxt,
                    CAST(NULL AS VARBINARY(10)) AS nvb" in
      assert_equal [ [`String ""; `String ""; `Binary ""; `Null; `Null; `Null] ]
        rows ~printer:string_of_rows
    )

let () =
    match get_params () with
  | None ->
     print_endline "Skipping Ct tests since MSSQL_TEST_* environment \
                    variables aren't set"
  | Some params ->
     ["basic", test_basic;
      "empty strings", test_empty_strings ]
     |> List.map (fun (name, test) -> name >:: test params)
     |> OUnit2.test_list
     |> OUnit2.run_test_tt_main

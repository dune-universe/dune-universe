open Freetds
open OUnit2
open Printf

let string_of_string s = s

let get_params () =
  [ "USER" ; "PASSWORD" ; "SERVER" ; "DATABASE" ]
  |> List.map (fun suffix ->
      let env_var = sprintf "MSSQL_TEST_%s" suffix in
      try
        Some (Sys.getenv env_var)
      with Not_found ->
        None)
  |> function
  | [ Some user ; Some password ; Some server ; Some database ] ->
    Some (user, password, server, database)
  | _ ->
    None

let with_conn (user, password, server, database) f =
  let conn = Dblib.connect ~user ~password server in
  Dblib.use conn database;
  try
    f conn;
    Dblib.close conn
  with e ->
    Dblib.close conn;
    raise e

let test_connect ((_, _, _, database) as params) _ =
  with_conn params (fun conn ->
      Dblib.name conn
      |> assert_equal ~printer:string_of_string database)

let test_basic_query params _ =
  with_conn params (fun conn ->
      Dblib.sqlexec conn "SELECT CAST(1 AS INT) AS test";
      Dblib.results conn
      |> assert_bool "query has results";
      Dblib.numcols conn
      |> assert_equal ~printer:string_of_int 1;
      Dblib.colname conn 1
      |> assert_equal ~printer:string_of_string "test";
      Dblib.coltype conn 1
      |> assert_equal ~printer:Dblib.string_of_col_type Dblib.SYBINT4;
      Dblib.nextrow conn
      |> assert_equal [ Dblib.INT 1 ];
      assert_raises Not_found (fun () -> Dblib.nextrow conn);
      Dblib.results conn
      |> assert_equal ~printer:string_of_bool false;
      Dblib.count conn
      |> assert_equal ~printer:string_of_int 1)

let () =
  match get_params () with
  | None ->
    print_endline "Skipping tests since MSSQL_TEST_* environment variables \
                   aren't set"
  | Some params ->
    [ "connect", test_connect
    ; "basic query", test_basic_query ]
    |> List.map (fun (name, test) -> name >:: test params)
    |> test_list
    |> run_test_tt_main
open Core
open Async_kernel
open Async_unix
open Poly
module Row = Mssql.Row

exception Environment_variables_not_set

let () =
  Caml.Printexc.register_printer (function
      | Environment_variables_not_set ->
        Some
          "Environment_variables_not_set. The following environment variables must be \
           set to run the Mssql tests: MSSQL_TEST_SERVER, MSSQL_TEST_DATABASE, \
           MSSQL_TEST_USERNAME, MSSQL_TEST_PASSWORD. Optionally, you can also set \
           MSSQL_TEST_PORT but it is not required."
      | _ -> None)
;;

let params =
  lazy
    ([ "MSSQL_TEST_SERVER"
     ; "MSSQL_TEST_DATABASE"
     ; "MSSQL_TEST_USERNAME"
     ; "MSSQL_TEST_PASSWORD"
     ; "MSSQL_TEST_PORT"
     ]
    |> List.map ~f:Sys.getenv
    |> function
    | [ Some host; Some db; Some user; Some password; port ] ->
      host, db, user, password, Option.map ~f:Int.of_string port
    | _ -> raise Environment_variables_not_set)
;;

let with_test_conn f =
  let host, db, user, password, port = Lazy.force params in
  Mssql.with_conn ~host ~db ~user ~password ?port f
;;

let test_select_and_convert () =
  with_test_conn (fun db ->
      Mssql.execute
        db
        "SELECT 1 AS intcol, 0 AS intcol2, 3 AS notboolint, -1 AS notboolint2, 5.9 AS \
         floatcol, 'some string' AS strcol, '' AS emptystrcol, '2017-01-05' AS datecol, \
         CAST('1998-09-12T12:34:56Z' AS DATETIME) AS datetimecol, CONVERT(BIT, 1) AS \
         boolcol, NULL AS nullcol")
  >>| function
  | [ row ] ->
    let assert_raises msg f =
      match Or_error.try_with ~backtrace:true f with
      | Ok _ -> failwithf "Expected exception for conversion: %s" msg ()
      | Error _ -> ()
    in
    let col = "intcol" in
    [%test_result: int option] ~expect:(Some 1) (Row.int row col);
    [%test_result: int32 option] ~expect:(Some Int32.one) (Row.int32 row col);
    [%test_result: int64 option] ~expect:(Some Int64.one) (Row.int64 row col);
    [%test_result: float option] ~expect:(Some 1.) (Row.float row col);
    [%test_result: string option] ~expect:(Some "1") (Row.str row col);
    [%test_result: bool option] ~expect:(Some true) (Row.bool row col);
    assert_raises "int as date" (fun () -> Row.date row col);
    assert_raises "int as datetime" (fun () -> Row.datetime row col);
    let col = "intcol2" in
    [%test_result: int option] ~expect:(Some 0) (Row.int row col);
    [%test_result: int32 option] ~expect:(Some Int32.zero) (Row.int32 row col);
    [%test_result: int64 option] ~expect:(Some Int64.zero) (Row.int64 row col);
    [%test_result: float option] ~expect:(Some 0.) (Row.float row col);
    [%test_result: string option] ~expect:(Some "0") (Row.str row col);
    [%test_result: bool option] ~expect:(Some false) (Row.bool row col);
    assert_raises "int as date" (fun () -> Row.date row col);
    assert_raises "int as datetime" (fun () -> Row.datetime row col);
    let col = "notboolint" in
    [%test_result: int option] ~expect:(Some 3) (Row.int row col);
    [%test_result: int32 option] ~expect:(Int32.of_int 3) (Row.int32 row col);
    [%test_result: int64 option] ~expect:(Some (Int64.of_int 3)) (Row.int64 row col);
    [%test_result: float option] ~expect:(Some 3.) (Row.float row col);
    [%test_result: string option] ~expect:(Some "3") (Row.str row col);
    assert_raises "int as date" (fun () -> Row.date row col);
    assert_raises "int as datetime" (fun () -> Row.datetime row col);
    assert_raises "int as bool" (fun () -> Row.bool row col);
    let col = "notboolint2" in
    [%test_result: int option] ~expect:(Some (-1)) (Row.int row col);
    [%test_result: int32 option] ~expect:(Int32.of_int (-1)) (Row.int32 row col);
    [%test_result: int64 option] ~expect:(Some (Int64.of_int (-1))) (Row.int64 row col);
    [%test_result: float option] ~expect:(Some (-1.)) (Row.float row col);
    [%test_result: string option] ~expect:(Some "-1") (Row.str row col);
    assert_raises "int as date" (fun () -> Row.date row col);
    assert_raises "int as datetime" (fun () -> Row.datetime row col);
    assert_raises "int as bool" (fun () -> Row.bool row col);
    let col = "floatcol" in
    [%test_result: float option] ~expect:(Some 5.9) (Row.float row col);
    [%test_result: string option] ~expect:(Some "5.9") (Row.str row col);
    [%test_result: int option] ~expect:(Some 5) (Row.int row col);
    [%test_result: int32 option] ~expect:(Int32.of_int 5) (Row.int32 row col);
    [%test_result: int64 option] ~expect:(Some (Int64.of_int 5)) (Row.int64 row col);
    assert_raises "float as date" (fun () -> Row.date row col);
    assert_raises "float as datetime" (fun () -> Row.datetime row col);
    assert_raises "float as bool" (fun () -> Row.bool row col);
    let col = "strcol" in
    [%test_result: string option] ~expect:(Some "some string") (Mssql.Row.str row col);
    assert_raises "string as float" (fun () -> Row.float row col);
    assert_raises "string as int" (fun () -> Row.int row col);
    assert_raises "string as int32" (fun () -> Row.int32 row col);
    assert_raises "string as int64" (fun () -> Row.int64 row col);
    assert_raises "string as date" (fun () -> Row.date row col);
    assert_raises "string as datetime" (fun () -> Row.datetime row col);
    assert_raises "string as bool" (fun () -> Row.bool row col);
    let col = "emptystrcol" in
    [%test_result: string option] ~expect:(Some "") (Mssql.Row.str row col);
    assert_raises "string as float" (fun () -> Row.float row col);
    assert_raises "string as int" (fun () -> Row.int row col);
    assert_raises "string as int32" (fun () -> Row.int32 row col);
    assert_raises "string as int64" (fun () -> Row.int64 row col);
    assert_raises "string as date" (fun () -> Row.date row col);
    assert_raises "string as datetime" (fun () -> Row.datetime row col);
    assert_raises "string as bool" (fun () -> Row.bool row col);
    let col = "datecol" in
    [%test_result: string option] ~expect:(Some "2017-01-05") (Mssql.Row.str row col);
    [%test_result: Date.t option]
      ~expect:(Some (Date.of_string "2017-01-05"))
      (Mssql.Row.date row col);
    assert_raises "date as float" (fun () -> Row.float row col);
    assert_raises "date as int" (fun () -> Row.int row col);
    assert_raises "date as int32" (fun () -> Row.int32 row col);
    assert_raises "date as int64" (fun () -> Row.int64 row col);
    assert_raises "date as bool" (fun () -> Row.bool row col);
    let col = "datetimecol" in
    [%test_result: string option]
      ~expect:(Some "1998-09-12 12:34:56.000000Z")
      (Row.str row col);
    [%test_result: Date.t option]
      ~expect:(Some (Date.of_string "1998-09-12"))
      (Row.date row col);
    [%test_result: Time.t option]
      ~expect:(Some (Time.of_string_abs "1998-09-12T12:34:56Z"))
      (Row.datetime row col);
    assert_raises "datetime as float" (fun () -> Row.float row col);
    assert_raises "datetime as int" (fun () -> Row.int row col);
    assert_raises "datetime as int32" (fun () -> Row.int32 row col);
    assert_raises "datetime as int64" (fun () -> Row.int64 row col);
    assert_raises "datetime as bool" (fun () -> Row.bool row col);
    let col = "boolcol" in
    [%test_result: string option] ~expect:(Some "true") (Row.str row col);
    [%test_result: bool option] ~expect:(Some true) (Row.bool row col);
    [%test_result: int option] ~expect:(Some 1) (Row.int row col);
    [%test_result: int32 option] ~expect:(Some Int32.one) (Row.int32 row col);
    [%test_result: int64 option] ~expect:(Some Int64.one) (Row.int64 row col);
    assert_raises "bool as float" (fun () -> Row.float row col);
    assert_raises "bool as date" (fun () -> Row.date row col);
    assert_raises "bool as datetime" (fun () -> Row.datetime row col);
    let col = "nullcol" in
    [%test_result: string option] ~expect:None (Row.str row col);
    [%test_result: float option] ~expect:None (Row.float row col);
    [%test_result: int option] ~expect:None (Row.int row col);
    [%test_result: int32 option] ~expect:None (Row.int32 row col);
    [%test_result: int64 option] ~expect:None (Row.int64 row col);
    [%test_result: Date.t option] ~expect:None (Row.date row col);
    [%test_result: Time.t option] ~expect:None (Row.datetime row col);
    [%test_result: bool option] ~expect:None (Row.bool row col)
  | _ -> assert false
;;

let test_in_clause_param () =
  with_test_conn (fun db ->
      Mssql.execute_unit db "CREATE TABLE #test (id varchar)"
      >>= fun () ->
      Mssql.execute_unit db "INSERT INTO #test (id) VALUES ('''')"
      >>= fun () ->
      Mssql.execute_map
        db
        ~params:Mssql.Param.[ Some (Array [ String "'"; String "''" ]) ]
        "SELECT id FROM #test WHERE id IN ($1)"
        ~f:(fun row -> Row.str row "id"))
  >>| [%test_result: string option list] ~expect:[ Some "'" ]
;;

let test_multiple_queries_in_execute () =
  with_test_conn (fun db ->
      Monitor.try_with_or_error ~here:[%here]
      @@ fun () -> Mssql.execute db "SELECT 1; SELECT 2")
  >>| [%test_pred: Mssql.Row.t list Or_error.t]
        (function
          | Ok _ -> false
          | Error e ->
            Error.to_string_hum e
            |> String.is_substring
                 ~substring:"expected one result set but got 2 result sets")
        ~message:
          "Multiple queries in execute should throw 'expected one result set' error but \
           threw different exception"
;;

let test_multiple_queries_in_execute_multi_result () =
  with_test_conn (fun db -> Mssql.execute_multi_result db "SELECT 1; SELECT 2")
  >>| List.map ~f:(List.map ~f:(fun row -> Row.int row ""))
  >>| [%test_result: int option list list] ~expect:[ [ Some 1 ]; [ Some 2 ] ]
;;

let test_not_result_queries_don't_count () =
  with_test_conn (fun db ->
      (* This query has 3 expressions but only the SELECT should actually return a result set *)
      Mssql.execute_single
        db
        "CREATE TABLE #test (id int); INSERT INTO #test (id) VALUES (1); SELECT * FROM \
         #test"
      >>| Option.map ~f:(fun row -> Mssql.Row.int_exn row "id"))
  >>| [%test_result: int option] ~expect:(Some 1)
;;

let test_empty_result_sets_still_count () =
  with_test_conn (fun db ->
      (* This query has 2 selects that both return empty sets of rows. We should still get both since
         they are legitimate result sets. *)
      Mssql.execute_multi_result
        db
        "CREATE TABLE #test (id int); SELECT * FROM #test; SELECT * FROM #test")
  >>| [%test_result: Mssql.Row.t list list] ~expect:[ []; [] ]
;;

let test_execute_unit () =
  with_test_conn (fun db ->
      [ "SET XACT_ABORT ON"
      ; "BEGIN TRANSACTION"
      ; "CREATE TABLE #test (id int)"
      ; "INSERT INTO #test (id) VALUES (1)"
      ; "UPDATE #test SET id = 2 WHERE id = 1"
      ; "COMMIT TRANSACTION"
      ]
      |> Deferred.List.iter ~how:`Sequential ~f:(Mssql.execute_unit db))
;;

let test_execute_unit_fail () =
  with_test_conn (fun db ->
      Mssql.execute_unit db "CREATE TABLE #test (id int)"
      >>= fun () ->
      Mssql.execute_unit db "INSERT INTO #test (id) VALUES (1)"
      >>= fun () ->
      Monitor.try_with_or_error ~here:[%here]
      @@ fun () -> Mssql.execute_unit db "SELECT id FROM #test")
  >>| [%test_pred: unit Or_error.t]
        Result.is_error
        ~message:"execute_unit with a SELECT should throw but didn't"
;;

let test_execute_single () =
  with_test_conn (fun db ->
      Mssql.execute_unit db "CREATE TABLE #test (id int)"
      >>= fun () ->
      Mssql.execute_unit db "INSERT INTO #test (id) VALUES (1)"
      >>= fun () -> Mssql.execute_single db "SELECT id FROM #test WHERE id = 1")
  >>| ignore
;;

let test_execute_single_fail () =
  with_test_conn (fun db ->
      Mssql.execute_unit db "CREATE TABLE #test (id int)"
      >>= fun () ->
      Mssql.execute_unit db "INSERT INTO #test (id) VALUES (1), (1)"
      >>= fun () ->
      Monitor.try_with_or_error ~here:[%here]
      @@ fun () -> Mssql.execute_single db "SELECT id FROM #test WHERE id > 0")
  >>| [%test_pred: Mssql.Row.t option Or_error.t]
        Result.is_error
        ~message:"execute_single returning multiple rows should throw but didn't"
;;

let test_order () =
  with_test_conn (fun db ->
      Mssql.execute_map db "SELECT 1 AS a UNION ALL SELECT 2 AS a" ~f:(fun row ->
          Row.int row "a"))
  >>| [%test_result: int option list] ~expect:[ Some 1; Some 2 ]
;;

let test_param_parsing () =
  let params = Mssql.Param.[ Some (String "'"); Some (Int 5); None ] in
  with_test_conn (fun db ->
      Mssql.execute
        ~params
        db
        "SELECT $1 AS single_quote, $2 AS five, '$1' AS \"$2\", '''$1' AS \"\"\"$2\", $3 \
         AS none")
  >>| function
  | [ row ] ->
    let single_quote = Row.str row "single_quote" in
    [%test_result: string option] ~expect:(Some "'") single_quote;
    let five = Row.int row "five" in
    [%test_result: int option] ~expect:(Some 5) five;
    let dollar_str = Row.str row "$2" in
    [%test_result: string option] ~expect:(Some "$1") dollar_str;
    let dollar_dollar_str = Row.str row "\"$2" in
    [%test_result: string option] ~expect:(Some "'$1") dollar_dollar_str;
    let none = Row.str row "none" in
    [%test_result: string option] ~expect:None none
  | rows -> failwithf !"Expected one row but got %{sexp: Mssql.Row.t list}" rows ()
;;

let test_param_out_of_range () =
  let open Mssql.Param in
  with_test_conn (fun db ->
      [ ( Some [ Some (String "asdf"); Some (Int 9) ]
        , "SELECT $1 AS a, $2 AS b, $3 AS c"
        , "Query has param $3 but there are only 2 params." )
      ; ( Some [ Some (String "asdf"); Some (Int 9) ]
        , "SELECT $1 AS a, $2 AS b, $0 AS c"
        , "Query has param $0 but params should start at $1." )
      ; None, "SELECT $1 AS a, $2 AS b", "Query has param $1 but there are only 0 params."
      ]
      |> Deferred.List.iter ~f:(fun (expect_params, expect_query, expect_msg) ->
             Monitor.try_with ~here:[%here] ~extract_exn:true (fun () ->
                 Mssql.execute ?params:expect_params db expect_query >>| ignore)
             >>| [%test_pred: (unit, exn) Result.t]
                   (function
                     | Error (Mssql.Error { msg; query; params; _ }) ->
                       expect_msg = msg
                       && params = Option.value ~default:[] expect_params
                       && Some expect_query = query
                     | Error _ | Ok () -> false)
                   ~message:"Command should have thrown param out of range exception"))
;;

let round_trip_tests =
  let all_chars = String.init 128 ~f:Char.of_int_exn in
  let open Mssql.Param in
  [ ( String ""
    , "VARCHAR(10)"
    , fun row -> Row.str row "" |> [%test_result: string option] ~expect:(Some "") )
  ; ( Bignum (Bignum.of_string "9223372036854775808")
    , "NUMERIC(38)"
    , fun row ->
        Row.bignum row ""
        |> [%test_result: Bignum.t option] (* FIXME: Why are we losing precision ? *)
             ~expect:(Some (Bignum.of_string "9223372036854775808")) )
  ; ( Bool true
    , "BIT"
    , fun row -> Row.bool row "" |> [%test_result: bool option] ~expect:(Some true) )
  ; ( Bool false
    , "BIT"
    , fun row -> Row.bool row "" |> [%test_result: bool option] ~expect:(Some false) )
  ; ( Float 3.1415
    , "FLOAT"
    , fun row -> Row.float row "" |> [%test_result: float option] ~expect:(Some 3.1415) )
  ; ( Int 5
    , "INT"
    , fun row -> Row.int row "" |> [%test_result: int option] ~expect:(Some 5) )
  ; ( Int32 Int32.max_value
    , "INT"
    , fun row ->
        Row.int32 row "" |> [%test_result: int32 option] ~expect:(Some Int32.max_value) )
    (* FIXME: If we sent Int64.max, SQL Server returns it as a FLOAT with
       rounding errors, even though we're explicitly casting to BIGINT. *)
  ; ( Int64 Int64.(max_value / of_int 1000000)
    , "BIGINT"
    , fun row ->
        Row.int64 row ""
        |> [%test_result: int64 option] ~expect:(Some Int64.(max_value / of_int 1000000))
    )
  ; ( Date (Time.of_string "2017-01-05 11:53:02Z")
    , "DATETIME"
    , fun row ->
        Row.datetime row ""
        |> [%test_result: Time.t option]
             ~expect:(Some (Time.of_string "2017-01-05 11:53:02Z")) )
  ]
  @ ([ all_chars
       (* try null, ' and a string in any order to make sure the iterative code
          is correct *)
     ; "\x00a'"
     ; "\x00'asd"
     ; "'\x00asd"
     ; "'asd\x00"
     ; "asd\x00"
     ; "asd'\x00'"
     ]
    |> List.map ~f:(fun str ->
           ( String str
           , "VARCHAR(256)"
           , fun row -> Row.str row "" |> [%test_result: string option] ~expect:(Some str)
           )))
  |> List.map ~f:(fun (param, type_name, f) ->
         ( sprintf "test_round_trip %s" type_name
         , fun () ->
             let params = [ Some param ] in
             let query = sprintf "SELECT CAST($1 AS %s)" type_name in
             with_test_conn (fun db -> Mssql.execute_single ~params db query)
             >>| (fun o -> Option.value_exn o ~message:"Expected one row but got 0")
             >>| f ))
;;

let test_execute_many () =
  let expect = List.init 100 ~f:(fun i -> [ Some i ])
  and params = List.init 100 ~f:(fun i -> Mssql.Param.[ Some (Int i) ]) in
  with_test_conn (fun db -> Mssql.execute_many ~params db "SELECT $1 AS result")
  >>| List.map ~f:(fun result_set ->
          List.map result_set ~f:(fun row -> Mssql.Row.int row "result"))
  >>| [%test_result: int option list list] ~expect
;;

let test_concurrent_queries () =
  let n = 10 in
  let query =
    List.range 1 (n + 1)
    |> List.map ~f:(sprintf "SELECT $%d")
    |> String.concat ~sep:" UNION ALL "
  in
  with_test_conn (fun db ->
      List.range 0 n
      |> Deferred.List.iter ~how:`Parallel ~f:(fun _ ->
             let vals = List.init n ~f:(fun _ -> Random.int 10000) in
             let expect = List.map vals ~f:Option.some in
             let params = List.map vals ~f:(fun n -> Some (Mssql.Param.Int n)) in
             Mssql.execute_map ~params db query ~f:(fun row -> Row.int row "")
             >>| [%test_result: int option list] ~expect))
;;

let recoding_tests =
  (* ç ß are different in CP1252 vs UTF-8; ∑ has no conversion *)
  [ ( "valid UTF-8"
    , "ç ß ∑ We’re testing iconv here"
    , (* round trip strips ∑ because we can't store it, but handles the rest *)
      "ç ß  We’re testing iconv here"
    , (* Inserting the literal char codes, we'll double-decode when we pull it
         back out of the DB (garbage output is by design here) *)
      "Ã§ ÃŸ âˆ‘ Weâ€™re testing iconv here" )
  ; ( "invalid UTF-8"
    , (* \x81 isn't valid in UTF-8 or CP1252 so both versions fallback to just
         using the ASCII chars *)
      "ç ß ∑ We’re testing iconv here \x81"
    , "   Were testing iconv here "
    , "   Were testing iconv here " )
  ]
  |> List.concat_map ~f:(fun (name, input, expect_roundtrip, expect_charcodes) ->
         [ ( "recoding, round-trip " ^ name
           , fun () ->
               let params = [ Some (Mssql.Param.String input) ] in
               with_test_conn
               @@ fun db ->
               Mssql.execute_single ~params db "SELECT $1"
               >>| Option.map ~f:Row.to_alist
               >>| [%test_result: (string * string) list option]
                     ~expect:(Some [ "", expect_roundtrip ]) )
         ; ( "recoding, sending literal char codes " ^ name
           , fun () ->
               with_test_conn
               @@ fun db ->
               String.to_list input
               |> List.map ~f:Char.to_int
               |> List.map ~f:(sprintf "CHAR(%d)")
               |> String.concat ~sep:"+"
               |> sprintf "SELECT %s"
               |> Mssql.execute_single db
               >>| Option.map ~f:Row.to_alist
               >>| [%test_result: (string * string) list option]
                     ~expect:(Some [ "", expect_charcodes ]) )
         ])
;;

let test_rollback () =
  let expect = [ [ "id", "1" ] ] in
  with_test_conn
  @@ fun db ->
  let%bind () = Mssql.execute_unit db "CREATE TABLE #test (id int)" in
  let%bind () = Mssql.execute_unit db "INSERT INTO #test VALUES (1)" in
  let%bind () = Mssql.begin_transaction db in
  let%bind () = Mssql.execute_unit db "INSERT INTO #test VALUES (2)" in
  let%bind () = Mssql.rollback db in
  Mssql.execute db "SELECT id FROM #test"
  >>| List.map ~f:Mssql.Row.to_alist
  >>| [%test_result: (string * string) list list] ~expect
;;

let test_auto_rollback () =
  let expect = [ [ "id", "1" ] ] in
  with_test_conn
  @@ fun db ->
  let%bind () = Mssql.execute_unit db "CREATE TABLE #test (id int)" in
  let%bind () = Mssql.execute_unit db "INSERT INTO #test VALUES (1)" in
  Monitor.try_with ~here:[%here] ~extract_exn:true (fun () ->
      Mssql.with_transaction db (fun db ->
          let%bind () = Mssql.execute_unit db "INSERT INTO #test VALUES (2)" in
          raise Caml.Not_found))
  >>= function
  | Error Caml.Not_found ->
    Mssql.execute_map db "SELECT id FROM #test" ~f:Mssql.Row.to_alist
    >>| [%test_result: (string * string) list list] ~expect
  | _ -> assert false
;;

let test_commit () =
  let expect = [ [ "id", "1" ]; [ "id", "2" ] ] in
  with_test_conn
  @@ fun db ->
  let%bind () = Mssql.execute_unit db "CREATE TABLE #test (id int)" in
  let%bind () = Mssql.execute_unit db "INSERT INTO #test VALUES (1)" in
  let%bind () = Mssql.begin_transaction db in
  let%bind () = Mssql.execute_unit db "INSERT INTO #test VALUES (2)" in
  let%bind () = Mssql.commit db in
  Mssql.execute_map db "SELECT id FROM #test" ~f:Mssql.Row.to_alist
  >>| [%test_result: (string * string) list list] ~expect
;;

let test_auto_commit () =
  let expect = [ [ "id", "1" ]; [ "id", "2" ] ] in
  with_test_conn
  @@ fun db ->
  let%bind () = Mssql.execute_unit db "CREATE TABLE #test (id int)" in
  let%bind () = Mssql.execute_unit db "INSERT INTO #test VALUES (1)" in
  Mssql.with_transaction db (fun db ->
      Mssql.execute_unit db "INSERT INTO #test VALUES (2)")
  >>= fun () ->
  Mssql.execute_map db "SELECT id FROM #test" ~f:Mssql.Row.to_alist
  >>| [%test_result: (string * string) list list] ~expect
;;

let test_other_execute_during_transaction () =
  with_test_conn
  @@ fun db ->
  let%bind () = Mssql.execute_unit db "CREATE TABLE #test (id int)" in
  let ivar = Ivar.create () in
  let%map () =
    Mssql.with_transaction db (fun db ->
        Ivar.fill ivar ();
        let%bind () = Mssql.execute_unit db "WAITFOR DELAY '00:00:01'" in
        Mssql.execute_unit db "INSERT INTO #test VALUES (1)")
  and res =
    let%bind () = Ivar.read ivar in
    Mssql.execute db "SELECT id FROM #test" >>| List.hd >>| Option.map ~f:Row.to_alist
  in
  [%test_result: (string * string) list option] ~expect:(Some [ "id", "1" ]) res
;;

let test_prevent_transaction_deadlock () =
  let expect =
    "Attempted to use outer DB handle inside of with_transaction. This would have lead \
     to a deadlock."
  in
  with_test_conn
  @@ fun db ->
  Mssql.with_transaction db (fun _ ->
      Monitor.try_with_or_error ~here:[%here] (fun () ->
          Mssql.execute_unit db "WAITFOR DELAY '00:00:00'"))
  >>| [%test_pred: unit Or_error.t]
        (function
          | Error err -> Error.to_string_mach err |> String.is_substring ~substring:expect
          | Ok () -> false)
        ~message:(sprintf "Expected exception containing %s" expect)
;;

let test_exception_thrown_in_callback () =
  with_test_conn (fun db ->
      Monitor.try_with ~here:[%here] ~extract_exn:true (fun () -> Mssql.execute db "\x81")
      >>| (function
            | Error exn ->
              if Exn.to_string_mach exn
                 |> String.is_substring ~substring:"CONVERSION"
                 |> not
              then raise exn
            | Ok _ -> assert false)
      >>= fun () -> Mssql.execute db "SELECT 1" |> Deferred.ignore_m)
;;

let test_exception_with_multiple_results () =
  (* Ensure that our code properly cleans up existing result sets before a new
     query. Before D13534, we sometimes saw:
     "Attempt to initiate a new Adaptive Server operation with results pending" *)
  with_test_conn (fun db ->
      Monitor.try_with ~here:[%here] ~extract_exn:true (fun () ->
          Mssql.execute
            db
            {|
              CREATE TABLE #test (id INT PRIMARY KEY);
              INSERT INTO #test (id) VALUES (1);
              INSERT INTO #test (id) VALUES (1); -- primary key violation
              SELECT * FROM #test;
            |})
      >>| (function
            | Error exn ->
              if Exn.to_string_mach exn
                 |> String.is_substring ~substring:"PRIMARY KEY"
                 |> not
              then raise exn
            | Ok res ->
              failwithf
                !"Expected an error but got results: %{sexp:Mssql.Row.t list}"
                res
                ())
      >>= fun () ->
      (* if our cleanup code works right, this won't throw an exception *)
      Mssql.execute db "SELECT 1" |> Deferred.ignore_m)
;;

let test_execute_pipe () =
  with_test_conn (fun db ->
      Mssql.execute_unit db "CREATE TABLE #test (id int)"
      >>= fun () ->
      let values = List.init 100 ~f:Fn.id in
      Deferred.List.iter values ~f:(fun value ->
          Mssql.execute_unit
            ~params:[ Some (Int value) ]
            db
            "INSERT INTO #test (id) VALUES ($1)")
      >>= fun () ->
      Mssql.execute_pipe db "SELECT id FROM #test ORDER BY id"
      |> Pipe.map ~f:(fun row -> Row.int_exn row "id")
      |> Pipe.to_list
      >>| [%test_result: int list] ~expect:values)
  >>| ignore
;;

let test_execute_pipe_error () =
  with_test_conn (fun db ->
      Monitor.try_with_or_error ~here:[%here]
      @@ fun () -> Mssql.execute_unit db "lkmsdflkmdsf")
  >>| [%test_pred: unit Or_error.t]
        Result.is_error
        ~message:"Invalid query should return an error"
;;

let () =
  try
    (Lazy.force params : string * string * string * string * int option) |> ignore;
    Thread_safe.block_on_async_exn
    @@ fun () ->
    [ ( "all"
      , [ "select and convert", test_select_and_convert
        ; "multiple queries in execute", test_multiple_queries_in_execute
        ; ( "multiple queries in execute_multi_result"
          , test_multiple_queries_in_execute_multi_result )
        ; "test_not_result_queries_don't_count", test_not_result_queries_don't_count
        ; "test_empty_result_sets_still_count", test_empty_result_sets_still_count
        ; "execute_unit", test_execute_unit
        ; "execute_unit fail", test_execute_unit_fail
        ; "execute_single", test_execute_single
        ; "execute_single fail", test_execute_single_fail
        ; "test list order", test_order
        ; "test params", test_param_parsing
        ; "test param out of range", test_param_out_of_range
        ; "test execute many", test_execute_many
        ; "test concurrent queries", test_concurrent_queries
        ; "test rollback", test_rollback
        ; "test auto rollback", test_auto_rollback
        ; "test commit", test_commit
        ; "test auto commit", test_auto_commit
        ; "test other execute during transaction", test_other_execute_during_transaction
        ; "test prevent transaction deadlock", test_prevent_transaction_deadlock
        ; "test exception in callback", test_exception_thrown_in_callback
        ; "test exception with multiple results", test_exception_with_multiple_results
        ; "test execute_pipe", test_execute_pipe
        ; "test execute_pipe_error", test_execute_pipe_error
        ]
        @ round_trip_tests
        @ recoding_tests
        |> List.map ~f:(fun (name, f) -> Alcotest_async.test_case name `Quick f) )
    ]
    |> Alcotest_async.run "mssql"
  with
  | Environment_variables_not_set as e -> Caml.Printexc.to_string e |> Caml.print_endline
;;

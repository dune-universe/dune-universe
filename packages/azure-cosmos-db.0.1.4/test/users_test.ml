open Lwt
open Test_common
open Cosmos

let dbname = "user_database"
let user_name = "a_user_name"
let replace_user_name = "replace_user_name"

let create_database_test _ () =
  let res = D.create dbname in
  res >>= fun (code, body) ->
  let _ = Alcotest.(check int) "Status same int" 201 code in
  let _ =
    match body with
    | Some {id; _} -> Alcotest.(check string) "Create name is correct" dbname id
    | None -> ()
  in
  return ()

let create_user_test _ () =
  let res = D.User.create dbname user_name in
  res >>= fun (code, body) ->
  let _ = Alcotest.(check int) "Status same int" 201 code in
  let _ =
    match body with
    | Some {id; _} -> Alcotest.(check string) "Create name is correct" user_name id
    | None -> ()
  in
  return ()

let list_user_test _ () =
  let res = D.User.list dbname in
  res >>= fun (code, {rid = _; users; count}) ->
  let db = List.filter (fun (x : Json_converter_t.user)-> x.id = user_name) users in
  let _ = Alcotest.(check int) "Status same int" 200 code in
  let _ = Alcotest.(check bool) "Count" true (count > 0) in
  let _ = Alcotest.(check string) "Name of user" user_name ((List.hd db).id) in
  return ()

let get_user_test _ () =
  let res = D.User.get dbname user_name in
  res >>= fun (code, body) ->
  let _ = Alcotest.(check int) "Status same int" 200 code in
  let _ =
    match body with
    | Some {id; _} -> Alcotest.(check string) "Create name is correct" user_name id
    | None -> ()
  in
  return ()

let replace_user_test _ () =
  let res = D.User.replace dbname user_name replace_user_name in
  res >>= fun (code, body) ->
  let _ = Alcotest.(check int) "Status same int" 200 code in
  let _ =
    match body with
    | Some {id; _} -> Alcotest.(check string) "Replace name is correct" replace_user_name id
    | None -> ()
  in
  return ()

let delete_user_test _ () =
  let res = D.User.delete dbname replace_user_name in
  res >>= fun code ->
  let _ = Alcotest.(check int) "Status same int" 204 code in
  return ()

let delete_database_test _ () =
  let res = D.delete dbname in
  res >>= fun code ->
  let _ = Alcotest.(check int) "Status same int" 204 code in
  return ()

let user_tests = [
  Alcotest_lwt.test_case "create database" `Slow create_database_test;
  Alcotest_lwt.test_case "create user" `Slow create_user_test;
  Alcotest_lwt.test_case "list user" `Slow list_user_test;
  Alcotest_lwt.test_case "get user" `Slow get_user_test;
  Alcotest_lwt.test_case "replace user" `Slow replace_user_test;
  Alcotest_lwt.test_case "delete user" `Slow delete_user_test;
  Alcotest_lwt.test_case "delete database" `Slow delete_database_test;
]

let test =
  if should_run () then user_tests else []

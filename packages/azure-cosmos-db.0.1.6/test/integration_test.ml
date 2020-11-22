open Lwt
open Cosmos
open Databases
open Json_j
open Test_common

let create_value counter =
  let string_counter = string_of_int counter in
  ({id = document_id ^ string_counter; firstName = "A First name " ^ string_counter; lastName = "a Last name"}: create_document)
  |> string_of_create_document

let replace_value counter =
  let string_counter = string_of_int counter in
  ({id = document_id ^ string_counter; firstName = "Something different"; lastName = "a Last name"}: create_document)
  |> string_of_create_document

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

let create_database_if_not_exists_test _ () =
  let res = D.create_if_not_exists dbname in
  res >>= fun (code, body) ->
  let _ = Alcotest.(check int) "Status same int" 200 code in
  let _ =
    match body with
    | Some {id; _} -> Alcotest.(check string) "Create name is correct" dbname id
    | None -> ()
  in
  return ()

let list_databases _ () =
  let res = D.list_databases () in
  res >>= fun (code, {_rid; databases; _count = count}) ->
  let db = List.filter (fun (x : Json_converter_t.database)-> x.id = dbname) databases in
  let _ = Alcotest.(check int) "Status same int" 200 code in
  let _ = Alcotest.(check bool) "Count" true (count > 0) in
  let _ = Alcotest.(check string) "Name of databases" dbname ((List.hd db).id) in
  return ()

let get_database_test _ () =
  let res = D.get dbname in
  res >>= fun (code, body) ->
  let _ = Alcotest.(check int) "Status same int" 200 code in
  let _ =
    match body with
    | Some {_rid; id; _self; _etag; _colls; _users; _ts} ->
      Alcotest.(check string) "Name of database" dbname id
    | None -> ()
  in
  return ()

let create_collection_test _ () =
  let res = D.Collection.create dbname collection_name in
  res >>= fun (code, body) ->
  let _ = Alcotest.(check int) "Status same int" 201 code in
  let _ =
    match body with
    | Some {id; _} -> Alcotest.(check string) "Create name is correct" collection_name id
    | None -> ()
  in
  return ()

let create_collection_if_not_exists_test _ () =
  let res = D.Collection.create_if_not_exists dbname collection_name in
  res >>= fun (code, body) ->
  let _ = Alcotest.(check int) "Status same int" 200 code in
  let _ =
    match body with
    | Some {id; _} -> Alcotest.(check string) "Create name is correct" collection_name id
    | None -> ()
  in
  return ()

let list_collection_test _ () =
  let res = D.Collection.list dbname in
  res >>= fun (code, {rid = _; document_collections = _; count}) ->
  let _ = Alcotest.(check int) "Status same int" 200 code in
  let _ = Alcotest.(check int) "Count" 1 count in
  return ()

let get_collection_test _ () =
  let res = D.Collection.get dbname collection_name in
  res >>= fun (code, body) ->
  let _ = Alcotest.(check int) "Status same int" 200 code in
  let _ =
    match body with
    | Some Json_converter_t.{
        id;
        rid = _;
        self = _;
        etag = _;
        ts = _;
        sprocs = _;
        triggers = _;
        docs = _;
        udfs = _;
        conflicts = _;
        indexing_policy = _;
        partition_key = _
      } -> Alcotest.(check string) "Name of database" collection_name id
    | None -> ()
  in
  return ()

let create_document_test _ () =
  let res = D.Collection.Document.create dbname collection_name (create_value 1) in
  res >>= fun (code, _) ->
  let _ = Alcotest.(check int) "Status same int" 201 code in
  return ()

let list_document_test _ () =
  let res = D.Collection.Document.list dbname collection_name in
  res >>= fun (code, headers, values) ->
  let _ = Alcotest.(check int) "Status same int" 200 code in
  let _ = Alcotest.(check (option string)) "Continuation" None (Response_headers.x_ms_continuation headers) in
  let _ =
    match values with
    | Some {rid = _; documents; count} ->
      let docs = List.map (fun (x, _) -> create_document_of_string x) documents in
      let {id; firstName; lastName} = List.hd docs in
      Alcotest.(check int) "Count field" count 1;
      Alcotest.(check int) "Count list" count (List.length documents);
      Alcotest.(check string) "id" id "document_id1";
      Alcotest.(check string) "firstName" firstName "A First name 1";
      Alcotest.(check string) "lastName" lastName "a Last name"
    | _ ->
      Alcotest.(check int) "list_document_test fail" 1 0
  in
  return ()

let range i j =
  let rec loop acc k =
    if i = k then
      k :: acc
    else
      loop (k :: acc) (pred k)
  in
  loop [] j
  
let create_a_lot_of_documents_test _ () =
  let ids = range 21 100 in
  let results = Lwt_list.map_p (fun id ->
  let res = D.Collection.Document.create dbname collection_name (create_value id) in
  res
  ) ids
  in
  results >>= fun result_list ->
  let results_length = List.filter (fun (code, _) -> code = 201) result_list |> List.length in
  let length_429 = List.filter (fun (code, _) -> code = 429) result_list |> List.length in
  let _ = Alcotest.(check int) "Documents that was rejected" 0 length_429 in
  let _ = Alcotest.(check int) "All documents should be created full list" (List.length ids) (List.length result_list) in
  let _ = Alcotest.(check int) "All documents should be created succesfully" (List.length ids) results_length in
  return_unit
  
let list_multiple_documents_test _ () =
  let list_values =
    let rec make_values i acc = if i <= 1 then acc else make_values (i - 1) (i::acc) in
    make_values 10 []
  in
  let values = List.map create_value list_values in
  let%lwt x = Lwt_list.map_p (fun x -> D.Collection.Document.create dbname collection_name x) values in
  let all_is_inserted = List.fold_left (fun x (y, _) -> y = 201 && x) true x in
  let _ = Alcotest.(check bool) "all_is_inserted" true all_is_inserted in
  let%lwt code, headers, result = D.Collection.Document.list ~max_item_count:5 dbname collection_name in
  let _ = Alcotest.(check int) "Status same int" 200 code in
  let continuation = Option.get (Response_headers.x_ms_continuation headers) in
  let%lwt code2, headers2, result2 = D.Collection.Document.list ~max_item_count:5 ~continuation dbname collection_name in
  let _ = Alcotest.(check int) "Status same int" 200 code2 in
  let _ = Alcotest.(check (option string)) "Continuation" None (Response_headers.x_ms_continuation headers2) in
  let _ =
    match result, result2 with
    | Some {rid = _; documents = _; count}, Some {rid = _; documents = _; count = count2} ->
      Alcotest.(check int) "Count field" (count + count2) 10;
    | _ ->
      Alcotest.(check int) "list_multiple_documents_test fail" 1 0
  in
  return ()

let change_feed_test _ () =
  (* list all documents *)
  let%lwt code, headers, _result = D.Collection.Document.list ~a_im:true dbname collection_name in
  let _ = Alcotest.(check int) "Status same int" 200 code in

  (* list all documents again, none returned *)
  let if_none_match = Option.get @@ Response_headers.etag headers in
  let%lwt code, _headers, _result = D.Collection.Document.list ~a_im:true ~if_none_match dbname collection_name in
  let _ = Alcotest.(check int) "Status same int" 304 code in

  (* insert record *)
  let%lwt code, _ = D.Collection.Document.create dbname collection_name (create_value 20) in
  let _ = Alcotest.(check int) "Status same int" 201 code in

  (* list all documents, one returned *)
  let%lwt code, _headers, _result = D.Collection.Document.list ~a_im:true ~if_none_match dbname collection_name in
  let _ = Alcotest.(check int) "Status same int" 200 code in
  return ()

let query_document_test _ () =
  let query =
    Json_converter_t.{query = "SELECT * FROM " ^ collection_name ^ " f WHERE f.firstName = @fname";
                      parameters = [{name = "@fname"; value = "A First name 1"}]
                     }
  in
  let res = D.Collection.Document.query dbname collection_name query in
  res >>= fun (code, _, values) ->
  let _ = Alcotest.(check int) "Status same int" 200 code in
  let _ =
    match values with
    | Some {rid = _; documents; count} ->
      let docs = List.map (fun (x, _) -> create_document_of_string x) documents in
      let {id; firstName; lastName} = List.hd docs in
      Alcotest.(check int) "Count field" count 1;
      Alcotest.(check int) "Count list" count (List.length documents);
      Alcotest.(check string) "id" id "document_id1";
      Alcotest.(check string) "firstName" firstName "A First name 1";
      Alcotest.(check string) "lastName" lastName "a Last name"
    | _ ->
      Alcotest.(check int) "query_document_test fail" 1 0
  in
  return ()

  let query_document_count_test _ () =
    let query =
      Json_converter_t.{query = "SELECT VALUE COUNT(1) FROM f";
                        parameters = []
                       }
    in
    let res = D.Collection.Document.query dbname collection_name query in
    res >>= fun (code, _, values) ->
    let _ = Alcotest.(check int) "Status same int" 200 code in
    let _ =
      match values with
      | Some {rid = _; documents; count} ->
        Alcotest.(check int) "Count field" count 1;
        let docs = List.map (fun (x, _) -> x) documents in
        let first = List.hd docs in
        let value_count = int_of_string first in
        Alcotest.(check int) "Value count field" value_count 11
      | _ ->
        Alcotest.(check int) "query_document_count_test fail" 1 0
    in
    return ()
  
  let get_document_test _ () =
  let res = D.Collection.Document.get dbname collection_name (document_id ^ "1") in
  res >>= fun (code, _) ->
  let _ = Alcotest.(check int) "Status same int" 200 code in
  return ()

let replace_document_test _ () =
  let res = D.Collection.Document.replace dbname collection_name (document_id ^ "1") (replace_value 1) in
  res >>= fun (code, _) ->
  let _ = Alcotest.(check int) "Status same int" 200 code in
  return ()

let delete_document_test _ () =
  let res = D.Collection.Document.delete dbname collection_name (document_id ^ "1") in
  res >>= fun code ->
  let _ = Alcotest.(check int) "Status same int" 204 code in
  return ()

let delete_collection_test _ () =
  let res = D.Collection.delete dbname collection_name in
  res >>= fun code ->
  let _ = Alcotest.(check int) "Status same int" 204 code in
  return ()

let delete_database_test _ () =
  let res = D.delete dbname in
  res >>= fun code ->
  let _ = Alcotest.(check int) "Status same int" 204 code in
  return ()

let cosmos_test = [
  Alcotest_lwt.test_case "create database" `Slow create_database_test;
  Alcotest_lwt.test_case "create database if not exists" `Slow create_database_if_not_exists_test;
  Alcotest_lwt.test_case "list database" `Slow list_databases;
  Alcotest_lwt.test_case "get database" `Slow get_database_test;

  Alcotest_lwt.test_case "create collection" `Slow create_collection_test;
  Alcotest_lwt.test_case "create collection if not exists" `Slow create_collection_if_not_exists_test;
  Alcotest_lwt.test_case "list collection" `Slow list_collection_test;
  Alcotest_lwt.test_case "get collection" `Slow get_collection_test;

  Alcotest_lwt.test_case "create document" `Slow create_document_test;
  Alcotest_lwt.test_case "list document" `Slow list_document_test;
  Alcotest_lwt.test_case "list multiple documents" `Slow list_multiple_documents_test;
  Alcotest_lwt.test_case "change feed" `Slow change_feed_test;
  Alcotest_lwt.test_case "query document" `Slow query_document_test;
  Alcotest_lwt.test_case "query document count" `Slow query_document_count_test;
  Alcotest_lwt.test_case "get document" `Slow get_document_test;
  Alcotest_lwt.test_case "replace document" `Slow replace_document_test;
  Alcotest_lwt.test_case "create a lot document test" `Slow create_a_lot_of_documents_test;

  Alcotest_lwt.test_case "delete document" `Slow delete_document_test;
  Alcotest_lwt.test_case "delete collection" `Slow delete_collection_test;
  Alcotest_lwt.test_case "delete database" `Slow delete_database_test;
]

let test =
  if should_run () then cosmos_test else []

let create_database_with_partition_key_test _ () =
  let res = D.create dbname_partition in
  res >>= fun (code, body) ->
  let _ = Alcotest.(check int) "Status same int" 201 code in
  let _ =
    match body with
    | Some {id; _} -> Alcotest.(check string) "Create name is correct" dbname id
    | None -> ()
  in
  return ()

let create_collection_with_partition_key_test _ () =
  let partition_key = Some Json_converter_t.({paths = ["/lastName"]; kind = "Hash"; version = None}) in
  let res = D.Collection.create ~partition_key dbname_partition collection_name_partition in
  res >>= fun (code, body) ->
  let _ = Alcotest.(check int) "Status same int" 201 code in
  let _ =
    match body with
    | Some {id; _} -> Alcotest.(check string) "Create name is correct" collection_name id
    | None -> ()
  in
  return ()

let create_document_with_partition_key_test _ () =
  let res = D.Collection.Document.create ~partition_key:"a Last name" dbname_partition collection_name_partition (create_value 1) in
  res >>= fun (code, _) ->
  let _ = Alcotest.(check int) "Status same int" 201 code in
  return ()

let delete_document_with_partition_key_test _ () =
  let res = D.Collection.Document.delete ~partition_key:"a Last name" dbname_partition collection_name_partition (document_id ^ "1") in
  res >>= fun code ->
  let _ = Alcotest.(check int) "Status same int" 204 code in
  return ()

let delete_collection_with_partition_key_test _ () =
  let res = D.Collection.delete dbname_partition collection_name_partition in
  res >>= fun code ->
  let _ = Alcotest.(check int) "Status same int" 204 code in
  return ()

let delete_database_with_partition_test _ () =
  let res = D.delete dbname_partition in
  res >>= fun code ->
  let _ = Alcotest.(check int) "Status same int" 204 code in
  return ()

let test_partition_key_cosmos = [
  Alcotest_lwt.test_case "create database" `Slow create_database_with_partition_key_test;
  Alcotest_lwt.test_case "create collection with partition key" `Slow create_collection_with_partition_key_test;
  Alcotest_lwt.test_case "create document with partition key" `Slow create_document_with_partition_key_test;
  Alcotest_lwt.test_case "delete document with partition key" `Slow delete_document_with_partition_key_test;
  Alcotest_lwt.test_case "delete collection with partition key" `Slow delete_collection_with_partition_key_test;
  Alcotest_lwt.test_case "delete database" `Slow delete_database_with_partition_test;
]

let test_partition_key =
  if should_run () then test_partition_key_cosmos else []

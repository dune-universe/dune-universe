open Ezsqlite.Value

let test_db t =
    let db = Ezsqlite.load "_test.db" in
    let _ = Test.check t "Database created" (fun () ->
        Sys.file_exists "_test.db") true
    in db

let test_stmt t db =
    let _ = Test.check_raise t "Invalid SQL" (fun () ->
        let _ = Ezsqlite.prepare db "testing" in ()) in

    (* Create table *)
    let _ =
        let stmt = Ezsqlite.prepare db "CREATE TABLE testing (id INTEGER PRIMARY KEY, a TEXT, b BLOB, c INT, d DOUBLE);" in
        Test.check t "Create Table Step" (fun () ->
            Ezsqlite.exec stmt) () in

    (* Set/Get value *)
    let _ =
        let stmt = Ezsqlite.prepare db "INSERT INTO testing (a, b, c, d) VALUES (?, ?, ? , ?)" in
        let _ = Ezsqlite.bind_list stmt Ezsqlite.[Text "test"; Blob (Bytes.of_string "abc"); Integer 123L; Double 0.6] in
        Ezsqlite.step stmt in

    let blob = Ezsqlite.Blob.open_blob db "testing" "b" 1L in
    let b = Bytes.create 3 in
    let _ = try Ezsqlite.Blob.read blob b 3 with _ -> print_endline "error" in
    let () = Test.check t "Read Blob" (fun () ->
            Bytes.to_string b) "abc" in

    let stmt = Ezsqlite.prepare db "SELECT * FROM testing" in
    let _ = Ezsqlite.iter stmt (fun s ->
        Test.check t "Value of 'a'" (fun () ->
            Ezsqlite.text stmt 1) "test";
        Test.check t "Value of 'b'" (fun () ->
            Ezsqlite.text stmt 2) "abc";
        Test.check t "Value of 'c'" (fun () ->
            Ezsqlite.text stmt 3) "123";
        Test.check t "Value of 'd'" (fun () ->
            Ezsqlite.text stmt 4) "0.6") in

    (* Custom functions *)

    let _ = Ezsqlite.create_function db "tostring" 1 (fun x ->
        Ezsqlite.Value.Text (Ezsqlite.Value.to_string x.(0))) in

    let _ = Ezsqlite.run db "SELECT tostring(12)" (fun x ->
        Test.check t "create_function" (fun () -> Ezsqlite.text x 0) "12") in


    (* Errors *)

    let _= Test.check t "error 1" (fun () ->
        try let _ = Ezsqlite.run_ign db "SELECT * from fakedb" () in false  with _ -> true) true in

    let _= Test.check t "error 2" (fun () ->
        try let _ = Ezsqlite.run_ign db "" () in false  with _ -> true) true in

    let _= Test.check t "error 3" (fun () ->
        try let _ = Ezsqlite.run_ign db "SELECT x from testing" () in false  with _ -> true) true


    in ()

let _ =
    let t = Test.start () in
    let _ = try Sys.remove "_test.db" with _ -> () in
    let db = test_db t in
    let _ = test_stmt t db in
    Test.finish t


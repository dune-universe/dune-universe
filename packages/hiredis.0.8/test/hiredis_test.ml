let test_reader  t =
    let r = Hiredis.Reader.create () in
    let _ = Hiredis.Reader.feed r "dfgdfgdfg" in
    let res = Hiredis.Reader.get_reply r in
    let _ = Test.check t "Reader Get reply (invalid)" (fun () -> res) None in

    let r = Hiredis.Reader.create () in
    let _ = Hiredis.Reader.feed r "$4\r\ntest\r\n" in
    let res = Hiredis.Reader.get_reply r in
    let _ = Test.check t "Reader Get reply (test)" (fun () -> res) (Some (Hiredis.String "test")) in

    let _ = Hiredis.Reader.feed r "*-1\r\n" in
    let res = Hiredis.Reader.get_reply r in
    let _ = Test.check t "Reader Get reply (nil)" (fun () -> res) (Some Hiredis.Nil) in

    let _ = Hiredis.Reader.feed r "*2\r\n" in
    let _ = Hiredis.Reader.feed r "$4\r\ntest\r\n" in
    let _ = Hiredis.Reader.feed r ":123\r\n" in
    let res = Hiredis.Reader.get_reply r in
    let _ = Test.check t "Reader Get reply (array)" (fun () -> res) (Some (Hiredis.Array [| Hiredis.String "test"; Hiredis.Integer 123L |]))
    in ()

let test_client t =
    let config = ["port", ["1234"]; "daemonize", ["no"]] in
    let server = Hiredis.Shell.Server.start ~config 1234 in
    let () = print_endline "Starting redis server" in
    let _ = Unix.sleep 1 in
    let client = Hiredis.Client.connect ~port:1234 "127.0.0.1" in
    let res = Hiredis.Client.run_v client [| Hiredis.String "SET"; Hiredis.String "a"; Hiredis.Integer 123L |] in
    if res = Hiredis.Nil then print_endline "redis-server is not installed"
    else
        let _ = Test.check t "Client response" (fun () -> res) (Hiredis.Status "OK") in
        let res = Hiredis.Client.run client [| "GET"; "a" |] in
        let _ = Test.check t "Client response2" (fun () -> res) (Hiredis.String "123") in
        let res = Hiredis.Client.run client [| "xxxx" |] in
        let _ = Test.check t "Client error response" (fun () -> res) (Hiredis.Error "ERR unknown command 'xxxx'") in
        let _ = Hiredis.Shell.Server.stop server in ()

let _ =
    let t = Test.start () in
    let _ = test_reader t in
    let _ = test_client t in
    Test.finish t


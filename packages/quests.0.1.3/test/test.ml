open Lwt

let test_get _ () =
  Quests.get "https://postman-echo.com/get" >|= fun r ->
  print_endline @@ Quests.Response.show r;
  Alcotest.(check int) "status code" 200 r.Quests.Response.status_code

let test_post _ () =
  Quests.(
    post "https://postman-echo.com/post" ~data:(Form [ ("key", "value") ]))
  >|= fun r ->
  print_endline @@ Quests.Response.show r;
  Alcotest.(check int) "status code" 200 r.Quests.Response.status_code

let test_put _ () =
  Quests.put "https://postman-echo.com/put" >|= fun r ->
  print_endline @@ Quests.Response.show r;
  Alcotest.(check int) "status code" 200 r.Quests.Response.status_code

let test_delete _ () =
  Quests.delete "https://postman-echo.com/delete" >|= fun r ->
  print_endline @@ Quests.Response.show r;
  Alcotest.(check int) "status code" 200 r.Quests.Response.status_code

let test_basic_auth _ () =
  Quests.(
    get "https://postman-echo.com/basic-auth"
      ~auth:(Basic ("postman", "password")))
  >>= fun r ->
  print_endline @@ Quests.Response.show r;
  Alcotest.(check int) "status code" 200 r.Quests.Response.status_code
  |> Lwt.return
  >>= fun () ->
  Quests.(
    get "https://postman-echo.com/basic-auth"
      ~auth:(Basic ("postman", "wrong_password")))
  >|= fun r ->
  print_endline @@ Quests.Response.show r;
  Alcotest.(check int) "status code" 401 r.Quests.Response.status_code

let test_session _ () =
  let open Quests in
  let s = Session.create () in
  Session.get s "https://postman-echo.com/get" >>= fun r ->
  print_endline @@ Quests.Response.show r;
  Alcotest.(check int) "status code" 200 r.Quests.Response.status_code;
  Session.post s "https://postman-echo.com/post" >>= fun r ->
  print_endline @@ Quests.Response.show r;
  Alcotest.(check int) "status code" 200 r.Quests.Response.status_code;
  Session.get s "https://postman-echo.com/get" >>= fun r ->
  print_endline @@ Quests.Response.show r;
  Alcotest.(check int) "status code" 200 r.Quests.Response.status_code;
  Session.put s "https://postman-echo.com/put" >|= fun r ->
  print_endline @@ Quests.Response.show r;
  Alcotest.(check int) "status code" 200 r.Quests.Response.status_code

let test_session_many _ () =
  let open Quests in
  let s = Session.create () in
  let urls = List.init 16 (fun _ -> "https://postman-echo.com/delay/2") in
  List.map (Session.get s) urls
  |> Lwt.all
  >|= List.iter (fun r ->
          Alcotest.(check int) "status code" 200 r.Response.status_code)
  >>= fun () ->
  List.map (Session.get s) urls
  |> Lwt.all
  >|= List.iter (fun r ->
          Alcotest.(check int) "status code" 200 r.Response.status_code)

let () =
  let open Alcotest_lwt in
  Lwt_main.run
  @@ Lwt.pick
       [
         Lwt_unix.sleep (60.0 *. 3.0);
         run "Quests"
           [
             ( "methods",
               [
                 test_case "Get" `Quick test_get;
                 test_case "Post" `Quick test_post;
                 test_case "Put" `Quick test_put;
                 test_case "Delete" `Quick test_delete;
               ] );
             ("authentication", [ test_case "Basic" `Quick test_basic_auth ]);
             ( "session",
               [
                 test_case "Methods" `Quick test_session;
                 test_case "Many methods" `Quick test_session_many;
               ] );
           ];
       ]

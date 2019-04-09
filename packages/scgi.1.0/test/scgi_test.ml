(** Tests for the scgi module *)
open Test_common

open Lwt

let mock_request () =
  let content = "What is the answer to life?" in
  let uri = Uri.of_string "/deepthought" in
  let req = Scgi.Request.make `POST uri [] content in
  Scgi.Request.to_string req

let tests =
  [ ( "scgi_header"
    , fun () ->
        let result = Scgi.Headers.of_string (mock_request ()) in
        assert_int "same length list" (List.length result) 4 )
  ; ( "scgi_request"
    , fun () ->
        Scgi.Request.of_stream (Lwt_stream.of_string (mock_request ()))
        >>= fun r ->
        let open Scgi.Request in
        assert_int "content_length" 27 (content_length r) >>= fun () ->
        assert_equal ~printer:Scgi.Http_method.to_string ~msg:"method"
          ~expected:`POST (meth r)
        >>= fun () ->
        assert_string "uri" "/deepthought" (path r) >>= fun () ->
        let body = contents r in
        assert_string ~msg:"content" ~expected:"What is the answer to life?"
          body )
  ]

let () = run tests

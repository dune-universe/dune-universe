open Test_common
open Lwt
open Scgi

let tests =
  [ ( "cookies"
    , fun () ->
        let make cookies =
          Request.make `GET
            (Uri.of_string "http://example.net")
            [ ("HTTP_COOKIE", cookies) ]
            ""
        in
        let printer = string_option_printer in
        let r = make "uid=123456" in
        assert_equal ~printer ~msg:"123456" ~expected:(Some "123456")
          (Request.cookie r "uid")
        >>= fun () ->
        let r = make "uid=ABCDEFG; foo=bar; baz=biz" in
        assert_equal ~printer ~msg:"ABCDEFG" ~expected:(Some "ABCDEFG")
          (Request.cookie r "uid")
        >>= fun () ->
        assert_equal ~printer ~msg:"foo=bar" ~expected:(Some "bar")
          (Request.cookie r "foo")
        >>= fun () ->
        assert_equal ~printer ~msg:"baz=biz" ~expected:(Some "biz")
          (Request.cookie r "baz")
        >>= fun () ->
        assert_equal ~printer ~msg:"bar=None" ~expected:None
          (Request.cookie r "bar") )
  ]

let () = run tests

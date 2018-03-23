open! Core
open Async
open Email_message
open Email_message.Private
open Expect_test_helpers

open Media_type

let last headers =
  let result = Option.value_exn (last headers) in
  print_s [%message "" ~_:(result : t)]
;;

let%expect_test "[Media_type.last]" =
  let headers =
    ["Content-Type", "multipart/mixed;\nboundary=\"BOUNDARY\""]
    |> Email_headers.of_list ~whitespace:`Normalize
  in
  last headers;
  let%bind () =
    [%expect {|
    ((mime_type    multipart)
     (mime_subtype mixed)
     (params ((boundary BOUNDARY)))) |}]
  in
  return ()
;;

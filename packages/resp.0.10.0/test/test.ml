module Value = Resp.String
open Lwt.Infix
open Resp

let test_roundtrip _ =
  Lwt_main.run
    (let v =
       Resp.(list id)
         [
           simple_string "x";
           int64 123L;
           string "abc";
           simple_string "";
           string "";
           array id [||];
           array int64 [| 1L |];
           alist string int64 [ ("a", 1L); ("b", 2L); ("c", 3L) ];
         ]
     in
     let output = ref "" in
     Value.write output v >>= fun () ->
     print_endline !output;
     Value.read output >>= fun v' ->
     Value.write output v' >|= fun () ->
     print_endline !output;
     Alcotest.(check bool) "compare" true (equal v v'))

let () =
  Alcotest.run "Resp"
    [ ("encoding", [ Alcotest.test_case "Roundtrip" `Quick test_roundtrip ]) ]

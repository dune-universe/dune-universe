module Value = Resp.String
open Lwt.Infix

let test_roundtrip _ () =
  let v = `Array [|`String "x"; `Integer 123L; `Bulk "abc"|] in
  let output = ref "" in
  Value.write output v
  >>= fun () ->
  Value.read output
  >|= fun v' -> Alcotest.(check bool) "roundtrip" (v = v') true

let () =
  Alcotest.run "Resp"
    [("encoding", [Alcotest_lwt.test_case "Get" `Quick test_roundtrip])]

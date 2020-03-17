let ( >>= ) = Lwt.( >>= )

let test_first_to_lwt _ () =
  Lambda_streams.Finite.Async.from_list ["foo"; "bar"; "baz"]
  |> Lambda_streams_lwt.Async.first_to_lwt
  >>= fun value -> Alcotest.(check string) "" value "foo" |> Lwt.return

let test_last_to_lwt _ () =
  Lambda_streams.Finite.Async.from_list ["foo"; "bar"; "baz"]
  |> Lambda_streams_lwt.Async.last_to_lwt
  >>= fun value -> Alcotest.(check string) "" value "baz" |> Lwt.return

let test_to_lwt_list _ () =
  Lambda_streams.Finite.Async.from_list ["foo"; "bar"; "baz"]
  |> Lambda_streams_lwt.Async.to_lwt_list
  >>= fun value -> Alcotest.(check (list string)) "" value ["foo"; "bar"; "baz"] |> Lwt.return

let test_from_lwt _ () =
  Lwt.return "foo"
  |> Lambda_streams_lwt.Async.from_lwt
  |> Lambda_streams.Finite.Async.map (fun value -> Alcotest.(check string) "" value "foo")
  |> Lambda_streams_lwt.Async.last_to_lwt

let test_to_lwt_stream _ () =
  let expected_list = ["foo"; "bar"; "baz"] in
  Lambda_streams.Finite.Async.from_list expected_list
  |> Lambda_streams_lwt.Async.to_lwt_stream
  |> Lwt_stream.to_list
  >>= fun list -> Alcotest.(check (list string)) "" list expected_list |> Lwt.return

let test_from_lwt_stream _ () =
  let array = [|"foo"; "bar"; "baz"|] and index = ref 0 in
  Lwt_stream.of_array array
  |> Lambda_streams_lwt.Async.from_lwt_stream
  |> Lambda_streams.Finite.Async.map (fun value ->
         Alcotest.(check string) "" value array.(!index);
         index := !index + 1)
  |> Lambda_streams_lwt.Async.last_to_lwt

let () =
  print_newline ();
  Lwt_main.run
  @@ Alcotest_lwt.run
       "Lambda_streams_lwt"
       [
         ( "Async",
           [
             test_first_to_lwt;
             test_last_to_lwt;
             test_to_lwt_list;
             test_from_lwt;
             test_to_lwt_stream;
             test_from_lwt_stream;
           ]
           |> List.map (fun test -> Alcotest_lwt.test_case "" `Quick test) );
       ]

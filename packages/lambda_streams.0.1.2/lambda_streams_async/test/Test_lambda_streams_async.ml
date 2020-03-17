let ( >>= ), ( >>| ) = Async_kernel.(( >>= ), ( >>| ))

let test_first_to_async _ =
  Lambda_streams.Finite.Async.from_list ["foo"; "bar"; "baz"]
  |> Lambda_streams_async.Async.first_to_async
  >>= fun value -> Alcotest.(check string) "" value "foo" |> Async.return

let test_last_to_async _ =
  Lambda_streams.Finite.Async.from_list ["foo"; "bar"; "baz"]
  |> Lambda_streams_async.Async.last_to_async
  >>= fun value -> Alcotest.(check string) "" value "baz" |> Async.return

let test_to_async_list _ =
  Lambda_streams.Finite.Async.from_list ["foo"; "bar"; "baz"]
  |> Lambda_streams_async.Async.to_async_list
  >>= fun value -> Alcotest.(check (list string)) "" value ["foo"; "bar"; "baz"] |> Async.return

let test_from_async _ =
  Async.return 123
  |> Lambda_streams_async.Async.from_async
  |> Lambda_streams.Finite.Async.map (( * ) 2)
  |> Lambda_streams_async.Async.first_to_async
  >>= fun value -> Alcotest.(check int) "" value 246 |> Async.return

let test_to_async_stream _ =
  Lambda_streams.Finite.Async.from_list ["foo"; "bar"; "baz"]
  |> Lambda_streams_async.Async.to_async_stream
  |> Async.Stream.to_list
  >>= fun value -> Alcotest.(check (list string)) "" value ["foo"; "bar"; "baz"] |> Async.return

let test_from_async_stream _ =
  let array = [|"foo"; "bar"; "baz"|] and index = ref 0 in
  Array.to_list array
  |> Async.Stream.of_list
  |> Lambda_streams_async.Async.from_async_stream
  |> Lambda_streams.Finite.Async.map (fun value ->
         Alcotest.(check string) "" value array.(!index);
         index := !index + 1)
  |> Lambda_streams_async.Async.first_to_async

let test_to_reader _ =
  Lambda_streams.Finite.Async.from_list ["foo"; "bar"; "baz"]
  |> Lambda_streams_async.Async.to_reader
  |> Async.Pipe.read_all
  >>| Core.Queue.to_list
  >>= fun value -> Alcotest.(check (list string)) "" value ["foo"; "bar"; "baz"] |> Async.return

let test_from_reader _ =
  Async.Pipe.of_list ["foo"; "bar"; "baz"]
  |> Lambda_streams_async.Async.from_reader
  |> Lambda_streams.Finite.Async.map (( ^ ) "!")
  |> Lambda_streams_async.Async.to_reader
  |> Async.Pipe.read_all
  >>| Core.Queue.to_list
  >>= fun value -> Alcotest.(check (list string)) "" value ["!foo"; "!bar"; "!baz"] |> Async.return

let () =
  print_newline ();
  let _ =
    Alcotest_async.run
      "Lambda_streams_async"
      [
        ( "Async",
          [
            test_first_to_async;
            test_last_to_async;
            test_to_async_list;
            test_from_async;
            test_to_async_stream;
            test_from_async_stream;
            test_to_reader;
            test_from_reader;
          ]
          |> List.map (fun test -> Alcotest_async.test_case "" `Quick test) );
      ]
  in
  Core.never_returns @@ Async_unix.Scheduler.go ()

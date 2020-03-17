open Lambda_streams

let ( >>= ) = Async_kernel.( >>= )

let first_to_async stream =
  let deferred = Async_kernel.Ivar.create () in
  stream
  |> Async.listen (function
         | Signal.Data value -> Async_kernel.Ivar.fill_if_empty deferred value
         | _ -> ());
  Async_kernel.Ivar.read deferred

let last_to_async stream =
  let deferred = Async_kernel.Ivar.create () and last_value = ref None in
  stream
  |> Async.listen (fun e ->
         match e, !last_value with
         | Signal.Data value, _ -> last_value := Some value
         | EndOfSignal, Some value -> Async_kernel.Ivar.fill_if_empty deferred value
         | _ -> ());
  Async_kernel.Ivar.read deferred

let to_async_list stream =
  let deferred = Async_kernel.Ivar.create () and list = ref [] in
  stream
  |> Async.listen (function
         | Signal.Data value -> list := value :: !list
         | EndOfSignal -> Async_kernel.Ivar.fill_if_empty deferred (List.rev !list));
  Async_kernel.Ivar.read deferred

let from_async async =
  Async.make @@ fun cb ->
  async
  >>= (fun value ->
        cb @@ Signal.Data value;
        cb EndOfSignal;
        Async_kernel.return ())
  |> ignore

let to_async_stream stream =
  Async_kernel.Stream.create (fun tail ->
      stream
      |> Async.listen (function
             | Signal.Data value -> Async_kernel.Tail.extend tail value
             | EndOfSignal -> Async_kernel.Tail.close_exn tail))

let from_async_stream async_stream =
  let is_closed = Async_kernel.Stream.closed async_stream in
  Async.make @@ fun cb ->
  Async_kernel.Deferred.all_unit
    [
      async_stream
      |> Async_kernel.Stream.iter' ~f:(fun value -> cb @@ Signal.Data value |> Async_kernel.return);
      (is_closed >>= fun () -> cb EndOfSignal |> Async_kernel.return);
    ]
  |> ignore

let to_reader stream =
  Async_kernel.Pipe.create_reader ~close_on_exception:true (fun writer ->
      stream
      |> Async.listen (function
             | Signal.Data value -> Async_kernel.Pipe.write writer value |> ignore
             | EndOfSignal -> Async_kernel.Pipe.close writer)
      |> Async_kernel.return)

let from_reader reader =
  let ( >>= ) = Async_kernel.( >>= ) in
  Async.make @@ fun cb ->
  reader
  |> Async_kernel.Pipe.iter ~f:(fun value -> cb @@ Signal.Data value |> Async_kernel.return)
  >>= (fun () -> cb EndOfSignal |> Async_kernel.return)
  |> ignore

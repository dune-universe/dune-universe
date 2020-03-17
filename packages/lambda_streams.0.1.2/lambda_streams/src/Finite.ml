module Sync = struct
  type 'a input = 'a Signal.t Sync.input

  type 'a output = 'a Sync.output

  let make_output = Sync.make_output

  let send = Sync.send

  let pure value =
    let was_sent = ref false in
    Sync.make_input @@ fun () ->
    match !was_sent with
    | true -> Signal.EndOfSignal
    | false ->
        was_sent := true;
        Data value

  let empty () : 'a input = Sync.make_input @@ fun () -> Signal.EndOfSignal

  let from_list list =
    let list' = ref list in
    Sync.make_input @@ fun () ->
    match !list' with
    | value :: rest ->
        list' := rest;
        Signal.Data value
    | [] -> EndOfSignal

  let from_array array =
    let index = ref 0 in
    Sync.make_input @@ fun () ->
    match !index with
    | index' when index' < Array.length array ->
        index := index' + 1;
        Signal.Data array.(index')
    | _ -> EndOfSignal

  let pipe output_stream input_stream =
    let more = ref true in
    while !more do
      match Sync.next input_stream with
      | Signal.Data value -> output_stream |> Sync.send value
      | EndOfSignal -> more := false
    done

  let map f stream = Sync.make_input @@ fun () -> stream |> Sync.next |> Signal.map f

  let filter f stream =
    Sync.make_input @@ fun () ->
    let value = ref @@ Sync.next stream in
    while
      match !value with
      | Signal.Data v -> not @@ f v
      | EndOfSignal -> false
    do
      value := Sync.next stream
    done;
    !value

  let take n stream =
    let index = ref 0 in
    Sync.make_input @@ fun () ->
    let value = if !index < n then Sync.next stream else Signal.EndOfSignal in
    index := !index + 1;
    value

  let take' n stream =
    let index = ref 0 in
    Sync.make_input @@ fun () ->
    let value = if !index < n then Signal.Data (Sync.next stream) else Signal.EndOfSignal in
    index := !index + 1;
    value

  let skip n stream =
    let index = ref 0 and skipped = ref false in
    Sync.make_input @@ fun () ->
    while not !skipped do
      if !index >= n then skipped := true else Sync.next stream |> ignore;
      index := !index + 1
    done;
    Sync.next stream

  let until f ma = Sync.make_input @@ fun () -> ma |> Sync.next |> Signal.filter f

  let fold_left f init stream =
    let more = ref true and result = ref init in
    while !more do
      match Sync.next stream with
      | Signal.Data value -> result := f !result value
      | EndOfSignal -> more := false
    done;
    !result

  let concat streams =
    let streams' = ref streams in
    let rec fetch_signal s =
      match s with
      | stream :: rest -> (
          match Sync.next stream with
          | Signal.Data value -> Signal.Data value
          | EndOfSignal ->
              streams' := rest;
              fetch_signal rest)
      | [] -> EndOfSignal
    in
    Sync.make_input @@ fun () -> fetch_signal !streams'

  let flatten stream_of_streams =
    let current_stream = ref @@ Sync.next stream_of_streams in
    let rec fetch s =
      match s with
      | Signal.Data stream -> (
          match Sync.next stream with
          | Signal.EndOfSignal ->
              let stream' = Sync.next stream_of_streams in
              current_stream := stream';
              fetch stream'
          | value -> value)
      | EndOfSignal -> EndOfSignal
    in
    Sync.make_input @@ fun () -> fetch !current_stream

  let to_rev_list stream =
    let list = ref [] and more = ref true in
    while !more do
      match Sync.next stream with
      | Signal.Data value -> list := value :: !list
      | EndOfSignal -> more := false
    done;
    !list

  let to_list x = to_rev_list x |> List.rev

  let to_array x = to_list x |> Array.of_list
end

module Async = struct
  type 'a t = 'a Signal.t Async.t

  let pure value =
    let was_sent = ref false in
    Async.make @@ fun cb ->
    match !was_sent with
    | true -> cb Signal.EndOfSignal
    | false ->
        cb (Data value);
        was_sent := true

  let empty () = Async.make @@ fun cb -> cb Signal.EndOfSignal

  let from_list list =
    let rec go cb = function
      | value :: rest ->
          cb (Signal.Data value);
          go cb rest
      | [] -> cb EndOfSignal
    in
    Async.make @@ fun cb -> go cb list

  let map f stream =
    Async.make @@ fun cb -> stream |> Async.listen (fun value -> cb (Signal.map f value))

  let filter f stream =
    Async.make @@ fun cb ->
    stream
    |> Async.listen (function
           | Signal.Data value when f value -> cb (Signal.Data value)
           | Data _ -> ()
           | EndOfSignal -> cb EndOfSignal)

  let scan f init stream =
    let acc = ref init in
    Async.make @@ fun cb ->
    stream
    |> Async.listen (function
           | Signal.Data value ->
               acc := f !acc value;
               cb (Signal.Data !acc)
           | EndOfSignal -> cb EndOfSignal)

  let take n stream =
    let index = ref 0 in
    Async.make @@ fun cb ->
    stream
    |> Async.listen (function
           | Signal.Data value when !index < n ->
               index := !index + 1;
               cb (Signal.Data value)
           | _ -> cb EndOfSignal)

  let take' ?close n stream =
    let index = ref 0 in
    Async.make @@ fun cb ->
    stream
    |> Async.listen (fun value ->
           match !index < n, close with
           | true, _ ->
               index := !index + 1;
               cb (Signal.Data value)
           | false, Some close' ->
               close' |> Sync.send ();
               cb EndOfSignal
           | false, None -> cb EndOfSignal)
end

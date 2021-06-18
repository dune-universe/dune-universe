let subscribe_sync host port =
  let open Redis_sync.Client in

  let print_value f = Printf.printf "%s" (string_of_reply f) in

  let print_stream_value v =
    List.iter print_value v;
    print_string "\n";
    flush stdout in

  let conn = connect {host=host; port=port} in
  let stream = (stream conn) in
  subscribe conn ["example"];
  while true do
    let response = Stream.next stream in
    print_stream_value response;
  done

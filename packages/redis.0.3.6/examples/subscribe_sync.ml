open Core.Std

let subscribe_sync host port =
  let open Redis_sync.Client in

  let print_value = function
    | `Bulk Some str -> Printf.printf "%s " str
    | `Error str -> Printf.printf "error: %s " str
    | `Status str -> Printf.printf "status: %s " str
    | `Int i -> Printf.printf "int: %d " i
    | _ -> () in

  let print_stream_value v =
    List.iter ~f:print_value v;
    print_string "\n";
    flush stdout in

  let conn = connect {host=host; port=port} in
  let stream = (stream conn) in
  subscribe conn ["example"];
  while true do
    let response = Stream.next stream in
    print_stream_value response;
  done

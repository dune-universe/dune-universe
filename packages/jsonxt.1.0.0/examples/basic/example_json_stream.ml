open Printf

let parse_stream_string s =
  let stream = Jsonxt.Basic_stream.stream_from_string s in
  Stream.iter
    (fun el ->
     let s = Jsonxt.Utilities.json_stream_to_string_repr el in
     printf "%s " s)
    stream;
  printf "\n"

let () =
    let json_s = {| [ { "id":10, "str":"foo" }, { "id":11, "str":"bar" } ] |} in
    parse_stream_string json_s

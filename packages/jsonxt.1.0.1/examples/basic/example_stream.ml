open Printf

let parse_stream_file filename =
  let stream = Jsonxt.Basic.stream_from_file filename in
  Stream.iter (fun json -> let s = Jsonxt.Basic.to_string json in printf "%s\n" s) stream

let () =
  if Array.length Sys.argv < 2 then
    printf "expected filename\n"
  else
    let filename = Sys.argv.(1) in
    parse_stream_file filename

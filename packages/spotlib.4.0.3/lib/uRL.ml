open Base

let encode s =
  let len = String.length s in
  let b = Buffer.create & len * 2 in
  for i = 0 to len - 1 do
    match String.unsafe_get s i with
    | 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '.' | '_' | '~' as c -> Buffer.add_char b c
    | c -> Buffer.add_string b & Printf.sprintf "%%%02x" (Char.code c)
  done;
  Buffer.contents b

let make_query kvs =
  String.concat "&" 
  & List.map (fun (k,v) ->
      let b = Buffer.create 100 in
      Buffer.add_string b & encode k; 
      Buffer.add_char b '=';
      Buffer.add_string b & encode v;
      Buffer.contents b) kvs

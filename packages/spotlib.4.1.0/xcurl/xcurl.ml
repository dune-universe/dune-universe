let ok200 = function
  | 200, v -> Ok v
  | n, mes -> Error (`Http (n, mes))

let get_string f = 
  let h = new Curl.handle in
  f h;
  let buf = Buffer.create 100 in
  h#set_writefunction (fun s -> Buffer.add_string buf s; String.length s);
  try
    h#perform;
    let code = h#get_httpcode in
    h#cleanup; (* Need to flush out cookies *)
    ok200 (code, Buffer.contents buf)
  with
  | Curl.CurlException (curlCode, int, string) ->
      Error (`Curl (curlCode, int, string))

let download dst f =
  let h = new Curl.handle in
  f h;
  let tmp = dst ^ ".tmp" in
  let oc = open_out_bin tmp in
  h#set_followlocation true;
  h#set_writefunction (fun s -> 
    output_string oc s; String.length s);
  h#perform;
  let code = h#get_httpcode in
  h#cleanup; (* Need to flush out cookies *)
  close_out oc;
  match code with
  | 200 -> Unix.rename tmp dst; Ok dst
  | e -> Error (`Http (e, tmp))
  

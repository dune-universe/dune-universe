let printf = Printf.printf

module R = Httph.Request

let hack2 request response =
  let mth = R.method' request in
  Httph.Response.status response 200 ;
  Httph.Response.header response "Content-Type" "text/plain" ;
  match mth with
  | "GET" ->
      Httph.Response.body response ("Got " ^ R.target request) ;
      ()
  | _ ->
      Httph.Response.body response "Not a GET" ;
      ()

let () =
  let port = 9000 in
  Printf.eprintf "\027[32m[INFO] \027[0m Listening on %d\n%!" port ;
  Httph.http_server_init hack2 port

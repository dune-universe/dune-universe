module H = Httph

let handle_request request response =
  H.Response.status response 200 ;
  match H.Request.target request with
  | "/plaintext" ->
      H.Response.header response "Content-Type" "text/plain" ;
      H.Response.body response "Hello, World!"
  | "/json" ->
      let json_data = `Assoc [("message", `String "Hello, World!")] in
      H.Response.header response "Content-Type" "application/json" ;
      H.Response.body response (Yojson.Basic.to_string json_data)
  | _ ->
      H.Response.status response 404 ;
      H.Response.header response "Content-Type" "text/html; charset=UTF-8" ;
      H.Response.body response "<h1>Not Found</h1>"

let () =
  let port = 8080 in
  Printf.eprintf "\027[32m[INFO] \027[0m Listening on %d\n%!" port;
  Httph.http_server_init handle_request 8080

let conn =
    Gremlin.Websocket.new_connection
    (Uri.of_string "http://localhost:8182/gremlin")

let get_vertices =
  Lwt_main.run (
    Gremlin.Websocket.run_query
      conn
      "g.V()"
  )

let () =
  match get_vertices with
    | Ok j -> print_endline (Yojson.Basic.pretty_to_string j)
    | Error e -> Gremlin.Websocket.print_message_status e

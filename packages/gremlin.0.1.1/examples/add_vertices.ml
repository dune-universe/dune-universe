let conn =
    Gremlin.Websocket.new_connection
    (Uri.of_string "http://localhost:8182/gremlin")

let add_vertices =
  Lwt_main.run (
    Gremlin.Websocket.run_queries_transaction
      conn
      ["g.addV('user').property('name', 'foo')";
      "g.addV('user').property('name', 'bar')";]
  )

let () =
  match add_vertices with
    | Ok j -> print_endline (Yojson.Basic.pretty_to_string j)
    | Error e -> Gremlin.Websocket.print_message_status e

open Lwt.Infix

let subscribe_lwt host port =
  let open Redis_lwt.Client in

  let print_value v = Lwt_io.printf "%s " (string_of_reply v) in

  let print_stream_value v =
    Lwt_list.iter_s print_value v >>= fun () ->
    Lwt_io.printf "%s" "\n" >>= fun () ->
    Lwt_io.flush Lwt_io.stdout
  in

  let t = (connect {host=host; port=port})
    >>= fun conn -> Redis_lwt.Client.subscribe conn ["example"]
    >>= fun () -> Lwt.return conn
    >>= fun conn -> Lwt_stream.iter_s print_stream_value (stream conn)
  in
  Lwt_main.run t;
  ()

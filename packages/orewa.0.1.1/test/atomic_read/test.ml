open Core
open Async

let host = "localhost"

(* OCAML on telephone keys *)
let port = 62265

let times = ref 3

let main () =
  let where =
    Tcp.Where_to_connect.of_host_and_port @@ Host_and_port.create ~host ~port
  in
  let%bind result =
    Tcp.with_connection where @@ fun _socket reader _writer ->
    let%bind response =
      Reader.read_one_iobuf_at_a_time reader ~handle_chunk:(fun iobuf ->
          match !times with
          | 0 ->
              let read = Iobuf.Consume.stringo ~len:1 iobuf in
              Log.Global.info "Done reading";
              return @@ `Stop read
          | n ->
              times := Int.pred n;
              Log.Global.info "Requesting more data %d more times" !times;
              return `Continue )
    in
    return
    @@
    match response with
    | `Eof -> "eof"
    | `Eof_with_unconsumed_data s -> Printf.sprintf "eof w/ data '%s'" (String.escaped s)
    | `Stopped v -> v
  in
  return @@ Log.Global.info "Read '%s'" result

let () =
  Command.async
    ~summary:"Run atomic read test"
    (Command.Let_syntax.return (fun () -> main ()))
  |> Command.run

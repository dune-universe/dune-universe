open Lwt.Infix
open Mirage_clock

module Main (Clock : PCLOCK) (Conduit : Conduit_mirage.S) = struct
  module R = Resp_mirage.Make (Conduit)
  module Server = R.Server.Default

  let commands =
    [
      ( "ping",
        fun _ client cmd nargs ->
          if nargs = 0 then Server.ok client
          else Server.recv client >>= fun x -> Server.send client x );
    ]

  let start clock conduit =
    let port = 8888 in
    let server = `TCP port in
    Logs.info (fun f -> f "Starting server on port 8888");
    let server = Server.create ~commands (conduit, server) () in
    Server.start server
end

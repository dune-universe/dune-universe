open Lwt.Infix

module Reader = Resp.Reader (struct
  type ic = Lwt_io.input_channel

  let read ic n = Lwt_io.read ic ~count:n
  let read_char = Lwt_io.read_char
  let read_line ic = Lwt_io.read_line ic
end)

module Writer = Resp.Writer (struct
  type oc = Lwt_io.output_channel

  let write oc s = Lwt_io.write oc s >>= fun () -> Lwt_io.flush oc
end)

module Backend (Data : sig
  type data
end) =
struct
  include Data

  type ic = Lwt_io.input_channel
  type oc = Lwt_io.output_channel
  type server = Conduit_lwt_unix.ctx * Conduit_lwt_unix.server

  let run server fn =
    let mode = snd server in
    let ctx = fst server in
    let on_exn exc = Printexc.to_string exc |> print_endline in
    Conduit_lwt_unix.serve ~on_exn ~ctx ~mode (fun _ ic oc -> fn (ic, oc))
end

module Client_backend = struct
  open Lwt.Infix

  type ic = Lwt_io.input_channel
  type oc = Lwt_io.output_channel
  type params = Conduit_lwt_unix.ctx * Conduit_lwt_unix.client

  let connect params =
    Conduit_lwt_unix.connect ~ctx:(fst params) (snd params)
    >|= fun (_, ic, oc) -> (ic, oc)
end

module Server = struct
  module Make
      (Auth : Resp_server.AUTH) (Data : sig
          type data
      end) =
    Resp_server.Make (Backend (Data)) (Auth) (Resp.Make (Reader) (Writer))

  module Default = Make (Resp_server.Auth.String) (struct type data = unit end)
end

module Client = Resp_client.Make (Client_backend) (Resp.Make (Reader) (Writer))

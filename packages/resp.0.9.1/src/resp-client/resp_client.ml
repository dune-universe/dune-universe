module type S = sig
  include Resp.S

  type t
  type params

  val connect : params -> t Lwt.t
  val read : t -> Resp.t Lwt.t
  val write : t -> Resp.t -> unit Lwt.t
  val run : t -> Resp.t array -> Resp.t Lwt.t
  val run_s : t -> string array -> Resp.t Lwt.t
  val decode : t -> Resp.lexeme -> Resp.t Lwt.t
  val read_lexeme : t -> Resp.lexeme Lwt.t
end

module type CLIENT = sig
  type ic
  type oc
  type params

  val connect : params -> (ic * oc) Lwt.t
end

module Make
    (Client : CLIENT)
    (S : Resp.S with type Reader.ic = Client.ic and type Writer.oc = Client.oc) =
struct
  include S
  open Lwt

  type t = Client.ic * Client.oc
  type params = Client.params

  let connect params = Client.connect params
  let read (ic, _) = S.read ic
  let write (_, oc) = S.write oc
  let decode (ic, _) = S.Reader.decode ic

  let read_lexeme (ic, _) =
    S.Reader.read_lexeme ic >>= fun x -> Resp.unwrap x |> Lwt.return

  let run_s client cmd =
    let cmd = Array.map (fun s -> `Bulk s) cmd in
    write client (`Array cmd) >>= fun () -> read client

  let run client cmd = write client (`Array cmd) >>= fun () -> read client
end

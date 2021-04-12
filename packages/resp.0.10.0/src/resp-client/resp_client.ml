include Resp_client_intf

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
    let cmd = List.to_seq cmd |> Seq.map (fun s -> Resp.string s) in
    write client (Array cmd) >>= fun () -> read client

  let run client cmd =
    let cmd = List.to_seq cmd in
    write client (Array cmd) >>= fun () -> read client
end

open Apero

let read sock buf len = 
  let io_vecs = Lwt_unix.IO_vectors.create () in
  Lwt_unix.IO_vectors.(Abuf.to_io_vecs ~idx:(Abuf.w_pos buf) ~len ~append_bytes ~append_bigarray io_vecs buf);
  let%lwt n = Lwt_unix.readv sock io_vecs in 
  Abuf.set_w_pos (Abuf.w_pos buf + n) buf; 
  Lwt.return n

let rec read_all sock buf len = 
  if len > 0
  then 
    let%lwt n = read sock buf len in
    if n > 0 
    then 
      let%lwt n2 = read_all sock buf (len - n) in
      Lwt.return (n + n2)
    else 
      Lwt.return n
  else 
    Lwt.return 0

let write sock buf = 
  let io_vecs = Lwt_unix.IO_vectors.create () in
  Lwt_unix.IO_vectors.(Abuf.to_io_vecs ~idx:(Abuf.r_pos buf) ~len:(Abuf.readable_bytes buf) ~append_bytes ~append_bigarray io_vecs buf);
  let%lwt n = Lwt_unix.writev sock io_vecs in 
  Abuf.set_r_pos (Abuf.r_pos buf + n) buf; 
  Lwt.return n

let rec write_all sock buf = 
  if Abuf.readable_bytes buf > 0
  then 
    let%lwt n = write sock buf in
    let%lwt n2 = write_all sock buf in 
    Lwt.return (n + n2)
  else 
    Lwt.return 0

let read_vle sock =
  let open Lwt.Infix in
  let bs = Bytes.create 8 in
  let rec extract_length (v:Vle.t) (bc:int) =
    Lwt_unix.read sock bs bc 1 >>= function 
    | 0 -> Lwt.fail @@ Exception (`ClosedSession (`Msg "Peer closed the session unexpectedly"))
    | _ -> 
      match Vle.of_char @@ Bytes.get bs bc with
      | c when c <= 0x7fL -> Lwt.return (Vle.logor v (Vle.shift_left c (bc * 7)))
      | c  ->
        let x : Vle.t = Vle.shift_left (Vle.logand c  0x7fL)  (bc * 7) in
        extract_length (Vle.logor v x) (bc + 1)
  in extract_length Vle.zero 0

let write_vle sock vle =
  let buf = Abuf.create_bytes 8 in
  fast_encode_vle vle buf;
  write_all sock buf

let safe_close fd =
  Lwt.catch
    (fun () -> 
      let%lwt _ = Logs_lwt.debug (fun m -> m "Closing socket...") in 
      Lwt_unix.close fd)
    (fun _ -> Lwt.return_unit)

let connect socket locator = 
  let saddr = match locator with 
    | Locator.Locator.UdpLocator ul -> 
      Endpoint.IpEndpoint.to_sockaddr @@ Iplocator.UdpLocator.endpoint ul   
    | Locator.Locator.TcpLocator tl -> 
      Endpoint.IpEndpoint.to_sockaddr @@ Iplocator.TcpLocator.endpoint tl
  in Lwt.Infix.(Lwt_unix.connect socket saddr >>= fun () -> Lwt.return socket)

(**
  * Like telnet ... but with ctx!
  *
  * @author Samuel Mimram
  *)

(* $Id$ *)

let () =
  Printexc.record_backtrace true

open Unix

let host = ref ""
let port = ref 443
let certificate = ref None
let password = ref None

let usage = "usage: stelnet host [-p port] [-c /path/to/certificate.p12] [-P password]"

let _ =
  Arg.parse
    [
      "-p", Arg.Int (fun i -> port := i), "\tPort";
      "-c", Arg.String (fun c -> certificate := Some c), "\tCertificate";
      "-P", Arg.String (fun p -> password := Some p), "\tCertificate password"
    ]
    (fun s -> host := s) usage;
  if !host = "" then (Printf.printf "%s\n\n" usage; exit 1);
  let ctx = SecureTransport.init SecureTransport.Client SecureTransport.Stream in
  SecureTransport.set_peer_domain_name ctx !host;
  begin
   match !certificate with
     | None -> ()
     | Some path ->
         let password = !password in
         match SecureTransport.import_p12_certificate ?password path with
           | cert::_ ->
               SecureTransport.set_certificate ctx cert
           | [] -> failwith "No certificate found in p12 file!"
  end;
  let he =
    (
      try
        gethostbyname !host
      with
        | Not_found -> failwith "Host not found"
    )
  in
  let sockaddr = ADDR_INET(he.h_addr_list.(0), !port) in
  let domain =
    match sockaddr with
      | Unix.ADDR_UNIX _ -> Unix.PF_UNIX
      | Unix.ADDR_INET(_, _) -> Unix.PF_INET
  in
  let sock =
    Unix.socket domain Unix.SOCK_STREAM 0
  in
  begin
    try
      Unix.connect sock sockaddr;
    with
      | exn -> Unix.close sock; raise exn
  end;
  Printf.eprintf "Connection established with %s:%d\n%!" !host !port;
  SecureTransport.set_connection ctx sock;
  SecureTransport.handshake ctx;
  Printf.eprintf "SSL handshake sucessful!\n%!";
  let bufsize = 1024 in
  let buf = Bytes.create bufsize in
  let loop = ref true in
    ignore
      (
        Thread.create
          (fun () ->
             let buf = Bytes.create bufsize in
               while !loop
               do
                 let r = SecureTransport.read ctx buf 0 bufsize in
                   Printf.printf "%s%!" (String.sub (Bytes.to_string buf) 0 r)
               done
          ) ()
      );
    while !loop
    do
      let r = Unix.read Unix.stdin buf 0 bufsize in
        if String.sub (Bytes.to_string buf) 0 4 = "exit" then
          loop := false;
        ignore (SecureTransport.write ctx buf 0 r);
    done;
    SecureTransport.close ctx

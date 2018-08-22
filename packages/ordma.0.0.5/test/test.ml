let sa2s = function
  |Unix.ADDR_INET(addr,port) ->
    Printf.sprintf "(%s,%i)" (Unix.string_of_inet_addr addr) port
  | _ -> Printf.sprintf "???"

external get32_prim : string -> int -> int32         = "%caml_string_get32"
external set32_prim : string -> int -> int32 -> unit = "%caml_string_set32"

let client host port =
  let () = Printf.printf "client (%s,%i)\n%!" host port in
  let addr = Unix.inet_addr_of_string host in
  let fd = Rsocket.rsocket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let () = Printf.printf "got an rsocket:%s\n%!" (Rsocket.show fd) in
  let sa = Unix.ADDR_INET (addr, port) in
  let () = Rsocket.rconnect fd sa in
  Printf.printf "connected\n%!";
  let len = 1024 in
  let rest = len - 4 in
  let bytes_out = Bytes.create len in
  let bytes_in  = Bytes.create len in
  let () = set32_prim bytes_out 0 (rest |> Int32.of_int) in
  let rec loop n =
    if n = 0
    then ()
    else
      let sent = Rsocket.rsend fd bytes_out 0 len [] in
      let () = Printf.printf "(n:%i) sent! %i\n%!" n sent in
      let received = Rsocket.rrecv fd bytes_in 0 len [] in
      Printf.printf "\treceived: %i\n%!" received;
      loop (n-1)
  in
  loop 1000

let create_buffer len =
  Bigarray.Array1.create Bigarray.Char Bigarray.C_layout len

let send_all fd ba off len flags =
  let rec loop off todo =
    let sent = Rsocket.rsend_ba fd ba off todo flags in
    if sent = todo
    then ()
    else
      let off' = off + sent
      and todo'= todo - sent
      in
      loop off' todo'
  in
  loop off len
       
let server host port =
  let () = Printf.printf "server (%s,%i)\n%!" host port in
  let addr = Unix.inet_addr_of_string host in
  let fd = Rsocket.rsocket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let () = Printf.printf "got an rsocket:%s\n%!" (Rsocket.show fd) in
  let sa = Unix.ADDR_INET (addr, port) in
  let () = Rsocket.rsetsockopt fd Unix.SO_REUSEADDR true in 
  let () = Printf.printf "set REUSEADDR true\n%!" in
  let () = Rsocket.rbind fd sa in
  let () = Printf.printf "bind ok\n%!" in
  let () = Rsocket.rlisten fd 10 in
  let () = Printf.printf "listen ok\n%!" in
  let protocol (client_fd, client_addr) =
    try
      let len = 4096 in
      let bytes = create_buffer len in
      let rec loop () =
        let received = Rsocket.rrecv_ba client_fd bytes 0 len [] in
        let () = send_all client_fd bytes 0 received [] in
        loop ()
      in
      loop ()
      with _ ->
        (* shutdown client_sock SHUTDOWN_ALL ?? *)
        Rsocket.rclose client_fd
  in
  let rec serve () = 
    let client = Rsocket.raccept fd in
    let (client_sock, _) = client in
    let () = Printf.printf "raccepted:%s\n%!" (Rsocket.show client_sock) in
    let _t = Thread.create protocol client in
    serve ()
  in
  let () = try serve () with _ -> Rsocket.rclose fd in
  ()

let () =
  if Array.length Sys.argv <> 4
  then
    Printf.printf "%s <client|server> host port\n%!" Sys.argv.(0)
  else
    let host = Sys.argv.(2) in
    let port_s = Sys.argv.(3) in
    let port = int_of_string port_s in
    match Sys.argv.(1) with
    | "client" -> client host port
    | "server" -> server host port
    | _        -> failwith "`client` or `server`"

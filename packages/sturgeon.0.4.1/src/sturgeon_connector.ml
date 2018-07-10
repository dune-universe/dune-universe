let header = "# sturgeon-connector v0.1"

let dir = Filename.concat (Filename.get_temp_dir_name ())
    (Printf.sprintf "sturgeon.%d" (Unix.getuid ()))

let debug =
  try Sys.getenv "STURGEON_DEBUG" = "1"
  with Not_found -> false

let check name =
  let path = Filename.concat dir name in
  let socket = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let addr = Unix.ADDR_UNIX path in
  try
    Unix.connect socket addr;
    Unix.close socket;
    true
  with exn ->
    if debug then
      prerr_endline (path ^ ": " ^ Printexc.to_string exn);
    Unix.close socket;
    Unix.unlink path;
    false

let list () =
  print_endline header;
  match Sys.readdir dir with
  | exception exn ->
    if debug then
      prerr_endline (dir ^ ": " ^ Printexc.to_string exn);
  | files ->
    List.iter print_endline
      (List.filter check (Array.to_list files))

let write_all buf fd sz =
  let x = ref 0 in
  while !x < sz do
    x := !x + Unix.write fd buf !x (sz - !x)
  done

let pipe path =
  let socket = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let addr = Unix.ADDR_UNIX path in
  try
    Unix.connect socket addr;
    let fd1  = socket in
    let fd1' = Unix.stdout in
    let fd2  = Unix.stdin in
    let fd2' = socket in
    let buf1 = Bytes.create 65536 in
    let buf2 = Bytes.create 65536 in
    let rec aux () =
      let fds, _, _ = Unix.select [fd1; fd2] [] [] (-1.0) in
      let b1 = List.mem fd1 fds in
      let b2 = List.mem fd2 fds in
      let sz1 = if b1 then Unix.read fd1 buf1 0 65536 else -1 in
      let sz2 = if b2 then Unix.read fd2 buf2 0 65536 else -1 in
      if sz2 > 0 then write_all buf2 fd2' sz2;
      if sz1 > 0 then write_all buf1 fd1' sz1;
      if sz1 = 0 || sz2 = 0 then
        ()
      else aux ()
    in
    aux ()
  with exn ->
    if debug then
      prerr_endline (path ^ ": " ^ Printexc.to_string exn);
    exit 1

let usage () =
  Printf.eprintf "Usage:\n%s list\n%s which <socket>\n%s pipe <socket>\n"
    Sys.argv.(0) Sys.argv.(0) Sys.argv.(0);
  exit 1

let arg =
  match Sys.argv with
  | [|_; "list"|] ->
    `List
  | [|_; "which"; socket|] ->
    `Which socket
  | [|_; "pipe"; socket|] ->
    `Pipe socket
  | _ -> usage ()

let () = match arg with
  | `List -> list ()
  | `Which name ->
    print_endline (Filename.concat dir name)
  | `Pipe name ->
    pipe (Filename.concat dir name)

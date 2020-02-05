open Alcotest

let socketpair _dom ty proto =
  let open Unix in
  let s = socket PF_INET ty proto
  and c = socket PF_INET ty proto in
  bind s (ADDR_INET (inet_addr_loopback, 0));
  let saddr = getsockname s in
  match ty with
  | SOCK_DGRAM ->
      connect c saddr;
      let caddr = getsockname c in
      connect s caddr;
      begin match select [c;s] [] [] 0. with
      | [], [], [] -> s, c
      | _ -> failwith "Unix.socketpair: unexpected data"
      end
  | SOCK_SEQPACKET
  | SOCK_STREAM ->
      listen s 1;
      set_nonblock c;
      (try connect c saddr; assert false
       with Unix_error ((EINPROGRESS | EWOULDBLOCK), _, _) -> ());
      clear_nonblock c;
      let s, caddr =
        match select [s] [] [] 0.1 with
        | [s'], [], [] when s' == s ->
            let ret = accept s in
            close s;
            ret
        | [], [], [] -> failwith "Unix.socketpair: timeout waiting for client"
        | _ -> assert false
      in
      assert begin
        match caddr with
        | ADDR_INET (addr, _) when addr = inet_addr_loopback -> true
        | _ -> false
      end;
      begin match select [] [c] [c] 0.1 with
      | [], [c'], _ when c == c' -> ()
      | [], [], [c'] when c == c' ->
          begin match getsockopt_error c with
          | Some e -> raise (Unix_error (e, "socketpair", ""))
          | None -> assert false
          end
      | [], [], [] -> failwith "Unix.socketpair: timeout waiting for server"
      | _ -> assert false
      end;
      s, c
  | SOCK_RAW ->
      invalid_arg "Unix.socketpair: SOCK_RAW not supported"

let bigstring_unix =
  let empty = char_of_int 0xdf in
  let source, drain = socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Unix.set_nonblock source;
  Unix.set_nonblock drain;
  let check_read ?(shift=0) ?off ?len () =
    let off' = match off with None -> 0 | Some off -> off in
    let buf = Bigstring.make 50 empty in
    let pre = Bigstring.sub buf 0 (10+off') in
    let sub = Bigstring.sub buf 10 30 in
    let check_buf read_len =
      Gc.compact ();
      let read_sub = Bigstring.sub sub off' read_len in
      let post = Bigstring.sub buf (10+off'+read_len) (40 - off' - read_len) in
      Bigstring.iter (check char "pre untouched" empty) pre;
      Bigstring.iteri (fun i c -> check char "correct read" (char_of_int (i + 97 + shift)) c) read_sub;
      Bigstring.iter (check char "post untouched" empty) post;
    in
    match Bigstring_unix.read source sub ?off ?len with
    | exception (Unix.Unix_error(Unix.EAGAIN, _, _) as e) ->
      check_buf 0;
      raise e
    | exception (Unix.Unix_error(Unix.EWOULDBLOCK, _, _) as e) ->
      check_buf 0;
      raise e
    | read_len ->
      check bool "sensible read size" true (read_len <= 30 - off' && read_len >= 0);
      check_buf read_len;
      read_len
  in
  let write ?off ?len () =
    let buf = Bytes.create 1024 in
    while try Unix.read source buf 0 (Bytes.length buf) |> ignore; true with _ -> false do () done;
    let count = Bigstring.init 20 (fun i -> char_of_int (i+97)) in
    Bigstring_unix.write drain count ?off ?len
    |> match len with None -> ignore |Some len -> check int "write length" len
  in
  "Bigstring_unix",
  [ "read", `Quick, begin fun () ->
      write ~len:15 (); check_read () |> check int "read length" 15;
      write ~len:15 (); check_read ~off:22 () |> check int "read length" 8;
      write ~len:15 (); check_read ~len:6 () |> check int "read length" 6;
      write ~len:15 (); check_read ~off:3 ~len:6 () |> check int "read length" 6;
      write ~len:7 (); check_read () |> check int "read length" 7;
    end
  ; "write", `Quick, begin fun () ->
      write (); check_read () |> check int "read length" 20;
      write ~off:0 ~len:10 (); check_read () |> check int "read length" 10;
      write ~off:2 ~len:10 (); check_read ~shift:2 () |> check int "read length" 10;
      write ~off:2 ~len:8 (); check_read ~shift:2 () |> check int "read length" 8;
      write ~off:2 (); check_read ~shift:2 () |> check int "read length" 18;
    end
  ; "bounds check", `Quick, begin fun () ->
      let buf = Bigstring.create 0 in
      check_raises "bounds -off" (Invalid_argument "Bigstring_unix.read")
      (fun () -> ignore @@ Bigstring_unix.read source buf ~off:~-1);
      check_raises "bounds -len" (Invalid_argument "Bigstring_unix.read")
      (fun () -> ignore @@ Bigstring_unix.read source buf ~len:~-1);
      check_raises "bounds off >" (Invalid_argument "Bigstring_unix.read")
      (fun () -> ignore @@ Bigstring_unix.read source buf ~off:21);
      check_raises "bounds len >" (Invalid_argument "Bigstring_unix.read")
      (fun () -> ignore @@ Bigstring_unix.read source buf ~len:21);
      check_raises "bounds off len >" (Invalid_argument "Bigstring_unix.read")
      (fun () -> ignore @@ Bigstring_unix.read source buf ~off:15 ~len:6);
      check_raises "bounds -off" (Invalid_argument "Bigstring_unix.write")
      (fun () -> write ~off:~-1 ());
      check_raises "bounds -len" (Invalid_argument "Bigstring_unix.write")
      (fun () -> write ~len:~-1 ());
      check_raises "bounds off >" (Invalid_argument "Bigstring_unix.write")
      (fun () -> write ~off:21 ());
      check_raises "bounds len >" (Invalid_argument "Bigstring_unix.write")
      (fun () -> write ~len:21 ());
      check_raises "bounds off len >" (Invalid_argument "Bigstring_unix.write")
      (fun () -> write ~off:15 ~len:6 ());
    end
  ]

let () =
  run "Bigstring"
  [ bigstring_unix ]

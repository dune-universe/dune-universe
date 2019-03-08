module U = Unix

type dst_info = INET of string * int | UNIX of string

type t = {
  dst_info:dst_info;
  conn_timeout:int;
  (* TODO: change these ones to immutable *)
  mutable socket:U.file_descr option;
  mutable last_conn_err_time:int option;
  mutable conn_err_count:int;
  dummy_one_byte:bytes;
}

let create dst_info conn_timeout =
  {
    dst_info=dst_info;
    conn_timeout=conn_timeout;
    socket=None;
    (* TODO: extract this and conn_err_count to a module *)
    last_conn_err_time=None;
    conn_err_count=0;
    dummy_one_byte=Bytes.create 1
  }

let print_unix_error label e fn param =
  Printf.eprintf "%s: [%s] [%s] [%s]\n" label (U.error_message e) fn param

let connect t = 
  let s =
    U.socket
      (match t.dst_info with INET _ -> U.PF_INET | UNIX _ -> U.PF_UNIX)
      U.SOCK_STREAM 0 in
  let conn_success () =
    t.last_conn_err_time <- None;
    t.conn_err_count <- 0;
    t.socket <- Some s in
  let conn_failed () =
    t.last_conn_err_time <- Some (int_of_float (U.time ()));
    t.conn_err_count <- t.conn_err_count + 1;
    t.socket <- None in
  match t.dst_info with
  | INET(host, port) -> (
    U.setsockopt s U.TCP_NODELAY true;
    U.set_close_on_exec s;
    let server_addresses =
      Array.to_list((U.gethostbyname host).U.h_addr_list) in
    try (
      ignore (
        List.find (
          fun addr ->
            try (
              U.connect s (U.ADDR_INET(addr, port));
              true
            )
            with U.Unix_error (e, fn, p) -> (
              print_unix_error "connect error" e fn p;
              false
            )
        ) server_addresses
      );
      conn_success ()
    )
    with Not_found -> conn_failed ()
  )
  | UNIX(path) -> 
    try (
      U.connect s (U.ADDR_UNIX(path));
      conn_success ()
    )
    with U.Unix_error (e, fn, p) -> (
      print_unix_error "connect error" e fn p;
      conn_failed ()
    )

let connect_if_needed t =
  match t.socket with
  | Some _ -> ()
  | None -> (
    match t.last_conn_err_time with
    | Some tm -> (
        let now = int_of_float (U.time ()) in
        let interval =
          int_of_float (2.0 ** (float_of_int t.conn_err_count)) in
        let max_conn_retain_interval = 60 in
        let interval =
          if interval < max_conn_retain_interval then interval
          else max_conn_retain_interval in
        if tm < now - interval then connect t
    )
    | None -> connect t
  )

let close t =
  match t.socket with
  | Some s -> (
      try U.close s
      with U.Unix_error (e, fn, p) ->
        print_unix_error "close error" e fn p
    );
    t.socket <- None
  | None -> ()

let update_conn t =
  match t.socket with
  | Some s -> (
    U.set_nonblock s;
    let connected =
      try U.read s t.dummy_one_byte 0 1 != 0
      with 
      | U.Unix_error (U.EAGAIN, _, _) -> true
      | _ -> false
    in (
      U.clear_nonblock s;
      if not connected then close t
    )
  )
  | None -> ()

let write t buf start offset =
  update_conn t;
  connect_if_needed t;
  match t.socket with
  | Some s -> (
    try Some (U.single_write s buf start offset)
    with U.Unix_error (e, fn, p) -> (
      print_unix_error "write error" e fn p;
      close t;
      None
    )
  )
  | None -> (
    prerr_endline "write error: not connected";
    close t;
    None
  )

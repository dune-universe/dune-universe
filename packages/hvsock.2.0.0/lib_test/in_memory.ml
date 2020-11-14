type sockaddr = unit

type queues =
  { read: Cstruct.t list ref
  ; write: Cstruct.t list ref
  ; shutdown_read: bool ref
  ; shutdown_write: bool ref }

type listening = {mutable connecting: t list; sockaddr: sockaddr}

and state =
  | Connected of queues
  | Listening of listening
  | Created
  | Bound of sockaddr
  | Closed

and t = {mutable state: state; m: Mutex.t; c: Condition.t}

let string_of_sockaddr () = "()"

let create () = {state= Created; m= Mutex.create (); c= Condition.create ()}

let bound_sockets = Hashtbl.create 7

let with_lock m f =
  Mutex.lock m ;
  try
    let r = f () in
    Mutex.unlock m ; r
  with e -> Mutex.unlock m ; raise e

let bind t sockaddr =
  with_lock t.m (fun () ->
      match t.state with
      | Created ->
          if Hashtbl.mem bound_sockets sockaddr then
            raise (Unix.Unix_error (Unix.EADDRINUSE, "bind", "")) ;
          Hashtbl.replace bound_sockets sockaddr t ;
          t.state <- Bound sockaddr
      | _ -> raise (Unix.Unix_error (Unix.EINVAL, "accept", "")) )

let listen t _ =
  with_lock t.m (fun () ->
      match t.state with
      | Bound sockaddr -> t.state <- Listening {connecting= []; sockaddr}
      | _ -> raise (Unix.Unix_error (Unix.EINVAL, "listen", "")) )

let accept t =
  with_lock t.m (fun () ->
      match t.state with
      | Listening l ->
          let rec wait () =
            match l.connecting with
            | [] -> Condition.wait t.c t.m ; wait ()
            | x :: xs ->
                l.connecting <- xs ;
                let qs =
                  { read= ref []
                  ; write= ref []
                  ; shutdown_read= ref false
                  ; shutdown_write= ref false }
                in
                with_lock x.m (fun () ->
                    x.state <- Connected qs ;
                    Condition.broadcast x.c ) ;
                let y =
                  { x with
                    state=
                      Connected
                        { read= qs.write
                        ; write= qs.read
                        ; shutdown_read= qs.shutdown_write
                        ; shutdown_write= qs.shutdown_read } }
                in
                (y, ())
          in
          let result = wait () in
          result
      | _ -> raise (Unix.Unix_error (Unix.EINVAL, "accept", "")) )

let connect ?timeout_ms:_ t sockaddr =
  with_lock t.m (fun () ->
      if Hashtbl.mem bound_sockets sockaddr then (
        let listening = Hashtbl.find bound_sockets sockaddr in
        with_lock listening.m (fun () ->
            match listening.state with
            | Listening l ->
                l.connecting <- t :: l.connecting ;
                Condition.signal listening.c
            | _ -> assert false ) ;
        let rec wait () =
          match t.state with
          | Connected _ -> ()
          | _ -> Condition.wait t.c t.m ; wait ()
        in
        wait () )
      else raise (Unix.Unix_error (Unix.ECONNREFUSED, "connect", "")) )

let writev t bufs =
  with_lock t.m (fun () ->
      match t.state with
      | Connected qs ->
          if !(qs.shutdown_write) then
            raise (Unix.Unix_error (Unix.EPIPE, "writev", "")) ;
          qs.write := !(qs.write) @ bufs ;
          Condition.signal t.c ;
          List.fold_left ( + ) 0 (List.map Cstruct.len bufs)
      | _ -> raise (Unix.Unix_error (Unix.ENOTCONN, "writev", "")) )

let read_into t buf =
  with_lock t.m (fun () ->
      let rec read () =
        match t.state with
        | Connected qs -> (
          match !(qs.read) with
          | [] ->
              if !(qs.shutdown_read) then 0
              else ( Condition.wait t.c t.m ; read () )
          | b :: bs ->
              let buf_len = Cstruct.len buf in
              let b_len = Cstruct.len b in
              let to_read = min b_len buf_len in
              Cstruct.blit b 0 buf 0 to_read ;
              if buf_len >= b_len then qs.read := bs
              else qs.read := Cstruct.shift b to_read :: bs ;
              to_read )
        | Closed -> 0
        | _ -> raise (Unix.Unix_error (Unix.ENOTCONN, "read_into", ""))
      in
      read () )

let shutdown_read _t = ()

(* ignored for now *)

let shutdown_write t =
  with_lock t.m (fun () ->
      match t.state with
      | Connected qs ->
          qs.shutdown_write := true ;
          Condition.broadcast t.c
      | _ -> raise (Unix.Unix_error (Unix.EINVAL, "shutdown_write", "")) )

let close t =
  with_lock t.m (fun () ->
      match t.state with
      | Connected _ ->
          t.state <- Closed ;
          Condition.broadcast t.c
      | Bound sockaddr -> Hashtbl.remove bound_sockets sockaddr
      | Listening {sockaddr; _} -> Hashtbl.remove bound_sockets sockaddr
      | _ -> raise (Unix.Unix_error (Unix.EINVAL, "close", "")) )

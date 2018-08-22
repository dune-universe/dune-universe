let _log fmt =
  (*let k s =
    let t0 = Unix.gettimeofday () in
    Printf.eprintf "%f %s\n%!" t0 s
  in*)
  let k s = () in
  Printf.ksprintf k fmt
                     
type lwt_rsocket = Lwt_unix.file_descr

let unix_fd_of lwt_rsocket = Lwt_unix.unix_file_descr lwt_rsocket
                                                      
let rsocket_of lwt_rsocket =
  let u = unix_fd_of lwt_rsocket in
  let (r : Rsocket.rsocket) = Obj.magic u  in
  r

let identifier lwt_rsocket =
  let u = unix_fd_of lwt_rsocket in
  let (i:int) = Obj.magic u in
  i
    
let wrap rsocket =
  let (unix_fd : Unix.file_descr) = Obj.magic rsocket in
  let lwt_fd = Lwt_unix.of_unix_file_descr unix_fd ~blocking:false in
  (lwt_fd : lwt_rsocket)
    
let socket domain typ proto =
  let rsocket = Rsocket.rsocket domain typ proto in
  let () = Rsocket.set_nonblock rsocket in
  wrap rsocket

  
let show lwt_rsocket =
  let rs = rsocket_of lwt_rsocket in
  Rsocket.show rs

open Lwt.Infix

let string_of_address = function
  | Unix.ADDR_INET(addr, port) ->
     Printf.sprintf
       "ADDR_INET(%s,%i)"
       (Unix.string_of_inet_addr addr) port
  | Unix.ADDR_UNIX s -> Printf.sprintf "ADDR_UNIX %s" s
                                       


type state = Lwt_unix.state = Opened | Closed | Aborted of exn 


(* this is a copy of Lwt_unix.file_descr *)
type rfile_descr = (* Lwt_unix.file_descr = *) {
  fd : Unix.file_descr;

  mutable state: state;
  mutable set_flags : bool;
  mutable blocking : bool Lwt.t Lazy.t;
  mutable event_readable : Lwt_engine.event option;
  mutable event_writable : Lwt_engine.event option;
  hooks_readable : (unit -> unit) Lwt_sequence.t;
  hooks_writable : (unit -> unit) Lwt_sequence.t;
  }

                                                 
let set_state rch state =
    rch.state <- state

let _rch2ch (rch: rfile_descr) =
  let (ch : Lwt_unix.file_descr) = Obj.magic rch in
  ch

let _ch2rch (ch: Lwt_unix.file_descr) =
  let (rch : rfile_descr) = Obj.magic ch in
  rch
    
let rec _check_descriptor rch =
  let id = identifier (_rch2ch rch) in 
  let () = _log "(%i) check_descriptor" id in
  match rch.state with
  | Opened ->
     let () = _log "(%i) check_descriptor => Opened" id in
     ()
  | Aborted e ->
        raise e
  | Closed ->
     raise (Unix.Unix_error (Unix.EBADF, "check_descriptor", ""))

let clear_events rch =
  Lwt_sequence.iter_node_l (fun node -> Lwt_sequence.remove node; Lwt_sequence.get node ()) rch.hooks_readable;
  Lwt_sequence.iter_node_l (fun node -> Lwt_sequence.remove node; Lwt_sequence.get node ()) rch.hooks_writable;
  begin
    match rch.event_readable with
      | Some ev ->
         rch.event_readable <- None;
         Lwt_engine.stop_event ev
      | None ->
          ()
  end;
  begin
    match rch.event_writable with
      | Some ev ->
         rch.event_writable <- None;
         Lwt_engine.stop_event ev
      | None ->
         ()
  end
 
let close lwt_rsocket =
  let (rch : rfile_descr) = Obj.magic lwt_rsocket in
  if rch.state = Closed then _check_descriptor rch;
  set_state rch Closed;
  clear_events rch;
  (* TODO: via job *)
  Lwt.return (Rsocket.rclose (rsocket_of lwt_rsocket))

let bind lwt_rsocket socketaddr =
  let rsocket = rsocket_of lwt_rsocket in
  Rsocket.rbind rsocket socketaddr

let listen lwt_rsocket n =
  let rsocket = rsocket_of lwt_rsocket in
  Rsocket.rlisten rsocket n
                                       
let setsockopt lwt_rsocket sockopt value =
  let rsocket = rsocket_of lwt_rsocket in
  Rsocket.rsetsockopt rsocket sockopt value


      



external _ordma_lwt_unix_recv :
  Unix.file_descr ->
  Bytes.t -> int -> int -> Unix.msg_flag list -> int =
  "ordma_lwt_unix_recv"

external _ordma_lwt_unix_send :
  Unix.file_descr -> Bytes.t -> int -> int -> Unix.msg_flag list -> int =
  "ordma_lwt_unix_send"


external _unix_stub_readable :
  Unix.file_descr -> bool =
  "ordma_lwt_unix_readable"

external _unix_stub_writable :
  Unix.file_descr -> bool =
  "ordma_lwt_unix_writable"


    
let rec _unix_readable fd =
  try
    _unix_stub_readable fd
  with Unix.Unix_error (Unix.EINTR, _, _) ->
    _unix_readable fd
                   
let _readable (rch: rfile_descr) =
  (* from Lwt_unix.ml *)
  _check_descriptor rch;
  _unix_readable rch.fd

let rec _unix_writable fd =
  try
    _unix_stub_writable fd
  with Unix.Unix_error (Unix.EINTR, _, _) ->
    _unix_writable fd

let _writable rch =
  _check_descriptor rch;
  _unix_writable rch.fd
                
let _wait_read (rch: rfile_descr) = 
  (* from Lwt_unix.ml *)
  Lwt.catch
    (fun () ->
      if _readable rch
      then Lwt.return_unit
      else
        let ch = _rch2ch rch in
        Lwt_unix.register_action Lwt_unix.Read ch ignore)
    Lwt.fail

let _wait_write (rch:rfile_descr) =
  Lwt.catch
    (fun () ->
      if _writable rch
      then Lwt.return_unit
      else
        let ch = _rch2ch rch in
        Lwt_unix.register_action Lwt_unix.Write ch ignore)
    Lwt.fail
    
(* Wraps a system call *)
let _wrap_syscall event rch action =
  _check_descriptor rch;
  Lazy.force rch.blocking >>= fun blocking ->
  let ch = _rch2ch rch in
  let id = identifier ch in
  try
    if not blocking
       || (event = Lwt_unix.Read  && _unix_readable rch.fd)
       || (event = Lwt_unix.Write && _unix_writable rch.fd)
    then
      let () = _log "(%i) immediate action" id in
      let r = action () in
      let () = _log "(%i) immediate action done" id
      in
      Lwt.return r
    else
      let () = _log "(%i) register action" id in
      Lwt_unix.register_action event ch action
  with     
    | Lwt_unix.Retry
    | Unix.Unix_error((Unix.EAGAIN | Unix.EWOULDBLOCK | Unix.EINTR), _, _)
    | Sys_blocked_io as e ->
       let () = _log "(%i) %s => register action anyway" id (Printexc.to_string e) in
        (* The action could not be completed immediately, register it: *)
        Lwt_unix.register_action event ch action
    | Lwt_unix.Retry_read ->
        Lwt_unix.register_action Lwt_unix.Read ch action
    | Lwt_unix.Retry_write ->
        Lwt_unix.register_action Lwt_unix.Write ch action
    | e ->
        Lwt.fail e

let connect lwt_rsocket socketaddr =
  (* from Lwt_unix.ml : *)
  let in_progress = ref false in
  let rsocket = rsocket_of lwt_rsocket in
  _wrap_syscall
    Lwt_unix.Write
    (_ch2rch lwt_rsocket)
    begin fun () ->
    if !in_progress
    then
      (* If the connection is in progress, [getsockopt_error] tells
           wether it succceed: *)
      match Rsocket.rgetsockopt_error rsocket with
      | None ->
         (* The socket is connected *)
         ()
      | Some err ->
         (* An error happened: *)
         raise (Unix.Unix_error(err, "connect", ""))
    else
      try
        (* We should pass only one time here, unless the system call
             is interrupted by a signal: *)
        Rsocket.rconnect rsocket socketaddr
      with
      | Unix.Unix_error (Unix.EINPROGRESS, _, _) ->
         in_progress := true;
         raise Lwt_unix.Retry
    end

let send lwt_rsocket buffer offset len flags =
  if offset < 0 || len < 0 || offset > Bytes.length buffer - len then
    invalid_arg "Lwt_unix.send"
  else
    _wrap_syscall
      Lwt_unix.Write
      (_ch2rch lwt_rsocket)
      (fun () ->
        let rsocket = rsocket_of lwt_rsocket in
        Rsocket.rsend rsocket buffer offset len flags)


let recv (lwt_rsocket: lwt_rsocket) buffer offset len flags =
  let unix_fd = unix_fd_of lwt_rsocket in
  if offset < 0
     || len < 0
     || offset > Bytes.length buffer - len
  then
    invalid_arg "Lwt_socket.recv"
  else
    _wrap_syscall
      Lwt_unix.Read
      (_ch2rch lwt_rsocket)
      (fun () ->
        let id = identifier lwt_rsocket in
        let () = _log "(%i) inside recv action" id in
        _ordma_lwt_unix_recv unix_fd buffer offset len flags
      )
    >>= fun r ->
    (* TODO: This is a symptom of something else? *)
    if r = 0
    then Lwt.fail End_of_file
    else Lwt.return r



    
let accept lwt_rsocket =
  (* 
     (* from Lwt_unix.ml :  *)
     let accept ch =
     wrap_syscall Read ch (fun _ -> let (fd, addr) = 
     Unix.accept ch.fd in (mk_ch ~blocking:false fd, addr)) 
   *)

  (*Lwt_preemptive.detach
    (fun () ->
      let rsocket = rsocket_of lwt_rsocket in
      let client_s,client_addr = Rsocket.raccept rsocket in
      let client_rs = wrap client_s in
      client_rs,client_addr
    )
    ()
   *)
  let inner () =
    
    Lwt.catch
      (fun () ->
        (*
        _wait_read lwt_rsocket >>= fun () ->
        let id = identifier lwt_rsocket in
        Lwt_io.printlf "(%i) ready for read" id >>= fun () ->

        let rsocket = rsocket_of lwt_rsocket in
        let client_s,client_addr = Rsocket.raccept rsocket in
        let client_rs = wrap client_s in
        let r =  client_rs,client_addr in
        Lwt.return (Some r)
         *)
      _wrap_syscall
        Lwt_unix.Read
        (_ch2rch lwt_rsocket)
        (fun () -> 
          let (fd, addr) = Rsocket.raccept (rsocket_of lwt_rsocket) in
          let () = Rsocket.set_nonblock fd in
          Some (wrap fd, addr)
        ) 
      )
      (fun exn ->
        Lwt_log.warning ~exn "failed raccept" >>= fun () ->
        Lwt.return_none)
  in
  let rec loop i =
    inner () >>= function
    | None ->
       (* Todo: don't ignore these symptoms *)
       if i > 1000
       then
         begin
           Lwt_log.fatal "Lwt_rsocket.accept : too many things went wrong" >>= fun ()->
           Lwt.return (assert false)
         end
       else
         Lwt_unix.sleep 0.1 >>= fun () ->
         Lwt_log.info_f "Lwt_rsocket.accept: i = %i" i >>= fun () ->
         loop (i+1)
    | Some r -> Lwt.return r
  in
  loop 0
  
  (*
  Lwt_unix.wait_read lwt_rsocket >>= fun () ->
  let rsocket = rsocket_of lwt_rsocket in
  let client_s,client_addr = Rsocket.raccept rsocket in
  let client_rs = wrap client_s in
  let r =  client_rs,client_addr in
  Lwt.return r
   *)
   
    
open Bigarray

type ba = (char, int8_unsigned_elt, c_layout) Array1.t
                                              
external _ordma_lwt_unix_bytes_recv:
  Unix.file_descr ->
  ba -> int -> int -> Unix.msg_flag list -> int =
  "ordma_lwt_unix_bytes_recv"

external _ordma_lwt_unix_bytes_send :
  Unix.file_descr -> ba -> int -> int -> Unix.msg_flag list -> int =
  "ordma_lwt_unix_bytes_send"

module Bytes = struct
  
  type t = ba

  let create len =
    Array1.create Char C_layout len
                           
  let send lwt_rsocket t  pos len flags =
    if pos < 0
       || len < 0
       || pos > Array1.dim t - len
    then
    invalid_arg "send"
    else
      (*let id = identifier lwt_rsocket in*)
      let unix_fd = unix_fd_of lwt_rsocket in
      _wrap_syscall
        Lwt_unix.Write
        (_ch2rch lwt_rsocket)
        (fun () -> _ordma_lwt_unix_bytes_send unix_fd t pos len flags)

  let recv lwt_rsocket buffer offset len flags =
    
    if offset < 0
       || len < 0
       || offset > Array1.dim buffer - len
    then
      invalid_arg "Lwt_rsocket.recv"
    else
      let id = identifier lwt_rsocket in
      let unix_fd = unix_fd_of lwt_rsocket in
      let () = _log "(%i) Bytes.recv _ %i %i\n%!" id offset len in
      _wrap_syscall
        Lwt_unix.Read
        (_ch2rch lwt_rsocket)
        (fun () -> _ordma_lwt_unix_bytes_recv unix_fd buffer offset len flags)
      >>= fun r ->
      if r = 0
      then Lwt.fail End_of_file
      else Lwt.return r
 
end

open Lwt_engine


external _rselect_select :
  Unix.file_descr list ->
  Unix.file_descr list ->
  Unix.file_descr list ->
  float ->
  Unix.file_descr list * Unix.file_descr list = "ordma_rselect_select"

let fds2s fds =
  let i_of fd : int = Obj.magic fd in
  let s_of fd = i_of fd |> string_of_int in
  String.concat ";" (List.map s_of fds)

(* rselect is good enough as it ends up 
   into rpoll anyway *)
class rselect = object
  inherit select_based
  method private 
      select : 
        Unix.file_descr list ->
        Unix.file_descr list ->
        float -> Unix.file_descr list * Unix.file_descr list
    = fun fds_r fds_w timeout ->
    (*let () = _log "rselect.select: ([%s],[%s],%f) begin" (fds2s fds_r) (fds2s fds_w) timeout
    in*)
    let r = _rselect_select fds_r fds_w [] timeout in
    (*
    let rr,rw = r in
    let () = _log "rselect.select => ([%s],[%s]) end" (fds2s rr) (fds2s rw)
    in *)
    
    r
end

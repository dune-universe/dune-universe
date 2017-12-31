open Core
open Import

module File_descr = Core.Unix.File_descr

type t = File_descr.t [@@deriving bin_io, sexp]

include (File_descr : Stringable with type t := t)

let create_fd core_fd =
  Fd.create Fifo core_fd (Info.of_string "dynamic-port-writer")
;;

let create () =
  let module Unix = Core.Unix in
  In_thread.syscall_exn ~name:"Dynamic_port_writer: pipe" (fun () -> Unix.pipe ())
  >>= fun (r, w) ->
  (* We only need the read-end of the pipe in the parent. *)
  In_thread.syscall_exn ~name:"Dynamic_port_writer: set_close_on_exec"
    (fun () -> Unix.set_close_on_exec r)
  >>= fun () ->
  (* Make sure the write-end of the pipe is available after the child execs, so the
     child can use it to send the port number back to the parent. *)
  In_thread.syscall_exn ~name:"Dynamic_port_writer: clear_close_on_exec"
    (fun () -> Unix.clear_close_on_exec w)
  >>= fun () ->
  let read_port =
    let reader = Reader.create (create_fd r) in
    Reader.with_close reader ~f:(fun () -> Reader.read_sexp reader)
    >>= fun result ->
    In_thread.syscall_exn ~name:"Dynamic_port_writer: close"
      (fun () -> Core.Unix.close w)
    >>| fun () ->
    match result with
    | `Ok sexp -> Ok (`Port (Int.t_of_sexp sexp))
    | `Eof -> Or_error.error_string "Dynamic_port_writer: failed while reading port"
  in
  return (w, read_port)
;;

let where_to_listen t =
  let w = Writer.create (create_fd t) in
  Tcp.Where_to_listen.create
    ~socket_type:Socket.Type.tcp
    ~address:(Socket.Address.Inet.create_bind_any ~port:0)
    ~listening_on:(fun (`Inet (_, port)) ->
      Writer.write_sexp w (Int.sexp_of_t port);
      Writer.newline w;
      don't_wait_for (Writer.close w);
      port)
;;

let arg = Command.Spec.Arg_type.create of_string

let flag_name = "-dynamic-port-writer"

let flag_args t = [ flag_name; to_string t ]

let flag =
  Command.Spec.(
    flag flag_name (required arg)
      ~doc:("DYNAMIC_PORT_WRITER to communicate a port to a parent process"))
;;

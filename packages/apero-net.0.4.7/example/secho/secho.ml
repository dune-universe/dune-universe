open Apero_net
open Cmdliner 
open NetService

(* Make the service with the TcpService functor *)
module EchoService = NetServiceTcp
module Config = EchoService.TcpConfig

(* The first step is to define the message that will be used to communicate 
   between TcpService and the service implementation *)
module Message = struct
  type message = Msg of string | Close | Stop | Nothing
  let of_string s = Msg s 
end

(* Create a configuration *)
let config = (Config.make (Apero.Option.get @@ TcpLocator.of_string "tcp/0.0.0.0:9999"))

open Apero 


    (* type reader = Lwt_unix.file_descr -> unit -> ((IOBuf.t, error) Result.t) Lwt.t
    type dispatcher = IOBuf.t -> (IOBuf.t list) Lwt.t *)

(* Define the reader. Notice that if you want to keep state, can implement a pattern similar to the
   state monad using. The service will call [reader sock] each time a new session is established
   to allow creating any session specific state may be necessary. *)
let reader sock = 
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input sock in
  fun () -> 
    let%lwt l = Lwt_io.read_line ic in 
    Lwt.return @@  match  l with 
    | "stop" ->  Result.ok Message.Stop
    | "close" -> Result.ok Message.Close
    | _ -> Result.ok (Message.of_string l)

(* Define the writer. Notice that if you want to keep state, can implement a pattern similar to the
   state monad using. The service will call [reader sock] each time a new session is established
   to allow creating any session specific state may be necessary. *)
let writer sock = 
  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output sock in 
  function
  | Message.Msg s -> 
    let%lwt _ = Lwt_io.write_line oc s in 
    Lwt.return_unit
  | _ -> Lwt.return_unit
  
let run_echo_service reader writer (svc: 'a EchoService.t) (sex: TxSession.t) =
  let sock = TxSession.socket sex in 
  let mreader = reader sock in 
  let mwriter = writer sock in 
  fun () -> 
    try     
      match%lwt  mreader () with 
      | Ok msg -> (match msg with 
        | Message.Msg _ as msg -> mwriter msg 
        | Message.Stop -> EchoService.stop svc 
        | Message.Close -> TxSession.close sex
          (* let e : Apero.error = `ClosedSession `NoMsg in 
          Lwt.fail @@ Exception e *)
        | _ -> Lwt.return_unit) 
      | _ -> Lwt.return_unit
    with 
    | e -> Logs_lwt.debug (fun m -> m "I/O Exception raised: %s" (Printexc.to_string e))

    

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let setup_log =
  let env = Arg.env_var "ECHO_VERBOSITY" in
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ~env ())

let () =
  let _ = Term.(eval (setup_log, Term.info "tool")) in  
  let svc = EchoService.make config  in   
  Lwt_main.run (EchoService.start svc (fun _ -> Lwt.return_unit) (run_echo_service reader writer svc)) 
  
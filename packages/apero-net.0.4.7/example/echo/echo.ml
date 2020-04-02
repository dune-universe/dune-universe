open Apero_net
open Cmdliner 


(* Make the service with the TcpService functor *)
module EchoService = TcpService.Make  (Apero.MVar_lwt)
module Config = TcpService.Config

(* The first step is to define the message that will be used to communicate 
   between TcpService and the service implementation *)
module Message = struct
  type message = Msg of string | Stop | Nothing
  let of_string s = Msg s 
end

(* Create a configuration *)
let config = (TcpService.Config.create (Apero.Option.get @@ TcpLocator.of_string "tcp/0.0.0.0:9999"))

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
    Lwt.return @@  if l = "stop" then Result.ok Message.Stop
    else Result.ok (Message.of_string l)

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
  
let run_echo_service reader writer (svc:EchoService.t) (sock : Lwt_unix.file_descr) =
  let mreader = reader sock in 
  let mwriter = writer sock in 
  fun () ->     
    match%lwt  mreader () with 
    | Ok msg -> (match msg with 
      | Message.Msg _ as msg -> mwriter msg 
      | Message.Stop -> EchoService.stop svc 
      | _ -> Lwt.return_unit) 
    | _ -> Lwt.return_unit
    

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
  let svc = EchoService.create config  in   
  Lwt_main.run (EchoService.start svc (run_echo_service reader writer svc)) 
  
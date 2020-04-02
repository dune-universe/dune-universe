open Apero_net
open Cmdliner 

(* The first step is to define the message that will be used to communicate 
   between TcpService and the service implementation *)
module Message = struct 
  type message = Msg of string | Stop | Nothing
  let of_string s = Msg s 
end

(* Make the service with the TcpService functor *)
module EchoService = TcpServiceES.Make (Message) (Apero)
module Config = TcpServiceES.Config

(* Create a configuration *)
let config = (Config.create (Apero.Option.get @@ TcpLocator.of_string "tcp/0.0.0.0:9999"))

open Apero 

(* Create the source and sink used to connect with the TcpService *)
let source,sink = EventStream.create 32

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
    Lwt.return (Result.ok ())
  | Nothing -> Lwt.return (Result.ok ())
  | Stop ->  Lwt.return (Result.ok ())


let rec run_echo_service source svc = 
  match%lwt EventStream.Source.get source with 
  | Some (EchoService.EventWithReplier {msg = msg ; sid = _ ; svc_id = _ ;  reply_to = reply_to }) -> 
    (match msg with 
      | Message.Msg _ -> let%lwt _ = reply_to msg in  run_echo_service source svc
      | Message.Stop -> 
        let%lwt _ = Logs_lwt.debug (fun m -> m "Stopping service.") in
        let%lwt _ = EchoService.stop svc in Lwt.return_unit
      | Message.Nothing -> run_echo_service source svc)
  | _ -> 
    let%lwt _ = Logs_lwt.debug (fun m -> m "Ignoring message... ") 
    in run_echo_service source svc

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
  let svc = EchoService.create config reader writer sink in   
  Lwt_main.run @@ Lwt.join [EchoService.start svc ; run_echo_service source svc]
  
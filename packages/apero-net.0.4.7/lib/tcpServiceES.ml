open Lwt 
open Apero
open Iplocator 
open Endpoint


module TcpServiceES = struct
    
  module Id = NumId.Make (Int64)

  module Config = struct 
    open Lwt_unix
    type t = 
      { locator : TcpLocator.t
      ; backlog : int
      ; stream_length : int
      ; max_connections : int
      ; socket_options : (Lwt_unix.file_descr -> unit) list
      ; svc_id : int }

    let reuseaddr reuse = fun sock -> setsockopt sock SO_REUSEADDR reuse
    let tcp_nodelay nodelay = fun sock -> setsockopt sock TCP_NODELAY nodelay
    let sndbuf size = fun sock -> setsockopt_int sock SO_SNDBUF size
    let rcvbuf size = fun sock -> setsockopt_int sock SO_SNDBUF size
    
    let create ?(backlog=10) ?(stream_length=64) ?(max_connections=8192) 
                ?(socket_options=[reuseaddr true; tcp_nodelay true]) 
                ?(svc_id=0) locator = 
      { locator
      ; backlog
      ; stream_length
      ; max_connections
      ; socket_options
      ; svc_id }

    let backlog c = c.backlog
    let locator c = c.locator 
    let stream_length c = c.stream_length    
    let socket_options c = c.socket_options
    let max_connectiosn c = c.max_connections
    let svc_id c = c.svc_id
  end

  module type S = sig     
    type t
    type message
    type error 

    type message_reader = Lwt_unix.file_descr -> unit -> (message, error) Result.t Lwt.t
    type message_writer = Lwt_unix.file_descr -> message ->  (unit, error) Result.t Lwt.t
    
    type replier = message -> unit Lwt.t    
    
    type event = 
      | EventWithReplier of { msg : message; svc_id : int; sid : Id.t; reply_to: replier }
      | Event of { msg : message; svc_id : int; sid : Id.t; reply_to: replier }
      

    type sink = event EventStream.Sink.s
    type source = event EventStream.Source.s
  
    val create : Config.t -> message_reader -> message_writer -> sink -> t
    
    val start : t -> unit Lwt.t
    val stop : t -> unit Lwt.t
    
    val socket : t -> Lwt_unix.file_descr    
  
  end 
  
  module Make (M : sig type message end)
              (E: sig type error  val show_error : error -> string end)  = struct 
    type message = M.message
    type error = E.error

    type message_reader = Lwt_unix.file_descr -> unit -> (message, error) Result.t Lwt.t
    type message_writer = Lwt_unix.file_descr -> message ->  (unit, error) Result.t Lwt.t
  
    type replier = message -> unit Lwt.t    
    
    type event = 
      | EventWithReplier of { msg : message; svc_id : int; sid : Id.t; reply_to: replier }
      | Event of { msg : message; svc_id : int; sid : Id.t; reply_to: replier }
      

    type sink = event EventStream.Sink.s
    type source = event EventStream.Source.s
    
    module ConnectionMap = Map.Make(Id)

    
    type t = {
        socket : Lwt_unix.file_descr
      ; mreader : message_reader
      ; mwriter : message_writer
      ; waiter : unit Lwt.t
      ; notifier : unit Lwt.u
      ; out_sink : sink      
      ; in_source : source
      ; in_sink : sink
      ; max_connections : Int64.t 
      ; connections_count : Int64.t Lwt_mvar.t
      ; connections : (Lwt_unix.file_descr ConnectionMap.t) Lwt_mvar.t
      ; svc_id : int
      ; stream_length : int }

    let create_server_socket config = 
      let open Lwt_unix in      
      let sock = socket PF_INET SOCK_STREAM 0 in      
      (Config.socket_options config) |> List.iter (fun setopt -> setopt sock ;) ;      
      let saddr = IpEndpoint.to_sockaddr @@ TcpLocator.endpoint (Config.locator config) in
      let _ = bind sock saddr in      
      let _ = listen sock (Config.backlog config) in sock
    
    
    let register_connection svc sock = 
      match%lwt  Lwt_mvar.take svc.connections_count with 
      | count when count < svc.max_connections ->
        let sid = Id.next_id () in 
        let%lwt connections = Lwt_mvar.take svc.connections in
        let%lwt _ = Lwt_mvar.put svc.connections (ConnectionMap.add sid sock connections) in 
        let%lwt _= Lwt_mvar.put svc.connections_count (Int64.add count Int64.one) in
        Lwt.return @@  Result.ok sid 
      | _ -> Lwt.return @@ Result.fail (`ResourceLimitViolation (`Msg "Too many connections"))
        
    let unregister_connection svc sid =       
      let%lwt connection_count = Lwt_mvar.take svc.connections_count in 
      let%lwt connections = Lwt_mvar.take svc.connections in 
      match ConnectionMap.find_opt sid connections with
      | Some sock ->               
        let%lwt _ = Lwt_mvar.put svc.connections (ConnectionMap.remove sid connections) in 
        let%lwt _ = Lwt_mvar.put svc.connections_count (Int64.sub connection_count Int64.one) in
        let%lwt _ = Net.safe_close sock in
        Lwt.return @@ Result.ok ()
      | None -> Lwt.return @@ Result.fail  @@ `InvalidSession  (`Msg "Unknown session id")

    let close_sessions svc =       
      let%lwt _ = Lwt_mvar.take svc.connections_count in 
      let%lwt _ = Lwt_mvar.put svc.connections_count 0L in 
      let%lwt connections = Lwt_mvar.take svc.connections in 
      let%lwt _ = Lwt_mvar.put svc.connections ConnectionMap.empty in 
      Lwt.join @@ ConnectionMap.fold (fun _ sock xs -> (Net.safe_close sock)::xs) connections []
      
    let serve_connection (sock:Lwt_unix.file_descr) (svc:t) (sid: Id.t) =       
      let%lwt _ = Logs_lwt.debug (fun m -> m "Serving session with Id: %s" (Id.to_string sid)) in 
      let mreader = svc.mreader sock in 
      let mwriter = svc.mwriter sock in 
      let sink = svc.out_sink in
      let svc_id = svc.svc_id in 
      let (csource, csink) = EventStream.create svc.stream_length in
      let reply_to = fun m -> EventStream.Sink.push m csink in 
      
      let rec receive_loop () = 
        match%lwt mreader () with 
        | Ok msg -> 
          EventStream.Sink.push (EventWithReplier { msg ; svc_id ; sid ; reply_to }) sink
          >>= receive_loop

        | Error e ->  
          let%lwt _ = Logs_lwt.warn (fun m -> m "Error while reading message:") in           
          let%lwt _ = Logs_lwt.warn (fun m -> m  "%s" (E.show_error e)) in           
          (** TODO: Should check and log for error *)
          let%lwt _ = unregister_connection svc sid in 
          Lwt.return_unit
      in 

      let rec send_loop () =
        match%lwt EventStream.Source.get csource with 
        | Some msg -> 
          (match%lwt mwriter msg with 
          | Ok _ ->  send_loop ()
          | Error e -> 
            let%lwt _ = Logs_lwt.warn (fun m -> m "Error while writing message:") in           
            let%lwt _ = Logs_lwt.warn (fun m -> m  "%s" (E.show_error e)) in
            let%lwt _ = unregister_connection svc sid in Lwt.return_unit)
        
        | None -> send_loop ()
      in 

      Lwt.catch (fun () -> Lwt.pick [ send_loop () ; receive_loop ()])
        (function 
          | Lwt.Canceled -> 
            let%lwt _ = unregister_connection svc sid in Lwt.return_unit
          | exn -> 
            let%lwt _ = unregister_connection svc sid in 
            Logs_lwt.warn (fun m -> m "Closing session %s because of: %s " (Id.to_string sid) (Printexc.to_string exn)))    
          
    let create config reader writer out_sink = 
      let socket = create_server_socket config in 
      let (waiter, notifier) = Lwt.wait () in 
      let (in_source, in_sink) = EventStream.create (Config.stream_length config) in 
      { socket
      ; mreader = reader
      ; mwriter = writer
      ; waiter
      ; notifier
      ; out_sink       
      ; in_sink 
      ; in_source 
      ; max_connections = Int64.of_int (Config.max_connectiosn config)
      ; connections_count = Lwt_mvar.create Int64.zero
      ; connections = Lwt_mvar.create (ConnectionMap.empty) 
      ; svc_id = Config.svc_id config 
      ; stream_length = Config.stream_length config }

    let start svc = 
      let%lwt _ = Logs_lwt.debug (fun m -> m "Starting TcpService with svc-id %d " svc.svc_id) in 
      let stop = svc.waiter >|= fun () -> `Stop in 
      let rec accept_connection svc =         
      Lwt.try_bind 
        (fun () ->
          let%lwt _ = Logs_lwt.debug (fun m -> m "TcpService ready to accept connection" ) in   
          let accept = Lwt_unix.accept svc.socket >|= (fun v -> `Accept v) in 
          Lwt.pick [accept ; stop] >|= (function
            | `Stop -> Lwt.cancel accept; `Stop 
            | `Accept _ as a -> a))
        (function 
          | `Accept (sock, _) ->              
            (match%lwt register_connection svc sock with 
              | Ok sid -> 
                let _ = serve_connection sock svc sid                 
                in accept_connection svc                    
              | Error e ->                   
                let%lwt _ = Logs_lwt.warn (fun m -> m "%s" @@ show_error e) 
                (* @AC: Perhaps we should wait for some connection to be closed instead of going back
                waiting for a connection. That would be more robust against DoS attack. *)
                in accept_connection svc)
          | `Stop ->  
            let%lwt _ = Logs_lwt.debug (fun m -> m "Stopping svc...") in
            let%lwt _ = Net.safe_close svc.socket in 
            let%lwt _ = Logs_lwt.debug (fun m -> m "Closing Session...") in
            Lwt.catch (fun () -> close_sessions svc) 
            (function 
            | exn -> Logs_lwt.warn (fun m -> m "Exception raised while closing session %s" @@ Printexc.to_string exn)))
        (function 
          | Lwt.Canceled -> Lwt.return_unit            
          | exn -> 
              Logs_lwt.debug (fun m  -> m "Exception raised while accepting session:\n\t%s" (Printexc.to_string exn)))
        in accept_connection svc

    let stop svc = 
      let%lwt _ = Net.safe_close svc.socket in 
      let%lwt _ = close_sessions svc in      
      Lwt.wakeup svc.notifier () ;
      Lwt.return_unit

    let socket svc = svc.socket
  end

end
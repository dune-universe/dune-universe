open Apero
open Iplocator 
open Endpoint
open Lwt.Infix

module TcpService = struct
    
  module Id = NumId.Make (Int64)

  module Config = struct 
    open Lwt_unix
    type t = 
      { locator : TcpLocator.t
      ; backlog : int
      ; max_connections : int
      ; socket_options : (Lwt_unix.file_descr -> unit) list
      ; svc_id : int 
      ; buf_size : int}

    let reuseaddr reuse = fun sock -> setsockopt sock SO_REUSEADDR reuse
    let tcp_nodelay nodelay = fun sock -> setsockopt sock TCP_NODELAY nodelay
    let sndbuf size = fun sock -> setsockopt_int sock SO_SNDBUF size
    let rcvbuf size = fun sock -> setsockopt_int sock SO_SNDBUF size
    
    let create ?(backlog=10) ?(max_connections=8192) ?(buf_size=65536)
                ?(socket_options=[reuseaddr true; tcp_nodelay true]) 
                ?(svc_id=0) locator = 
      { locator
      ; backlog
      ; max_connections
      ; socket_options
      ; svc_id 
      ; buf_size}

    let backlog c = c.backlog
    let locator c = c.locator 
    let socket_options c = c.socket_options
    let max_connectiosn c = c.max_connections
    let svc_id c = c.svc_id
    let buf_size c = c.buf_size
  end

  module type S = sig     
    type t

    type io_service = Lwt_unix.file_descr -> unit -> unit Lwt.t
    
    val create : Config.t -> t
    
    val start : t -> io_service ->  unit Lwt.t
    val stop : t -> unit Lwt.t
    
    val socket : t -> Lwt_unix.file_descr    

    val config : t -> Config.t
  end 
  
  module Make (MVar : MVar)  = struct 
    
    type io_service = Lwt_unix.file_descr -> unit -> unit Lwt.t
        
    module ConnectionMap = Map.Make(Id)

    type t = {
        socket : Lwt_unix.file_descr      
      ; waiter : unit Lwt.t
      ; notifier : unit Lwt.u      
      ; max_connections : Int64.t 
      ; connections_count : Int64.t MVar.t
      ; connections : (Lwt_unix.file_descr ConnectionMap.t) MVar.t
      ; config : Config.t}


    let create_server_socket config = 
      let open Lwt_unix in      
      let sock = socket PF_INET SOCK_STREAM 0 in      
      (Config.socket_options config) |> List.iter (fun setopt -> setopt sock ;) ;      
      let saddr = IpEndpoint.to_sockaddr @@ TcpLocator.endpoint (Config.locator config) in
      let _ = bind sock saddr in      
      let _ = listen sock (Config.backlog config) in sock
    
    
    let register_connection svc sock = 
      match%lwt  MVar.take svc.connections_count with 
      | count when count < svc.max_connections ->
        let sid = Id.next_id () in 
        let%lwt connections = MVar.take svc.connections in
        let%lwt _ = MVar.put svc.connections (ConnectionMap.add sid sock connections) in 
        let%lwt _= MVar.put svc.connections_count (Int64.add count Int64.one) in
        Lwt.return @@  Result.ok sid 
      | _ -> Lwt.return @@ Result.fail (`ResourceLimitViolation (`Msg "Too many connections"))
        
    let unregister_connection svc sid =       
      let%lwt connection_count = MVar.take svc.connections_count in 
      let%lwt connections = MVar.take svc.connections in 
      match ConnectionMap.find_opt sid connections with
      | Some sock ->               
        let%lwt _ = MVar.put svc.connections (ConnectionMap.remove sid connections) in 
        let%lwt _ = MVar.put svc.connections_count (Int64.sub connection_count Int64.one) in
        let%lwt _ = Net.safe_close sock in
        Lwt.return @@ Result.ok ()
      | None -> Lwt.return @@ Result.fail  @@ `InvalidSession  (`Msg "Unknown session id")

    let close_sessions svc =       
      let%lwt _ = MVar.take svc.connections_count in 
      let%lwt _ = MVar.put svc.connections_count 0L in 
      let%lwt connections = MVar.take svc.connections in 
      let%lwt _ = MVar.put svc.connections ConnectionMap.empty in 
      Lwt.join @@ ConnectionMap.fold (fun _ sock xs -> (Net.safe_close sock)::xs) connections []
          
    let serve_connection (sock:Lwt_unix.file_descr) (svc:t) (sid: Id.t) (io_svc: io_service) =       
      let%lwt _ = Logs_lwt.debug (fun m -> m "Serving session with Id: %s" (Id.to_string sid)) in 
      let mio_svc = io_svc sock in     
      let rec loop () = mio_svc () >>= loop        
      in 
        Lwt.catch (fun () -> loop ()) 
        (fun exn -> 
          let _ = Logs_lwt.warn (fun m -> m "Closing session %s because of: %s\n%s" (Id.to_string sid) (Printexc.to_string exn) (Printexc.get_backtrace())) in
          unregister_connection svc sid)

    let create config  = 
      let socket = create_server_socket config in 
      let (waiter, notifier) = Lwt.wait () in 
      { 
        socket
        ; waiter
        ; notifier
        ; max_connections = Int64.of_int (Config.max_connectiosn config)
        ; connections_count = MVar.create Int64.zero
        ; connections = MVar.create (ConnectionMap.empty) 
        ; config } 
      


    let start (svc : t) io_svc =       
      let%lwt _ = 
        Logs_lwt.debug (fun m -> m "Starting TcpService at %s with svc-id %d " 
          (TcpLocator.to_string @@ Config.locator svc.config) 
          (Config.svc_id svc.config)) in 

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
                let _ = serve_connection sock svc sid io_svc                
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

    let config svc = svc.config
  end

end
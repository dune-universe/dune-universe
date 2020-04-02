open Iplocator 
open Endpoint


module NetServiceWebSock = struct
  
  module WebSockConfig = struct 
    open Lwt_unix
    type t = 
      { locator : WebSockLocator.t
      ; backlog : int
      ; max_connections : int
      ; socket_options : (Lwt_unix.file_descr -> unit) list
      ; svc_id : int 
      ; buf_size : int }

    let reuseaddr reuse = fun sock -> setsockopt sock SO_REUSEADDR reuse
    let tcp_nodelay nodelay = fun sock -> setsockopt sock TCP_NODELAY nodelay
    let sndbuf size = fun sock -> setsockopt_int sock SO_SNDBUF size
    let rcvbuf size = fun sock -> setsockopt_int sock SO_SNDBUF size

    let make ?(backlog=10) ?(max_connections=8192) ?(buf_size=65536)
        ?(socket_options=[reuseaddr true; tcp_nodelay true]) 
        ?(svc_id=1) locator  = 
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

  module Config = WebSockConfig  
  type config = WebSockConfig.t
    
    type t = {
        waiter : unit Lwt.t
      ; notifier : unit Lwt.u      
      ; config : Config.t      
    }
        
    let make config = 
        let (waiter, notifier) = Lwt.wait () in 
        { waiter ; notifier ; config }

    let server_endp (config: WebSockConfig.t) = 
        (* let open Conduit_lwt_unix in  *)
        let port = IpEndpoint.port (WebSockLocator.endpoint config.locator) in 
        `TCP ( `Port port)
        
    let on_exception ex = 
        let _ = Logs.warn (fun m -> m "FEWS: the following exception was reaised %s" @@ Printexc.to_string ex) in ()
    
    let rec handle_session rbuf wbuf client handler = 
        let open Lwt.Infix in 
        handler rbuf wbuf client >>= fun () -> handle_session rbuf wbuf client handler

    let start svc handler =         
        let ctx = Conduit_lwt_unix.default_ctx in         
        let safe_handler client = 
            Lwt.catch (fun () -> 
                let len = 64*1024 in 
                let rbuf = Abuf.create ~grow:len len in 
                let wbuf = Abuf.create ~grow:len len in 
                handle_session rbuf wbuf client handler)
                      (fun ex -> Lwt.return @@ on_exception ex)
        in            
        Websocket_lwt.establish_standard_server ~ctx:ctx ~stop:svc.waiter ~on_exn:on_exception ~mode:(server_endp svc.config) safe_handler
    
    let stop svc = Lwt.return @@ Lwt.wakeup svc.notifier ()          

    let config svc = svc.config
end

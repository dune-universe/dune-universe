open Iplocator 

module NetServiceWebSock : sig 

  module WebSockConfig : sig 
    type t

    val reuseaddr : bool -> Lwt_unix.file_descr -> unit 
    val tcp_nodelay : bool -> Lwt_unix.file_descr -> unit 
    val sndbuf : int -> Lwt_unix.file_descr -> unit 
    val rcvbuf: int -> Lwt_unix.file_descr -> unit 

    val make : ?backlog:int -> ?max_connections:int -> ?buf_size:int ->
        ?socket_options:(Lwt_unix.file_descr -> unit) list -> ?svc_id:int 
        -> WebSockLocator.t -> t

    val backlog :  t -> int
    val locator : t -> WebSockLocator.t
    val socket_options : t -> (Lwt_unix.file_descr -> unit) list
    val max_connectiosn : t -> int 
    val svc_id : t -> int
    val buf_size : t -> int
  end
   
   module Config = WebSockConfig
   type config = WebSockConfig.t
   type t                      
   val make : config -> t
   val start : t -> (Abuf.t -> Abuf.t -> Websocket_lwt.Connected_client.t -> unit Lwt.t) -> unit Lwt.t 
   val stop : t -> unit Lwt.t 
   val config : t -> config 
      
end
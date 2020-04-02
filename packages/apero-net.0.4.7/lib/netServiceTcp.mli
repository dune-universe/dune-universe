open Iplocator 


module NetServiceTcp : sig 


  module TcpConfig : sig 
    type t

    val reuseaddr : bool -> Lwt_unix.file_descr -> unit 
    val tcp_nodelay : bool -> Lwt_unix.file_descr -> unit 
    val sndbuf : int -> Lwt_unix.file_descr -> unit 
    val rcvbuf: int -> Lwt_unix.file_descr -> unit 

    val make : ?backlog:int -> ?max_connections:int -> ?buf_size:int ->
        ?socket_options:(Lwt_unix.file_descr -> unit) list -> ?svc_id:int 
        -> TcpLocator.t -> t

    val backlog :  t -> int
    val locator : t -> TcpLocator.t
    val socket_options : t -> (Lwt_unix.file_descr -> unit) list
    val max_connectiosn : t -> int 
    val svc_id : t -> int
    val buf_size : t -> int
  end
  
  include NetService.S with type config = TcpConfig.t
    

end
open Iplocator 
open Apero


module TcpService : sig 

  module Id : Id.S

  module Config : sig 
    type t

    val reuseaddr : bool -> Lwt_unix.file_descr -> unit 
    val tcp_nodelay : bool -> Lwt_unix.file_descr -> unit 
    val sndbuf : int -> Lwt_unix.file_descr -> unit 
    val rcvbuf: int -> Lwt_unix.file_descr -> unit 

    val create : ?backlog:int -> ?max_connections:int -> ?buf_size:int ->
        ?socket_options:(Lwt_unix.file_descr -> unit) list -> ?svc_id:int 
        -> TcpLocator.t -> t

    val backlog :  t -> int
    val locator : t -> TcpLocator.t
    val socket_options : t -> (Lwt_unix.file_descr -> unit) list
    val max_connectiosn : t -> int 
    val svc_id : t -> int
    val buf_size : t -> int
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

  module Make (MVar : MVar) : S 

end
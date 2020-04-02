open Iplocator 
open Apero


module TcpServiceES : sig 

  module Id : Id.S

  module Config : sig 
    type t

    val reuseaddr : bool -> Lwt_unix.file_descr -> unit 
    val tcp_nodelay : bool -> Lwt_unix.file_descr -> unit 
    val sndbuf : int -> Lwt_unix.file_descr -> unit 
    val rcvbuf: int -> Lwt_unix.file_descr -> unit 

    val create : ?backlog:int -> ?stream_length:int -> ?max_connections:int -> 
        ?socket_options:(Lwt_unix.file_descr -> unit) list -> ?svc_id:int 
        -> TcpLocator.t -> t

    val backlog :  t -> int
    val locator : t -> TcpLocator.t
    val socket_options : t -> (Lwt_unix.file_descr -> unit) list
    val max_connectiosn : t -> int 
    val stream_length : t -> int
    val svc_id : t -> int
  end

  module type S = sig 
    type t
    type message
    type error 

    type message_reader = Lwt_unix.file_descr -> unit -> ((message, error) Result.t) Lwt.t
    type message_writer = Lwt_unix.file_descr -> message -> ((unit, error) Result.t) Lwt.t
    
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

  module Make (M : sig type message  end)
              (E : sig type error  val show_error : error -> string end) 
              : S with type message = M.message and type error = E.error

end
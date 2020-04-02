(* open Iobuf
open Atypes
open Common *)

type mtu = Unlimited | Limited of int

module Id : Apero.NumId.S

module TxSession : sig                 
  type t     
  val make : close:(unit -> unit Lwt.t) -> wait_on_close:bool  Lwt.t -> mtu:mtu -> Id.t -> Lwt_unix.file_descr -> t
  val mtu : t -> mtu                         
  val socket : t -> Lwt_unix.file_descr
  val close : t -> unit Lwt.t
  val id : t -> Id.t
  val when_closed : t -> bool Lwt.t
end


module type S = sig 
  type 'a io_init = TxSession.t -> 'a Lwt.t
  type 'a io_service = TxSession.t -> 'a -> 'a Lwt.t
  type config 
  type 'a t                      
  val make : config -> 'a t
  val mtu : mtu                         
  val start : 'a t -> 'a io_init -> 'a io_service -> unit Lwt.t 
  val stop : 'a t -> unit Lwt.t 
  val config : 'a t -> config 
  val socket : 'a t -> Lwt_unix.file_descr
  val establish_session : 'a t -> Locator.Locator.t -> TxSession.t Lwt.t 
end

(* 

module TxF : sig 
  module type CoDec = sig 
    type t
    type msg
    val encode : t -> msg -> IOBuf.t -> (IOBuf.t, error) Result.t
    val decode : t -> IOBuf.t -> (msg * IOBuf.t, error) Result.t
  end
  module type Session = sig 
    type t 
    type msg
    val mtu : t -> mtu
    val send : t -> msg -> unit Lwt.t
    val recv : t ->  msg Lwt.t
    val close : t -> unit Lwt.t 
  end

  module type SessionHandler = sig 
    type t
    type session
    val handle_open : t -> session -> unit Lwt.t
    val handle_close : t -> session -> unit Lwt.t
    val handle_message : t -> session ->  unit Lwt.t
  end 
  module type S = sig 
    type session   
    type config 
    type t                      
    type session_handler = session -> unit Lwt.t
    val make : config -> t
    val mtu : mtu                         
    val start : t -> session_handler -> unit Lwt.t 
    val stop : t -> unit Lwt.t 
    val config : t -> config 
    val socket : t -> Lwt_unix.file_descr
    val open_session : t -> Locator.Locator.t -> session Lwt.t 
  end
end



             *)
(* open Iobuf
open Atypes
open Common *)

type mtu = Unlimited | Limited of int

module Id = Apero.NumId.Make (Int64)

module TxSession = struct
  type t = 
    { sock : Lwt_unix.file_descr
    ; close : unit -> unit Lwt.t
    ; mtu : mtu 
    ; sid : Id.t 
    ; wait_on_close : bool Lwt.t}
  let make ~close ~wait_on_close ~mtu sid sock = { sock; close; mtu; sid; wait_on_close }  
  let mtu s = s.mtu 
  let socket s = s.sock
  let close s = s.close ()
  let id s = s.sid
  let when_closed s = s.wait_on_close
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

module TxSessionF = struct
  module type CoDec = sig 
    type t
    type msg
    val encode : t -> msg -> IOBuf.t -> (IOBuf.t, error) Result.t
    val decode : t -> IOBuf.t -> (msg * IOBuf.t, error) Result.t
  end
  module type S = sig 
    type t 
    type msg
    val mtu : t -> mtu
    val send : t -> msg -> unit Lwt.t
    val recv : t ->  msg Lwt.t
    val close : t -> unit Lwt.t 
  end
end *)
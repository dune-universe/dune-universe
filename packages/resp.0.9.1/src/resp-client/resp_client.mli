module type S = sig
  include Resp.S

  type t
  type params

  val connect : params -> t Lwt.t
  val read : t -> Resp.t Lwt.t
  val write : t -> Resp.t -> unit Lwt.t
  val run : t -> Resp.t array -> Resp.t Lwt.t
  val run_s : t -> string array -> Resp.t Lwt.t
  val decode : t -> Resp.lexeme -> Resp.t Lwt.t
  val read_lexeme : t -> Resp.lexeme Lwt.t
end

module type CLIENT = sig
  type ic
  type oc
  type params

  val connect : params -> (ic * oc) Lwt.t
end

module Make
    (Client : CLIENT)
    (S : Resp.S with type Reader.ic = Client.ic and type Writer.oc = Client.oc) :
  S
  with module Reader = S.Reader
   and module Writer = S.Writer
   and type t = Client.ic * Client.oc
   and type params = Client.params

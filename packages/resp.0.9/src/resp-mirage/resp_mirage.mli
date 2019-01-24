type buffer =
  { flow : Conduit_mirage.Flow.flow
  ; mutable buffer : Cstruct.t }

module Reader : Resp.READER with type ic = buffer
module Writer : Resp.WRITER with type oc = Conduit_mirage.Flow.flow

module Backend (Data : sig
  type data
end) :
  Resp_server.SERVER
  with type oc = Conduit_mirage.Flow.flow
   and type ic = buffer
   and type data = Data.data

module Server : sig
  module Make
      (Auth : Resp_server.AUTH) (Data : sig
          type data
      end) :
    Resp_server.S
    with module Auth = Auth
     and type ic = Reader.ic
     and type oc = Writer.oc
     and type server = Conduit_mirage.conduit * Conduit_mirage.server
     and type client = Reader.ic * Writer.oc
     and type data = Data.data

  module Default :
    Resp_server.S
    with type Auth.t = string
     and type ic = Reader.ic
     and type oc = Writer.oc
     and type server = Conduit_mirage.conduit * Conduit_mirage.server
     and type client = Reader.ic * Writer.oc
     and type data = unit
end

module Client :
  Resp_client.S
  with type params = Conduit_mirage.conduit * Conduit_mirage.client
   and type t = Reader.ic * Writer.oc

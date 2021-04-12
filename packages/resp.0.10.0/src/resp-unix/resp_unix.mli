module Reader : Resp.READER with type ic = Lwt_io.input_channel

module Writer : Resp.WRITER with type oc = Lwt_io.output_channel

module Backend (Data : Resp_server.DATA) :
  Resp_server.SERVER
    with type oc = Lwt_io.output_channel
     and type ic = Lwt_io.input_channel
     and type data = Data.data
     and type Client.t = Data.Client.t

module Server : sig
  module Make (Auth : Resp_server.AUTH) (Data : Resp_server.DATA) :
    Resp_server.S
      with module Auth = Auth
       and type ic = Reader.ic
       and type oc = Writer.oc
       and type server = Conduit_lwt_unix.ctx * Conduit_lwt_unix.server
       and type data = Data.data
       and type Client.t = Data.Client.t

  module Default :
    Resp_server.S
      with type Auth.t = string
       and type ic = Reader.ic
       and type oc = Writer.oc
       and type server = Conduit_lwt_unix.ctx * Conduit_lwt_unix.server
       and type data = unit
       and type Client.t = unit
end

module Client :
  Resp_client.S
    with type params = Conduit_lwt_unix.ctx * Conduit_lwt_unix.client
     and type t = Reader.ic * Writer.oc

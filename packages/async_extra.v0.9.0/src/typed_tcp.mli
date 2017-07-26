open Typed_tcp_intf

module type Arg = Arg
module type Binable_t = Binable_t
module type S = S

module Make (Arg : Arg) () :
  S with module Client_message = Arg.Client_message
     and module Server_message = Arg.Server_message

module Simple
    (Client_message : Binable_t)
    (Server_message : Binable_t) ()
  : S
    with type Client_message.t = Client_message.t
    with type Server_message.t = Server_message.t

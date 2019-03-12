open Cohttp_lwt
open Cohttp_lwt_unix

include Ocamlapi.Make (Request)
                      (Body)
                      (Response)
                      (IO)

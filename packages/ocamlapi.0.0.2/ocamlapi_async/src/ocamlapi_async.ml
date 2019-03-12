open Cohttp_async

include Ocamlapi.Make (Request)
                      (Body)
                      (Response)
                      (Io)

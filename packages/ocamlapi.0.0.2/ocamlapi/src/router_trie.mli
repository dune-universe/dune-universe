(** Instantiate a Router module with custom default values.

Example using [Cohttp_async]:

{[

    open Cohttp_async

    module Custom_config = struct

        let default_exn_handler ?vars:_ exn =
            let status, body =
                match exn with
                | Exceptions.Bad_request msg -> `Bad_request, Body.empty
                | _ -> `Internal_server_error,
                        Body.of_string "An unexpected error occurred" in
            Io.return (Response.make ~status (), body)

        let default_fallback ?vars:_ _req _body =
            Io.return (Response.make ~status:`Not_found (), Body.empty)
    end

    module Router = Make (Request) (Body) (Response) (Io) (Custom_config)
]}

 *)
module Make_with_config : functor (Req: Cohttp.S.Request)
                                  (Body: S.Body)
                                  (Resp: Cohttp.S.Response)
                                  (IO: S.Io)
                                  (Config : S.Router_config with 
                                    type req := Req.t and
                                    type body := Body.t and
                                    type resp := Resp.t and
                                    type 'a io := 'a IO.t) -> 
                                  (S.Router with
                                    type req := Req.t and
                                    type body := Body.t and
                                    type resp := Resp.t and
                                    type 'a io := 'a IO.t)

                                  
(** Instantiate a Router module.

Example using [Cohttp_async]:
{[ 
    open Cohttp_async 
 
    module Router = Make (Request) (Body) (Response) (Io)
]}*)
module Make : functor (Req: Cohttp.S.Request)
                      (Body: S.Body)
                      (Resp: Cohttp.S.Response)
                      (IO: S.Io) ->
                      (S.Router with
                        type req := Req.t and
                        type body := Body.t and
                        type resp := Resp.t and
                        type 'a io := 'a IO.t)

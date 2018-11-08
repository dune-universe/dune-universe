module Meth = Meth
module Cookie = Cookie
module Header = Header
module Request = Request
module Response = Response
module Path = Path
module Request_handler = Request_handler
module Path_mapping = Path_mapping
module Path_handler = Path_handler

(** [Jerboa.default_request_handler] is the default request handler that will be run when no matching path is found by Jerboa.
  It gives back a http 404 response with an empty body.
*)
let default_request_handler _ = 
  Response.create 404 ""

(** [Jerboa.start ?port ?default_request_handler ?middleware_config path_handler_config] is the starting point of Jerboa, which starts the framework.
  - port(optional): gives the port of the running framework (default is 8080)
  - default_request_handler(optional): request handler, which will be run if Jerboa can't find matching path_handler (default is default_request_handler)
  - middleware_config(optional): list of middlewares that transforms the request before the matching handler is found (default is empty list)
  - path_handler_config: list of path handlers that will be matched by jerboa based on the request
*)
let start ?(port = 8080) ?(default_request_handler = default_request_handler) ?(middleware_config = []) path_handler_config =
  let server = Server.create port default_request_handler middleware_config  path_handler_config in
  Lwt_main.run server
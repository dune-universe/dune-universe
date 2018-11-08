(** [Jerboa.Path_handler] module consists of the type definition of the Path_handler and an easy to use constructor.*)

(** [Path_handler.t] is the type of the path handler, which will be used to handle a specific request.*)
type t = {
  meth: Meth.t;
  path_mapping: Path_mapping.t;
  request_handler: Request_handler.t;
}

(** [Path_handler.create meth path_mapping request_handler] is a constructor for path handler records, which work based on:
  - meth: http method to match
  - path_mapping: path mapping to used for matching the request's path
  - request_handler: handler, which will transform the request into a response
*)
let create meth path_mapping request_handler = {
    meth;
    path_mapping;
    request_handler;
}

let handle_path_handler path_handler request =
  let path_mapping = path_handler.path_mapping in
  let request = Request.add_path_parameters request path_mapping in
  path_handler.request_handler request

let apply_path_handler path_handler request default_request_handler =
  match path_handler with
  | Some path_handler -> handle_path_handler path_handler request
  | None -> default_request_handler request

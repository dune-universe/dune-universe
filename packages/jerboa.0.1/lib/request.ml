(** [Jerboa.request] module consists of the type definition of the Request record.*)

(** [Request.t] consists of the main parts of a http request like metho, path, header, paramters and it's body.*)
type t = {
  meth: Meth.t;
  path: string;
  header: Header.t;
  path_parameter: (string * string) list;
  query_parameter: (string * string list) list;
  body: string;
}

let create request body =
  let open Lwt.Infix in
  let uri = Cohttp.Request.uri request in
  let meth = Cohttp.Request.meth request in
  let header =  Cohttp.Request.headers request in
  let path = Uri.path uri in
  let path_parameter = [] in
  let query_parameter = Uri.query uri in
  Cohttp_lwt.Body.to_string body >>= fun body ->
  Lwt.return {
    meth;
    path;
    header;
    path_parameter;
    query_parameter;
    body;
  }

(* This function can throw an exception, but it's highly unlikely, 
  because of the path checking done in the path_handle chossing phase. *)
let find_path_parameter accumulator path_and_path_part =
  let open Path in
  let path, path_part = path_and_path_part in
  Base.Option.value_map path.name ~default:accumulator ~f:(fun path_name ->
    let path_regex = Re.compile path.regex in
    let match_group = Re.exec path_regex path_part in
    let first_match = Re.Group.get match_group 0 in
    Base.List.cons (path_name, first_match) accumulator
  )

let find_path_parameters path_and_path_part =
  Base.List.fold path_and_path_part ~init:[] ~f:find_path_parameter

let add_path_parameters request path_handler =
  let path_parts = Path.split_into_path_parts request.path in
  let path_handler_and_parts = Base.List.zip path_handler path_parts in
  let path_parameter_option = Base.Option.map path_handler_and_parts ~f:find_path_parameters in
  let path_parameter = Base.Option.value path_parameter_option ~default:[] in
  {request with path_parameter = path_parameter;}
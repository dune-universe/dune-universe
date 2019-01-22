let create_config_dir () =
  let dirname = sprintf "%s/%s"
      (Unix.getenv_exn "HOME") ".gemini" in
  try_with ~extract_exn:true
    (fun () -> Unix.mkdir ?p:None ?perm:None dirname) >>=
  function
  | Result.Ok () -> Deferred.unit
  | Result.Error
      (Unix.Unix_error (Unix.Error.EEXIST, _, _)) -> Deferred.unit
  | Result.Error e ->
    Log.Global.error "failed to create gemini config directory
        at %S." dirname; raise e

let param ?default ~name ~env () =
  let name = sprintf "GEMINI_%s_%s"
      (String.uppercase env) name in
  match Unix.getenv name with
  | Some param -> param
  | None ->
    match default with
    | None ->
      failwithf "Environment variable \"%s\" must be specified"
        name ()
    | Some default -> default

let host ~env =
  let env = String.lowercase env in 
  match env with
  | "production" -> sprintf "api.gemini.com"
  | _ -> sprintf "api.%s.gemini.com" env
let version_1 = "v1"


(** Gemini configuration. These values are the only
    necessary parameters and secrets to connect to an endpoint.

    For each gemini environment a different configuration module
    should be instantiated. *)
module type S = sig
  val version : string
  val api_host : string
  val api_key : string
  val api_secret : string
end

let api_key = param ~name:"API_KEY"
let api_secret = param ~name:"API_SECRET"

(** Makes a configuration module given a desired
    Gemini environment. It then infers the values from
    environment variables.

    Any required fields not specified will generate a runtime error.
*)
let make env =
  let module M = struct
    let env = env
    let version = version_1
    let api_host = host ~env
    let api_key = api_key ~env ()
    let api_secret = api_secret ~env ()
  end in
  (module M : S)


(** Creates a configuration module for the sandbox environment.
    Will query the unix environment for the necessary parameters.

    It is recommended to generate this module exactly once in your
    program. *)
module Sandbox () =
struct
  include (val make "sandbox" : S)
end

(** Creates a configuration module for the production environment.
    Will query the unix environment for the necessary parameters.

    It is recommended to generate this module exactly once in your
    program. *)
module Production () =
struct
  include (val make "production" : S)
end

(** Produces a configuration module given a string environment name
    which should be one of "production" or "sandbox".
*)
let of_string s =
  match String.lowercase s with
  | "production" ->
    let module Cfg : S = Production () in
    (module Cfg : S)
  | "sandbox" ->
    let module Cfg : S = Sandbox () in
    (module Cfg : S)
  | unsupported_env ->
    failwithf "environment %s not supported"
      unsupported_env ()

let arg_type = Command.Arg_type.create of_string
let param =
  Command.Param.(
    flag "-cfg" (optional arg_type)
      ~doc:(
        sprintf "STRING the configuration the client will connect with \
                 (eg. sandbox or production. defaults to sandbox). Use \
                 GEMINI_ENV to override the default value."
      )
  )

(** Ensures a configuration chosen given an optional configuration module,
    usually provided from the command line. If the module is
    provided, it returns it unwrapped. Otherwise the unix environment is
    queried to produce a module, defaulting to sandbox if no
    environment variables exist. *)
let or_default param =
  match param with
  | None ->
    (match Unix.getenv "GEMINI_ENV" with
     | Some env -> of_string env
     | None -> (module Sandbox())
    )
  | Some param -> param

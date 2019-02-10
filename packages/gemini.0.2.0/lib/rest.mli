(** REST api support for the Gemini trading exchange. These endpoints are used
    to manage orders and check balances. *)

(** Represents all error conditions possible by the REST services. In general
    the REST services don't throw exceptions- instead they report an error
    variant with some provisional failure information. *)
module Error : sig

  (** An error at the http protocol level. Each variant is named according
      to it's corresponding http status code. The body of the http response
      is append to the variant payload if it exists. *)
  type http = [ `Bad_request of string
              | `Not_found
              | `Service_unavailable of string
              | `Not_acceptable of string
              | `Unauthorized of string] [@@deriving sexp]

  (** An error type indicating a json parse error. *)
  type json_error = {message:string;body:string} [@@deriving sexp]

  (** Json level error conditions. *)
  type json = [`Json_parse_error of json_error] [@@deriving sexp]

  (** Used to provide more details on a particular error condition *)
  type detail = {reason:string;message:string} [@@deriving sexp, yojson]
  (** Application level error conditions *)
  type response = [`Error of detail] [@@deriving sexp]

  (** Any error condition possible from an http post request *)
  type post = [http|json|response] [@@deriving sexp]
end

module Request = Nonce.Request


(** Operations denote a single REST operation endpoint. *)
module Operation : sig

  (** Minimal specification to define a new REST operation. *)
  module type S = sig

    (** The human readable name of this operation. *)
    val name : string

    (** The uri path of the REST endpoint. *)
    val path : string list

    (** The type of the request payload for this REST endpoint. *)
    type request [@@deriving sexp, to_yojson]
    (** The type of the response payload for this REST endpoint. *)
    type response [@@deriving sexp, of_yojson]
  end

  (** A REST operation endpoint which takes no request parameters. *)
  module type S_NO_ARG = sig
    include S with type request = unit
  end

end

(** Support for parsing responses from a Gemini api REST endpoint. *)
module Response :
sig
  module Json_result :
  sig
    type t = [ `Error | `Ok ] [@@derivin sexp, yojson, enumerate]
    val to_string : t -> string
    val dict : (string * t) sexp_list
    val of_string : string -> t
    val of_string_opt : string -> t option
     val split :
      [< Yojson.Safe.json ] ->
      (t option * [> Yojson.Safe.json ], string) result
  end
  type result_field =
    { result : Json_result.t; } [@@deriving sexp, of_yojson]
  type t =
    { result : Json_result.t; payload : Yojson.Safe.json; }
  val parse :
    Yojson.Safe.json ->
    ([> Yojson.Safe.json ] -> ('a, string) result) ->
    [ `Error of Error.detail
    | `Json_parse_error of Error.json_error
    | `Ok of 'a
    ]
end

(** Creates a REST endpoint using the http post method with
    operation specified by provided module [Operation] *)
module Post :
  functor (Operation : Operation.S) ->
  sig
    val post :
      (module Cfg.S) ->
      Nonce.reader ->
      Operation.request ->
      [ `Ok of Operation.response
      | Error.post
      ] Deferred.t
  end

(** Creates a REST endpoint using the http post method with
    operation specified by provided module [Operation].
    Also produces a command line interface hook. *)
module Make :
  functor (Operation : Operation.S) ->
  sig
    val post :
      (module Cfg.S) ->
      Nonce.reader ->
      Operation.request ->
      [`Ok of Operation.response
      | Error.post
      ] Deferred.t
    val command : string * Core.Command.t
  end

(** Creates a REST endpoint using the http post method with no argument
    operation specified by provided module [Operation].
    Also produces a command line interface hook. *)
module Make_no_arg :
  functor (Operation : Operation.S_NO_ARG) ->
  sig
    val post :
      (module Cfg.S) ->
      Nonce.reader ->
      unit ->
      [ Error.post
      | `Ok of Operation.response
      ] Deferred.t
    val command : string * Core.Command.t
  end

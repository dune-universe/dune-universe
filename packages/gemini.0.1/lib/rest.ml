module Error = struct
  type http = [ `Bad_request of string
              | `Not_found
              | `Service_unavailable of string
              | `Not_acceptable of string
              | `Unauthorized of string] [@@deriving sexp]
  type json_error = {message:string;body:string} [@@deriving sexp]
  type json = [`Json_parse_error of json_error] [@@deriving sexp]

  type detail = {reason:string;message:string} [@@deriving sexp, yojson]
  type response = [`Error of detail] [@@deriving sexp]

  type post = [http|json|response] [@@deriving sexp]
end


module Operation = struct

  module type S = sig
    val name : string
    val path : string list
    type request [@@deriving sexp, to_yojson]
    type response [@@deriving sexp, of_yojson]
  end

  module type S_NO_ARG = sig
    include S with type request = unit
  end

end

module Request = Nonce.Request

module Response = struct

  module Json_result = struct
    module T = struct
    type t = [`Error | `Ok] [@@deriving sexp, enumerate]
    let to_string = function
      | `Error -> "error"
      | `Ok -> "ok"
    end
    include T
    include Json.Make(T)

    let split = function
      | `Assoc assoc as json ->
      (List.Assoc.find assoc ~equal:String.equal
        "result" |> function
       | None -> Result.Ok (None, json)
       | Some json' ->
        of_yojson json' |> Result.map ~f:(fun x ->
          (Some x,
           `Assoc
             (List.Assoc.remove
                assoc ~equal:String.equal "result"
             )
          )
        )
      )
      | #Yojson.Safe.json as json ->
        Result.Ok (None, json)

  end

  type result_field =
    {result:Json_result.t} [@@deriving of_yojson, sexp]

  type t = {result:Json_result.t; payload:Yojson.Safe.json}


  let parse json ok_of_yojson =
    match Json_result.split json with
    | Result.Ok (result, payload) ->
      (match result with
      | None
      | Some `Ok ->
        (ok_of_yojson payload |> function
          | Result.Ok x -> `Ok x
          | Result.Error e ->
            `Json_parse_error
              Error.{message=e; body=Yojson.Safe.to_string payload}

        )
      | Some `Error ->
        (Error.detail_of_yojson payload |> function
          | Result.Ok x -> `Error x
          | Result.Error e ->
            `Json_parse_error
              Error.{message=e; body=Yojson.Safe.to_string payload}
        )
      )
    | Result.Error e ->
      `Json_parse_error
        Error.{message=e;body=Yojson.Safe.to_string json}

end

module Post(Operation:Operation.S) :
  sig
    val post :
      (module Cfg.S) ->
      Nonce.reader ->
      Operation.request ->
      [
      | `Ok of Operation.response
      | Error.post
      ] Deferred.t
  end =
struct
  let post
      (module Cfg : Cfg.S)
      (nonce : Nonce.reader)
      (request : Operation.request) :
    [ `Ok of Operation.response
    | Error.post] Deferred.t =
    let payload =
      Operation.request_to_yojson request in
    let path = Path.to_string Operation.path in
     Request.make ~nonce
      ~request:path ~payload () >>=
    fun request ->
    (Request.to_yojson request |>
     Yojson.Safe.pretty_to_string |>
     fun s ->
     Log.Global.debug "request as json:\n %s" s;
     return @@ Auth.of_payload s
    )
    >>= fun payload ->
    let headers = Auth.to_headers (module Cfg) payload in
    let uri = Uri.make
        ~scheme:"https"
        ~host:Cfg.api_host
        ~path
        ?query:None
        () in
    Cohttp_async.Client.post
      ~headers
      ?chunked:None
      ?interrupt:None
      ?ssl_config:None
      ?body:None
      uri >>= fun (response, body) ->
    match Cohttp.Response.status response with
    | `OK ->
      (
        Cohttp_async.Body.to_string body
        >>|
        (fun s ->
           Log.Global.debug "result as json:\n %s" s;
           let yojson = Yojson.Safe.from_string s in
           Response.parse yojson Operation.response_of_yojson
        )
      )
    | `Not_found -> return `Not_found
    | `Not_acceptable ->
      Cohttp_async.Body.to_string body >>| fun body ->
      `Not_acceptable body
    | `Bad_request ->
      Cohttp_async.Body.to_string body >>| fun body ->
      `Bad_request body
    | `Service_unavailable ->
      Cohttp_async.Body.to_string body >>| fun body ->
     `Service_unavailable body
    | (code : Cohttp.Code.status_code) ->
      Cohttp_async.Body.to_string body >>| fun body ->
      failwiths (sprintf "unexpected status code (body=%S)" body)
        code Cohttp.Code.sexp_of_status_code
end


module Make(Operation:Operation.S) =
struct
  include Post(Operation)
  let command =
    let open Command.Let_syntax in
    (Operation.name,
     Command.async
       ~summary:(Path.to_summary ~has_subnames:false Operation.path)
       [%map_open
         let config = Cfg.param
         and request = anon ("request" %: sexp)
         in
         fun () ->
           let request = Operation.request_of_sexp request in
           Log.Global.info "request:\n %s"
             (Operation.sexp_of_request request |> Sexp.to_string);
           let config = Cfg.or_default config in
           Nonce.File.(pipe ~init:default_filename)
             () >>= fun nonce ->
           post config nonce request >>= function
           | `Ok response ->
             Log.Global.info "response:\n %s"
               (Sexp.to_string_hum
                  (Operation.sexp_of_response response)
               ); Log.Global.flushed ()
           | #Error.post as post_error ->
             failwiths
               (sprintf
                  "post for operation %S failed"
                  (Path.to_string Operation.path)
               )
               post_error
               Error.sexp_of_post
       ]
    )

end

module Make_no_arg(Operation:Operation.S_NO_ARG) =
struct
  include Post(Operation)

  let command =
    let open Command.Let_syntax in
    (Operation.name,
     Command.async
       ~summary:(Path.to_summary ~has_subnames:false Operation.path)
       [%map_open
         let config = Cfg.param in
         fun () ->
           let request = () in
           let config = Cfg.or_default config in
           Nonce.File.(pipe ~init:default_filename) () >>= fun nonce ->
           post config nonce request >>= function
           | `Ok response ->
             Log.Global.info "response:\n %s"
               (Sexp.to_string_hum
                  (Operation.sexp_of_response response)
               ); Log.Global.flushed ()
           | #Error.post as post_error ->
             failwiths
               (sprintf
                  "post for operation %S failed"
                  (Path.to_string Operation.path)
               )
               post_error
               Error.sexp_of_post
       ]
    )

end

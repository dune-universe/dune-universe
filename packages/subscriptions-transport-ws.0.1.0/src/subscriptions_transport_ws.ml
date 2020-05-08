(*----------------------------------------------------------------------------
 *  Copyright (c) 2018-2020 António Nuno Monteiro
 *
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are met:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *  this list of conditions and the following disclaimer.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *  notice, this list of conditions and the following disclaimer in the
 *  documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the name of the copyright holder nor the names of its
 *  contributors may be used to endorse or promote products derived from this
 *  software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 *  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 *  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 *  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 *  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 *  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 *  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 *  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 *  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 *  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 *  POSSIBILITY OF SUCH DAMAGE.
 *---------------------------------------------------------------------------*)

module type SubscriptionsManager = sig
  type t

  val add : t -> string -> (unit -> unit) -> unit

  val create : int -> t

  val remove : t -> string -> unit

  val mem : t -> string -> bool

  val find_opt : t -> string -> (unit -> unit) option

  val iter : (string -> (unit -> unit) -> unit) -> t -> unit

  val clear : t -> unit
end

module Json = Yojson.Basic.Util
open Websocketaf

(* https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md *)
module Protocol = struct
  module Client = struct
    type t =
      | Gql_connection_init
      | Gql_start of
          { id : string
          ; query : string
          ; variables : (string * Graphql_parser.const_value) list
          ; operation_name : string option
          }
      | Gql_stop of string
      | Gql_connection_terminate

    let of_json json =
      match Json.(to_string (member "type" json)) with
      | "connection_init" ->
        Some Gql_connection_init
      | "start" ->
        let opId = Json.(json |> member "id" |> to_string) in
        let payload_json = Json.member "payload" json in
        let query = Json.(payload_json |> member "query" |> to_string) in
        let variables =
          try Json.(payload_json |> member "variables" |> to_assoc) with
          | _ ->
            []
        in
        let operation_name =
          Json.(payload_json |> member "operationName" |> to_string_option)
        in
        Some
          (Gql_start
             { id = opId
             ; query
             ; variables =
                 (variables :> (string * Graphql_parser.const_value) list)
             ; operation_name
             })
      | "stop" ->
        let opId = Json.(json |> member "id" |> to_string) in
        Some (Gql_stop opId)
      | "connection_terminate" ->
        Some Gql_connection_terminate
      | _ ->
        None
      | exception _ ->
        None
  end

  module Server = struct
    type t =
      (* | Gql_connection_error *)
      | Gql_connection_ack
      | Gql_data
      | Gql_error
      | Gql_complete

    (* | Gql_connection_keep_alive *)

    let to_string = function
      (* | Gql_connection_error -> *)
      (* "connection_error" *)
      | Gql_connection_ack ->
        "connection_ack"
      | Gql_data ->
        "data"
      | Gql_error ->
        "error"
      | Gql_complete ->
        "complete"

    (* | Gql_connection_keep_alive -> "ka" *)
  end
end

type 'a handlers =
  { schedule :
      'a
      -> on_recv:((Yojson.Basic.t, Yojson.Basic.t) result -> unit)
      -> on_close:(unit -> unit)
      -> unit
  ; destroy : 'a -> unit
  }

module SubscriptionsManager = struct
  type 't t = (module SubscriptionsManager with type t = 't) * 't

  let make module_ t = module_, t
end

let create_message wsd ?(opcode = `Text) ?id ?(payload = `Null) typ =
  let open Protocol in
  let frame_payload =
    `Assoc
      [ "type", `String (Server.to_string typ)
      ; ("id", match id with Some id -> `String id | None -> `Null)
      ; "payload", payload
      ]
  in
  let json = Yojson.Basic.to_string frame_payload in
  Wsd.send_bytes
    wsd
    ~kind:opcode
    ~off:0
    ~len:(String.length json)
    (Bytes.unsafe_of_string json)

let on_recv
    : type t.
      t SubscriptionsManager.t
      -> subscribe:
           (variables:Graphql.Schema.variables
            -> ?operation_name:string
            -> string
            -> (( [< `Response of Yojson.Basic.t | `Stream of 'a ]
                , Yojson.Basic.t )
                result
                -> unit)
            -> unit)
      -> 'a handlers
      -> Wsd.t
      -> opcode:Websocket.Opcode.t
      -> is_fin:bool
      -> Bigstringaf.t
      -> off:int
      -> len:int
      -> unit
  =
 fun ((module Subscriptions), t) ~subscribe handlers ->
  let open Protocol in
  let websocket_handler wsd ~opcode ~is_fin:_ bs ~off ~len =
    match opcode with
    | `Binary | `Continuation | `Text ->
      let json =
        Yojson.Basic.from_string (Bigstringaf.substring bs ~off ~len)
      in
      (match Client.of_json json with
      | None ->
        let id = Json.(json |> member "id" |> to_string_option) in
        let payload = `Assoc [ "message", `String "Invalid message type!" ] in
        create_message wsd ?id ~payload Server.Gql_error
      | Some message ->
        (match message with
        | Gql_connection_init ->
          (* TODO: allow a user-defined `on_connect` handler *)
          (* TODO: check for `graphql-ws` in the request headers, otherwise
             terminate connection *)
          create_message wsd Gql_connection_ack
        | Gql_start { id; query; variables; operation_name } ->
          (* TODO: unsubscribe if there's a subscription with the same id *)
          subscribe ~variables ?operation_name query (function
              | Error message ->
                let payload = `Assoc [ "message", message ] in
                create_message wsd ~payload ~id Gql_error
              | Ok (`Response json) ->
                create_message wsd ~id ~payload:json Gql_data
              | Ok (`Stream stream) ->
                Subscriptions.add t id (fun () -> handlers.destroy stream);
                handlers.schedule
                  stream
                  ~on_recv:(fun x ->
                    let (Ok x | Error x) = x in
                    (* XXX: OGS doesn't yet have a way of effectively killing
                     * a stream – so if we've been asked to unsubscribe, don't
                     * push the execution result to the websocket.
                     *)
                    if Subscriptions.mem t id then
                      create_message wsd ~id ~payload:x Gql_data)
                  ~on_close:(fun () ->
                    if Subscriptions.mem t id then
                      create_message wsd ~id Gql_complete))
        | Gql_stop id ->
          (match Subscriptions.find_opt t id with
          | None ->
            ()
          | Some unsubscribe ->
            unsubscribe ();
            Subscriptions.remove t id)
        | Gql_connection_terminate ->
          Subscriptions.iter (fun _ f -> f ()) t;
          Subscriptions.clear t;
          Wsd.close wsd))
    | `Connection_close ->
      Wsd.close wsd
    | `Ping ->
      Wsd.send_pong wsd
    | `Pong | `Other _ ->
      ()
  in
  websocket_handler

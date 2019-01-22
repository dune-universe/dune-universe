(** Order events web sockets api for the gemini trading exchange. This
    is used to get private events about orders and trades without having to
    depend on the slower and clumsier json REST api. Users still must submit
    orders using REST, however. This is only for responses.
*)

open Common

module Request = Nonce.Request


(** The order events api has no uri arguments .*)
type uri_args = [`None] [@@deriving sexp, enumerate]


(** Represents order message types supported by the Gemini exchange *)
module Message_type : sig
  (* The support message types for order events -
     either [`Update] or [`Heartbeat].
  *)
  type t = [`Update | `Heartbeat] [@@deriving sexp, enumerate]
  include Json.ENUM_STRING with type t := t
end

(** Response type for heart beat messages. *)
type heartbeat = {timestampms:Timestamp.ms;
                  sequence:int_number;
                  trace_id:string;
                  socket_sequence:int_number
                 } [@@deriving sexp, yojson]


(** Represents different order event types. *)
module Event_type : sig

  (** Enumerates all possible order events. *)
  type t = [ `Subscription_ack
           | `Heartbeat
           | `Initial
           | `Accepted
           | `Rejected
           | `Booked
           | `Fill
           | `Cancelled
           | `Closed
           ] [@@deriving sexp, enumerate]

  include Json.ENUM_STRING with type t:= t
end

(** Events can be filtered by symbol using [Symbol_filter symbol] or
    for an event type using [Event_type_filter event_type] or using
    an api session filter [Api_session_filter session-id].
*)
type query = [ `Symbol_filter of Symbol.t
             | `Event_type_filter of Event_type.t
             | `Api_session_filter of string
             ] [@@deriving sexp]

(** Represents differents reasons an order event occurred. *)
module Reason : sig
  type t =
    [`Place | `Trade | `Cancel | `Initial] [@@deriving sexp, enumerate]
  include Json.ENUM_STRING with type t := t
end

(** Represents different liquidity types *)
module Liquidity : sig

  (** [Taker] is the only known liquidity type but this
      is poorly documented so other values might exist.
  *)
  type t = [`Taker] [@@deriving sexp, enumerate]
  include Json.ENUM_STRING with type t := t
end

(** Type type of an order fill event. *)
type fill = {
  trade_id:int_string;
  liquidity:Liquidity.t;
  price:decimal_string;
  amount:decimal_string;
  fee:decimal_string;
  fee_currency:Currency.t;
} [@@deriving sexp, yojson]

(** Represents an api session name *)
module Api_session : sig

  (** An api session name. *)
  type t = string [@@deriving sexp, yojson]
end

(** The type of an order event. *)
type order_event =
  {order_id:string;
   api_session:Api_session.t;
   client_order_id:string option [@default None];
   event_id:string option [@default None];
   order_type:Order_type.t;
   symbol:Symbol.t;
   side:Side.t;
   behavior:string option [@default None];
   type_ : Event_type.t [@key "type"];
   timestamp:Timestamp.t;
   timestampms:Timestamp.ms;
   is_live : bool;
   is_cancelled : bool;
   is_hidden : bool;
   avg_execution_price : decimal_string option [@default None];
   executed_amount : decimal_string option [@default None];
   remaining_amount : decimal_string option [@default None];
   original_amount : decimal_string option [@default None];
   price : decimal_string option [@default None];
   total_spend : decimal_string option [@default None];
   fill : fill option [@default None];
   socket_sequence:int_number
  } [@@deriving sexp, yojson]

(** The type of a subscription acknowledgement event. *)
type subscription_ack =
  {account_id:int_number [@key "accountId"];
   subscription_id:string [@key "subscriptionId"];
   symbol_filter:Symbol.t list [@key "symbolFilter"];
   api_session_fiter:string list [@key "apiSessionFilter"];
   event_type_filter:Event_type.t list [@key "eventTypeFilter"]
  } [@@deriving sexp, yojson]

(** The response type for any order message. *)
type response =
  [ `Subscription_ack of subscription_ack
  | `Heartbeat of heartbeat
  | `Order_event of order_event
  | `Order_events of order_event list
  ] [@@deriving sexp]

include Ws.CHANNEL
  with type uri_args := uri_args
  with type query := query
  with type response := response

val command : string * Async.Command.t

val client :
  nonce:int Pipe.Reader.t ->
  (module Cfg.S) ->
  ?query:Sexp.t list ->
  ?uri_args:uri_args ->
  unit -> response Pipe.Reader.t Deferred.t


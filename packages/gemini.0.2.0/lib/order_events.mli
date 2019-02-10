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
type heartbeat = {timestampms:Timestamp.Ms.t;
                  sequence:Int_number.t;
                  trace_id:string;
                  socket_sequence:Int_number.t
                 } [@@deriving sexp, yojson]


(** Represents different order event types. *)
module Order_event_type : sig

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
           ] [@@deriving sexp, enumerate, compare]

  include Json.S with type t := t
  include Comparable with type t := t

end

(** Events can be filtered by symbol using [Symbol_filter symbol] or
    for an event type using [Order_event_type_filter event_type] or using
    an api session filter [Api_session_filter session-id].
*)
type query = [ `Symbol_filter of Symbol.t
             | `Event_type_filter of Order_event_type.t
             | `Api_session_filter of string
             ] [@@deriving sexp]

(** Represents differents reasons an order event occurred. *)
module Reason : sig
  type t =
    [`Place | `Trade | `Cancel | `Initial] [@@deriving sexp, enumerate]
  include Json.ENUM_STRING with type t := t
end


module Reject_reason :
sig
  type t = [`Invalid_quantity] [@@deriving sexp, enumerate]
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
module Fill :
sig
  type t = {
    trade_id:Int_string.t;
    liquidity:Liquidity.t;
    price:Decimal_string.t;
    amount:Decimal_string.t;
    fee:Decimal_string.t;
    fee_currency:Currency.t;
  } [@@deriving sexp, yojson, fields, csv]
end

(** Represents an api session name *)
module Api_session : sig

  (** An api session name. *)
  type t = string [@@deriving sexp, yojson]
end


module Order_event : sig
(** The type of an order event. *)
type t =
  {order_id:string;
   api_session:Api_session.t;
   client_order_id:string option [@default None];
   event_id:string option [@default None];
   order_type:Order_type.t;
   symbol:Symbol.t;
   reason:Reject_reason.t option [@default None];
   side:Side.t;
   behavior:string option [@default None];
   type_ : Order_event_type.t [@key "type"];
   timestamp:Timestamp.t;
   timestampms:Timestamp.Ms.t;
   is_live : bool;
   is_cancelled : bool;
   is_hidden : bool;
   avg_execution_price : Decimal_string.t option [@default None];
   executed_amount : Decimal_string.t option [@default None];
   remaining_amount : Decimal_string.t option [@default None];
   original_amount : Decimal_string.t option [@default None];
   price : Decimal_string.t option [@default None];
   total_spend : Decimal_string.t option [@default None];
   fill : Fill.t option [@default None];
   socket_sequence:Int_number.t
  } [@@deriving sexp, yojson, fields, csv]
end

module Subscription_ack : sig
(** The type of a subscription acknowledgement event. *)
type t =
  {account_id:Int_number.t [@key "accountId"];
   subscription_id:string [@key "subscriptionId"];
   symbol_filter:Symbol.t list [@key "symbolFilter"];
   api_session_fiter:string list [@key "apiSessionFilter"];
   event_type_filter:Order_event_type.t list [@key "eventTypeFilter"]
  } [@@deriving sexp, yojson, fields, csv]
end
(** The response type for any order message. *)
type response =
  [ `Subscription_ack of Subscription_ack.t
  | `Heartbeat of heartbeat
  | `Order_event of Order_event.t
  | `Order_events of Order_event.t list
  ] [@@deriving sexp]

module Event_type : sig
  type t = [`Order_event | `Subscription_ack]
  [@@deriving sexp, enumerate, compare]
  include Comparable.S with type t := t
  include Json.S with type t := t
end


include Ws.CHANNEL
  with module Event_type := Event_type
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


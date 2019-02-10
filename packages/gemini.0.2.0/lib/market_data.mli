(** Market data websockets api for the Gemini trading exchange. This
    broadcasts only public events and require no authentication.
*)
open! Common

(** Encapsulates various concepts of side in the market dat api. *)
module Side :
sig

  (** Represents a bid or ask side. *)
  module Bid_ask :
  sig
    type t = [ `Ask | `Bid ] [@@deriving sexp]
    include Json.S with type t := t
  end

  (** Represents an auction style side. *)
  module Auction :
  sig
    type t = [ `Auction ] [@@deriving sexp]
    include Json.S with type t := t
  end

  (** A most general type of side- one of [`Bid], [`Ask], or [`Auction]. *)
  type t = [ Bid_ask.t | Auction.t] [@@deriving sexp]

  include Json.S with type t := t
end

(** Represents market data message types supported by the Gemini exchange. *)
module Message_type :
sig
  (* The support message types for order events -
     either [`Update] or [`Heartbeat].
  *)
  type t = [`Heartbeat | `Update] [@@deriving sexp]
  include Json.S with type t := t
end

(** Represents different types of market data events *)
module Event_type :
sig
  (** Market data event types. One of  [`Trade], [`Change], [`Auction],
      or [`Block_trade].
  *)
  type t = [`Trade | `Change | `Auction | `Block_trade] [@@deriving sexp]
  include Json.S with type t := t
  include Comparable with type t := t
end

(** The heartbeat response type has no payload *)
type heartbeat = unit [@@deriving sexp, of_yojson]


(** Different reasons a market data event occured *)
module Reason :
sig
  (** Reasons a market data event occured *)
  type t =
    [`Place | `Trade | `Cancel | `Initial]
  [@@deriving sexp]
  include Json.S with type t := t
end

(** An change event to an order. The [reaosn] field indicates
    the type of change. *)
module Change_event :
sig
  type t = {
    price : Decimal_string.t;
    side : Side.Bid_ask.t;
    reason : Reason.t;
    remaining : Decimal_string.t;
    delta : Decimal_string.t;
  } [@@deriving sexp, of_yojson, csv, fields]
end

(** An trade event of an order. *)
module Trade_event :
sig
  type t = {
    tid : Int_number.t;
    price : Decimal_string.t;
    amount : Decimal_string.t;
    maker_side : Side.t;
  } [@@deriving sexp, of_yojson, fields, csv]
end

(** A block trade event. *)
module Block_trade_event :
sig
  type t = {
    price : Decimal_string.t;
    amount : Decimal_string.t;
  } [@@deriving sexp, of_yojson, fields, csv]
end

(** An auction open event. *)
module Auction_open_event :
sig
  type t = {
    auction_open_ms : Timestamp.Ms.t;
    auction_time_ms : Timestamp.Ms.t;
    first_indicative_ms : Timestamp.Ms.t;
    last_cancel_time_ms : Timestamp.Ms.t;
  } [@@deriving sexp, of_yojson, fields, csv]
end


(** Represents different results possible from an auction. *)
module Auction_result :
sig
  (** the type of an auction result- one of [`Success] or [`Failure]. *)
  type t =
    [`Success | `Failure] [@@deriving sexp]
  include Json.S with type t := t
end


(** An auction indicative price event. *)
module Auction_indicative_price_event :
sig
  type t = {
    eid : Int_number.t;
    result : Auction_result.t;
    time_ms : Timestamp.Ms.t;
    highest_bid_price : Decimal_string.t;
    lowest_ask_price : Decimal_string.t;
    collar_price : Decimal_string.t;
    indicative_price : Decimal_string.t;
    indicative_quantity : Decimal_string.t;
  } [@@deriving sexp, of_yojson, fields, csv]
end

(** An auction outcome event. *)
module Auction_outcome_event :
sig
  type t = {
    eid : Int_number.t;
    result : Auction_result.t;
    time_ms : Timestamp.Ms.t;
    highest_bid_price : Decimal_string.t;
    lowest_ask_price : Decimal_string.t;
    collar_price : Decimal_string.t;
    auction_price : Decimal_string.t;
    auction_quantity : Decimal_string.t;
  } [@@deriving sexp, of_yojson, fields, csv]
end

(** Represents different auction event types. *)
module Auction_event_type :
sig

  (** Enumerates an auction event type. *)
  type t =
    [ `Auction_open | `Auction_indicative_price | `Auction_outcome ]
  [@@deriving sexp]
  include Json.S with type t := t
end

module Auction_event : sig
(** The type of an auction event, unified over all auction event types. *)
type t =
  [ `Auction_indicative_price of Auction_indicative_price_event.t
  | `Auction_open of Auction_open_event.t
  | `Auction_outcome of Auction_outcome_event.t ]
[@@deriving sexp, of_yojson]
end

(** The type of event, unified over auction, change, and trade events. *)
type event =
  [ `Auction of Auction_event.t
  | `Change of Change_event.t
  | `Trade of Trade_event.t
  | `Block_trade of Block_trade_event.t
  ]
[@@deriving sexp, of_yojson]


(** The type of a market data update message. *)
type update = {
  event_id : Int_number.t;
  events : event array;
  timestamp : Timestamp.Sec.t option;
  timestampms : Timestamp.Ms.t option;
} [@@deriving sexp, of_yojson]


(** The type of a market data message- a heartbeat or update. *)
type message =
  [ `Heartbeat of heartbeat | `Update of update ]
[@@deriving sexp]


(** The type of a market data response. *)
type response = {
  socket_sequence : Int_number.t;
  message : message;
} [@@deriving sexp]

include Ws.CHANNEL
  with module Event_type := Event_type
  with type uri_args = Symbol.t
  with type query = unit
  with type response := response

val client :
  (module Cfg.S) ->
  ?query:Sexp.t sexp_list ->
  ?uri_args:uri_args -> unit ->
  response Pipe.Reader.t Deferred.t

val command : string * Command.t

(** A strongly typed gemini trading api written in pure OCaml
    with both websocket and rest endpoints supported.

    Submodules of the [V1] module are either REST operations which
    can be invoked via a well typed invocation of [Operation.post],
    or they are web socket interfaces such as [Market_data] and
    [Order_events]. Use the [client] function to get a typed response pipe over
    the socket of these services.

    All service invocations require a [Cfg] module which is usually
    provided from the command line or environment variables for api host and
    secret information.
*)

open Common
module Auth = Auth
module Result = Json.Result


(** Version v1 of the Gemini REST and web socket apis. *)
module V1 : sig
  val path : string list

  (** Heartbeat REST api operation. *)
  module Heartbeat :
    sig
      type request = unit [@@deriving sexp, of_yojson]
      type response = { result : bool; } [@@deriving sexp]
      include Rest.Operation.S
        with type request := request
        with type response := response
      val post :
        (module Cfg.S) ->
        Nonce.reader ->
        request ->
        [
        | Rest.Error.post
        | `Ok of response
        ] Deferred.t
    end
  module Timestamp : module type of Timestamp
  module Currency : module type of Currency
  module Symbol : module type of Symbol
  module Exchange : module type of Exchange
  module Side : module type of Side
  module Order_type : module type of Order_type

  (** Represents different execution rules for an order. *)
  module Order_execution_option :
    sig

      (** The type of an order execution rule. *)
      type t =
        [ `Auction_only | `Immediate_or_cancel | `Maker_or_cancel ]
      include Json.S with type t := t
    end

  (** Represents an order on the Gemini trading exchange over
      the REST api. *)
  module Order :
    sig
      val name : string
      val path : string list

      (** Represents the status of an order on the Gemini trading
          exchange over the REST api. *)
      module Status :
      sig
        type request =
          { order_id : Int_number.t; } [@@deriving sexp, of_yojson]
       type response = {
          client_order_id : string option;
          order_id : Int_string.t;
          id : Int_string.t;
          symbol : Symbol.t;
          exchange : Exchange.t;
          avg_execution_price : Decimal_string.t;
          side : Side.t;
          type_ : Order_type.t;
          timestamp : Timestamp.Sec.t;
          timestampms : Timestamp.Ms.t;
          is_live : bool;
          is_cancelled : bool;
          is_hidden : bool;
          was_forced : bool;
          executed_amount : Decimal_string.t;
          remaining_amount : Decimal_string.t;
          options : Order_execution_option.t list;
          price : Decimal_string.t;
          original_amount : Decimal_string.t;
        } [@@deriving sexp]
        include Rest.Operation.S
          with type request := request
          with type response := response
        val post :
          (module Cfg.S) ->
          Nonce.reader ->
          request ->
          [
            | Rest.Error.post
            | `Ok of response
          ] Deferred.t
        val command : string * Command.t
      end

      (** Represents a new order request on the Gemini trading exchange
          over the REST api. *)
      module New :
        sig
          type request = {
            client_order_id : string;
            symbol : Symbol.t;
            amount : Decimal_string.t;
            price : Decimal_string.t;
            side : Side.t;
            type_ : Order_type.t;
            options : Order_execution_option.t list;
          } [@@deriving sexp, of_yojson]
          type response = Status.response [@@deriving sexp]
          include Rest.Operation.S
            with type request := request
            with type response := response
          val post :
            (module Cfg.S) ->
            Nonce.reader ->
            request ->
            [
              | Rest.Error.post
              | `Ok of response
            ] Deferred.t
          val command : string * Command.t
        end

      (** Represents order cancellation features on the
          Gemini trading exchange over the REST api. *)
      module Cancel :
        sig
          val name : string
          val path : string list
          module By_order_id :
            sig
              type request =
                { order_id : Int_string.t; } [@@deriving sexp, of_yojson]
              type response = Status.response [@@deriving sexp]
              include Rest.Operation.S
                with type request := request
                with type response := response
               val post :
                (module Cfg.S) ->
                Nonce.reader ->
                request ->
                [
                  | Rest.Error.post
                  | `Ok of response
                ] Deferred.t
              val command : string * Command.t
            end
          type details = {
            cancelled_orders : Status.response list;
            cancel_rejects : Status.response list;
          } [@@deriving sexp, yojson]
          module All :
            sig
              type request = unit [@@deriving sexp, of_yojson]
              type response =
                { details : details; } [@@deriving sexp]
              include Rest.Operation.S
                with type request := request
                with type response := response
              val post :
                (module Cfg.S) ->
                Nonce.reader ->
                request ->
                [
                  | Rest.Error.post
                  | `Ok of response
                ] Deferred.t
              val command : string * Command.t
            end
          module Session :
            sig
              type request = unit [@@deriving sexp, of_yojson]
              type response = { details : details; }
              [@@deriving sexp]
              include Rest.Operation.S
                with type request := request
                with type response := response
              val post :
                (module Cfg.S) ->
                Nonce.reader ->
                request ->
                [
                  | Rest.Error.post
                  | `Ok of response
                ] Deferred.t
              val command : string * Command.t
            end
          val command : string * Command.t
        end
      val command : string * Command.t
    end

   (** Gets the status of all open orders on
       Gemini trading exchange over the REST api. *)
   module Orders :
    sig
      type request = unit [@@deriving sexp, of_yojson]
      type response = Order.Status.response list [@@deriving sexp]
      include Rest.Operation.S
        with type request := request
        with type response := response
       val post :
        (module Cfg.S) ->
        Nonce.reader ->
        request ->
        [
          | Rest.Error.post
          | `Ok of response
        ] Deferred.t
      val command : string * Command.t
    end

   (** Gets all trades executed by this api user on
       Gemini trading exchange over the REST api. *)
   module Mytrades :
    sig

      (** Represents one instance of a trade exchanged on Gemini. *)
      type trade = {
        price : Decimal_string.t;
        amount : Decimal_string.t;
        timestamp : Timestamp.Sec.t;
        timestampms : Timestamp.Ms.t;
        type_ : Side.t;
        aggressor : bool;
        fee_currency : Currency.t;
        fee_amount : Decimal_string.t;
        tid : Int_number.t;
        order_id : Int_string.t;
        client_order_id : string option;
        is_auction_fill : bool;
        exchange : Exchange.t;
      } [@@deriving sexp, yojson]


      (** Trade request parameters. *)
      type request = {
        symbol : Symbol.t;
        limit_trades : int option;
        timestamp : Timestamp.Sec.t option;
      } [@@deriving sexp, of_yojson]


      (** Mytrades repsonse type- the type of a list trades. *)
      type response = trade list [@@deriving sexp]

      include Rest.Operation.S
        with type request := request
        with type response := response
       val post :
        (module Cfg.S) ->
        Nonce.reader ->
        request ->
        [
          | Rest.Error.post
          | `Ok of response
        ] Deferred.t
      val command : string * Command.t
    end
  (** Gets all trade volume executed by on the
      Gemini trading exchange over the REST api. *)
  module Tradevolume :
    sig

      (** The type of a trade volume entity for
          one particular symbol on the Gemini trading exchange.
      *)
      type volume = {
        account_id : Int_number.t;
        symbol : Symbol.t;
        base_currency : Currency.t;
        notional_currency : Currency.t;
        data_date : string;
        total_volume_base : Decimal_number.t;
        maker_buy_sell_ratio : Decimal_number.t;
        buy_maker_base : Decimal_number.t;
        buy_maker_notional : Decimal_number.t;
        buy_maker_count : Int_number.t;
        sell_maker_base : Decimal_number.t;
        sell_maker_notional : Decimal_number.t;
        sell_maker_count : Int_number.t;
        buy_taker_base : Decimal_number.t;
        buy_taker_notional : Decimal_number.t;
        buy_taker_count : Int_number.t;
        sell_taker_base : Decimal_number.t;
        sell_taker_notional : Decimal_number.t;
        sell_taker_count : Int_number.t;
      } [@@deriving sexp, yojson]
      type request = unit [@@deriving sexp, of_yojson]

      (** The type of a trade volume response, which is a list of list
          of volumes grouped by exchange symbol. *)
      type response = volume list list [@@deriving sexp]
      include Rest.Operation.S
        with type request := request
        with type response := response
       val post :
        (module Cfg.S) ->
        Nonce.reader ->
        request ->
        [
          | Rest.Error.post
          | `Ok of response
        ] Deferred.t
      val command : string * Command.t
    end

   (** Gets all balances for this api user on the
       Gemini trading exchange over the REST api. *)
   module Balances :
    sig

      (** Balance queries have no request parameters. *)
      type request = unit [@@deriving sexp, of_yojson]

      (** The type of a balance for one specific currency. *)
      type balance =
      {
        currency : Currency.t;
        amount : Decimal_string.t;
        available : Decimal_string.t;
        available_for_withdrawal : Decimal_string.t;
        type_ : string;
      } [@@deriving sexp, yojson]

      (* A list of balances, one for each supported currency. *)
      type response = balance list [@@deriving sexp]
      include Rest.Operation.S
        with type request := request
        with type response := response
       val post :
        (module Cfg.S) ->
        Nonce.reader ->
        request ->
        [
          | Rest.Error.post
          | `Ok of response
        ] Deferred.t
      val command : string * Command.t
    end
  module Market_data = Market_data
  val command : Command.t
end

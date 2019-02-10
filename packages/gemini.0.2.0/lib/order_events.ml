open Common
module Request = Nonce.Request

module T = struct
  let name = "orderevents"
  let version = v1
  let path = version::["order";"events"]
  type uri_args = [`None] [@@deriving sexp, enumerate]

  let authentication = `Private

  let default_uri_args = None
  let encode_uri_args _ =
    failwith "uri path arguments not support for order events"

  module Message_type = struct
    module T = struct
      type t = [`Update | `Heartbeat] [@@deriving sexp, enumerate]
      let to_string = function
        | `Update -> "update"
        | `Heartbeat -> "heartbeat"
    end
    include T
    include (Json.Make(T) : Json.S with type t := t)
  end

  type heartbeat = {timestampms:Timestamp.Ms.t;
                    sequence:Int_number.t;
                    trace_id:string;
                    socket_sequence:Int_number.t
                   } [@@deriving sexp, yojson]

 module Order_event_type = struct
    module T = struct

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

      let to_string : t -> string = function
        | `Subscription_ack -> "subscription_ack"
        | `Heartbeat -> "heartbeat"
        | `Initial -> "initial"
        | `Accepted -> "accepted"
        | `Rejected -> "rejected"
        | `Booked -> "booked"
        | `Fill -> "fill"
        | `Cancelled -> "cancelled"
        | `Closed -> "closed"
    end
    include T
    include Comparable.Make(T)
    include (Json.Make(T) : Json.S with type t := t)
  end


 type query =
   [ `Symbol_filter of Symbol.t
   | `Event_type_filter of Order_event_type.t
   | `Api_session_filter of string
   ] [@@deriving sexp]

  let encode_query = function
      | `Symbol_filter symbol ->
        "symbolFilter", Symbol.to_string symbol
      | `Event_type_filter event_type ->
        "eventTypeFilter", Order_event_type.to_string event_type
      | `Api_session_filter session ->
        "apiSessionFilter", session

   module Reason = struct
    module T = struct
      type t =
        [`Place | `Trade | `Cancel | `Initial] [@@deriving sexp, enumerate]
      let to_string = function
        | `Place -> "place"
        | `Trade -> "trade"
        | `Cancel -> "cancel"
        | `Initial -> "initial"
    end
    include T
    include (Json.Make(T) : Json.S with type t := t)
  end

   module Reject_reason = struct
    module T = struct
      type t =
        [`Invalid_quantity] [@@deriving sexp, enumerate]
      let to_string = function
        | `Invalid_quantity -> "InvalidQuantity"
    end
    include T
    include (Json.Make(T) : Json.S with type t := t)
  end

  module Liquidity = struct
  module T = struct
  type t = [`Taker] [@@deriving sexp, enumerate]
  (* TODO determine other liquidities *)
  let to_string = function
    | `Taker -> "Taker"
end
  include T
  include (Json.Make(T) : Json.S with type t := t)
  end

  module Fill = struct
    type t = {
      trade_id:Int_string.t;
      liquidity:Liquidity.t;
      price:Decimal_string.t;
      amount:Decimal_string.t;
      fee:Decimal_string.t;
      fee_currency:Currency.t;
    } [@@deriving sexp, yojson, fields, csv]
  end

  module Api_session = struct
    (* TODO make an enum from UI, and whatever the other values are *)
    type t = string [@@deriving sexp, yojson]
    include
      (Csvfields.Csv.Atom(String) : Csvfields.Csv.Csvable with type t := t)
  end

  module Decimal_string_option =
    Csv_support.Optional.Make
      (Csv_support.Optional.Default_args(Decimal_string))


  module Fill_option =
  struct
    module T =
    struct
      type t = Fill.t option [@@deriving yojson, sexp]
      let of_string  = function
        | "" -> None
        | s -> String.split ~on:' ' s |> Fill.t_of_row |> Option.some

      let to_string = function
        | None -> ""
        | Some fill -> Fill.row_of_t fill |> String.concat ~sep:" "
    end
    include T
    include (Csvfields.Csv.Atom(T) :  Csvfields.Csv.Csvable with type t := t)
  end

  module Reject_reason_option =
    Csv_support.Optional.Make_default(Reject_reason)

  module Order_event = struct
    open Csv_support
  type t =
    {order_id:string;
     api_session:Api_session.t;
     client_order_id:Optional.String.t [@default None];
     event_id:Optional.String.t [@default None];
     order_type:Order_type.t;
     symbol:Symbol.t;
     reason:Reject_reason_option.t [@default None];
     side:Side.t;
     behavior:Optional.String.t [@default None] (* TODO make enum *);
     type_ : Order_event_type.t [@key "type"];
     timestamp:Timestamp.t;
     timestampms:Timestamp.Ms.t;
     is_live : bool;
     is_cancelled : bool;
     is_hidden : bool;
     avg_execution_price : Decimal_string_option.t [@default None];
     executed_amount : Decimal_string_option.t [@default None];
     remaining_amount : Decimal_string_option.t [@default None];
     original_amount : Decimal_string_option.t [@default None];
     price : Decimal_string_option.t [@default None];
     total_spend : Decimal_string_option.t [@default None];
     fill : Fill_option.t [@default None];
     socket_sequence:Int_number.t
    } [@@deriving sexp, yojson, fields, csv]
end

module Event_type_list = Csv_support.List.Make_default(Order_event_type)
module Symbol_list = Csv_support.List.Make_default(Symbol)

module Subscription_ack = struct
type t =
  {account_id:Int_number.t [@key "accountId"];
   subscription_id:string [@key "subscriptionId"];
   symbol_filter:Symbol_list.t [@key "symbolFilter"];
   api_session_fiter:Csv_support.List.String.t [@key "apiSessionFilter"];
   event_type_filter:Event_type_list.t [@key "eventTypeFilter"]
  } [@@deriving sexp, yojson, fields, csv]
end

type response =
  [ `Subscription_ack of Subscription_ack.t
  | `Heartbeat of heartbeat
  | `Order_event of Order_event.t
  | `Order_events of Order_event.t list
  ] [@@deriving sexp]


let response_of_yojson :
    Yojson.Safe.json -> ('a, string) Result.t = function
    | `Assoc assoc as json ->
      (List.Assoc.find assoc ~equal:String.equal
         "type" |> function
       | None ->
         Log.Global.debug "no type in event payload: %s"
           (Yojson.Safe.to_string json);
         Subscription_ack.of_yojson json |>
         Result.map ~f:(fun event -> `Subscription_ack event);
       | Some event_type ->
         Log.Global.debug "found type in event payload: %s"
           (Yojson.Safe.to_string json);
          Order_event_type.of_yojson event_type |> function
         | Result.Error _ as e -> e
         | Result.Ok event_type ->
           let json' = `Assoc
               (List.Assoc.remove ~equal:String.equal assoc "type") in
           (match event_type with
            | `Heartbeat ->
              heartbeat_of_yojson json' |>
              Result.map ~f:(fun event -> `Heartbeat event)
            | `Subscription_ack ->
              Subscription_ack.of_yojson json' |>
              Result.map ~f:(fun event -> `Subscription_ack event)
            | #Order_event_type.t ->
              Order_event.of_yojson json |>
              Result.map ~f:(fun event -> `Order_event event)
           )
      )

    | (`List l : Yojson.Safe.json) ->
      Result.(all (List.map l ~f:Order_event.of_yojson) |>
              map ~f:(fun x -> `Order_events x)
             )
    | #Yojson.Safe.json as json ->
      Result.failf "expected association type in json payload: %s"
        (Yojson.Safe.to_string json)


  module Event_type = struct
    module T =
    struct
      type t = [`Order_event | `Subscription_ack]
      [@@deriving sexp, yojson, enumerate, compare]

      let to_string = function
        | `Order_event -> "order_event"
        | `Subscription_ack -> "subscription_ack"
    end

    include T
    include Comparable.Make(T)
    include (Json.Make(T) : Json.S with type t := t)
  end

  module Csv_of_event = Ws.Csv_of_event(Event_type)
  let events_of_response (response:response) =
    let csv_of_event = Csv_of_event.empty in
    match response with
    | `Order_event order_event ->
      Csv_of_event.add' csv_of_event `Order_event
        (module Order_event) [order_event]
    | `Heartbeat _ -> csv_of_event
    | `Order_events order_events ->
      Csv_of_event.add' csv_of_event `Order_event
        (module Order_event) order_events
    | `Subscription_ack ack ->
      Csv_of_event.add' csv_of_event `Subscription_ack
        (module Subscription_ack) [ack]

end

include T
include Ws.Make(T)


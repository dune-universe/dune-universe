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
    include Json.Make(T)
  end

  type heartbeat = {timestampms:Timestamp.ms;
                    sequence:int_number;
                    trace_id:string;
                    socket_sequence:int_number
                   } [@@deriving sexp, yojson]

 module Event_type = struct
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
               ] [@@deriving sexp, enumerate]

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
    include Json.Make(T)
  end


    type query = [ `Symbol_filter of Symbol.t
               | `Event_type_filter of Event_type.t
               | `Api_session_filter of string
               ] [@@deriving sexp]

  let encode_query = function
      | `Symbol_filter symbol ->
        "symbolFilter", Symbol.to_string symbol
      | `Event_type_filter event_type ->
        "eventTypeFilter", Event_type.to_string event_type
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
    include Json.Make(T)
  end

  module Liquidity = struct
  module T = struct
  type t = [`Taker] [@@deriving sexp, enumerate]
  (* TODO determine other liquidities *)
  let to_string = function
    | `Taker -> "Taker"
end
  include T
  include Json.Make(T)
  end

  type fill = {
    trade_id:int_string;
    liquidity:Liquidity.t;
    price:decimal_string;
    amount:decimal_string;
    fee:decimal_string;
    fee_currency:Currency.t;
  } [@@deriving sexp, yojson]

  module Api_session = struct
    (* TODO make an enum from UI, and whatever the other values are *)
    type t = string [@@deriving sexp,yojson]
  end

  type order_event =
    {order_id:string;
     api_session:Api_session.t;
     client_order_id:string option [@default None];
     event_id:string option [@default None];
     order_type:Order_type.t;
     symbol:Symbol.t;
     side:Side.t;
     behavior:string option [@default None] (* TODO make enum *);
     type_ : Event_type.t [@key "type"] (* TODO make subtype *);
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

type subscription_ack =
  {account_id:int_number [@key "accountId"];
   subscription_id:string [@key "subscriptionId"];
   symbol_filter:Symbol.t list [@key "symbolFilter"];
   api_session_fiter:string list [@key "apiSessionFilter"];
   event_type_filter:Event_type.t list [@key "eventTypeFilter"]
  } [@@deriving sexp, yojson]

type response =
  [ `Subscription_ack of subscription_ack
  | `Heartbeat of heartbeat
  | `Order_event of order_event
  | `Order_events of order_event list
  ] [@@deriving sexp]


let response_of_yojson :
    Yojson.Safe.json -> ('a, string) Result.t = function
    | `Assoc assoc as json ->
      (List.Assoc.find assoc ~equal:String.equal
         "type" |> function
       | None ->
         Log.Global.debug "no type in event payload: %s"
           (Yojson.Safe.to_string json);
         subscription_ack_of_yojson json |>
         Result.map ~f:(fun event -> `Subscription_ack event);
       | Some event_type ->
         Log.Global.debug "found type in event payload: %s"
           (Yojson.Safe.to_string json);
          Event_type.of_yojson event_type |> function
         | Result.Error _ as e -> e
         | Result.Ok event_type ->
           let json' = `Assoc
               (List.Assoc.remove ~equal:String.equal assoc "type") in
           (match event_type with
            | `Heartbeat ->
              heartbeat_of_yojson json' |>
              Result.map ~f:(fun event -> `Heartbeat event)
            | `Subscription_ack ->
              subscription_ack_of_yojson json' |>
              Result.map ~f:(fun event -> `Subscription_ack event)
            | #Event_type.t ->
              order_event_of_yojson json |>
              Result.map ~f:(fun event -> `Order_event event)
           )
      )

    | (`List l : Yojson.Safe.json) ->
      Result.(all (List.map l ~f:order_event_of_yojson) |>
              map ~f:(fun x -> `Order_events x)
             )
    | #Yojson.Safe.json as json ->
      Result.failf "expected association type in json payload: %s"
        (Yojson.Safe.to_string json)

end
include T
include Ws.Make(T)



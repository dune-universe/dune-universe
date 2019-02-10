open Common
module Side =
struct
  module Bid_ask = struct
    module T = struct
      type t = [`Bid | `Ask] [@@deriving sexp, enumerate]
      let to_string : [<t] -> string = function
        | `Bid -> "bid"
        | `Ask -> "ask"
    end
    include T
    include (Json.Make(T) : Json.S with type t := t)
  end

  module Auction = struct
    module T = struct
      type t = [`Auction] [@@deriving sexp, enumerate]
      let to_string = function
          `Auction -> "auction"
    end
    include T
    include (Json.Make(T) : Json.S with type t := t)
  end

  module T = struct
    type t = [Bid_ask.t | Auction.t] [@@deriving sexp, enumerate]
    let to_string : [<t] -> string = function
      | #Bid_ask.t as bid_ask -> Bid_ask.to_string bid_ask
      | #Auction.t as auction -> Auction.to_string auction
  end
  include T
  include (Json.Make(T) : Json.S with type t := t)
end

module T = struct
  let name = "marketdata"
  let version = "v1"
  let path = v1::["marketdata"]
  type uri_args = Symbol.t [@@deriving sexp, yojson, enumerate]

  let authentication = `Public
  let default_uri_args = Some `Ethusd
  let encode_uri_args = Symbol.to_string

  type query = unit [@@deriving sexp]
  let encode_query _ = failwith "queries not supported"

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

  module Event_type = struct
    module T = struct
      type t = [`Trade | `Change | `Auction | `Block_trade]
      [@@deriving sexp, enumerate, compare]

      let to_string = function
        | `Trade -> "trade"
        | `Change -> "change"
        | `Auction -> "auction"
        | `Block_trade -> "block_trade"

    end
    include T
    include Comparable.Make(T)
    include (Json.Make(T) : Json.S with type t := t)
  end

  type heartbeat = unit [@@deriving sexp, of_yojson]

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

  module Change_event = struct
     type t =
       {price:Decimal_string.t;
        side:Side.Bid_ask.t;
        reason:Reason.t;
        remaining:Decimal_string.t;
        delta:Decimal_string.t
    } [@@deriving sexp, of_yojson, fields, csv]
  end


  module Trade_event = struct
    type t =
      {tid:Int_number.t;
       price:Decimal_string.t;
       amount:Decimal_string.t;
       maker_side:Side.t [@key "makerSide"]
      } [@@deriving of_yojson, sexp, fields, csv]
  end

 module Block_trade_event = struct
   type t =
     {price:Decimal_string.t;
      amount:Decimal_string.t
     } [@@deriving of_yojson, sexp, fields, csv]
 end

 module Auction_open_event = struct
  type t =
    {auction_open_ms:Timestamp.Ms.t;
     auction_time_ms:Timestamp.Ms.t;
     first_indicative_ms:Timestamp.Ms.t;
     last_cancel_time_ms:Timestamp.Ms.t;
    } [@@deriving sexp, of_yojson, fields, csv]
end


  module Auction_result = struct
    module T = struct
      type t =
        [`Success | `Failure] [@@deriving sexp, enumerate]
      let to_string = function
        | `Success -> "success"
        | `Failure -> "failure"
    end
    include T
    include (Json.Make(T) : Json.S with type t := t)
  end

  module Auction_indicative_price_event = struct
    type t =
      {eid:Int_number.t;
       result:Auction_result.t;
       time_ms:Timestamp.Ms.t;
       highest_bid_price:Decimal_string.t;
       lowest_ask_price:Decimal_string.t;
       collar_price:Decimal_string.t;
       indicative_price:Decimal_string.t;
       indicative_quantity:Decimal_string.t
      } [@@deriving sexp, of_yojson, fields, csv]
  end


  module Auction_outcome_event = struct
  type t =
    {eid:Int_number.t;
     result:Auction_result.t;
     time_ms:Timestamp.Ms.t;
     highest_bid_price:Decimal_string.t;
     lowest_ask_price:Decimal_string.t;
     collar_price:Decimal_string.t;
     auction_price:Decimal_string.t;
     auction_quantity:Decimal_string.t
    } [@@deriving sexp, of_yojson, fields, csv]
  end

  module Auction_event_type = struct
    module T = struct
      type t =
        [ `Auction_open
        | `Auction_indicative_price
        | `Auction_outcome ]
      [@@deriving sexp, enumerate]

      let to_string = function
        | `Auction_open ->
          "auction_open"
        | `Auction_indicative_price ->
          "auction_indicative_price"
        | `Auction_outcome ->
          "auction_outcome"
    end
    include T
    include (Json.Make(T) : Json.S with type t := t)
  end

  module Auction_event = struct
  type t =
    [
      | `Auction_open of Auction_open_event.t
      | `Auction_indicative_price of
          Auction_indicative_price_event.t
      | `Auction_outcome of Auction_outcome_event.t
    ] [@@deriving sexp]


  let of_yojson :
    Yojson.Safe.json -> (t, string) Result.t = function
    | `Assoc assoc as json ->
      (List.Assoc.find assoc ~equal:String.equal
         "type" |> function
       | None ->
         Result.failf "no auction event type in json payload: %s"
           (Yojson.Safe.to_string json)
       | Some event_type ->
         Auction_event_type.of_yojson event_type |> function
         | Result.Error _ as e -> e
         | Result.Ok event_type ->
           let json' = `Assoc
               (List.Assoc.remove ~equal:String.equal assoc "type") in
           (match event_type with
            | `Auction_open ->
              Auction_open_event.of_yojson json' |>
              Result.map ~f:(fun event -> `Auction_open event)
            | `Auction_indicative_price ->
              Auction_indicative_price_event.of_yojson json' |>
              Result.map ~f:(fun event -> `Auction_indicative_price event)
            | `Auction_outcome ->
              Auction_outcome_event.of_yojson json' |>
              Result.map ~f:(fun event -> `Auction_outcome event)
           )
      )
    | #Yojson.Safe.json as json ->
      Result.failf "expected association type in json payload: %s"
        (Yojson.Safe.to_string json)
end

  type event =
    [ `Change of Change_event.t
    | `Trade of Trade_event.t
    | `Auction of Auction_event.t
    | `Block_trade of Block_trade_event.t
    ] [@@deriving sexp]

  let event_of_yojson :
    Yojson.Safe.json -> (event, string) Result.t = function
    | `Assoc assoc as json ->
      (List.Assoc.find assoc ~equal:String.equal
         "type" |> function
       | None ->
         Result.failf "no event type in json payload: %s"
           (Yojson.Safe.to_string json)
       | Some event_type ->
         Event_type.of_yojson event_type |> function
         | Result.Error _ as e -> e
         | Result.Ok event_type ->
           let json' = `Assoc
               (List.Assoc.remove ~equal:String.equal assoc "type") in
           (match event_type with
            | `Change ->
              Change_event.of_yojson json' |>
              Result.map ~f:(fun event -> `Change event)
            | `Trade ->
              Trade_event.of_yojson json' |>
              Result.map ~f:(fun event -> `Trade event)
            | `Auction ->
              Auction_event.of_yojson json' |>
              Result.map ~f:(fun event -> `Auction event)
            | `Block_trade ->
              Block_trade_event.of_yojson json' |>
              Result.map ~f:(fun event -> `Block_trade event)
           )
      )
    | #Yojson.Safe.json as json ->
      Result.failf "expected association type in json payload: %s"
        (Yojson.Safe.to_string json)


  type update =
    { event_id : Int_number.t [@key "eventId"];
      events : event array;
      timestamp : Timestamp.Sec.t option [@default None];
      timestampms : Timestamp.Ms.t option [@default None]
    } [@@deriving sexp, of_yojson]

  type message =
    [`Heartbeat of heartbeat | `Update of update] [@@deriving sexp]

  type response =
    {
      socket_sequence : Int_number.t;
      message : message
    } [@@deriving sexp]

  let response_of_yojson :
    Yojson.Safe.json -> (response, string) Result.t = function
    | `Assoc assoc as json ->
      (
        (
          List.Assoc.find ~equal:String.equal assoc "socket_sequence",
          List.Assoc.find ~equal:String.equal assoc "type"
        ) |> function
        | (None, _) ->
          Result.failf "no sequence number in json payload: %s"
            (Yojson.Safe.to_string json)
        | (_, None) ->
          Result.failf "no message type in json payload: %s"
            (Yojson.Safe.to_string json)
        | (Some socket_sequence, Some message_type) ->
          Result.both
            (Int_number.of_yojson socket_sequence)
            (Message_type.of_yojson message_type) |> function
          | Result.Error _ as e -> e
          | Result.Ok (socket_sequence, message_type) ->
            let json' = `Assoc
                (List.Assoc.remove
                   ~equal:String.equal assoc "type" |> fun assoc ->
                 List.Assoc.remove
                   ~equal:String.equal assoc "socket_sequence"
                ) in
            (
              (match message_type with
               | `Heartbeat ->
                 heartbeat_of_yojson json' |>
                 Result.map ~f:(fun event -> `Heartbeat event)
               | `Update ->
                 update_of_yojson json' |>
                 Result.map ~f:(fun event -> `Update event)
              )
              |> Result.map
                ~f:(fun message -> {socket_sequence;message})
            )
      )
    | #Yojson.Safe.json as json ->
      Result.failf "response_of_yojson:expected association type \
                    in json payload: %s"
        (Yojson.Safe.to_string json)

  module Csv_of_event = Ws.Csv_of_event(Event_type)

  let events_of_response (response:response) =
    let csv_of_events = Csv_of_event.empty in
    match response.message with
    | `Heartbeat _ -> csv_of_events
    | `Update (update:update) ->
      Array.fold ~init:csv_of_events update.events
        ~f:
          (fun csv_of_events (event:event) ->
             (match event with
              | `Change change ->
                Csv_of_event.add' csv_of_events `Change
                  (module Change_event)
                  [change]
              | `Trade trade ->
                Csv_of_event.add' csv_of_events `Trade
                  (module Trade_event)
                  [trade]
              | `Auction _auction -> csv_of_events
              | `Block_trade block_trade ->
                Csv_of_event.add' csv_of_events `Block_trade
                  (module Block_trade_event)
                  [block_trade]
             )
          )
end
include T
include Ws.Make_no_request(T)



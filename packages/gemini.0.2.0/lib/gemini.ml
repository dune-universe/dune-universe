open Common


module Auth = Auth
module Result = Result

module V1 = struct
  let path = ["v1"]
  module Side = Side
  module Symbol = Symbol
  module Exchange = Exchange
  module Timestamp = Timestamp
  module Market_data = Market_data
  module Order_events = Order_events
  module Currency = Currency
  module Order_type = Order_type

  module Heartbeat = struct
    module T = struct
      let name = "heartbeat"
      let path = path@["heartbeat"]
      type request = unit [@@deriving sexp, yojson]
      type response = {result:bool [@default true]}
      [@@deriving sexp, of_yojson]
    end
    include T
    include Rest.Make_no_arg(T)
  end

  module Order_execution_option = struct

    module T = struct
      type t =
        [`Maker_or_cancel
        |`Immediate_or_cancel
        |`Auction_only
        ] [@@deriving sexp, enumerate]

      let to_string = function
        | `Maker_or_cancel -> "maker_or_cancel"
        | `Immediate_or_cancel -> "immediate_or_cancel"
        | `Auction_only -> "auction_only"

    end
    include T
    include (Json.Make(T) : Json.S with type t:= t)

  end



  module Order =
  struct
    let name = "order"
    let path = path@["order"]

    module Status =
    struct
      module T = struct
        let name = "status"
        let path = path@["status"]

        type request = {
          order_id:Int_number.t;
        } [@@deriving yojson, sexp]

        type response = {
          client_order_id : string option [@default None];
          order_id : Int_string.t;
          id : Int_string.t;
          symbol : Symbol.t;
          exchange : Exchange.t;
          avg_execution_price : Decimal_string.t;
          side : Side.t;
          type_ : Order_type.t [@key "type"];
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
        } [@@deriving yojson, sexp]
      end
      include T
      include Rest.Make(T)
    end

    module New = struct
      module T = struct
        let name = "new"
        let path = path@["new"]

        type request = {
          client_order_id:string;
          symbol:Symbol.t;
          amount:Decimal_string.t;
          price:Decimal_string.t;
          side:Side.t;
          type_:Order_type.t [@key "type"];
          options: Order_execution_option.t list;
        } [@@deriving sexp, yojson]

        type response = Status.response [@@deriving of_yojson, sexp]
      end
      include T
      include Rest.Make(T)

    end

    module Cancel = struct
      let name = "cancel"
      let path = path@["cancel"]

      module By_order_id = struct
        module T = struct
          let name = "by-order-id"
          let path = path

          type request = {order_id:Int_string.t} [@@deriving sexp, yojson]

          type response = Status.response [@@deriving sexp, of_yojson]
        end
        include T
        include Rest.Make(T)
      end
      type details =
        {cancelled_orders:Status.response list [@key "cancelledOrders"];
         cancel_rejects:Status.response list [@key "cancelRejects"]
        } [@@deriving sexp, yojson]

      module All = struct
        module T = struct
          let name = "all"
          let path = path@["all"]
          type request = unit [@@deriving sexp, yojson]

          type response = {details:details} [@@deriving sexp, of_yojson]
        end
        include T
        include Rest.Make_no_arg(T)
      end

      module Session = struct
        module T = struct
          let name = "session"
          let path = path@["session"]
          type request = unit [@@deriving sexp, yojson]
          type response = {details:details} [@@deriving sexp, of_yojson]
        end
        include T
        include Rest.Make_no_arg(T)
      end

      let command : string * Command.t =
        (name,
         Command.group
           ~summary:(Path.to_summary ~has_subnames:true path)
           [By_order_id.command;
            Session.command;
            All.command
           ]
        )
    end

    let command : string * Command.t =
      (name,
       Command.group
         ~summary:(Path.to_summary ~has_subnames:true path)
         [New.command;
          Cancel.command;
          Status.command
         ]
      )

  end

  module Orders = struct

    module T = struct
      let name = "orders"
      let path = path@["orders"]

      type request = unit [@@deriving sexp, yojson]
      type response =
        Order.Status.response list [@@deriving of_yojson, sexp]
    end
    include T
    include Rest.Make_no_arg(T)
  end

  module Mytrades = struct
    type trade = {price:Decimal_string.t;
                  amount:Decimal_string.t;
                  timestamp:Timestamp.Sec.t;
                  timestampms:Timestamp.Ms.t;
                  type_: Side.t [@key "type"];
                  aggressor: bool;
                  fee_currency: Currency.t;
                  fee_amount : Decimal_string.t;
                  tid:Int_number.t;
                  order_id : Int_string.t;
                  client_order_id : string option [@default None];
                  is_auction_fill : bool;
                  exchange : Exchange.t;
                  (*break : string option [@default None] (* TODO make enum *) *)
                 } [@@deriving yojson, sexp]
    module T = struct
      let name = "mytrades"
      let path = path@["mytrades"]
      type request =
        { symbol : Symbol.t;
          limit_trades: int option [@default None];
          timestamp: Timestamp.Sec.t option [@default None]
        } [@@deriving sexp, yojson]
      type response = trade list [@@deriving of_yojson, sexp]
    end
    include T
    include Rest.Make(T)

  end

  module Tradevolume = struct

    type volume =
      {account_id:Int_number.t;
       symbol:Symbol.t;
       base_currency:Currency.t;
       notional_currency:Currency.t;
       data_date:string; (*TODO use timestamp or a date module with MMMM-DD-YY *)
       total_volume_base:Decimal_number.t;
       maker_buy_sell_ratio:Decimal_number.t;
       buy_maker_base:Decimal_number.t;
       buy_maker_notional:Decimal_number.t;
       buy_maker_count:Int_number.t;
       sell_maker_base:Decimal_number.t;
       sell_maker_notional:Decimal_number.t;
       sell_maker_count:Int_number.t;
       buy_taker_base:Decimal_number.t;
       buy_taker_notional:Decimal_number.t;
       buy_taker_count:Int_number.t;
       sell_taker_base:Decimal_number.t;
       sell_taker_notional:Decimal_number.t;
       sell_taker_count:Int_number.t;
      } [@@deriving yojson, sexp]
    module T = struct
      let name = "tradevolume"
      let path = path@["tradevolume"]
      type request = unit [@@deriving yojson, sexp]
      type response = volume list list [@@deriving of_yojson, sexp]
    end
    include T
    include Rest.Make_no_arg(T)
  end

  module Balances = struct

    module T = struct
      let name = "balances"
      let path = path@["balances"]

      type request = unit [@@deriving yojson, sexp]
      type balance =
        {currency:Currency.t;
         amount:Decimal_string.t;
         available:Decimal_string.t;
         available_for_withdrawal:Decimal_string.t
             [@key "availableForWithdrawal"];
         type_: string [@key "type"]
        } [@@deriving yojson, sexp]
      type response = balance list [@@deriving of_yojson, sexp]
    end

    include T
    include Rest.Make_no_arg(T)
  end

  let command : Command.t =
    Command.group
      ~summary:"Gemini Command System"
      [Heartbeat.command;
       Order.command;
       Orders.command;
       Mytrades.command;
       Tradevolume.command;
       Balances.command;
       Market_data.command;
       Order_events.command
      ]

end

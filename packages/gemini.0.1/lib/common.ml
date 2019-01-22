(** Shared modules across various Gemini api endpoints *)

module Auth = Auth

module Result = Json.Result

type int_number = int64 [@encoding `number] [@@deriving sexp, yojson]
type int_string = int64 [@encoding `string] [@@deriving sexp, yojson]
type decimal_number = float [@encoding `number] [@@deriving sexp, yojson]
type decimal_string = string [@@deriving sexp, yojson]

(** Represents an order side. *)
module Side =
struct
  module T = struct

    (** The type of an order side - [`Buy] or [`Sell]. *)
    type t = [`Buy | `Sell] [@@deriving sexp, enumerate]

    let to_string = function
      | `Buy -> "buy"
      | `Sell -> "sell"
  end
  include T
  include Json.Make(T)
end


(** A symbol on the gemini exchange - Symbols are two currency
    names appended together which can be thought as
    "Buy the first one, Sell the second one". So [`Btcusd] implies
    you are buying btc and sell usd, effectively exchanging your
    currency from usd to btc in the process.
*)
module Symbol = struct
  module T = struct
    (** The type of a symbol pair. See the [Symbol] module for details. *)
    type t =
      [ `Btcusd | `Ethusd | `Ethbtc
      | `Zecusd | `Zecbtc | `Zeceth | `Zecbch | `Zecltc
      | `Ltcusd | `Ltcbtc | `Ltceth | `Ltcbch
      | `Bchusd | `Bchbtc | `Bcheth
      ]
    [@@deriving sexp, enumerate]

    let to_string : [<t] -> string = function
      | `Btcusd -> "btcusd"
      | `Bchusd -> "bchusd"
      | `Bchbtc -> "bchbtc"
      | `Bcheth -> "bcheth"
      | `Ethusd -> "ethusd"
      | `Ethbtc -> "ethbtc"
      | `Zecusd -> "zecusd"
      | `Zecbtc -> "zecbtc"
      | `Zeceth -> "zeceth"
      | `Zecbch -> "zecbch"
      | `Zecltc -> "zecltc"
      | `Ltcusd -> "ltcusd"
      | `Ltcbtc -> "ltcbtc"
      | `Ltceth -> "ltceth"
      | `Ltcbch -> "ltcbch"


  end
  include T
  include Json.Make(T)

end

(** Represents an exchange type. Only gemini is currently supported *)
module Exchange = struct

  module T = struct
    (** The exchange type - gemini only, currently. *)
    type t = [`Gemini] [@@deriving sexp, enumerate]
    let to_string `Gemini = "gemini"
  end
  include T
  include Json.Make(T)
end


(** Represents all styles of timestamps possibly returned
    by various gemini endpoints. *)
module Timestamp = struct


  (** A timestamp is just a core time instance that
      was converted from some raw json date. *)
  type t = Time.t [@@deriving sexp]

  (** Alias for millisecond granularity timestamps. *)
  type ms = t [@@deriving sexp]

  (** Alias for second granularity timestamps. *)
  type sec = t [@@deriving sexp]

  let to_yojson t =
    Time.to_span_since_epoch t |>
    Time.Span.to_ms |>
    Float.to_string_hum ~decimals:0 |>
    fun s -> `String s

  let of_yojson_with_span span_fn json =
    (match json with
    | `String s ->
      `Ok (Float.of_string s)
    | `Int i ->
      `Ok (Float.of_int i)
    | `Int64 i ->
      `Ok (Float.of_int64 i)
    | #Yojson.Safe.json as json ->
      `Error json
    ) |>
    function
     | `Error json ->
      Result.Error
        (sprintf "expected float as json but got %S"
           (Yojson.Safe.pretty_to_string json))
     | `Ok f ->
       span_fn f |>
       Time.of_span_since_epoch |>
       fun ok -> Result.Ok ok


  let of_yojson (ms:Yojson.Safe.json) =
    of_yojson_with_span Time.Span.of_ms ms

  let ms_of_yojson = of_yojson

  let ms_to_yojson (ms:ms) = to_yojson ms

  let sec_of_yojson (sec:Yojson.Safe.json) =
    of_yojson_with_span Time.Span.of_sec sec
  let sec_to_yojson (sec:sec) = to_yojson sec

end

(** Represents currencies supported by Gemini. *)
module Currency = struct

  module T = struct

    (** An enumerated set of all supported currencies supported
        currently by Gemini.
    *)
    type t = [`Eth | `Btc | `Usd | `Zec | `Bch | `Ltc]
    [@@deriving sexp, enumerate]
    let to_string = function
      | `Eth -> "eth"
      | `Btc -> "btc"
      | `Usd -> "usd"
      | `Zec -> "zec"
      | `Bch -> "bch"
      | `Ltc -> "ltc"
  end
  include T
  include Json.Make(T)

end


(** Represents order types supported on Gemini. *)
module Order_type = struct
  module T = struct

    (** The type of order types- only [`Exchange_limit] is
        currently supported. *)
    type t = [`Exchange_limit] [@@deriving sexp, enumerate]
    let to_string = function
      | `Exchange_limit -> "exchange limit"
  end
  include T
  include Json.Make(T)

end

(** The protocol version. *)
let v1 = "v1"

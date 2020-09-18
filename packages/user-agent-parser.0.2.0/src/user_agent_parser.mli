module UAParser : sig
  type t
  type result = {
    family : string;
    major : string option;
    minor : string option;
    patch : string option;
  } [@@deriving eq, show]
  val init : unit -> t
  val parse : t -> string -> result
end

module OSParser : sig
  type t
  type result = {
    family : string;
    major : string option;
    minor : string option;
    patch : string option;
    patch_minor : string option;
  } [@@deriving eq, show]
  val init : unit -> t
  val parse : t -> string -> result
end

module DeviceParser : sig
  type t
  type result = {
    family : string;
    brand : string option;
    model : string option;
  } [@@deriving eq, show]
  val init : unit -> t
  val parse : t -> string -> result
end

type t = {
  ua_parser: UAParser.t;
  os_parser: OSParser.t;
  device_parser: DeviceParser.t;
}

type result = {
  ua: UAParser.result;
  os: OSParser.result;
  device: DeviceParser.result;
} [@@deriving eq, show]

val init : unit -> t
val parse : t -> string -> result

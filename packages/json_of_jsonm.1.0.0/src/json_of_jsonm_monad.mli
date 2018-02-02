type json =
  [ `Null
  | `Bool of bool
  | `Float of float
  | `String of string
  | `List of json list
  | `Assoc of (string * json) list
  ]

module type IO = sig
  type 'a t

  val return : 'a -> 'a t
  val (>>=)  : 'a t -> ('a -> 'b t) -> 'b t
end

module type Json_encoder_decoder = sig
  module IO : IO

  type nonrec json = json

  (** [decode] decodes the byte stream provided by [reader]. [reader buf len] reads
      up to [len] bytes into [buf] and returns the number of bytes read. *)
  val decode : reader:(Bytes.t -> int -> int IO.t) -> (json, string) result IO.t

  (** [decode_exn] - the same as [decode] but raises on error *)
  val decode_exn : reader:(Bytes.t -> int -> int IO.t) -> json IO.t

  (** [decode_string] - decode a [string] to a [json] type *)
  val decode_string : string -> (json, string) result IO.t

  (** [decode_string_exn] - the same as [decode_sting] but raises on error *)
  val decode_string_exn : string -> json IO.t

  (** [encode] encodes the supplied [json] type using [writer[ to output the text.
      [writer buf len] writes [len] bytes from [buf] and returns [init]. 
      returns and error if a float [NaN] or [Inf] is encountered in the [json]
      type *)
  val encode : writer:(Bytes.t -> int -> unit IO.t) -> json -> (unit, string) result IO.t

  (** [encode_exn] - the same as [encode] but raises on error *)
  val encode_exn : writer:(Bytes.t -> int -> unit IO.t) -> json -> unit IO.t

  (** [encode_string] - encode a [json] type to a  [string] *)
  val encode_string : json -> (string, string) result IO.t

  (** [encode_string_exn] - the same as [encode_string]  but raises on error *)
  val encode_string_exn : json -> string IO.t

  (** [encode_string_hum] - same as [encode_string] but formats the output for
      humans to read *)
  val encode_string_hum : json -> (string, string) result IO.t
end


module Make(IO : IO) : Json_encoder_decoder with module IO := IO

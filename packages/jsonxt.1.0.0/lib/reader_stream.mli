module type Reader_stream = sig
  type json_stream
  type stream

  (** The steaming reader provides two interfaces to decode input into a stream of
      [json_stream] elements
      - The internal create_* and decode_stream mechanism
      - A stream_from_* [json_stream Stream.t] based approach

      Note that the returned stream has no structure thus [[1,2,3]] will be returned
      as the stream [`Ae, `Int 1, `Int 2, `Int 3, `Ae] (assuming basic or extended
      compliance) *)

  (** {2 json_stream_* and decode functions} *)

  (** [json_stream_of_string s] creates a stream parser that returns
      a series of [json_stream] elements from the string [s].  *)
  val json_stream_of_string : string -> stream

  (** [json_stream_of_channel ic] creates a stream parser that returns
      a series of [json_stream] elements from the [in_channel] [ic].  *)
  val json_stream_of_channel : in_channel -> stream

  (** [json_stream_of_function f] creates a stream parser that returns a
      series of [json_stream] elements from the reader function [f]. [f buf len]
      takes a buffer [buf] and maximum length [len] and returns the
      number of bytes read to a maximum of [len] *)
  val json_stream_of_function : (bytes -> int -> int) -> stream

  (** [decode_stream t] decode the next element of the input, [Ok None] indicates
      end of the stream *)
  val decode_stream : stream -> (json_stream option, string) result

  (** {2 [Stream.t] functions}
      These functions provide a [Stream.t] API on top of the 
      json_stream_of_* and decode_stream functions.
      *)

  (** [stream_from_string s] creates a [json_stream Stream.t] from [json_stream_of_string s] 
      and [decode_stream] *)
  val stream_from_string : string -> json_stream Stream.t

  (** [stream_from_channel ic] creates a [json_stream Stream.t] from [json_stream_of_channel ic] 
      and [decode_stream] *)
  val stream_from_channel : in_channel -> json_stream Stream.t

  (** [stream_from_function f] creates a [json_stream Stream.t] from [json_stream_of_function f] 
      and [decode_stream] *)
  val stream_from_function : (bytes -> int -> int) -> json_stream Stream.t
end

module Make (Lexxer : Compliant_lexxer.Lex ) (Parser : Parser_stream.Parser) : Reader_stream
  with type json_stream = Parser.Compliance.json_stream

module type Intf = sig
  type t

  (** [create_encoder' ~add_char ~add_string ~incr ~eol] is the low level function
      used to create the various stream encoders.  The [add_char c] and [add_string s]
      functions handle adding a [char] [c] and [string] [s] to the output respectively.
      [incr] and [eol] work together to output human readable output. [incr]
      defines the increase in indentation and [eol] the end of line sequence.
      A type [t] is returned *)
  val create_encoder'
    :  add_char:(char -> unit)
    -> add_string:(string -> unit)
    -> incr:int
    -> eol:string
    -> t

  (** [create_encoder ~add_char ~add_string] creates a compact encoder. [add_char]
      and [add_string] add a [char] and [string] to the output respectively
      A type [t] is returned *)
  val create_encoder
    :  add_char:(char -> unit)
    -> add_string:(string -> unit)
    -> t

  (** [create_encoder_hum ~add_char ~add_string] creates a human readable encoder.
      [add_char] and [add_string] add a [char] and [string] to the output respectively.
      The increment is set to 2 and end of line LF (\n).  A type [t] is returned *)
  val create_encoder_hum
    :  add_char:(char -> unit)
    -> add_string:(string -> unit)
    -> t

  (** [create_encoder_channel oc] creates a compact encoder outputing
      to channel [oc] *)
  val create_encoder_channel : out_channel -> t

  (** [create_encoder_channel_hum oc] creates a human readable encoder outputing
      to channel [oc] *)
  val create_encoder_channel_hum : out_channel -> t

  (** [encode_stream_exn t json_stream] encodes and outputs the element [json_stream].
      Errors cause a Failure exception to be raised. *)
  val encode_stream_exn : t -> 'a Json_internal.constrained_stream -> unit

  (** [encode_stream_exn t json_stream] encodes and outputs the element [json_stream].
      Errors are reported via the [result] value. *)
  val encode_stream : t -> 'a Json_internal.constrained_stream -> (unit, string) result
end

module Make (Compliance : Compliance.S) : Intf

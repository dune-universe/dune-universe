module type Intf = sig
  type json

  (** [json_to_string json] converts [json] to a [string], returning an error if the
      json value contains data that fails compliance checks *)
  val json_to_string : json -> (string, string) result

  (** [json_to_string_exn json] converts [json] to a [string], raising a [Failure] excepion
      if the json value contains data that fails compliance checks *)
  val json_to_string_exn : json -> string

  (** [to_string] is an alias for json_to_string_exn *)
  val to_string : json -> string

  (** [json_to_string_hum json] converts [json] to a [string] in human readable format,
      returning an error if the json value contains data that fails compliance checks *)
  val json_to_string_hum : json -> (string, string) result

  (** [json_to_string_hum_exn json] converts [json] to a [string] in human readable format,
      raising a [Failure] excepion if the json value contains data that fails compliance checks *)
  val json_to_string_hum_exn : json -> string

  (** [to_string_hum] is an alias for json_to_string_hum_exn *)
  val to_string_hum : json -> string

  (** [json_to_file file json] converts [json] to a string and writes it to [file],
      returning an error if the json value contains data that fails compliance checks.
      The file will be closed on error. *)
  val json_to_file : string -> json -> (unit, string) result

  (** [json_to_file_hum file json] converts [json] to a string in human readable format
      and writes it to [file], returning an error if the json value contains data that
      fails compliance checks.  The file will be closed on error. *)
  val json_to_file_hum : string -> json -> (unit, string) result

  (** [json_to_file_exn file json] converts [json] to a string and writes it to [file],
      raising a [Failure] exception  if the json value contains data that fails compliance checks.
      The file will be closed on error. *)
  val json_to_file_exn : string -> json -> unit

  (** [json_to_file_hum_exn file json] converts [json] to a string in human readable format
      and writes it to [file], raising [Failure] exception  if the json value contains data that
      fails compliance checks.  The file will be closed on error. *)
  val json_to_file_hum_exn : string -> json -> unit

  (** [json_to_channel channel json] converts [json] to a string and writes it to [channel],
      returning an error if the json value contains data that fails compliance checks.
      The channel is not closed. *)
  val json_to_channel :  out_channel -> json -> (unit, string) result

  (** [json_to_channel_exn channel json] converts [json] to a string and writes it to [channel],
      raising a [Failure] exception  if the json value contains data that fails compliance checks.
      The channel will be closed on error. *)
  val json_to_channel_exn :  out_channel -> json -> unit

  (** [json_to_channel_hum channel json] converts [json] to a string in human readable format
      and writes it to [channel], returning an error if the json value contains data that
      fails compliance checks.  The channel is not closed. *)
  val json_to_channel_hum :  out_channel -> json -> (unit, string) result

  (** [json_to_channel_hum_exn channel json] converts [json] to a string in human readable format
      and writes it to [channel], raising [Failure] exception  if the json value contains data that
      fails compliance checks.  The channel is not closed *)
  val json_to_channel_hum_exn :  out_channel -> json -> unit

  (** [to_file] is an alias for json_to_file_exn *)
  val to_file : string -> json -> unit

  (** [to_file_hum] is an alias for json_to_file_hum_exn *)
  val to_file_hum : string -> json -> unit

  (** [to_channel] is an alias for json_to_channel_exn *)
  val to_channel :  out_channel -> json -> unit

  (** [to_channel_hum] is an alias for json_to_channel_hum_exn *)
  val to_channel_hum :  out_channel -> json -> unit

  (** [json_to_buffer buf json] converts and outputs [json] to the supplied [buf], returning
       an error  if the json value contains data that fails compliance checks.  *)
  val json_to_buffer : Buffer.t -> json -> (unit, string) result

  (** [json_to_buffer_exn buf json] converts and outputs [json] to the supplied [buf], raising
       a [Failure] exception  if the json value contains data that fails compliance checks.  *)
  val json_to_buffer_exn : Buffer.t -> json -> unit

  (** [json_to_buffer_hum buf json] converts and outputs [json] in a human readable format to
      the supplied [buf], returning an eror if the json value contains data that fails
      compliance checks.  *)
  val json_to_buffer_hum : Buffer.t -> json -> (unit, string) result

  (** [json_to_buffer_hum_exn buf json] converts and outputs [json] in a human readable format to
      the supplied [buf], raising a [Failure] exception  if the json value contains data
      that fails compliance checks.  *)
  val json_to_buffer_hum_exn : Buffer.t -> json -> unit

  (** [to_buffer] is an alias for json_to_buffer_exn *)
  val to_buffer : Buffer.t -> json -> unit

  (** [to_buffer_hum] is an alias for json_to_buffer_hum_exn *)
  val to_buffer_hum : Buffer.t -> json -> unit

  (** [pretty_print out json] pretty prints the [json] tree to the [Formater.formatter]
      The output is more compact than the _hum versions but still readable *)
  val pretty_print : Format.formatter -> json -> unit

  (** [pretty_print_to_string json] converts the [json] tree into a pretty printed string.
      The output is more compact than the _hum versions but still readable *)
  val pretty_print_to_string : json -> string

  (** [pretty_print oc json] pretty prints the [json] tree to the output channel [out]
      The output is more compact than the _hum versions but still readable *)
  val pretty_print_to_channel : out_channel -> json -> unit

  (** [stream_to_string stream] converts a [Stream.t] of [json] values to a string, separating the enties
      with newlines *)
  val stream_to_string : json Stream.t -> string

  (** [stream_to_channel out_channel] converts a [Stream.t] of [json] values to a newline separated list
      of compact json strings and outputs them to [out_channel] *)
  val stream_to_channel : out_channel -> json Stream.t -> unit

  (** [stream_to_file stream file] converts a [Stream.t] of [json] values to a newline separated list of compact json strings and
      outputs them to [file]  *)
  val stream_to_file : string -> json Stream.t -> unit

  (** [stream_to_buffer buf stream] converts a [Stream.t] of [json] values to compact strings and outputs them,
      separating by newlines, to [buf] *)
  val stream_to_buffer : Buffer.t -> json Stream.t -> unit
end


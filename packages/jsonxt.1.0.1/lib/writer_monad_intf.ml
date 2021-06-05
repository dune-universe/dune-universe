module type Intf = sig
  module IO : Io.IO
  type json

  (** [json_writer ~writer ~eol ~incr ~psep json] converts [json] to a string [s]
      and writes it out using the [writer string] function.  [incr], [eol] and [psep]
      work together to output human readable output. [incr] defines the increase
      in indentation, [eol] the end of line sequence and [psep] the string to
      seperate the : from the value in objects

      The [writer string] function takes a string and returns a [unit IO.t]
  *)
  val json_writer
       : writer:(string -> unit IO.t)
      -> eol:string
      -> incr:int
      -> psep:string
      -> json
      -> unit IO.t

  (** [create_encoder ~writer] creates a compact encoder using [json_writer].  *)
  val write_json : writer:(string -> unit IO.t) -> json -> unit IO.t

  (** [create_encoder ~writer] creates a human readable encoder using [json_writer]
      with [incr] set to 2 and eol to '\n'. *)
  val write_json_hum : writer:(string -> unit IO.t) -> json -> unit IO.t
end

module type Intf = sig
  val json_to_string : 'a Json_internal.constrained -> (string, string) result
  val json_to_string_exn : 'a Json_internal.constrained -> string
  val to_string : 'a Json_internal.constrained -> string
  val json_to_string_hum : 'a Json_internal.constrained -> (string, string) result
  val json_to_string_hum_exn : 'a Json_internal.constrained -> string
  val to_string_hum : 'a Json_internal.constrained -> string
  val json_to_buffer : Buffer.t -> 'a Json_internal.constrained -> (unit, string) result
  val json_to_buffer_exn : Buffer.t -> 'a Json_internal.constrained -> unit
  val json_to_buffer_hum : Buffer.t -> 'a Json_internal.constrained -> (unit, string) result
  val json_to_buffer_hum_exn : Buffer.t -> 'a Json_internal.constrained -> unit
  val to_buffer : Buffer.t -> 'a Json_internal.constrained -> unit
  val to_buffer_hum : Buffer.t -> 'a Json_internal.constrained -> unit
  val stream_to_string : 'a Json_internal.constrained Stream.t -> string
  val stream_to_buffer : Buffer.t -> 'a Json_internal.constrained Stream.t -> unit
end

module Make (Compliance : Compliance.S) : Intf

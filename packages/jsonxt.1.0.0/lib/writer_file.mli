module type Intf = sig
  val json_to_file : string -> 'a Json_internal.constrained -> (unit, string) result
  val json_to_file_exn : string -> 'a Json_internal.constrained -> unit
  val json_to_channel :  out_channel ->  'a Json_internal.constrained -> (unit, string) result
  val json_to_channel_exn :  out_channel ->  'a Json_internal.constrained -> unit
  val json_to_file_hum : string -> 'a Json_internal.constrained -> (unit, string) result
  val json_to_file_hum_exn : string -> 'a Json_internal.constrained -> unit
  val json_to_channel_hum :  out_channel -> 'a Json_internal.constrained -> (unit, string) result
  val json_to_channel_hum_exn :  out_channel -> 'a Json_internal.constrained -> unit
  val to_file : string -> 'a Json_internal.constrained -> unit
  val to_file_hum : string -> 'a Json_internal.constrained -> unit
  val to_channel :  out_channel -> 'a Json_internal.constrained -> unit
  val to_channel_hum :  out_channel -> 'a Json_internal.constrained -> unit
  val stream_to_channel : out_channel -> 'a Json_internal.constrained Stream.t -> unit
  val stream_to_file : string -> 'a Json_internal.constrained Stream.t -> unit
end

module Make (Compliance : Compliance.S) : Intf

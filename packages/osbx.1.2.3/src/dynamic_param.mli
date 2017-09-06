module Common : sig
  val silence_settings         : Progress_report.silence_settings ref

  val set_silence_settings     : Progress_report.silence_level -> unit
end

module Decode : sig
  val failure_list_max_length              : int64 ref
  val set_failure_list_max_length          : int64        -> unit
  val set_failure_list_max_length_possibly : int64 option -> unit
end

module Show : sig
  val meta_list_max_length                 : int64 ref
  val set_meta_list_max_length             : int64        -> unit
  val set_meta_list_max_length_possibly    : int64 option -> unit
end

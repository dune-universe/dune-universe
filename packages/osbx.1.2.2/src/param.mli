module Common : sig
  val block_scan_alignment     : int

  val silence_settings         : Progress_report.silence_settings ref

  val set_silence_settings     : Progress_report.silence_level option -> unit
end

module Encode : sig
  val progress_report_interval : float

  module Encode_progress : sig
    val display_while_active    : Progress_report.progress_element list
    val display_on_finish       : Progress_report.progress_element list
    val display_on_finish_early : Progress_report.progress_element list
  end
end

module Decode : sig
  val progress_report_interval : float

  module Scan_progress : sig
    val display_while_active    : Progress_report.progress_element list
    val display_on_finish       : Progress_report.progress_element list
    val display_on_finish_early : Progress_report.progress_element list
  end

  module Hash_progress : sig
    val display_while_active    : Progress_report.progress_element list
    val display_on_finish       : Progress_report.progress_element list
    val display_on_finish_early : Progress_report.progress_element list
  end

  module Decode_progress : sig
    val display_while_active    : Progress_report.progress_element list
    val display_on_finish       : Progress_report.progress_element list
    val display_on_finish_early : Progress_report.progress_element list
  end

  val failure_list_max_length              : int64 ref
  val set_failure_list_max_length          : int64        -> unit
  val set_failure_list_max_length_possibly : int64 option -> unit
end

module Rescue : sig
  val progress_report_interval : float

  module Rescue_progress : sig
    val display_while_active    : Progress_report.progress_element list
    val display_on_finish       : Progress_report.progress_element list
    val display_on_finish_early : Progress_report.progress_element list
  end

  val log_write_interval       : float
end

module Show : sig
  val progress_report_interval : float

  module Show_progress : sig
    val display_while_active    : Progress_report.progress_element list
    val display_on_finish       : Progress_report.progress_element list
    val display_on_finish_early : Progress_report.progress_element list
  end

  val meta_list_max_length     : int64 ref
  val set_meta_list_max_length          : int64        -> unit
  val set_meta_list_max_length_possibly : int64 option -> unit
end

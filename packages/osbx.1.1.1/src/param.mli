module Encode : sig
  val progress_report_interval : float
end

module Decode : sig
  val ref_block_scan_alignment : int
  val progress_report_interval : float
  val failure_list_max_length  : int64
end

module Rescue : sig
  val scan_alignment           : int
  val progress_report_interval : float
  val log_write_interval       : float
end

module Show : sig
  val progress_report_interval : float
  val meta_list_max_length     : int64
end

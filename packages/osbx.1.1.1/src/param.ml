module Common = struct
  let block_scan_alignment     = 128  (* largest common divisor of version 1, 2, 3 block sizes *)
end

module Encode = struct
  let progress_report_interval = 0.2  (* update every 200ms *)
end

module Decode = struct
  let ref_block_scan_alignment = Common.block_scan_alignment
  let progress_report_interval = 0.2  (* update every 200ms *)
  let failure_list_max_length  = 500L
end

module Rescue = struct
  let scan_alignment           = Common.block_scan_alignment
  let progress_report_interval = 0.2  (* update every 200ms *)
  let log_write_interval       = 0.1  (* write  every 100ms *)
end

module Show = struct
  let progress_report_interval = 0.2  (* update every 200ms *)
  let meta_list_max_length     = 100L
end

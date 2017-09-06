module Common = struct
  let block_scan_alignment     = 128  (* largest common divisor of version 1, 2, 3 block sizes *)
end

module Rescue = struct
  let log_write_interval       = 1.0  (* write  every 1000ms *)
end

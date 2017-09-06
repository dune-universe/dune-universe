open Progress_report
open Misc_utils

module Common = struct
  let silence_settings         = ref default_silence_settings

  let set_silence_settings (level:silence_level) : unit =
    silence_settings := Progress_report.Helper.silence_level_to_silence_settings level
  ;;
end

module Decode = struct
  let failure_list_max_length  = ref 100L

  let set_failure_list_max_length (n:int64) : unit =
    failure_list_max_length := ensure_at_least ~at_least:0L n
  ;;

  let set_failure_list_max_length_possibly (n:int64 option) : unit =
    match n with
    | Some n -> failure_list_max_length := ensure_at_least ~at_least:0L n
    | None   -> ()
  ;;
end

module Show = struct
  let meta_list_max_length     = ref 0L

  let set_meta_list_max_length (n:int64) : unit =
    meta_list_max_length := ensure_at_least ~at_least:0L n
  ;;

  let set_meta_list_max_length_possibly (n:int64 option) : unit =
    match n with
    | Some n -> meta_list_max_length := ensure_at_least ~at_least:0L n
    | None   -> ()
  ;;
end

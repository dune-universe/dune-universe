type silence_level = L0 | L1

type silence_settings =
  { silent_while_active : bool
  ; silent_when_done    : bool
  }

type ('a, 'b, 'c) progress_print_functions =
  { print_progress            :
      start_time_src:'a   ->
      units_so_far_src:'b ->
      total_units_src:'c  ->
      unit
  ; print_newline_if_not_done :
      start_time_src:'a   ->
      units_so_far_src:'b ->
      total_units_src:'c  ->
      unit
  }

type progress_element = Percentage
                      | Progress_bar
                      | Current_rate_short
                      | Average_rate_short
                      | Time_used_short
                      | Time_left_short
                      | Current_rate_long
                      | Average_rate_long
                      | Time_used_long
                      | Time_left_long

type info = { percent   : int
            ; cur_time  : float
            ; cur_rate  : float
            ; avg_rate  : float
            ; unit      : string
            ; time_used : float
            ; time_left : float
            }

module Helper = struct
  let seconds_to_hms (total_secs:int) : int * int * int =
    let hour   : int = total_secs  / (60 * 60) in
    let minute : int = (total_secs - hour * 60 * 60) / 60 in
    let second : int = (total_secs - hour * 60 * 60 - minute * 60) in
    (hour, minute, second)
  ;;

  let calc_percent ~(units_so_far:int64) ~(total_units:int64) : int =
    Int64.to_int (Int64.div
                    (Int64.mul
                       100L
                       units_so_far)
                    total_units) 
  ;;

  let make_readable_rate ~(rate:float) ~(unit:string) : string =
    let rate_string : string =
      if      rate >  1_000_000_000_000. then
        let adjusted_rate =
          rate     /. 1_000_000_000_000. in
        Printf.sprintf "%6.2f%c" adjusted_rate 'T'
      else if rate >      1_000_000_000. then
        let adjusted_rate =
          rate     /.     1_000_000_000. in
        Printf.sprintf "%6.2f%c" adjusted_rate 'G'
      else if rate >          1_000_000. then
        let adjusted_rate =
          rate     /.         1_000_000. in
        Printf.sprintf "%6.2f%c" adjusted_rate 'M'
      else if rate >              1_000. then
        let adjusted_rate =
          rate     /.             1_000. in
        Printf.sprintf "%6.0f%c" adjusted_rate 'K'
      else
        Printf.sprintf "%7.0f"   rate in
    String.concat "" [rate_string; " "; unit; "/s"]
  ;;

  let make_progress_bar ~(percent:int) : string =
    let fill_char   = '#' in
    let empty_char  = '-' in
    let total_len   = 25 in
    let filled_len  = total_len * percent / 100 in
    let empty_len   = total_len - filled_len in
    let filled_part = String.make filled_len fill_char  in
    let empty_part  = String.make empty_len  empty_char in
    String.concat "" ["["; filled_part; empty_part; "]"]
  ;;

  let silence_level_to_silence_settings (level:silence_level option) : silence_settings =
    match level with
    | None    -> { silent_while_active = false; silent_when_done = false }
    | Some L0 -> { silent_while_active = true ; silent_when_done = false }
    | Some L1 -> { silent_while_active = true ; silent_when_done = true  }
  ;;
end

let default_silence_settings = Helper.silence_level_to_silence_settings None;;

let make_message ~(info:info) ~(elements:progress_element list) : string =
  let { percent; cur_time; cur_rate; avg_rate; unit; time_used; time_left } = info in
  let make_string_for_element (element:progress_element) : string =
    match element with
    | Percentage         -> Printf.sprintf "%3d%%" percent
    | Progress_bar       -> Helper.make_progress_bar ~percent
    | Current_rate_short -> String.concat "" ["cur : "; (Helper.make_readable_rate ~rate:cur_rate ~unit)]
    | Current_rate_long  -> String.concat "" ["Current rate : "; (Helper.make_readable_rate ~rate:cur_rate ~unit)]
    | Average_rate_short -> String.concat "" ["avg : "; (Helper.make_readable_rate ~rate:avg_rate ~unit)]
    | Average_rate_long  -> String.concat "" ["Average rate : "; (Helper.make_readable_rate ~rate:avg_rate ~unit)]
    | Time_used_short    ->
      let (hour, min, sec) = Helper.seconds_to_hms (int_of_float time_used) in
      Printf.sprintf "used : %02d:%02d:%02d" hour min sec
    | Time_used_long     ->
      let (hour, min, sec) = Helper.seconds_to_hms (int_of_float time_used) in
      Printf.sprintf "Time elapsed : %02d:%02d:%02d" hour min sec
    | Time_left_short    ->
      let (hour, min, sec) = Helper.seconds_to_hms (int_of_float time_left) in
      Printf.sprintf "left : %02d:%02d:%02d" hour min sec
    | Time_left_long     ->
      let (hour, min, sec) = Helper.seconds_to_hms (int_of_float time_left) in
      Printf.sprintf "Time remaining : %02d:%02d:%02d" hour min sec in
  let rec make_message_internal (elements:progress_element list) (acc:string list) : string =
    match elements with
    | []      -> String.concat "  " acc
    | e :: tl -> make_message_internal tl ((make_string_for_element e) :: acc) in
  make_message_internal (List.rev elements) []
;;

let make_info
    ~(percent:int)
    ~(unit:string)
    ~(cur_time:float)
    ~(start_time:float)
    ~(total_units:int64)
    ~(last_report_time:float)
    ~(last_reported_units:int64)
    ~(units_so_far:int64)
  : info =
  let time_used              : float = cur_time -. start_time in
  let time_since_last_report : float = cur_time -. last_report_time in
  let units_remaining        : int64 = Int64.sub total_units units_so_far in
  let cur_rate               : float =
    (Int64.to_float (Int64.sub units_so_far last_reported_units)) /. time_since_last_report in
  { percent
  ; cur_time
  ; cur_rate
  ; avg_rate  = (Int64.to_float units_so_far) /. time_used
  ; unit
  ; time_used
  ; time_left = (Int64.to_float units_remaining) /. cur_rate +. 1.
  }
;;

let gen_print_generic
    (type a b c)
    ~(header:string)
    ~(silence_settings:silence_settings ref)
    ~(display_while_active:progress_element list)
    ~(display_on_finish:progress_element list)
    ~(display_on_finish_early:progress_element list)
    ~(unit:string)
    ~(print_interval:float)
    ~(eval_start_time:a -> float)
    ~(eval_units_so_far:b -> int64)
    ~(eval_total_units:c -> int64) 
  : (a, b, c) progress_print_functions =
  let max_print_length    : int   ref = ref 0     in
  let printed_header      : bool  ref = ref false in
  let not_printed_yet     : bool  ref = ref true  in
  let printed_at_100      : bool  ref = ref false in
  let call_count          : int   ref = ref 0     in
  let call_per_interval   : int   ref = ref 0     in

  let last_report_time    : float ref = ref 0.    in
  let last_reported_units : int64 ref = ref 0L    in
  let start_time          : float option ref = ref None in
  let total_units         : int64 option ref = ref None in

  { print_progress =
      (fun ~(start_time_src:a) ~(units_so_far_src:b) ~(total_units_src:c) : unit ->
         let { silent_while_active; silent_when_done } = !silence_settings in
         if not (silent_while_active && silent_when_done) then
           begin
             (* interval is estimated dynamically using call count to reduce floating point operations *)
             call_count              := succ !call_count;
             let units_so_far : int64 = eval_units_so_far units_so_far_src in
             let total_units  : int64 =
               Misc_utils.get_option_ref_init_if_none (fun () -> eval_total_units total_units_src) total_units in
             let percent      : int   = Helper.calc_percent ~units_so_far ~total_units in

             (* print header once *)
             if not !printed_header then
               begin
                 print_endline header;
                 printed_header := true
               end;

             (* initialise start time *)
             if !start_time = None then start_time := Some (eval_start_time start_time_src);

             (* always print if not printed yet or reached 100% *)
             if ((percent <> 100 && not silent_while_active) && (!call_count > !call_per_interval || !not_printed_yet))
             || ((percent =  100 && not silent_when_done   ) && not !printed_at_100)
             then
               begin
                 let start_time =
                   match !start_time with
                   | Some n -> n
                   | None   -> assert false in
                 let cur_time = Sys.time () in
                 let info =
                   make_info
                     ~percent
                     ~unit
                     ~cur_time
                     ~start_time
                     ~total_units
                     ~last_report_time:!last_report_time
                     ~last_reported_units:!last_reported_units
                     ~units_so_far in
                 let time_since_last_report : float = cur_time -. !last_report_time in

                 not_printed_yet     := false;
                 call_per_interval   := int_of_float ((float_of_int !call_count) /. (time_since_last_report /. print_interval));
                 call_count          := 0;
                 last_report_time    := cur_time;
                 last_reported_units := units_so_far;
                 printed_at_100      := percent = 100;

                 let message        = make_message ~info ~elements:display_while_active in
                 let padded_message = Misc_utils.pad_string message !max_print_length ' ' in
                 max_print_length := max (String.length padded_message) !max_print_length;
                 Printf.printf "\r%s " padded_message;
                 flush stdout;
                 if percent = 100 then
                   let message        = (make_message ~info ~elements:display_on_finish) in
                   let padded_message = Misc_utils.pad_string message !max_print_length ' ' in
                   if message = "" then
                     Printf.printf "\r%s \r" padded_message
                   else
                     Printf.printf "\r%s \n" padded_message
               end
           end
      )
  ; print_newline_if_not_done =
      (fun ~(start_time_src:a) ~(units_so_far_src:b) ~(total_units_src:c) : unit ->
         let { silent_when_done; _ } = !silence_settings in
         if not silent_when_done then
           begin
             (* should be already initialised if the functions are used properly
              * but putting it here to avoid exception in case the functions are not used appropriately
              *)
             if !start_time = None then start_time := Some (eval_start_time start_time_src);

             let start_time =
               match !start_time with
               | Some n -> n
               | None   -> assert false in
             let units_so_far : int64 = eval_units_so_far units_so_far_src in
             let total_units  : int64 =
               Misc_utils.get_option_ref_init_if_none (fun () -> eval_total_units total_units_src) total_units in
             let percent      : int   = Helper.calc_percent ~units_so_far ~total_units in
             if percent <> 100 then
               let info =
                 make_info
                   ~percent
                   ~unit
                   ~cur_time:(Sys.time ())
                   ~start_time
                   ~total_units
                   ~last_report_time:!last_report_time
                   ~last_reported_units:!last_reported_units
                   ~units_so_far in
               let message        = make_message ~info ~elements:display_on_finish_early in
               let padded_message = Misc_utils.pad_string message !max_print_length ' ' in
               Printf.printf "\r%s \n" padded_message
           end
      )
  }
;;

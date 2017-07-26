let seconds_to_hms (total_secs:int) : int * int * int =
  let hour               : int   = total_secs  / (60 * 60) in
  let minute             : int   = (total_secs - hour * 60 * 60) / 60 in
  let second             : int   = (total_secs - hour * 60 * 60 - minute * 60) in
  (hour, minute, second)
;;

let gen_print_generic ~(header:string) ~(unit:string) ~(print_interval:float) =
  let last_report_time    : float ref = ref 0. in
  let last_reported_units : int64 ref = ref 0L in
  let max_print_length    : int   ref = ref 0  in
  (fun ~(start_time:float) ~(units_so_far:int64) ~(total_units:int64) : unit ->
     let percent : int =
       Int64.to_int (Int64.div
                       (Int64.mul
                          100L
                          units_so_far)
                       total_units) in
     let cur_time               : float = Sys.time () in
     let time_since_last_report : float = cur_time -. !last_report_time in
     if time_since_last_report > print_interval || percent = 0 || percent = 100 (* always print if at 0% or reached 100% *) then 
       begin
         let cur_rate               : float     = (Int64.to_float (Int64.sub units_so_far !last_reported_units)) /. time_since_last_report in
         (* let time_elapsed           : float     = cur_time -. start_time in *)
         (* let avg_rate               : float     = (Int64.to_float units_so_far) /. time_elapsed in *)
         let units_remaining        : int64     = Int64.sub total_units units_so_far in
         let etc_total_secs         : int       = int_of_float ((Int64.to_float units_remaining) /. cur_rate +. 1.) in
         let (etc_hour, etc_minute, etc_second) = seconds_to_hms etc_total_secs in
         last_report_time    := cur_time;
         last_reported_units := units_so_far;
         let message = Printf.sprintf "\r%s : %Ld / %Ld %s - %d%%  cur : %.0f %s/s  etc : %02d:%02d:%02d"
           header
           units_so_far
           total_units
           unit
           percent
           cur_rate
           unit
           etc_hour
           etc_minute
           etc_second in
         let padding =
           let msg_len = String.length message in
           let pad_len = !max_print_length - msg_len in
           if pad_len > 0 then
             String.make pad_len ' '
           else
             begin
               max_print_length := msg_len;
               ""
             end in
         Printf.printf "%s%s " message padding;
         flush stdout
       end;
     if percent = 100 then
       print_newline ()
  )
;;

let print_newline_if_not_done ~(units_so_far:int64) ~(total_units:int64) : unit =
  let percent : int =
    Int64.to_int (Int64.div
                    (Int64.mul
                       100L
                       units_so_far)
                    total_units) in
  if percent != 100 then
    print_newline ()
  else
    ()  (* do nothing *)
;;

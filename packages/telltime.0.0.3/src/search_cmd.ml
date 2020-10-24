open Cmdliner

type time_format =
  [ `Plain_human_readable
  | `Plain_unix_second
  ]

let tz_offset_s_arg =
  let doc = "Time zone offset in seconds" in
  Arg.(value & opt int Config.tz_offset_s & info [ "tz-offset" ] ~docv:"N" ~doc)

let search_years_ahead_arg =
  let doc = "Number of years to search ahead" in
  let open Arg in
  value
  & opt int Config.default_search_years_ahead
  & info [ "years-ahead" ] ~docv:"N" ~doc

let time_slot_count_arg =
  let doc = "Number of time slots to display" in
  let open Arg in
  value
  & opt int Config.default_time_slot_count
  & info [ "time-slots" ] ~docv:"N" ~doc

let format_string_arg =
  let doc = "Format string" in
  let open Arg in
  value
  & opt string
    "[{syear} {smon:Xxx} {smday:0X} {swday:Xxx} \
     {shour:0X}:{smin:0X}:{ssec:0X}, {eyear} {emon:Xxx} {emday:0X} \
     {ewday:Xxx} {ehour:0X}:{emin:0X}:{esec:0X})"
  & info [ "format" ] ~doc

let sep_arg =
  let doc = "Separator" in
  Arg.(value & opt string "\n" & info [ "sep" ] ~doc)

let expr_arg =
  let doc = "Time expression" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"EXPR" ~doc)

let run (tz_offset_s : int) (search_years_ahead : int) (time_slot_count : int)
    (format_string : string) (sep : string) (expr : string) : unit =
  let search_param =
    Misc_utils.make_search_param ~tz_offset_s ~search_years_ahead
      ~from_unix_second:Config.cur_unix_second
    |> Result.get_ok
  in
  match Daypack_lib.Time_expr.of_string expr with
  | Error msg -> print_endline msg
  | Ok expr -> (
      match Daypack_lib.Time_expr.matching_time_slots search_param expr with
      | Error msg -> print_endline msg
      | Ok s -> (
          Printf.printf
            "Searching in time zone offset (seconds)            : %d\n"
            tz_offset_s;
          Printf.printf
            "Search by default starts from (in above time zone) : %s\n"
            ( Daypack_lib.Time.To_string.yyyymondd_hhmmss_string_of_unix_second
                ~display_using_tz_offset_s:(Some tz_offset_s)
                Config.cur_unix_second
              |> Result.get_ok );
          print_newline ();
          match s () with
          | Seq.Nil -> print_endline "No matching time slots"
          | Seq.Cons _ ->
            s
            |> OSeq.take time_slot_count
            |> OSeq.iteri (fun i ts ->
                match
                  Daypack_lib.Time.To_string.string_of_time_slot
                    ~format:format_string
                    ~display_using_tz_offset_s:(Some tz_offset_s) ts
                with
                | Ok s ->
                  if i = 0 then Printf.printf "%s" s
                  else Printf.printf "%s%s" sep s
                | Error msg -> Printf.printf "Error: %s\n" msg);
            print_newline ()
            (* |>
             * match time_format with
             * | `Plain_human_readable ->
             *   print_endline "Matching time slots (in above time zone):";
             *   Seq.iter (fun (x, y) ->
             *       let x =
             *         Daypack_lib.Time.To_string
             *         .yyyymondd_hhmmss_string_of_unix_second
             *           ~display_using_tz_offset_s:(Some tz_offset_s) x
             *         |> Result.get_ok
             *       in
             *       let y =
             *         Daypack_lib.Time.To_string
             *         .yyyymondd_hhmmss_string_of_unix_second
             *           ~display_using_tz_offset_s:(Some tz_offset_s) y
             *         |> Result.get_ok
             *       in
             *       Printf.printf "[%s, %s)\n" x y)
             * | `Plain_unix_second ->
             *   print_endline "Matching time slots:";
             *   Seq.iter (fun (x, y) -> Printf.printf "[%Ld, %Ld)\n" x y) *) ) )

let cmd =
  ( (let open Term in
     const run
     $ tz_offset_s_arg
     $ search_years_ahead_arg
     $ time_slot_count_arg
     $ format_string_arg
     $ sep_arg
     $ expr_arg),
    Term.info "search" )

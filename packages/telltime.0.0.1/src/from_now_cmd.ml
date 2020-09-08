open Cmdliner

let expr_arg =
  let doc = "Duration expression" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"EXPR" ~doc)

let run (duration_expr : string) : unit =
  match Daypack_lib.Duration.of_string duration_expr with
  | Error msg -> print_endline msg
  | Ok duration ->
    let duration_in_seconds = Daypack_lib.Duration.to_seconds duration in
    Printf.printf "Now                   : %s\n"
      ( Daypack_lib.Time.To_string.yyyymmdd_hhmmss_string_of_unix_second
          ~display_using_tz_offset_s:(Some Config.tz_offset_s)
          Config.cur_unix_second
        |> Result.get_ok );
    Printf.printf "Duration (original)   : %s\n" duration_expr;
    Printf.printf "Duration (normalized) : %s\n"
      (Daypack_lib.Duration.To_string.human_readable_string_of_duration
         duration);
    Printf.printf "Now + duration        : %s\n"
      ( Daypack_lib.Time.To_string.yyyymmdd_hhmmss_string_of_unix_second
          ~display_using_tz_offset_s:(Some Config.tz_offset_s)
          (Int64.add Config.cur_unix_second duration_in_seconds)
        |> Result.get_ok )

let cmd = (Term.(const run $ expr_arg), Term.info "from-now")

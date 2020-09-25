open Cmdliner

let run () : unit =
  print_endline
    ( Daypack_lib.Time.To_string.yyyymmdd_hhmmss_string_of_unix_second
        ~display_using_tz_offset_s:(Some Config.tz_offset_s)
        Config.cur_unix_second
      |> Result.get_ok )

let cmd = (Term.(const run $ const ()), Term.info "now")

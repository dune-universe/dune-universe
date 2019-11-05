(** Given the command line arguments create the config and start the server before handling the state *)
let main work_duration short_break_duration long_break_duration
    number_work_sessions notify_script socket_file style_renderer log_level =
  Fmt_tty.setup_std_outputs ?style_renderer () ;
  Logs.set_level log_level ;
  Logs.set_reporter (Logs_fmt.reporter ()) ;
  let open Owork in
  let config =
    Config.make
      ~work_duration:(Duration.of_min work_duration)
      ~short_break_duration:(Duration.of_min short_break_duration)
      ~long_break_duration:(Duration.of_min long_break_duration)
      ~number_work_sessions ~notify_script ~socket_file
  in
  Lwt_main.run
    (let%lwt () = Logs_lwt.debug (fun f -> f "%s" (Config.show config)) in
     Server.run config)

open Cmdliner

let work_duration =
  let doc = "Length in minutes of the work session." in
  Arg.(value & opt int 30 & info ["w"; "work-duration"] ~doc)

let short_break_duration =
  let doc = "Length in minutes of the short break." in
  Arg.(value & opt int 5 & info ["s"; "short-break"] ~doc)

let long_break_duration =
  let doc = "Length in minutes of the long break." in
  Arg.(value & opt int 15 & info ["l"; "long-break"] ~doc)

let number_work_sessions =
  let doc = "Number of work sessions to be completed before a long break." in
  Arg.(value & opt int 4 & info ["n"; "work-sessions"] ~doc)

let notify_script =
  let doc =
    "Location of the script to handle the notifications. If not specified \
     then the server will try to use a default notification system."
  in
  Arg.(value & opt string "" & info ["notify-script"] ~doc)

let socket_file =
  let doc = "Location to use for socket file." in
  Arg.(value & opt string "/tmp/owork.sock" & info ["socket-file"] ~doc)

let program =
  Term.(
    const main $ work_duration $ short_break_duration $ long_break_duration
    $ number_work_sessions $ notify_script $ socket_file
    $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let info =
  let doc = "A productivity timing server." in
  Term.info "owork" ~doc ~exits:Term.default_exits

let () = Term.exit @@ Term.eval (program, info)

open Rresult.R.Infix

let rec connect ?(old = false) (host, port) =
  let connect_server () =
    try
      let sockaddr = Unix.ADDR_INET (host, port) in
      let s = Unix.(socket PF_INET SOCK_STREAM 0) in
      Unix.(connect s sockaddr);
      Ok s
    with
    | Unix.Unix_error (err, f, _) ->
      Logs.err (fun m -> m "unix error in %s: %s" f (Unix.error_message err));
      Error (`Msg "connect failure")
  in
  connect_server () >>= fun s ->
  let hello =
    if old then
      Builder.(Client_hello cmds)
    else
      Builder.(Client_hello2 (`Client, client_cmds))
  in
  Builder.write_cmd s hello >>= fun () ->
  match Builder.read_cmd s with
  | Ok (Builder.Server_hello _ | Builder.Server_hello2) -> Ok s
  | Ok cmd ->
    Logs.err (fun m -> m "expected Server Hello, got %a" Builder.pp_cmd cmd);
    Error (`Msg "bad communication")
  | Error (`Msg _) when old = false ->
    Logs.warn (fun m -> m "retrying with old = true");
    connect ~old:true (host, port)
  | Error _ as e -> e

let observe_latest () remote =
  (connect remote >>= fun s ->
   Builder.write_cmd s Builder.Info >>= fun () ->
   let r = Builder.read_cmd s in Unix.close s; r) >>= function
  | Info_reply { Builder.running ; _ } ->
    (match List.fold_left
             (fun acc ((start, _, _) as x) ->
                match acc with
                | None -> Some x
                | Some (start', _, _) ->
                  if Ptime.is_later start ~than:start'
                  then Some x else acc)
             None running
     with
     | None -> Error (`Msg "No running jobs")
     | Some (_start, uuid, job) ->
       Logs.app (fun m -> m "Observing %s (%a)" job.Builder.name Uuidm.pp uuid);
       connect remote >>= fun s ->
       Builder.write_cmd s (Builder.Observe uuid) >>= fun () ->
       let rec read () =
         Builder.read_cmd s >>= fun cmd ->
         Logs.app (fun m -> m "%a" Builder.pp_cmd cmd);
         read ()
       in
       read ())
  | cmd -> Error (`Msg (Fmt.str "Unexpected reply to 'info': %a" Builder.pp_cmd cmd))


let observe () remote id =
  match Uuidm.of_string id with
  | None -> Error (`Msg "error parsing uuid")
  | Some uuid ->
    connect remote >>= fun s ->
    Builder.write_cmd s (Builder.Observe uuid) >>= fun () ->
    let rec read () =
      Builder.read_cmd s >>= fun cmd ->
      Logs.app (fun m -> m "%a" Builder.pp_cmd cmd);
      read ()
    in
    read ()

let info_ () remote =
  connect remote >>= fun s ->
  Builder.write_cmd s Builder.Info >>= fun () ->
  Builder.read_cmd s >>= fun cmd ->
  Logs.app (fun m -> m "%a" Builder.pp_cmd cmd);
  Ok ()

let unschedule () remote name =
  connect remote >>= fun s ->
  Builder.write_cmd s (Builder.Unschedule name)

let execute () remote name =
  connect remote >>= fun s ->
  Builder.write_cmd s (Builder.Execute name)

let schedule () remote name script period =
  Bos.OS.File.read (Fpath.v script) >>= fun script ->
  let job = Builder.{ name ; script } in
  connect remote >>= fun s ->
  Builder.write_cmd s (Builder.Schedule (period, job))

let schedule_orb_build () remote name opam_package period =
  let job = Builder.{ name ; opam_package } in
  connect remote >>= fun s ->
  Builder.write_cmd s (Builder.Schedule_orb_build (period, job))

let reschedule () remote name next period =
  connect remote >>= fun s ->
  Builder.write_cmd s (Builder.Reschedule (name, next, period))

let help () man_format cmds = function
  | None -> `Help (`Pager, None)
  | Some t when List.mem t cmds -> `Help (man_format, Some t)
  | Some x ->
    print_endline ("unknown command '" ^ x ^ "', available commands:");
    List.iter print_endline cmds;
    `Ok ()

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ~dst:Format.std_formatter ())

open Cmdliner

let host_port : (Unix.inet_addr * int) Arg.converter =
  let parse s =
    match String.split_on_char ':' s with
    | [ hostname ;  port ] ->
      begin try
          `Ok (Unix.inet_addr_of_string hostname, int_of_string port)
        with
          Not_found -> `Error "failed to parse IP:port"
      end
    | _ -> `Error "broken: no port specified"
  in
  parse, fun ppf (h, p) -> Format.fprintf ppf "%s:%d"
      (Unix.string_of_inet_addr h) p

let remote =
  let doc = "The remote host:port to connect to" in
  let env = Term.env_info "BUILDER_REMOTE" in
  Arg.(value & opt host_port (Unix.inet_addr_loopback, 1234) &
       info [ "r" ; "remote" ] ~env ~doc ~docv:"IP:PORT")

let nam =
  let doc = "The job name" in
  Arg.(required & pos 0 (some string) None & info [ ] ~doc ~docv:"NAME")

let id =
  let doc = "The job ID" in
  Arg.(required & pos 0 (some string) None & info [ ] ~doc ~docv:"ID")

let p : Builder.period Arg.converter =
  let parse = function
    | "hourly" -> `Ok Builder.Hourly
    | "daily" -> `Ok Builder.Daily
    | "weekly" -> `Ok Builder.Weekly
    | s -> `Error ("failed to parse period " ^ s)
  in
  parse, Builder.pp_period

let period =
  let doc = "The periodic execution interval" in
  Arg.(value & opt p Builder.Daily & info [ "period" ] ~doc ~docv:"PERIOD")

let period_opt =
  let doc = "The periodic execution interval" in
  Arg.(value & opt (some p) None & info [ "period" ] ~doc ~docv:"PERIOD")

let next =
  let ptime : Ptime.t Arg.converter =
    let parse s = match Ptime.of_rfc3339 s with
      | Ok (ptime, (None | Some 0), _) ->  `Ok ptime
      | Ok (_, _, _) ->  `Error "I don't like the timezone :(" (* FIXME *)
      | Error (`RFC3339 (_, e)) ->
        `Error (Fmt.str "bad RFC3339 date: %a" Ptime.pp_rfc3339_error e)
    in
    parse, Ptime.pp_rfc3339 ()
  in
  let doc = "The next execution time (RFC3339 date)" in
  Arg.(required & pos 1 (some ptime) None & info [ ] ~doc ~docv:"NEXT")

let script =
  let doc = "The script to execute" in
  Arg.(required & pos 1 (some file) None & info [ ] ~doc ~docv:"FILE")

let opam_package =
  let doc = "The opam package to build" in
  Arg.(required & pos 1 (some string) None & info [ ] ~doc ~docv:"OPAM")

let setup_log =
  Term.(const setup_log
        $ Fmt_cli.style_renderer ()
        $ Logs_cli.level ())

let observe_latest_cmd =
  Term.(term_result (const observe_latest $ setup_log $ remote)),
  Term.info "observe-latest"

let observe_cmd =
  Term.(term_result (const observe $ setup_log $ remote $ id)),
  Term.info "observe"

let info_cmd =
  Term.(term_result (const info_ $ setup_log $ remote)),
  Term.info "info"

let unschedule_cmd =
  Term.(term_result (const unschedule $ setup_log $ remote $ nam)),
  Term.info "unschedule"

let schedule_cmd =
  Term.(term_result (const schedule $ setup_log $ remote $ nam $ script $ period)),
  Term.info "schedule"

let schedule_orb_build_cmd =
  Term.(term_result (const schedule_orb_build $ setup_log $ remote $ nam $ opam_package $ period)),
  Term.info "orb-build"

let reschedule_cmd =
  Term.(term_result (const reschedule $ setup_log $ remote $ nam $ next $ period_opt)),
  Term.info "reschedule"

let execute_cmd =
  Term.(term_result (const execute $ setup_log $ remote $ nam)),
  Term.info "execute"

let help_cmd =
  let doc = "Builder client" in
  Term.(ret (const help $ setup_log $ Term.man_format $ Term.choice_names $ Term.pure None)),
  Term.info "builder-client" ~version:Builder.version ~doc

let cmds = [ help_cmd ; schedule_cmd ; unschedule_cmd ; info_cmd ; observe_latest_cmd ; observe_cmd ; execute_cmd ; schedule_orb_build_cmd ; reschedule_cmd ]

let () = match Term.eval_choice help_cmd cmds with `Ok () -> exit 0 | _ -> exit 1

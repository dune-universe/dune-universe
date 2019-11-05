let notify (config : Config.t) state =
  if config.notify_script = "" then
    let command =
      Printf.sprintf "notify-send -a owork 'Owork' '%s'"
      @@ State.to_string state
    in
    ignore @@ Lwt_unix.system command
  else
    let command =
      Printf.sprintf "%s %s" config.notify_script @@ State.to_string state
    in
    ignore @@ Lwt_unix.system command

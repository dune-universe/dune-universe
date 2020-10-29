(* No shutdown events on Solo5. *)
let await_shutdown_request ?can_poweroff:_ ?can_reboot:_ () =
  fst (Lwt.wait ())

open Incr_dom

let () =
  Start_app.component_old_do_not_use (module App) ~initial_model:(App.initial_model ())
;;

open Incr_dom

let () =
  Start_app.simple (module App) ~initial_model:(App.create ())

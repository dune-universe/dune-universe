open Opam_compiler

let github_client_fail_all =
  let pr_info _ = assert false in
  { Github_client.pr_info }

let runner_fail_all =
  let run ?extra_env:_ _ = assert false in
  let run_out _ = assert false in
  { Runner.run; run_out }

let ( let$ ) (x, finally) f = Fun.protect ~finally (fun () -> f x)

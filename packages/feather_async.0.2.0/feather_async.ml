open! Base
open! Async

let run_bg = `Use_deferred_dot_upon

let last_exit = `Do_not_use

let run ?cwd ?env cmd = In_thread.run (fun () -> Feather.run ?cwd ?env cmd)

let fzf ?cwd ?env cmd = In_thread.run (fun () -> Feather.fzf ?cwd ?env cmd)

let collect_lines ?cwd ?env cmd =
  In_thread.run (fun () -> Feather.collect_lines ?cwd ?env cmd)

let collect_stdout ?cwd ?env cmd =
  In_thread.run (fun () -> Feather.collect_stdout ?cwd ?env cmd)

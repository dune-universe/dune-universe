open Core
open Async

module F = Core_extended.Std.Fd_leak_check

let percent_fds_in_use () =
  In_thread.run F.percent_fds_in_use
;;

let enabled = ref false

let disable_periodic_check () = enabled := false

let enable_periodic_check () =
  enabled := true;
  let rec loop () =
    after (sec 10.)
    >>> fun () ->
    if !enabled then begin
      percent_fds_in_use ()
      >>> fun in_use ->
      if in_use < 0.95 then
        loop ()
      else begin
        F.report_open_files ();
        shutdown 1;
      end
    end
  in
  loop ()
;;

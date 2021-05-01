(*
 * Copyright (c) 2015 David Sheets <sheets@alum.mit.edu>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

let create_flags = Fsevents.CreateFlags.detailed_interactive

let run_loop_mode = Cf.RunLoop.Mode.Default

let watcher = Fsevents_lwt.create 0. create_flags ["."]

let stream = Fsevents_lwt.stream watcher
let event_stream = Fsevents_lwt.event_stream watcher

let string_of_event { Fsevents_lwt.path; flags; _ } =
  path^"\n"^(Fsevents.EventFlags.to_string flags)

let print_event event = Lwt_io.printf "%s%!\n" (string_of_event event)

let print_events = Lwt_stream.iter_s print_event stream

let timer =
  let open Lwt in
  let rec print_time () =
    let { Unix.tm_hour; tm_min; tm_sec; _ } =
      Unix.(localtime (gettimeofday ()))
    in
    Lwt_io.printf "%02d:%02d:%02d%!\n" tm_hour tm_min tm_sec
    >>= fun () ->
    Lwt_unix.sleep 10.
    >>= fun () ->
    print_time ()
  in print_time ()

;;
Lwt.(async (fun () ->
    Cf_lwt.RunLoop.run_thread (fun runloop ->
        Fsevents.schedule_with_run_loop event_stream runloop run_loop_mode;
        if not (Fsevents.start event_stream)
        then prerr_endline "failed to start FSEvents stream"
      ) >>= fun _ -> Lwt.return_unit
));
Lwt.async (fun () -> print_events);
Lwt_main.run timer

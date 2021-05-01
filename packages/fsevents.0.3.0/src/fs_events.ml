(*
 * Copyright (c) 2016 David Sheets <dsheets@docker.com>
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

type format =
  | String

let string_of_event = function
  | String ->
    fun path flags id ->
      Printf.sprintf "%s %s %s"
        (Unsigned.UInt64.to_string (Fsevents.EventId.to_uint64 id))
        path
        (Fsevents.EventFlags.to_string_one_line flags)

let print_event format path flags id =
  print_endline (string_of_event format path flags id)

(* TODO: termination *)
let stream format paths =
  let create_flags = Fsevents.CreateFlags.({
    use_cf_types = false;
    no_defer = true;
    watch_root = true;
    ignore_self = true;
    file_events = true;
    mark_self = false;
  }) in
  let watcher = Fsevents.create 0. create_flags (print_event format) paths in
  Lwt_main.run begin
    let open Lwt.Infix in
    Cf_lwt.RunLoop.run_thread (fun runloop ->
      Fsevents.schedule_with_run_loop watcher runloop Cf.RunLoop.Mode.Default;
      if not (Fsevents.start watcher)
      then
        let paths_s = String.concat " " paths in
        (Printf.eprintf "Failed to start FSEvents watcher for [ %s ]\n%!"
           paths_s;
         exit 1)
    )
    >>= fun _runloop ->
    let never, _waken = Lwt.wait () in
    never
  end

open Cmdliner

let stream_cmd =
  let doc = "output an FSEvents event stream" in
  let format = Arg.(value (opt (enum [
      "string", String;
    ]) String (info ~docv:"FORMAT" ["format"])))
  in
  let paths = Arg.(
    value (pos_all file [Sys.getcwd ()] (info ~docv:"PATH" []))
  ) in
  Term.(ret (pure stream $ format $ paths)),
  Term.info "fsevents" ~doc

;;
match Term.eval stream_cmd with
| `Error _ -> exit 1
| _ -> exit 0

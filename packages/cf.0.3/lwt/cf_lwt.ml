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

module RunLoop = struct
  let spin_thread f x =
    let stream, push = Lwt_stream.create () in
    let return x =
      Lwt_preemptive.run_in_main (fun () ->
          push (Some x);
          Lwt.return_unit)
    in
    let _thread = Thread.create (fun x -> f return x) x in
    Lwt_stream.next stream

  let runloop_thread setup run =
    spin_thread
      (fun return () ->
        let runloop =
          try Cf.RunLoop.get_current ()
          with _ ->
            print_endline "exn in get_current";
            exit 7
        in
        let started = ref false in
        let start_observer =
          Cf.RunLoop.Observer.(
            create
              Activity.(Only [ Entry ])
              ~repeats:false
              (fun _ ->
                started := true;
                return runloop))
        in
        Cf.RunLoop.(add_observer runloop start_observer Mode.Default);
        setup runloop;
        run ();
        (* If setup didn't actually register anything on the runloop, this is
           necessary to unblock the main thread. *)
        if not !started then return runloop;
        Cf.RunLoop.release runloop)
      ()

  let run_thread setup = runloop_thread setup Cf.RunLoop.run

  let run_thread_in_mode ?return_after_source_handled ?seconds mode setup after
      =
    runloop_thread setup (fun () ->
        let result =
          Cf.RunLoop.run_in_mode ?return_after_source_handled ?seconds mode
        in
        Lwt_preemptive.run_in_main (fun () -> after result))
end

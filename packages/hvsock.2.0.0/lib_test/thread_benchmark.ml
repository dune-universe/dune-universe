module type MAIN = sig
  val run: 'a Lwt.t -> 'a
end

module type TIMER = sig
  val sleep: ms:int -> unit Lwt.t
end

module Benchmark(Fn: Lwt_hvsock.FN)(Main: MAIN)(Timer: TIMER) = struct

  let run () =
    let open Lwt.Infix in
    let counter = ref 0 in
    let f = Fn.create (fun () -> incr counter) in
    let iterations = 1_000_000 in
    let start = Unix.gettimeofday () in
    (* Uwt will exit prematurely unless certain kinds of threads are
       waiting, such as timers *)
    let rec sleep () =
      Timer.sleep 1_000 >>= fun () ->
      Printf.fprintf stderr ".%!";
      if !counter < iterations then sleep () else Lwt.return_unit in
    Main.run begin
      let rec loop = function
        | 0 -> Lwt.return_unit
        | n ->
          Fn.fn f ()
          >>= fun () ->
          loop (n - 1) in
      Lwt.pick [ sleep (); loop iterations ]
    end;
    Alcotest.(check int) "Iterations" iterations (!counter);
    let per_sec = float_of_int iterations /. (Unix.gettimeofday () -. start) in
    per_sec
end

module Lwt_timer = struct
  let sleep ~ms = Lwt_unix.sleep (float_of_int ms /. 1000.)
end
module L_main_thread = Benchmark(Lwt_hvsock_main_thread.Make(Lwt_preemptive))(Lwt_main)(Lwt_timer)
module L_detach = Benchmark(Lwt_hvsock_detach)(Lwt_main)(Lwt_timer)

module Uwt_timer = struct
  let sleep ~ms = Uwt.Timer.sleep ms
end
module U_main_thread = Benchmark(Lwt_hvsock_main_thread.Make(Uwt_preemptive))(Uwt.Main)(Uwt_timer)
module Uwt_hvsock_detach = struct
  type ('request, 'response) t = 'request -> 'response
  let create f = f
  let destroy _ = ()
  let fn = Uwt_preemptive.detach
end
module U_detach = Benchmark(Uwt_hvsock_detach)(Uwt.Main)(Uwt_timer)


let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Printf.printf "Lwt_main run_in_main_thread: %.1f calls per second\n" (L_main_thread.run ());
  Printf.printf "Lwt_main detach:             %.1f calls per second\n" (L_detach.run ());
  Printf.printf "Uwt_main run_in_main_thread: %.1f calls per second\n" (U_main_thread.run ());
  Printf.printf "Uwt_main detach:             %.1f calls per second\n" (U_detach.run ())

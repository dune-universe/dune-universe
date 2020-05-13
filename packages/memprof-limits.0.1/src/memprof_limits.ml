(* Reimplementation of the memprof interface for two simultaneous
   clients *)

module Memprof_server = struct
  (* Main instance *)
  let started_1 = ref false

  (* Secondary instance : memprof limits *)
  let started_2 = ref false

  let make_alloc_callback ~sampling_rate ~callback_1 =
    let limits_callback () = Limits_callbacks.callback ~sampling_rate in
    (fun x ->
       if !started_2 then limits_callback () ;
       callback_1 x)

  let rec really_do_stop_memprof () =
    (* FIXME: loops if already stopped *)
    try Gc.Memprof.stop () with _ -> really_do_stop_memprof ()

  let start
        ~sampling_rate
        ?(callstack_size = max_int)
        (tracker : ('a, 'b) Gc.Memprof.tracker) =
    if !started_1 then failwith "Memprof.start: already started" ;
    let sampling_rate =
      if sampling_rate < Limits_callbacks.limits_sampling_rate then
        failwith (
          Printf.sprintf
            "Cannot go lower than the memprof-limits sampling rate: %g"
            Limits_callbacks.limits_sampling_rate
        )
      else
        (* make sure the expectancy is an integer *)
        1. /. (Float.round (1. /. sampling_rate))
    in
    really_do_stop_memprof () ;
    let alloc_minor = make_alloc_callback
                        ~sampling_rate
                        ~callback_1:tracker.alloc_minor
    in
    let alloc_major = make_alloc_callback
                        ~sampling_rate
                        ~callback_1:tracker.alloc_major
    in
    let tracker_with_limits = { tracker with alloc_major = alloc_major ;
                                             alloc_minor = alloc_minor }
    in
    Gc.Memprof.start ~sampling_rate ~callstack_size tracker_with_limits ;
    started_1 := true

  let restart_2 () =
    (* if memprof already runs for 1, then starting amounts to just
       setting the flag. *)
    if not !started_1 then (
      let sampling_rate = Limits_callbacks.limits_sampling_rate in
      let callback =
        make_alloc_callback ~sampling_rate ~callback_1:(fun _ -> None)
      in
      let tracker = { Gc.Memprof.null_tracker with alloc_major = callback ;
                                                   alloc_minor = callback }
      in
      Gc.Memprof.start ~sampling_rate ~callstack_size:0 tracker
    ) ;
    started_2 := true

  let stop () =
    if not !started_1 then failwith "Memprof.start: not started" ;
    Gc.Memprof.stop () ;
    started_1 := false ;
    if !started_2 then restart_2 ()

  let start_2 () =
    if !started_2 then failwith "memprof-limits: already started" ;
    restart_2 ()

  let stop_2 () =
    if not !started_2 then failwith "memprof-limits: not started" ;
    (* if the user uses memprof, then stopping is simply setting the
       flag. *)
    if not !started_1 then Gc.Memprof.stop () ;
    started_2 := false
end

(* FIXME: can end in an uninterruptible loop if the user interferes by
   calling Gc.Memprof.stop. (Inherits Memprof behaviour.) *)
let rec really_do_stop_2 () =
  try Memprof_server.stop_2 () with _ -> really_do_stop_2 ()

(* public interface *)

let with_memprof_limits f =
  (* let () = match Sys.backend_type with
    | Native -> ()
    | _ -> failwith "Memprof_limits: unsupported backend type (only \
                     native is supported)"
  in *)
  Fun.with_resource
    ~acquire:Memprof_server.start_2 ()
    f
    ~release:(fun () ->
      really_do_stop_2 () ;
      Limits_callbacks.reset ()
    )

let set_global_memory_limit l = Limits_callbacks.global_limit := l

let with_global_memory_limit x =
  if not !Memprof_server.started_2
  then failwith "with_global_memory_limit: not started" ;
  Limits_callbacks.with_global_memory_limit x

let with_allocation_limit ~limit =
  if not !Memprof_server.started_2
  then failwith "with_allocation_limit: not started" ;
  Limits_callbacks.with_allocation_limit ~limit

let max_allocation_limit = Limits_callbacks.max_allocation_limit

type 'a result = ('a, exn) Result.t

(* Export interface to memprof for user's profiling needs *)

module Memprof = struct
  include Memprof_server

  type allocation = Stdlib.Gc.Memprof.allocation =
    private { n_samples : int ;
              size : int ;
              unmarshalled : bool ;
              callstack : Printexc.raw_backtrace }

  type ('minor, 'major) tracker = ('minor, 'major) Stdlib.Gc.Memprof.tracker = {
    alloc_minor: allocation -> 'minor option;
    alloc_major: allocation -> 'major option;
    promote: 'minor -> 'major option;
    dealloc_minor: 'minor -> unit;
    dealloc_major: 'major -> unit;
  }

  let null_tracker = Stdlib.Gc.Memprof.null_tracker

end

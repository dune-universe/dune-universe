(* Reimplementation of the memprof interface for two simultaneous
   clients *)

open Resource_bind

(* Protect global state *)
let lock = Mutex.create ()

let with_mask ~scope =
  Masking.with_resource
    ~acquire:scope ()
    ~scope:(fun x -> x)
    ~release:(fun _ -> ())

let with_memprof_lock ~scope =
  let& () = with_mask in
  let& () = Mutex_aux.with_lock lock in
  scope ()

(* Main instance *)
let started_1 = ref false

(* Secondary instance : memprof limits *)
let started_2 = Atomic.make false

let invalid_argument =
  Printf.ksprintf (fun s -> raise (Invalid_argument s))

let check_memprof_limits_started s =
  if not (Atomic.get started_2) then failwith (Printf.sprintf "%s: not started" s)

let alloc_callback ~sampling_rate ~callback_1 x =
  (* Ignore any masked allocation. For the limits this does not
     change the statistical properties. For callback_1, this
     means that we do not profile masked allocations for fear it
     might raise. Not great, not terrible. *)
  if Masking.is_blocked () then None
  else (
    (* seen as true if happens after [check_memprof_limits_started] *)
    if Atomic.get started_2 then Limits_callbacks.callback ~sampling_rate x ;
    callback_1 x
  )

let start_for_limits_only () =
  let sampling_rate = Limits_callbacks.limits_sampling_rate in
  let callback = alloc_callback ~sampling_rate ~callback_1:(fun _ -> None)
  in
  let tracker = { Gc.Memprof.null_tracker with alloc_major = callback ;
                                               alloc_minor = callback }
  in
  Gc.Memprof.start ~sampling_rate ~callstack_size:0 tracker

let start_2 () =
  let& () = with_memprof_lock in
  if Atomic.get started_2 then failwith "memprof-limits: already started" ;
  (* if memprof already runs for 1, then starting amounts to just
     setting the flag. *)
  if not !started_1 then start_for_limits_only () ;
  Atomic.set started_2 true

let start_1
      ~sampling_rate
      ?(callstack_size = max_int)
      (tracker : ('a, 'b) Gc.Memprof.tracker) =
  let& () = with_memprof_lock in
  if !started_1 then failwith "Memprof.start: already started" ;
  if Atomic.get started_2 then Gc.Memprof.stop () ;
  let sampling_rate =
    if sampling_rate < Limits_callbacks.limits_sampling_rate then
      invalid_argument
        "Cannot go lower than the memprof-limits sampling rate: %g"
        Limits_callbacks.limits_sampling_rate
    else
      (* make sure the expectancy is an integer *)
      1. /. (Float.round (1. /. sampling_rate))
  in
  let alloc_minor = alloc_callback ~sampling_rate ~callback_1:tracker.alloc_minor in
  let alloc_major = alloc_callback ~sampling_rate ~callback_1:tracker.alloc_major in
  let tracker_with_limits = { tracker with alloc_major ; alloc_minor } in
  Gc.Memprof.start ~sampling_rate ~callstack_size tracker_with_limits ;
  started_1 := true

let stop_1 () =
  let& () = with_memprof_lock in
  if not !started_1 then failwith "Memprof.start: not started" ;
  Gc.Memprof.stop () ;
  started_1 := false ;
  if Atomic.get started_2 then start_for_limits_only ()

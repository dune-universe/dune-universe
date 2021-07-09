let callback _ = None

let tracker = { Stdlib.Gc.Memprof.null_tracker with
                alloc_minor = callback ;
                alloc_major = callback }

let sampling_rate = 0.01

let () = Memprof_limits.Memprof.start ~sampling_rate tracker

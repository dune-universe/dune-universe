module M = Memprof_limits

let rec alloc n x = if n = 0 then x else let n = n - 1 in alloc n (() :: x)

let alloc_kib n x =
  (* three words per node *)
  let bytes_per_node = 3 * Sys.word_size / 8 in
  Sys.opaque_identity (alloc (n * 1024 / bytes_per_node) x)

let alloc_mib n = alloc_kib (int_of_float (n *. 1024.)) []

let () =
  M.start_memprof_limits () ;
  let f () =
    M.set_global_memory_limit 0 ;
    let more = 2699 (* 15 *. Float.log 10 /. sampling_rate /. 1024 *. 8 *)
    in
    (* Allocating more kiB. This test has a probability of less than
       1E-15 of failing with current sampling rate of 1E-4, so a
       failure is likely to indicate a bug. *)
    ignore (alloc_kib more [])
  in
  match M.limit_global_memory f with
  | Ok () -> failwith "not stopped"
  | Error _ -> exit 0 (* success *)

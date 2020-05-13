module M = Memprof_limits

let rec alloc n x = if n = 0 then x else alloc (pred n) (() :: x)

let alloc_kib n =
  (* three words per node *)
  let bytes_per_node = 3 * Sys.word_size / 8 in
  ignore (Sys.opaque_identity (alloc (n * 1024 / bytes_per_node) []))

let alloc_mibs f =
  alloc_kib (int_of_float ((Float.rem f 1.) *. 1024.)) ;
  for _ = 1 to int_of_float f do
    alloc_kib 1024
  done

(* Has a probability of less than 10^-14 to not trigger a memprof
   callback. *)
let alloc_enough () = alloc_mibs 2.5

let fail i s = Printf.printf "%n: %s" i s ; exit 1

(* Run with a limit of [limit_mib] MiB. Try to allocate [(1 - delta) *
   limit_mib] MiB and report failure if interrupted. Then try to
   allocate [2 * delta * limit_mib] MiB more and report failure if not
   interrupted. *)
let test i limit_mib delta : unit =
  let limit = Int64.of_float (limit_mib *. 1024.) in
  match
    M.with_allocation_limit ~limit @@ fun () ->
    let () =
      match
        (* this allocation limit must not interfere on the allocation count *)
        M.with_allocation_limit ~limit:M.max_allocation_limit @@ fun _ ->
        alloc_mibs (limit_mib *. (1. -. delta))
      with
      | Error _ -> fail i "stopped too early"
      | _ -> ()
    in
    let () =
      match
        (* this allocation limit must not interfere on the allocation count *)
        M.with_allocation_limit ~limit:M.max_allocation_limit @@ fun _ ->
        alloc_mibs (limit_mib *. (2. *. delta))
      with
      | exception _ -> fail i "token not set after nested"
      | Error _ -> ()
      | _ -> fail i "not stopped inside nested"
    in
    (* both allocating and returning normally should count as being
       interrupted at this point *)
    if i mod 2 = 0 then (
      alloc_enough () ;
      fail i "not stopped"
    )
  with
  | Ok () -> fail i "not stopped after returning"
  | Error _ -> () (* success *)
  | exception _ -> fail i "token not set"

let () =
  M.with_memprof_limits @@ fun () ->
  (* These tests have a probability of less than 10^-14 of failing with
     current sampling rate of 10^-4, so a failure is likely to
     indicate a bug (in Memprof_limits or in Memprof's sampling). *)
  for i = 1 to 20 do
    test i 1024. 0.07
  done ;
  for i = 21 to 40 do
    test i 20. 0.375
  done

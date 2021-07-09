module M = Memprof_limits

let rec alloc n x = if n = 0 then x else alloc (pred n) (() :: x)

let alloc_k_words n =
  (* three words per node *)
  ignore (Sys.opaque_identity (alloc ((n * 1000) / 3) []))

let alloc_m_words f =
  alloc_k_words (int_of_float ((Float.rem f 1.) *. 1000.)) ;
  for _ = 1 to int_of_float f do
    alloc_k_words 1000
  done

(* Has a probability higher than (1 - 1E-14) to trigger a memprof
   callback. *)
let alloc_enough () = alloc_m_words 0.328

let allocated_words () =
  let stat = Gc.quick_stat () in
  int_of_float (stat.minor_words +. stat.major_words -. stat.promoted_words)

let fail i allocated_start s =
  let allocated = allocated_words () - allocated_start in
  Printf.printf "%n: %s (allocated %#d words)" i s allocated ; exit 1

let () = M.start_memprof_limits ()

let () =
  (* control test, disabled *)
  if false then (
    alloc_m_words 10. ; (* choose a different starting point *)
    let f () =
      let alloc_start = allocated_words () in
      alloc_m_words 100. ;
      allocated_words () - alloc_start
    in
    match M.limit_allocations ~limit:M.max_allocation_limit f with
    | Ok (real, approx) ->
      let approx_int = Int64.to_int approx * 1000 in
      Printf.printf "control: real %#d words, estimated %#d words" real approx_int ;
      exit 1
    | Error _ -> failwith "unexpected limit"
  )

(* Run with a limit of [limit_mw] Mw. Try to allocate [safe_mw] Mw and
   report failure if interrupted. Then try to allocate [stop_mw -
   safe_mw] Mw more and report failure if not interrupted. The three
   parameters must be chosen accoding to the statistical analysis. *)
let test i safe_mw limit_mw stop_mw : unit =
  let limit = Int64.of_float (limit_mw *. 1000.) in
  let allocated_words_start = allocated_words () in
  let fail = fail i allocated_words_start in
  match (
    M.limit_allocations ~limit @@ fun () ->
    begin
      match
        (* this allocation limit must not interfere with the allocation count *)
        let f () = alloc_m_words safe_mw in
        M.limit_allocations ~limit:M.max_allocation_limit f
      with
      | Error _ -> fail "stopped too early"
      | _ -> ()
    end ;
    begin
      match
        (* this allocation limit must not interfere with the allocation count *)
        let f () = alloc_m_words (stop_mw -. safe_mw) in
        M.limit_allocations ~limit:M.max_allocation_limit f
      with
      | exception _ -> fail "token not set after nested?"
      | Error _ -> ()
      | _ -> fail "not stopped inside nested"
    end ;
    (* both allocating and returning normally should count as being
       interrupted at this point *)
    if i mod 2 = 0 then (
      alloc_enough () ;
      fail "not stopped"
    )
  ) with
  | Ok ((), _) -> fail "not stopped after returning"
  | Error _ -> () (* success *)
  | exception _ -> fail "token not set"

let () =
  (* test takes on the order of n * 2 minutes on today's laptops. *)
  let n = 1 in
  let next = ref 1 in
  let test_concurrent safe_mib limit_mib stop_mib count =
    let threads = ref [] in
    let (first, last) = (!next, !next + n * count - 1) in
    next := last + 1 ;
    for i = first to last do
      let f () = test i safe_mib limit_mib stop_mib in
      let th = Thread.create f () in
      threads := th :: !threads
    done ;
    List.iter Thread.join !threads ;
  in
  (* The individual tests have a probability of less than 10^-14 of
     failing with current sampling rate of 10^-4, so a failure is
     likely to indicate a bug. *)
  test_concurrent 122.29    133.75  139.41  20 ;
  test_concurrent 5.4625    7.5     9.625   300 ;
  test_concurrent 4.627E-3  0.125   0.59625 2000

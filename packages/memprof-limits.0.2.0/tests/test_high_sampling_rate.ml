module M = Memprof_limits

let rec alloc n x = if n = 0 then x else alloc (pred n) (() :: x)

let alloc_3k_words n =
  (* three words per node *)
  ignore (Sys.opaque_identity (alloc (n * 1000)) [])

let alloc_m_words n =
  assert (n mod 3 = 0) ;
  let m = ref ((n / 3) * 1000) in
  let i = 10 in
  while !m > 0 do
    alloc_3k_words (min !m i) ;
    m := !m - i
  done

let mw = 3

let () =
  M.start_memprof_limits () ;
  M.Memprof.start ~sampling_rate:1. ~callstack_size:0 Gc.Memprof.null_tracker ;
  alloc_3k_words 0 ; (* choose a different starting point *)
  match M.limit_allocations ~limit:M.max_allocation_limit (fun () -> alloc_m_words mw) with
  | Ok ((), approx) ->
    let approx_kw = Int64.to_int approx in
    let real_kw = mw * 1000 in
    if approx_kw <> mw * 1000
    then failwith (Printf.sprintf "mismatch at sampling rate 1: real \
                                   %#dkw <> estimated %#dkw" real_kw approx_kw) ;
    exit 0
  | Error _ -> failwith "unexpected limit"

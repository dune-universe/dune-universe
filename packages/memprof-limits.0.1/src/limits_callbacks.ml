(* Low sampling rate is ok; it also determines the rate at which the
   exception arises again until some memory has been freed. *)
let limits_sampling_rate = 1e-4

exception Limit_reached of string

let () = Printexc.register_printer @@ function
| Limit_reached s -> Some ("Memprof-limit: " ^ s)
| _ -> None

let result_of_token ~with_token work =
  with_token @@ fun token ->
  try
    let x = work () in
    if Token.is_set token then
      raise (Limit_reached "function returned normally while limit is set") ;
    Result.ok x
  with e when (
    (* Race *)
    Token.release token ;
    Token.is_set token
  ) -> Result.error e

(* Global memory limits *)

let global_map = Thread_map.make ()

let global_limit = ref 0

let with_global_memory_limit work =
  let with_token f =
    let limit_reached = Token.make () in
    Thread_map.with_value global_map ~value:limit_reached f limit_reached
  in
  result_of_token ~with_token work

let check_global_limit () =
  if !global_limit <> 0 then (
    match Thread_map.get global_map with
    | Some limit_reached when Token.armed limit_reached -> (
        let mem = Gc.(quick_stat ()).heap_words / 1024 in
        if mem > !global_limit / (Sys.word_size / 8)
        then Token.set limit_reached ;
        if Token.is_set limit_reached
        then raise (Limit_reached "global memory limit reached")
      )
    | _ -> ()
  )

(* Allocation limits *)

(* we require enough accuracy on 32-bit platforms and when the user
   has a high sampling rate. *)
type allocation_limit = { token : Token.t
                        ; mutable count : Int64.t (* in words *)
                        ; limit : Int64.t (* in words *)
                        }

let allocation_map = Thread_map.make ()

let word_per_kib = Int64.of_int (1024 / (Sys.word_size / 8))

let max_allocation_limit = Int64.(div max_int word_per_kib)

let make_allocation_limit limit =
  let open Int64 in
  let () =
    if compare limit max_allocation_limit > 0
    || compare limit 0L < 0
    then failwith "with_allocation_limit: invalid limit value"
  in
  (* convert from KiB to words *)
  let limit_words = mul limit word_per_kib in
  (* FIXME: needs masking *)
  let min_limits =
    (* get the min of the current limit (if any) and the requested limit *)
    match Thread_map.get allocation_map with
    | Some allocation_limit'
      when compare allocation_limit'.count limit_words < 0 ->
      allocation_limit'.count
    | _ -> limit_words
  in
  { token = Token.make () ; count = min_limits ; limit = min_limits }

let update_old_count allocation_limit =
  let open Int64 in
  (* FIXME: needs masking *)
  match Thread_map.get allocation_map with
  | Some allocation_limit' ->
    (* If limit is equal to max_allocation_limit, this can overflow if
       count is negative. At current allocation rates of 4GiB/s this
       is unlikely to happen before 500 years. *)
    let allocated = sub allocation_limit.limit allocation_limit.count in
    allocation_limit'.count <- sub allocation_limit'.count allocated ;
    if compare allocation_limit'.count 0L <= 0
    && Token.is_set allocation_limit.token
    && Token.armed allocation_limit'.token
    then Token.set allocation_limit'.token
  | _ -> ()

let with_allocation_limit ~limit work =
  let with_token f =
    let run_f allocation_limit =
      Thread_map.with_value
        allocation_map
        ~value:allocation_limit
        f
        allocation_limit.token
    in
    Fun.with_resource
      ~acquire:make_allocation_limit limit
      run_f
      ~release:update_old_count
  in
  result_of_token ~with_token work


let check_allocation_limit ~sampling_rate =
  let open Int64 in
  match Thread_map.get allocation_map with
  | Some allocation_limit when Token.armed allocation_limit.token -> (
      allocation_limit.count <- sub
                                  allocation_limit.count
                                  (of_float (1. /. sampling_rate));
      if compare allocation_limit.count 0L <= 0
      then Token.set allocation_limit.token ;
      if Token.is_set allocation_limit.token
      then raise (Limit_reached "allocation limit reached")
    )
  | _ -> ()

(* our memprof callback *)
let callback ~sampling_rate =
  (* FIXME: can run in another thread, which threatens the statistical
     analysis. *)
  check_global_limit () ;
  check_allocation_limit ~sampling_rate

let reset () =
  Thread_map.reset global_map ;
  Thread_map.reset allocation_map

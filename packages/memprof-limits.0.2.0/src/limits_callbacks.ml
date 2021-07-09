open Resource_bind

(* Low sampling rate is ok; it also determines the rate at which the
   exception arises again until some memory has been freed.

   See the statistical analysis in `docs/statistical.md`. *)
let limits_sampling_rate = 1e-4

exception Limit_reached of string

let () = Printexc.register_printer @@ function
| Limit_reached s -> Some ("Memprof_limits.Limit_reached: " ^ s)
| _ -> None

let result_of_token token ~f =
  match f () with
  | _ when Token.is_set token -> (
      let e = Limit_reached "function returned normally while limit has been reached" in
      Result.error e
    )
  | x -> Result.ok x
  | exception e when Token.is_set token -> Result.error e

(* Global memory limits *)

let global_map = Thread_map.create ()

let global_limit = ref (-1)

let limit_global_memory ~f =
  let limit_reached_token = Token.create () in
  let f_with_token () =
    let& () = Thread_map.with_value global_map ~value:limit_reached_token in
    f ()
  in
  result_of_token limit_reached_token ~f:f_with_token

(* in async callback *)
let check_global_limit () =
  match Thread_map.get global_map with
  | Some limit_reached -> (
      if !global_limit >= 0 then (
        let mem = Gc.(quick_stat ()).heap_words / 1024 in
        if mem > !global_limit / (Sys.word_size / 8)
        then Token.set limit_reached
      ) ;
      if Token.is_set limit_reached then
        raise (Limit_reached "global memory limit reached")
    )
  | _ -> ()

let status_global_limit () =
  match Thread_map.get global_map with
  | Some limit_reached -> Token.is_set limit_reached
  | _ -> false

(* Allocation limits *)

(* we require enough accuracy on 32-bit platforms and when the user
   has a high sampling rate. *)
type allocation_limit = { token : Token.t
                        ; mutable count : Int64.t (* in words *)
                        ; limit : Int64.t (* in words *)
                        }

let allocation_map = Thread_map.create ()

let words_per_unit = 1000L

let max_allocation_limit = Int64.(div max_int words_per_unit)

let make_allocation_limit limit =
  Masking.assert_blocked () ;
  let open Int64 in
  let () =
    if limit > max_allocation_limit || limit < 0L
    then failwith "with_allocation_limit: invalid limit value"
  in
  let limit_words = mul limit words_per_unit in
  let min_limits =
    (* a call to with_allocation_limit cannot be used to go beyond a
       limit already set in the current thread *)
    match Thread_map.get allocation_map with
    | Some allocation_limit' when allocation_limit'.count < limit_words ->
      allocation_limit'.count
    | _ -> limit_words
  in
  { token = Token.create () ; count = min_limits ; limit = min_limits }

let allocated { count ; limit ; _ } =
  let diff = Int64.sub limit count in
  (* When limit is equal to max_allocation_limit, the difference can
     overflow if count is negative (e.g. when the limit has been
     exceeded). At current max allocation rates of 4GiB/s this is
     unlikely to happen before 500 years. *)
  if diff >= 0L then diff else max_allocation_limit

let sub_allocation_limit allocation_limit alloc =
  let open Int64 in
  let new_count = sub allocation_limit.count alloc in
  let new_count =
    if allocation_limit.count < 0L && new_count > 0L
    then min_int (* underflow *) else new_count
  in
  allocation_limit.count <- new_count

(* With nested allocation limits in the same thread, we must count
   allocations against both limits. We do this by updating the count
   of the enclosing limit at the end of the enclosed one. *)
let update_old_count allocation_limit =
  Masking.assert_blocked () ;
  match Thread_map.get allocation_map with
  | Some allocation_limit' ->
    sub_allocation_limit allocation_limit' (allocated allocation_limit) ;
    if allocation_limit'.count <= 0L && Token.is_set allocation_limit.token
    then Token.set allocation_limit'.token
  | _ -> ()

let limit_allocations ~limit ~f =
  let& allocation_limit =
    Masking.with_resource
      ~acquire:make_allocation_limit limit
      ~release:update_old_count
  in
  let f_with_token () =
    let res =
      let& () = Thread_map.with_value allocation_map ~value:allocation_limit in
      f ()
    in
    let count = Int64.div (allocated allocation_limit) words_per_unit in
    (res, count)
  in
  result_of_token allocation_limit.token ~f:f_with_token

(* in async callback *)
let check_allocation_limit ~sampling_rate (allocation : Gc.Memprof.allocation) =
  let open Int64 in
  match Thread_map.get allocation_map with
  | Some allocation_limit -> (
      (* This runs inside a memprof callback, so we know that there
         can be no concurrent memprof callback. *)
      let n_allocs = allocation.n_samples * (int_of_float (1. /. sampling_rate)) in
      sub_allocation_limit allocation_limit (of_int n_allocs) ;
      if allocation_limit.count <= 0L then
        Token.set allocation_limit.token ;
      if Token.is_set allocation_limit.token then
        raise (Limit_reached "allocation limit reached")
    )
  | _ -> ()

let status_allocation_limit () =
  match Thread_map.get allocation_map with
  | Some allocation_limit -> Token.is_set allocation_limit.token
  | _ -> false

(* token limits *)

let tokens_map : (Token.t list) Thread_map.t = Thread_map.create ()

let limit_with_token ~token ~f =
  let f_with_token () =
    if Token.is_set token then
      raise (Limit_reached "token already set before starting the task") ;
    let new_tokens = match Thread_map.get tokens_map with
      | Some l -> token :: l
      | None -> [token]
    in
    let& () = Thread_map.with_value tokens_map ~value:new_tokens in
    f ()
  in
  result_of_token token ~f:f_with_token

let status_token_limit () =
  match Thread_map.get tokens_map with
  | Some l -> List.exists Token.is_set l
  | _ -> false

let check_token_limit () =
  if status_token_limit () then raise (Limit_reached "token set")

(* our memprof callback *)
let callback ~sampling_rate allocation =
  check_global_limit () ;
  check_allocation_limit ~sampling_rate allocation ;
  check_token_limit ()

let is_interrupted () =
  status_global_limit () ||
  status_allocation_limit () ||
  status_token_limit ()

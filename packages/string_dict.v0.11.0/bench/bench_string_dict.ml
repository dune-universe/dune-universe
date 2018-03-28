open! Base

module type Params = sig
  type value [@@deriving compare]
  val pattern_match : string -> value option
  val names : string list
  val desc : string
end

(* Prevent inlining to avoid things being compiled to a no-op *)
module [@inlined never] Make(P : Params) = struct
  open P

  let pattern_match_exn x =
    match pattern_match x with
    | Some x -> x
    | None   -> raise Caml.Not_found

  let assoc = List.map names ~f:(fun x -> (x, pattern_match_exn x))

  let dict  = String_dict.of_alist_exn assoc
  let map   = Map.of_alist_exn (module String) assoc
  let array =
    List.sort assoc ~compare:(fun (a, _) (b, _) -> String.compare a b)
    |> List.map ~f:(fun (key, v) -> (key, Some v))
    |> Array.of_list

  let binary_search =
    let rec loop t key a b =
      if a >= b then
        None
      else
        let c = Caml.(lsr) (a + b) 1 in
        let (key', v) = t.(c) in
        let d = String.compare key key' in
        if d < 0 then
          loop t key a c
        else if d > 0 then
          loop t key (c + 1) b
        else
          v
    in
    fun t key -> loop t key 0 (Array.length t)

  let binary_search_exn t key =
    match binary_search t key with
    | Some x -> x
    | None   -> raise Caml.Not_found

  let () =
    let () = try failwith Caml.Sys.argv.(0) with _ -> () in
    List.iter names ~f:(fun name ->
      let v = pattern_match_exn name in
      let (=) = [%compare.equal: value] in
      assert (String_dict.find_exn dict  name = v);
      assert (Map. find_exn        map   name = v);
      assert (binary_search_exn    array name = v))

  let keep x = ignore (Caml.Obj.tag (Caml.Obj.repr x) : int)

  let%bench_module "" [@name_suffix desc] =
    (module struct
      let%bench_fun "benchmark overhead"  =
        let f name = keep name in
        fun () -> List.iter names ~f

      let%bench_fun "pattern match" =
        let f name = keep (pattern_match name : value option) in
        fun () -> List.iter names ~f

      let%bench_fun "dict" =
        let f name = keep (String_dict.find dict name : value option) in
        fun () -> List.iter names ~f

      let%bench_fun "map" =
        let f name = keep (Map.find map name : value option) in
        fun () -> List.iter names ~f

      let%bench_fun "binary_search" =
        let f name = keep (binary_search array name : value option) in
        fun () -> List.iter names ~f
    end)
end

module Two_fields = Make
    (struct
      type value = int
      let compare_value = Int.compare

      let desc = "2fields"

      let pattern_match = function
        | "x" -> Some 0
        | "y" -> Some 1
        | _   -> None

      let names = [ "x"; "y" ]
    end)

module Three_fields = Make
    (struct
      type value = int
      let compare_value = Int.compare

      let desc = "3fields"

      let pattern_match = function
        | "x" -> Some 0
        | "y" -> Some 1
        | "z" -> Some 2
        | _   -> None

      let names = [ "x"; "y"; "z" ]
    end)

module Big = Make
    (struct
      type value = int
      let compare_value = Int.compare

      let desc = "Big"

      (* Names taken from the async config record *)
      let pattern_match = function
        | "abort_after_thread_pool_stuck_for"   -> Some 0
        | "check_invariants"                    -> Some 1
        | "detect_invalid_access_from_thread"   -> Some 2
        | "dump_core_on_job_delay"              -> Some 3
        | "epoll_max_ready_events"              -> Some 4
        | "file_descr_watcher"                  -> Some 5
        | "max_inter_cycle_timeout"             -> Some 6
        | "max_num_open_file_descrs"            -> Some 7
        | "max_num_threads"                     -> Some 8
        | "max_num_jobs_per_priority_per_cycle" -> Some 9
        | "min_inter_cycle_timeout"             -> Some 10
        | "print_debug_messages_for"            -> Some 11
        | "record_backtraces"                   -> Some 12
        | "report_thread_pool_stuck_for"        -> Some 13
        | "timing_wheel_config"                 -> Some 14
        | _                                     -> None

      let names =
        [ "abort_after_thread_pool_stuck_for"
        ; "check_invariants"
        ; "detect_invalid_access_from_thread"
        ; "dump_core_on_job_delay"
        ; "epoll_max_ready_events"
        ; "file_descr_watcher"
        ; "max_inter_cycle_timeout"
        ; "max_num_open_file_descrs"
        ; "max_num_threads"
        ; "max_num_jobs_per_priority_per_cycle"
        ; "min_inter_cycle_timeout"
        ; "print_debug_messages_for"
        ; "record_backtraces"
        ; "report_thread_pool_stuck_for"
        ; "timing_wheel_config"
        ]
    end)

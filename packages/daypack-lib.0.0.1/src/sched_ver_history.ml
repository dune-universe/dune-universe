module Task_ = Task

type t = { mutable history : Sched.sched list }

type head_choice =
  | Replace_head of Sched.sched
  | New_head of Sched.sched
  | Do_nothing

type action_record =
  | Updated_head of Sched.sched_id
  | Added_new_head of Sched.sched_id
  | Did_nothing

let make_empty () = { history = [] }

let of_sched_list history = { history }

let map_head (f : Sched.sched -> 'a * head_choice) (t : t) : 'a * action_record
  =
  match t.history with
  | [] -> (
      let empty_sid, _ = Sched.empty in
      let ret, choice = f Sched.empty in
      ( ret,
        match choice with
        | Replace_head (_, sd) ->
          t.history <- [ (empty_sid, sd) ];
          Added_new_head empty_sid
        | New_head (sid, sd) ->
          t.history <- [ (sid, sd) ];
          Added_new_head sid
        | Do_nothing -> Did_nothing ) )
  | hd :: tl -> (
      let hd_sid, hd_sd = hd in
      let ret, choice = f hd in
      ( ret,
        match choice with
        | Replace_head (_, sd) ->
          t.history <- (hd_sid, sd) :: tl;
          if Sched.Equal.sched_data_equal hd_sd sd then Did_nothing
          else Updated_head hd_sid
        | New_head (_, sd) ->
          let new_sid = succ hd_sid in
          t.history <- (new_sid, sd) :: hd :: tl;
          Added_new_head new_sid
        | Do_nothing -> Did_nothing ) )

let map_head_no_ret (f : Sched.sched -> head_choice) (t : t) : action_record =
  let (), ret = map_head (fun x -> ((), f x)) t in
  ret

module Read = struct
  let get_head (t : t) : Sched.sched =
    let res, _ = map_head (fun s -> (s, Do_nothing)) t in
    res
end

module In_place_head = struct
  module Task = struct
    module Add = struct
      let add_task ~parent_user_id (data : Task_.task_data)
          (task_inst_data_list : Task_.task_inst_data list) (t : t) :
        Task_.task * Task_.task_inst list * action_record =
        let (task, task_inst_list), ar =
          map_head
            (fun sched ->
               let task, task_inst_list, sched =
                 Sched.Task.Add.add_task ~parent_user_id data task_inst_data_list
                   sched
               in
               ((task, task_inst_list), Replace_head sched))
            t
        in
        (task, task_inst_list, ar)
    end

    module Move = struct
      let move_task_internal
          ~(move_task_by_id : Task_.task_id -> Sched.sched -> Sched.sched)
          (task_inst_id : Task_.task_id) (t : t) : action_record =
        map_head_no_ret
          (fun sched ->
             let sched = move_task_by_id task_inst_id sched in
             Replace_head sched)
          t

      let move_task_to_completed (task_id : Task_.task_id) (t : t) :
        action_record =
        move_task_internal
          ~move_task_by_id:Sched.Task.Move.move_task_to_completed task_id t

      let move_task_to_uncompleted (task_id : Task_.task_id) (t : t) :
        action_record =
        move_task_internal
          ~move_task_by_id:Sched.Task.Move.move_task_to_uncompleted task_id t

      let move_task_to_discarded (task_id : Task_.task_id) (t : t) :
        action_record =
        move_task_internal
          ~move_task_by_id:Sched.Task.Move.move_task_to_discarded task_id t
    end
  end

  module Task_inst = struct
    module Add = struct
      let add_task_inst ~parent_task_id (data : Task_.task_inst_data) (t : t) :
        Task_.task_inst * action_record =
        map_head
          (fun sched ->
             let task_inst, sched =
               Sched.Task_inst.Add.add_task_inst ~parent_task_id data sched
             in
             (task_inst, Replace_head sched))
          t
    end

    module Move = struct
      let move_task_inst_internal
          ~(move_task_inst_by_id :
              Task_.task_inst_id -> Sched.sched -> Sched.sched)
          (task_inst_id : Task_.task_inst_id) (t : t) : action_record =
        map_head_no_ret
          (fun sched ->
             let sched = move_task_inst_by_id task_inst_id sched in
             Replace_head sched)
          t

      let move_task_inst_to_completed (task_inst_id : Task_.task_inst_id)
          (t : t) : action_record =
        move_task_inst_internal
          ~move_task_inst_by_id:Sched.Task_inst.Move.move_task_inst_to_completed
          task_inst_id t

      let move_task_inst_to_uncompleted (task_inst_id : Task_.task_inst_id)
          (t : t) : action_record =
        move_task_inst_internal
          ~move_task_inst_by_id:
            Sched.Task_inst.Move.move_task_inst_to_uncompleted task_inst_id t

      let move_task_inst_to_discarded (task_inst_id : Task_.task_inst_id)
          (t : t) : action_record =
        move_task_inst_internal
          ~move_task_inst_by_id:Sched.Task_inst.Move.move_task_inst_to_discarded
          task_inst_id t
    end
  end

  module Task_seg = struct
    module Move = struct
      let move_task_seg_internal
          ~(move_task_seg_by_id :
              Task_.task_seg_id -> Sched.sched -> Sched.sched)
          (task_seg_id : Task_.task_seg_id) (t : t) : action_record =
        map_head_no_ret
          (fun sched ->
             let sched = move_task_seg_by_id task_seg_id sched in
             Replace_head sched)
          t

      let move_task_seg_to_completed (task_seg_id : Task_.task_seg_id) (t : t) :
        action_record =
        move_task_seg_internal
          ~move_task_seg_by_id:Sched.Task_seg.Move.move_task_seg_to_completed
          task_seg_id t

      let move_task_seg_to_uncompleted (task_seg_id : Task_.task_seg_id) (t : t)
        : action_record =
        move_task_seg_internal
          ~move_task_seg_by_id:Sched.Task_seg.Move.move_task_seg_to_uncompleted
          task_seg_id t

      let move_task_seg_to_discarded (task_seg_id : Task_.task_seg_id) (t : t) :
        action_record =
        move_task_seg_internal
          ~move_task_seg_by_id:Sched.Task_seg.Move.move_task_seg_to_discarded
          task_seg_id t
    end
  end

  module Sched_req = struct
    module Add = struct
      let add_sched_req (data : Sched_req.sched_req_data) (t : t) :
        (Sched_req.sched_req, unit) result * action_record =
        map_head
          (fun sched ->
             match Sched.Sched_req.Add.add_sched_req_data data sched with
             | Ok (sched_req, sched) -> (Ok sched_req, Replace_head sched)
             | Error () -> (Error (), Do_nothing))
          t
    end
  end

  module Recur = struct
    let instantiate ~start ~end_exc (t : t) : action_record =
      map_head_no_ret
        (fun sched ->
           let sched = Sched.Recur.instantiate ~start ~end_exc sched in
           Replace_head sched)
        t
  end

  module Progress = struct
    module Add = struct
      let add_task_seg_progress_chunk (task_seg_id : Task_.task_seg_id)
          (chunk : int64 * int64) (t : t) : action_record =
        map_head_no_ret
          (fun sched ->
             let sched =
               Sched.Progress.Add.add_task_seg_progress_chunk task_seg_id chunk
                 sched
             in
             Replace_head sched)
          t

      let add_task_inst_progress_chunk (task_inst_id : Task_.task_inst_id)
          (chunk : int64 * int64) (t : t) : action_record =
        map_head_no_ret
          (fun sched ->
             let sched =
               Sched.Progress.Add.add_task_inst_progress_chunk task_inst_id chunk
                 sched
             in
             Replace_head sched)
          t
    end
  end
end

module Maybe_append_to_head = struct
  let remove_task (task_id : Task_.task_id) (t : t) : action_record =
    map_head_no_ret
      (fun hd ->
         let task_seg_place_seq =
           Sched.Agenda.Find.find_task_seg_place_seq_by_task_id task_id hd
         in
         let no_task_seg_places_recorded = OSeq.is_empty task_seg_place_seq in
         let no_task_inst_progress_recorded =
           OSeq.is_empty
             (Sched.Progress.Find.find_task_inst_progress_chunk_seq_by_task_id
                task_id hd)
         in
         let no_task_seg_progress_recorded =
           OSeq.is_empty
             (Sched.Progress.Find.find_task_seg_progress_chunk_seq_by_task_id
                task_id hd)
         in
         let hd' =
           hd
           |> Sched.Task.Remove.remove_task_all task_id
           |> Sched.Sched_req.Remove.Pending.remove_pending_sched_req_by_task_id
             task_id
           |> Sched.Sched_req.Remove.Record.remove_sched_req_record_by_task_id
             task_id
         in
         if
           no_task_seg_places_recorded
           && no_task_inst_progress_recorded
           && no_task_seg_progress_recorded
         then Replace_head hd'
         else
           let hd' =
             hd'
             |> Sched.Sched_req.Remove.Record.remove_sched_req_record_by_task_id
               task_id
             |> Sched.Agenda.Remove.remove_task_seg_place_seq task_seg_place_seq
           in
           New_head hd')
      t

  let remove_task_inst (task_inst_id : Task_.task_inst_id) (t : t) :
    action_record =
    map_head_no_ret
      (fun hd ->
         let task_seg_place_seq =
           Sched.Agenda.Find.find_task_seg_place_seq_by_task_inst_id task_inst_id
             hd
         in
         let no_task_seg_places_recorded = OSeq.is_empty task_seg_place_seq in
         let no_task_inst_progress_recorded =
           OSeq.is_empty
             (Sched.Progress.Find.find_task_inst_progress_chunk_seq task_inst_id
                hd)
         in
         let no_task_seg_progress_recorded =
           OSeq.is_empty
             (Sched.Progress.Find
              .find_task_seg_progress_chunk_seq_by_task_inst_id task_inst_id hd)
         in
         let hd' =
           hd
           |> Sched.Task_inst.Remove.remove_task_inst_all task_inst_id
           |> Sched.Sched_req.Remove.Pending
              .remove_pending_sched_req_by_task_inst_id task_inst_id
           |> Sched.Sched_req.Remove.Record
              .remove_sched_req_record_by_task_inst_id task_inst_id
         in
         if
           no_task_seg_places_recorded
           && no_task_inst_progress_recorded
           && no_task_seg_progress_recorded
         then Replace_head hd'
         else
           let hd' =
             hd'
             |> Sched.Sched_req.Remove.Record
                .remove_sched_req_record_by_task_inst_id task_inst_id
             |> Sched.Agenda.Remove.remove_task_seg_place_seq task_seg_place_seq
           in
           New_head hd')
      t

  let remove_task_seg_progress_chunk (task_seg_id : Task_.task_seg_id)
      (chunk : int64 * int64) (t : t) : action_record =
    map_head_no_ret
      (fun sched ->
         let chunks =
           Sched.Progress.Find.find_task_seg_progress_chunk_set task_seg_id sched
         in
         if Int64_int64_set.mem chunk chunks then
           let hd' =
             sched
             |> Sched.Progress.Remove.remove_task_seg_progress_chunk task_seg_id
               chunk
           in
           Replace_head hd'
         else Do_nothing)
      t

  let remove_task_inst_progress_chunk (task_inst_id : Task_.task_inst_id)
      (chunk : int64 * int64) (t : t) : action_record =
    map_head_no_ret
      (fun sched ->
         let chunks =
           Sched.Progress.Find.find_task_inst_progress_chunk_set task_inst_id
             sched
         in
         if Int64_int64_set.mem chunk chunks then
           let hd' =
             sched
             |> Sched.Progress.Remove.remove_task_inst_progress_chunk
               task_inst_id chunk
           in
           Replace_head hd'
         else Do_nothing)
      t

  let remove_pending_sched_req (sched_req_id : Sched_req.sched_req_id) (t : t) :
    action_record =
    map_head_no_ret
      (fun sched ->
         match
           Sched.Sched_req.Find.Pending.find_pending_sched_req sched_req_id sched
         with
         | None -> Do_nothing
         | Some _ ->
           New_head
             (Sched.Sched_req.Remove.Pending.remove_pending_sched_req
                sched_req_id sched))
      t

  let sched ~start ~end_exc ~include_sched_reqs_starting_within_time_slot
      ~include_sched_reqs_ending_within_time_slot ~up_to_sched_req_id_inc
      (t : t) : (unit, unit) result * action_record =
    map_head
      (fun hd ->
         let sched_req_records, hd' =
           hd
           |> Sched.Sched_req.Allocate_task_segs
              .allocate_task_segs_for_pending_sched_reqs ~start ~end_exc
             ~include_sched_reqs_starting_within_time_slot
             ~include_sched_reqs_ending_within_time_slot
             ~up_to_sched_req_id_inc
         in
         match sched_req_records with
         | [] -> (Ok (), Do_nothing)
         | _ -> (
             let possible_scheds =
               Sched_search.backtracking_search_multi ~start ~end_exc ~base:hd'
                 sched_req_records
             in
             match possible_scheds () with
             | Seq.Nil -> (Error (), Do_nothing)
             | Seq.Cons (hd', _) -> (Ok (), New_head hd') ))
      t
end

module Append_to_head = struct
  let snapshot (t : t) : action_record =
    map_head_no_ret (fun sched -> New_head sched) t
end

module Equal = struct
  let equal t1 t2 =
    List.for_all2
      (fun s1 s2 -> Sched.Equal.sched_equal s1 s2)
      t1.history t2.history
end

module Serialize = struct
  let base_and_diffs_of_list (l : Sched.sched list) :
    (Sched.sched * Sched.sched_diff list) option =
    let rec aux
        (base_and_last_and_diffs :
           (Sched.sched * Sched.sched * Sched.sched_diff list) option)
        (l : Sched.sched list) =
      match l with
      | [] -> (
          match base_and_last_and_diffs with
          | None -> None
          | Some (base, _, diffs) -> Some (base, List.rev diffs) )
      | sched :: rest -> (
          match base_and_last_and_diffs with
          | None -> aux (Some (sched, sched, [])) rest
          | Some (base, last, diffs) ->
            let diff = Sched.Diff.diff_sched ~old:last sched in
            aux (Some (base, sched, diff :: diffs)) rest )
    in
    aux None (List.rev l)

  let to_base_and_diffs (t : t) : (Sched.sched * Sched.sched_diff list) option =
    base_and_diffs_of_list t.history

  let write_to_dir ~(dir : string) (t : t) : (unit, string) result =
    try
      if Sys.is_directory dir then (
        match to_base_and_diffs t with
        | None -> Ok ()
        | Some (base, diffs) ->
          (let base_str = Sched.Serialize.json_string_of_sched base in
           let oc = open_out (Filename.concat dir "sched_v0.json") in
           Fun.protect
             ~finally:(fun () -> close_out oc)
             (fun () -> output_string oc base_str));
          diffs
          |> List.to_seq
          |> Seq.map Sched.Serialize.json_string_of_sched_diff
          |> OSeq.iteri (fun i sched_diff_str ->
              let oc =
                open_out
                  (Filename.concat dir
                     (Printf.sprintf "sched_v%d.json" (i + 1)))
              in
              Fun.protect
                ~finally:(fun () -> close_out oc)
                (fun () -> output_string oc sched_diff_str));
          Ok () )
      else Error "File is not a directory"
    with
    | Sys_error msg -> Error msg
    | _ -> Error "Failed to write to directory"
end

module Deserialize = struct
  let list_of_base_and_diffs (base : Sched.sched)
      (diffs : Sched.sched_diff list) : Sched.sched list =
    let rec aux (acc : Sched.sched list) (cur : Sched.sched)
        (diffs : Sched.sched_diff list) : Sched.sched list =
      match diffs with
      | [] -> acc
      | diff :: diffs ->
        let next = Sched.Diff.add_diff_sched diff cur in
        aux (next :: acc) next diffs
    in
    aux [ base ] base diffs

  let of_base_and_diffs base diffs : t =
    let history = list_of_base_and_diffs base diffs in
    { history }

  let read_from_dir ~(dir : string) : (t, string) result =
    try
      if Sys.is_directory dir then
        if Sys.readdir dir = [||] then Ok (make_empty ())
        else
          let base =
            let ic = open_in (Filename.concat dir "sched_v0.json") in
            Fun.protect
              ~finally:(fun () -> close_in ic)
              (fun () -> really_input_string ic (in_channel_length ic))
            |> Sched.Deserialize.sched_of_json_string
          in
          let diffs =
            Sys.readdir dir
            |> Array.to_seq
            |> Seq.filter_map (fun s ->
                try
                  Scanf.sscanf s "sched_v%d.json" (fun i ->
                      if i = 0 then None else Some (i, s))
                with Stdlib.Scanf.Scan_failure _ -> None)
            |> Seq.map (fun (i, s) ->
                let ic = open_in (Filename.concat dir s) in
                ( i,
                  Fun.protect
                    ~finally:(fun () -> close_in ic)
                    (fun () -> really_input_string ic (in_channel_length ic))
                ))
            |> OSeq.sort ~cmp:(fun (i1, _) (i2, _) -> compare i1 i2)
            |> Seq.map (fun (_i, s) ->
                Sched.Deserialize.sched_diff_of_json_string s)
            |> List.of_seq
          in
          Ok (of_base_and_diffs base diffs)
      else Error "Path is not a directory"
    with
    | Sys_error msg -> Error msg
    | _ -> Error "Failed to read from directory"
end

module To_string = struct
  let debug_string_of_sched_ver_history ?(indent_level = 0)
      ?(buffer = Buffer.create 4096) (t : t) =
    Debug_print.bprintf ~indent_level buffer "sched ver history\n";
    List.iter
      (fun sched ->
         Sched.To_string.debug_string_of_sched ~indent_level:(indent_level + 1)
           ~buffer sched
         |> ignore)
      (List.rev t.history);
    Buffer.contents buffer

  let debug_string_of_action_record ?(indent_level = 0)
      ?(buffer = Buffer.create 4096) (ar : action_record) =
    Debug_print.bprintf ~indent_level buffer "action record: %s"
      ( match ar with
        | Updated_head id -> Printf.sprintf "updated head sched #%d" id
        | Added_new_head id -> Printf.sprintf "added new head sched #%d" id
        | Did_nothing -> "did nothing" );
    Buffer.contents buffer
end

module Print = struct
  let debug_print_sched_ver_history ?(indent_level = 0) (t : t) =
    print_string (To_string.debug_string_of_sched_ver_history ~indent_level t)

  let debug_print_action_record ?(indent_level = 0) (ar : action_record) =
    print_endline (To_string.debug_string_of_action_record ~indent_level ar)
end

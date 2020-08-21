open Daypack_lib

let debug_single_task_seg_shift () =
  print_endline "Debug print for Task_seg_place_gens.single_task_seg_shift";
  let incre = 2L in
  let cur_pos = 0L in
  let task_seg_id = (0L, 0L, 0L, 0L, None) in
  let task_seg_size = 4L in
  let task_seg = (task_seg_id, task_seg_size) in
  let time_slots = [ (0L, 10L); (20L, 24L) ] in
  Printf.printf "incre : %Ld\n" incre;
  Printf.printf "cur_pos : %Ld\n" cur_pos;
  Printf.printf "task_seg_size : %Ld\n" task_seg_size;
  List.iteri
    (fun i (start, end_exc) ->
       Printf.printf "time_slot #%d : (%Ld, %Ld)\n" i start end_exc)
    time_slots;
  print_newline ();
  let time_slots = List.to_seq time_slots in
  Task_seg_place_gens.single_task_seg_shift ~incre ~cur_pos ~task_seg time_slots
  |> Seq.iter (fun (id, start, end_exc) ->
      Printf.printf "possible time slot to use : %s [%Ld, %Ld)\n"
        (Task.Id.string_of_task_seg_id id)
        start end_exc)

let debug_single_task_seg_shift_rev () =
  print_endline "Debug print for Task_seg_place_gens.single_task_seg_shift_rev";
  let incre = 2L in
  let cur_end_pos_exc = 50L in
  let task_seg_id = (0L, 0L, 0L, 0L, None) in
  let task_seg_size = 4L in
  let task_seg = (task_seg_id, task_seg_size) in
  let time_slots = [ (0L, 10L); (20L, 24L) ] in
  Printf.printf "incre : %Ld\n" incre;
  Printf.printf "cur_end_pos_exc : %Ld\n" cur_end_pos_exc;
  Printf.printf "task_seg_size : %Ld\n" task_seg_size;
  List.iteri
    (fun i (start, end_exc) ->
       Printf.printf "time_slot #%d : (%Ld, %Ld)\n" i start end_exc)
    time_slots;
  print_newline ();
  let time_slots = List.to_seq time_slots in
  Task_seg_place_gens.single_task_seg_shift_rev ~incre ~cur_end_pos_exc
    ~task_seg time_slots
  |> Seq.iter (fun (id, start, end_exc) ->
      Printf.printf "possible time slot to use : %s [%Ld, %Ld)\n"
        (Task.Id.string_of_task_seg_id id)
        start end_exc)

let debug_multi_task_segs_shift () =
  print_endline "Debug print for Task_seg_place_gens.multi_task_segs_shift";
  let incre = 2L in
  let task_segs =
    [ ((0L, 0L, 0L, 0L, None), 4L); ((0L, 0L, 0L, 1L, None), 2L) ]
  in
  let time_slots = [ (0L, 10L); (20L, 24L) ] in
  Printf.printf "incre : %Ld\n" incre;
  List.iter
    (fun (id, size) ->
       Printf.printf "time seg id : %s size : %Ld\n"
         (Task.Id.string_of_task_seg_id id)
         size)
    task_segs;
  List.iteri
    (fun i (start, end_exc) ->
       Printf.printf "time_slot #%d : (%Ld, %Ld)\n" i start end_exc)
    time_slots;
  print_newline ();
  let time_slots = List.to_seq time_slots in
  Task_seg_place_gens.multi_task_segs_shift ~incre ~task_segs time_slots
  |> Seq.iter (fun pos_s ->
      Printf.printf "possible schedule :\n";
      List.iter
        (fun (id, start, end_exc) ->
           Printf.printf "  %s - [%Ld, %Ld)\n"
             (Task.Id.string_of_task_seg_id id)
             start end_exc)
        pos_s)

let debug_single_task_seg_single_split () =
  print_endline
    "Debug print for Task_seg_place_gens.single_task_seg_single_split";
  let min_seg_size = 2L in
  let max_seg_size = 20L in
  let task_seg_id = (0L, 0L, 0L, 0L, None) in
  let task_seg_size = 10L in
  let task_seg = (task_seg_id, task_seg_size) in
  Printf.printf "min_seg_size : %Ld\n" min_seg_size;
  Printf.printf "task_seg_size : %Ld\n" task_seg_size;
  Task_seg_place_gens.single_task_seg_single_split ~min_seg_size ~max_seg_size
    ~cur_split_pos:0L ~task_seg
  |> Seq.iter (fun ((_, size1), (_, size2)) ->
      Printf.printf "splits : %Ld, %Ld\n" size1 size2)

let debug_single_task_seg_multi_splits_exact () =
  print_endline
    "Debug print for Task_seg_place_gens.single_task_seg_multi_splits_exact";
  let min_seg_size = 3L in
  let max_seg_size = Some 20L in
  let split_count = 2L in
  let task_seg_id = (0L, 0L, 0L, 0L, None) in
  let task_seg_size = 10L in
  let task_seg = (task_seg_id, task_seg_size) in
  Printf.printf "min_seg_size : %Ld\n" min_seg_size;
  Printf.printf "split count : %Ld\n" split_count;
  Printf.printf "task_seg_size : %Ld\n" task_seg_size;
  Task_seg_place_gens.single_task_seg_multi_splits_exact ~min_seg_size
    ~max_seg_size ~split_count ~task_seg
  |> Seq.iter (fun splits ->
      Printf.printf "splits :\n";
      List.iter
        (fun (id, x) ->
           Printf.printf "  %s - %Ld\n" (Task.Id.string_of_task_seg_id id) x)
        splits)

let debug_single_task_seg_multi_splits_max () =
  print_endline
    "Debug print for Task_seg_place_gens.single_task_seg_multi_splits_max";
  let min_seg_size = 3L in
  let max_seg_size = Some 20L in
  let split_count = 2L in
  let task_seg_id = (0L, 0L, 0L, 0L, None) in
  let task_seg_size = 10L in
  let task_seg = (task_seg_id, task_seg_size) in
  Printf.printf "min_seg_size : %Ld\n" min_seg_size;
  Printf.printf "split count : %Ld\n" split_count;
  Printf.printf "task_seg_size : %Ld\n" task_seg_size;
  Task_seg_place_gens.single_task_seg_multi_splits_max ~min_seg_size
    ~max_seg_size ~split_count ~task_seg
  |> Seq.iter (fun splits ->
      Printf.printf "splits :\n";
      List.iter
        (fun (id, x) ->
           Printf.printf "  %s - %Ld\n" (Task.Id.string_of_task_seg_id id) x)
        splits)

let debug_single_task_seg_multi_splits_exact_shift () =
  print_endline
    "Debug print for \
     Task_seg_place_gens.single_task_seg_multi_splits_exact_shift";
  let min_seg_size = 3L in
  let max_seg_size = Some 20L in
  let incre = 1L in
  let split_count = 0L in
  let task_seg_id = (0L, 0L, 0L, 0L, None) in
  let task_seg_size = 7L in
  let task_seg = (task_seg_id, task_seg_size) in
  let time_slots = [ (0L, 10L); (11L, 15L) ] in
  Printf.printf "min_seg_size : %Ld\n" min_seg_size;
  Printf.printf "split count : %Ld\n" split_count;
  Printf.printf "task_seg_size : %Ld\n" task_seg_size;
  List.iteri
    (fun i (start, end_exc) ->
       Printf.printf "time_slot #%d : (%Ld, %Ld)\n" i start end_exc)
    time_slots;
  let time_slots = List.to_seq time_slots in
  Task_seg_place_gens.single_task_seg_multi_splits_exact_shift ~min_seg_size
    ~max_seg_size ~split_count ~incre ~task_seg time_slots
  |> Seq.iter (fun splits ->
      Printf.printf "splits :\n";
      List.iter
        (fun (id, start, end_exc) ->
           Printf.printf "  %s - [%Ld, %Ld)\n"
             (Task.Id.string_of_task_seg_id id)
             start end_exc)
        splits)

let debug_single_task_seg_multi_splits_max_shift () =
  print_endline
    "Debug print for Task_seg_place_gens.single_task_seg_multi_splits_max_shift";
  let min_seg_size = 3L in
  let max_seg_size = Some 20L in
  let incre = 1L in
  let split_count = 0L in
  let task_seg_id = (0L, 0L, 0L, 0L, None) in
  let task_seg_size = 7L in
  let task_seg = (task_seg_id, task_seg_size) in
  let time_slots = [ (0L, 10L); (11L, 15L) ] in
  Printf.printf "min_seg_size : %Ld\n" min_seg_size;
  Printf.printf "split count : %Ld\n" split_count;
  Printf.printf "task_seg_size : %Ld\n" task_seg_size;
  List.iteri
    (fun i (start, end_exc) ->
       Printf.printf "time_slot #%d : (%Ld, %Ld)\n" i start end_exc)
    time_slots;
  let time_slots = List.to_seq time_slots in
  Task_seg_place_gens.single_task_seg_multi_splits_max_shift ~min_seg_size
    ~max_seg_size ~split_count ~incre ~task_seg time_slots
  |> Seq.iter (fun splits ->
      Printf.printf "splits :\n";
      List.iter
        (fun (id, start, end_exc) ->
           Printf.printf "  %s - [%Ld, %Ld)\n"
             (Task.Id.string_of_task_seg_id id)
             start end_exc)
        splits)

let debug_multi_tasks_interleave () =
  print_endline "Debug print for Task_seg_place_gens.multi_task_segs_interleave";
  let task_segs =
    [
      ((0L, 0L, 0L, 0L, None), 2L);
      ((0L, 0L, 0L, 1L, None), 5L);
      ((0L, 0L, 0L, 2L, None), 9L);
    ]
  in
  let time_slots = [ (0L, 100L) ] in
  let time_slots = List.to_seq time_slots in
  let interval_size = 2L in
  let s =
    Task_seg_place_gens.multi_task_segs_interleave ~interval_size ~task_segs
      time_slots
  in
  Seq.iter
    (fun (id, start, end_exc) ->
       Printf.printf "possible time slot to use : %s [%Ld, %Ld)\n"
         (Task.Id.string_of_task_seg_id id)
         start end_exc)
    s

let debug_single_task_seg_multi_even_splits () =
  print_endline
    "Debug print for Task_seg_place_gens.single_task_seg_multi_even_splits";
  let task_seg = ((0L, 0L, 0L, 0L, None), 230L) in
  let buckets =
    [
      (0L, 24L);
      (24L, 48L);
      (48L, 72L);
      (100L, 124L);
      (124L, 148L);
      (148L, 172L);
      (200L, 224L);
      (224L, 248L);
      (248L, 272L);
      (300L, 324L);
      (324L, 348L);
      (348L, 372L);
    ]
  in
  let usable_time_slots = [ (0L, 500L) ] in
  let usable_time_slots = List.to_seq usable_time_slots in
  let s =
    Task_seg_place_gens.single_task_seg_multi_even_splits ~incre:5L ~task_seg
      ~buckets ~usable_time_slots
  in
  s
  |> Seq.iter (fun splits ->
      Printf.printf "splits :\n";
      List.iter
        (fun (id, start, end_exc) ->
           Printf.printf "  %s - [%Ld, %Ld)\n"
             (Task.Id.string_of_task_seg_id id)
             start end_exc)
        splits)

let debug_slice_time_slots_start () =
  print_endline "Debug print for Time_slots.slice start";
  let time_slots = [ (0L, 10L); (11L, 20L); (25L, 30L) ] in
  let time_slots = List.to_seq time_slots in
  Time_slots.Slice.slice ~start:12L time_slots
  |> Seq.iter (fun (start, end_exc) ->
      Printf.printf "  [%Ld, %Ld)\n" start end_exc)

let debug_slice_time_slots_end_exc () =
  print_endline "Debug print for Time_slots.slice end_exc";
  let time_slots = [ (0L, 10L); (11L, 20L); (25L, 30L) ] in
  let time_slots = List.to_seq time_slots in
  Time_slots.Slice.slice ~end_exc:12L time_slots
  |> Seq.iter (fun (start, end_exc) ->
      Printf.printf "  [%Ld, %Ld)\n" start end_exc)

let debug_slice_time_slots_start_rev () =
  print_endline "Debug print for Time_slots.slice_rev start";
  let time_slots = [ (0L, 10L); (11L, 20L); (25L, 30L) ] in
  let time_slots = time_slots |> List.rev |> List.to_seq in
  Time_slots.Slice.slice_rev ~start:12L time_slots
  |> Seq.iter (fun (start, end_exc) ->
      Printf.printf "  [%Ld, %Ld)\n" start end_exc)

let debug_slice_time_slots_end_exc_rev () =
  print_endline "Debug print for Time_slots.slice_rev end_exc";
  let time_slots = [ (0L, 10L); (11L, 20L); (25L, 30L) ] in
  let time_slots = time_slots |> List.rev |> List.to_seq in
  Time_slots.Slice.slice_rev ~end_exc:12L time_slots
  |> Seq.iter (fun (start, end_exc) ->
      Printf.printf "  [%Ld, %Ld)\n" start end_exc)

let debug_normalize_time_slots () =
  print_endline "Debug print for Time_slots.normalize";
  let time_slots =
    [ (0L, 10L); (10L, 11L); (11L, 20L); (22L, 25L); (20L, 22L); (25L, 30L) ]
  in
  let time_slots = List.to_seq time_slots in
  Time_slots.Normalize.normalize time_slots
  |> Seq.iter (fun (start, end_exc) ->
      Printf.printf "  [%Ld, %Ld)\n" start end_exc)

let debug_invert_time_slots () =
  print_endline "Debug print for Time_slots.invert";
  let time_slots = [ (0L, 10L); (11L, 20L); (25L, 30L) ] in
  let time_slots = List.to_seq time_slots in
  Time_slots.invert ~start:0L ~end_exc:22L time_slots
  |> Seq.iter (fun (start, end_exc) ->
      Printf.printf "  [%Ld, %Ld)\n" start end_exc)

let debug_relative_complement_time_slots () =
  print_endline "Debug print for Time_slots.relative_complement";
  let mem_of_time_slots = [ (0L, 10L); (11L, 20L); (25L, 30L) ] in
  let not_mem_of_time_slots = [ (0L, 5L); (6L, 15L); (25L, 30L) ] in
  let mem_of_time_slots = List.to_seq mem_of_time_slots in
  let not_mem_of_time_slots = List.to_seq not_mem_of_time_slots in
  Time_slots.relative_complement ~not_mem_of:not_mem_of_time_slots
    mem_of_time_slots
  |> Seq.iter (fun (start, end_exc) ->
      Printf.printf "  [%Ld, %Ld)\n" start end_exc)

let debug_inter_time_slots () =
  print_endline "Debug print for Time_slots.inter";
  let time_slots1 = [ (0L, 10L); (11L, 20L); (25L, 30L) ] in
  let time_slots2 = [ (0L, 1L); (2L, 3L); (10L, 20L); (25L, 30L) ] in
  let time_slots1 = List.to_seq time_slots1 in
  let time_slots2 = List.to_seq time_slots2 in
  Time_slots.inter time_slots1 time_slots2
  |> Seq.iter (fun (start, end_exc) ->
      Printf.printf "  [%Ld, %Ld)\n" start end_exc)

let debug_union_time_slots () =
  print_endline "Debug print for Time_slots.union";
  let time_slots1 = [ (0L, 10L); (11L, 20L); (25L, 30L) ] in
  let time_slots2 = [ (0L, 1L); (2L, 3L); (10L, 20L); (25L, 30L) ] in
  let time_slots1 = List.to_seq time_slots1 in
  let time_slots2 = List.to_seq time_slots2 in
  Time_slots.Union.union time_slots1 time_slots2
  |> Seq.iter (fun (start, end_exc) ->
      Printf.printf "  [%Ld, %Ld)\n" start end_exc)

let debug_sched_backtracking_search_pending () =
  print_endline "Debug print for Sched_search.backtracking_search_pending";
  let sched_req_data_list =
    let open Sched_req_data_unit_skeleton in
    [
      [
        Split_even
          {
            task_seg_related_data = ((0L, 0L, 0L), 20L);
            time_slots = [ (0L, 50L) ];
            buckets = [ (0L, 10L); (10L, 20L) ];
            incre = 1L;
          };
        Split_even
          {
            task_seg_related_data = ((0L, 0L, 0L), 20L);
            time_slots = [ (50L, 100L) ];
            buckets = [ (50L, 60L); (60L, 70L) ];
            incre = 1L;
          };
      ];
      [
        Shift
          {
            task_seg_related_data_list = [ ((0L, 0L, 0L), 10L) ];
            time_slots = [ (0L, 50L) ];
            incre = 1L;
          };
      ];
      [
        Shift
          {
            task_seg_related_data_list = [ ((0L, 0L, 0L), 10L) ];
            time_slots = [ (0L, 50L) ];
            incre = 1L;
          };
      ];
      [
        Split_and_shift
          {
            task_seg_related_data = ((0L, 0L, 2L), 15L);
            time_slots = [ (50L, 150L) ];
            split_count = Max_split 5L;
            incre = 1L;
            min_seg_size = 2L;
            max_seg_size = None;
          };
      ];
      [
        Time_share
          {
            task_seg_related_data_list =
              [ ((0L, 0L, 2L), 30L); ((0L, 0L, 3L), 20L) ];
            time_slots = [ (50L, 200L) ];
            interval_size = 30L;
          };
      ];
      [
        Push_toward
          {
            task_seg_related_data = ((0L, 0L, 4L), 10L);
            target = 100L;
            time_slots = [ (0L, 200L) ];
            incre = 1L;
          };
      ];
      [
        Push_toward
          {
            task_seg_related_data = ((0L, 0L, 5L), 10L);
            target = 75L;
            time_slots = [ (0L, 200L) ];
            incre = 1L;
          };
      ];
    ]
  in
  let quota =
    [
      ((0L, 0L, 0L), 20L);
      ((0L, 0L, 1L), 10L);
      ((0L, 0L, 2L), 50L);
      ((0L, 0L, 3L), 10L);
      ((0L, 0L, 4L), 10L);
      ((0L, 0L, 5L), 10L);
      ((0L, 0L, 6L), 10L);
      ((0L, 0L, 7L), 10L);
    ]
    |> List.to_seq
    |> Daypack_lib.Task_inst_id_map.of_seq
  in
  print_endline "scheduling requests";
  List.iter
    (Sched_req.Print.debug_print_sched_req_data ~indent_level:1)
    sched_req_data_list;
  print_newline ();
  let _, base =
    Sched.empty
    |> Sched.Quota.update_quota quota
    |> Sched.Sched_req.Add.add_sched_req_data_list sched_req_data_list
    |> Result.get_ok
  in
  Sched_search.backtracking_search_pending ~start:0L ~end_exc:50L
    ~include_sched_reqs_starting_within_time_slot:true
    ~include_sched_reqs_ending_within_time_slot:true
    ~up_to_sched_req_id_inc:None ~base
  |> OSeq.take 1
  |> Seq.iter (fun sched -> Sched.Print.debug_print_sched sched)

let debug_sched_agenda_range () =
  let sched =
    Sched.empty
    |> Sched.Agenda.Add.add_task_seg_place_list
      [
        ((0L, 1L, 0L, 0L, None), 0L, 10L);
        ((0L, 1L, 1L, 0L, None), 10L, 20L);
        ((0L, 2L, 0L, 0L, Some 1L), 20L, 30L);
        ((0L, 3L, 0L, 0L, None), 30L, 50L);
        ((0L, 4L, 0L, 0L, None), 60L, 90L);
      ]
  in
  sched
  |> Sched.Agenda.To_seq.task_seg_place_all ~start:9L ~end_exc:40L
    ~include_task_seg_place_starting_within_time_slot:true
    ~include_task_seg_place_ending_within_time_slot:true
  |> Seq.iter (fun task_seg_place ->
      Task.Print.debug_print_task_seg_place task_seg_place)

let debug_sched_usage_simulation () =
  let add_task ~parent_user_id task_data task_inst_data_list t : unit =
    let task, _task_inst_list, ar =
      Sched_ver_history.In_place_head.Task.Add.add_task ~parent_user_id
        task_data task_inst_data_list t
    in
    print_endline "Added task";
    Sched_ver_history.Print.debug_print_action_record ar;
    Task.Print.debug_print_task ~indent_level:1 task
  in
  let time_profile_store = Time_profile_store.make_empty () in
  let sched_ver_history = Sched_ver_history.make_empty () in
  add_task ~parent_user_id:0L
    Task.
      {
        splittable = false;
        parallelizable = false;
        task_type = One_off;
        name = "Test1";
      }
    Task.[ { task_inst_type = Reminder } ]
    sched_ver_history;
  add_task ~parent_user_id:0L
    Task.
      {
        splittable = false;
        parallelizable = false;
        task_type =
          Recurring
            {
              excluded_time_slots = [];
              recur_type =
                Arithemtic_seq
                  ( { start = 0L; end_exc = Some 100L; diff = 50L },
                    {
                      task_inst_data = { task_inst_type = Reminder };
                      sched_req_template =
                        [ Fixed { task_seg_related_data = 1L; start = 0L } ];
                    } );
            };
        name = "Test2";
      }
    [] sched_ver_history;
  add_task ~parent_user_id:0L
    Task.
      {
        splittable = false;
        parallelizable = false;
        task_type = One_off;
        name = "Test3";
      }
    Task.[ { task_inst_type = Reminder } ]
    sched_ver_history;
  List.iter
    (fun sched_req_data ->
       Sched_ver_history.In_place_head.Sched_req.Add.add_sched_req sched_req_data
         sched_ver_history
       |> ignore)
    [
      [
        Split_even
          {
            task_seg_related_data = ((0L, 2L, 0L), 20L);
            time_slots = [ (0L, 50L) ];
            buckets = [ (0L, 10L); (10L, 20L) ];
            incre = 1L;
          };
        Split_even
          {
            task_seg_related_data = ((0L, 2L, 0L), 20L);
            time_slots = [ (50L, 100L) ];
            buckets = [ (50L, 60L); (60L, 70L); (90L, 100L) ];
            incre = 1L;
          };
      ];
      [
        Shift
          {
            task_seg_related_data_list = [ ((0L, 0L, 0L), 10L) ];
            time_slots = [ (0L, 50L) ];
            incre = 1L;
          };
      ];
      [
        Shift
          {
            task_seg_related_data_list = [ ((0L, 0L, 0L), 10L) ];
            time_slots = [ (0L, 50L) ];
            incre = 1L;
          };
      ];
      (* [
       *   Split_and_shift
       *     {
       *       task_seg_related_data = ((0L, 0L, 2L), 15L);
       *       time_slots = [ (50L, 150L) ];
       *       split_count = Max_split 5L;
       *       incre = 1L;
       *       min_seg_size = 2L;
       *       max_seg_size = None;
       *     };
       * ]; *)
      (* [
       *   Time_share
       *     {
       *       task_seg_related_data_list =
       *         [ ((0L, 0L, 2L), 30L); ((0L, 0L, 3L), 20L) ];
       *       time_slots = [ (50L, 200L) ];
       *       interval_size = 30L;
       *     };
       * ]; *)
      (* [
       *   Push_toward
       *     {
       *       task_seg_related_data = ((0L, 0L, 4L), 10L);
       *       target = 100L;
       *       time_slots = [ (0L, 200L) ];
       *       incre = 1L;
       *     };
       * ];
       * [
       *   Push_toward
       *     {
       *       task_seg_related_data = ((0L, 0L, 5L), 10L);
       *       target = 75L;
       *       time_slots = [ (0L, 200L) ];
       *       incre = 1L;
       *     };
       * ]; *)
    ];
  Sched_ver_history.Print.debug_print_sched_ver_history sched_ver_history;
  print_endline "=====";
  print_endline "Instantiating";
  Sched_ver_history.In_place_head.Recur.instantiate ~start:0L ~end_exc:100L
    sched_ver_history
  |> Sched_ver_history.Print.debug_print_action_record;
  Sched_ver_history.Print.debug_print_sched_ver_history sched_ver_history;
  print_endline "=====";
  ( match
      Sched_ver_history.Maybe_append_to_head.sched ~start:0L ~end_exc:100L
        ~include_sched_reqs_starting_within_time_slot:true
        ~include_sched_reqs_ending_within_time_slot:true
        ~up_to_sched_req_id_inc:None sched_ver_history
    with
    | Ok (), ar ->
      print_endline "Scheduled successfully";
      Sched_ver_history.Print.debug_print_action_record ar;
      Sched_ver_history.Print.debug_print_sched_ver_history sched_ver_history
    | Error (), ar ->
      print_endline "Scheduling failed";
      Sched_ver_history.Print.debug_print_action_record ar );
  print_endline "=====";
  print_endline "Removing task/task inst";
  Sched_ver_history.Maybe_append_to_head.remove_task_inst (0L, 0L, 0L)
    sched_ver_history
  |> Sched_ver_history.Print.debug_print_action_record;
  (* Sched_ver_history.Maybe_append_to_head.remove_task (0L, 0L) sched_ver_history; *)
  Sched_ver_history.Print.debug_print_sched_ver_history sched_ver_history;
  print_endline "=====";
  print_endline "Recording prgress";
  Sched_ver_history.Print.debug_print_sched_ver_history sched_ver_history;
  (* Sched_ver_history.In_place_head.move_task_seg_to_completed
   *   (0L, 1L, 0L, 0L, None) sched_ver_history; *)
  Sched_ver_history.In_place_head.Task_inst.Move.move_task_inst_to_completed
    (0L, 1L, 0L) sched_ver_history
  |> Sched_ver_history.Print.debug_print_action_record;
  Sched_ver_history.In_place_head.Progress.Add.add_task_seg_progress_chunk
    (0L, 0L, 1L, 0L, None) (0L, 100L) sched_ver_history
  |> Sched_ver_history.Print.debug_print_action_record;
  Sched_ver_history.Print.debug_print_sched_ver_history sched_ver_history;
  print_endline "=====";
  print_endline "Serializing sched_ver_history";
  let sched_ver_history_dir =
    Core.Filename.temp_dir "daypack" "sched_ver_history"
  in
  Printf.printf "cwd : %s\n" (Sys.getcwd ());
  ( match
      Sched_ver_history.Serialize.write_to_dir ~dir:sched_ver_history_dir
        sched_ver_history
    with
    | Ok () -> print_endline "Okay"
    | Error msg -> print_endline msg );
  print_endline "=====";
  print_endline "Deserializing";
  Printf.printf "cwd : %s\n" (Sys.getcwd ());
  let sched_ver_history =
    match
      Sched_ver_history.Deserialize.read_from_dir ~dir:sched_ver_history_dir
    with
    | Ok x ->
      print_endline "Okay";
      x
    | Error msg ->
      print_endline msg;
      failwith "Deserialization failed"
  in
  Sched_ver_history.Print.debug_print_sched_ver_history sched_ver_history;
  print_endline "=====";
  print_endline "Serializing time_profile_store";
  let time_profile_store_dir =
    Core.Filename.temp_dir "daypack" "time_profile_store"
  in
  Printf.printf "cwd : %s\n" (Sys.getcwd ());
  ( match
      Time_profile_store.Serialize.write_to_dir ~dir:time_profile_store_dir
        time_profile_store
    with
    | Ok () -> print_endline "Okay"
    | Error msg -> print_endline msg );
  print_endline "=====";
  print_endline "Deserializing";
  Printf.printf "cwd : %s\n" (Sys.getcwd ());
  let time_profile_store =
    match
      Time_profile_store.Deserialize.read_from_dir ~dir:time_profile_store_dir
    with
    | Ok x ->
      print_endline "Okay";
      x
    | Error msg ->
      print_endline msg;
      failwith "Deserialization failed"
  in
  Time_profile_store.Print.debug_print_time_profile_store time_profile_store;
  print_endline "=====";
  (* Sched_ver_history.Print.debug_print_sched_ver_history sched_ver_history; *)
  (* ( match
   *     Sched_ver_history.Maybe_append_to_head.sched ~start:100L ~end_exc:200L
   *       ~include_sched_reqs_partially_within_time_slot:true
   *       ~up_to_sched_req_id_inc:None sched_ver_history
   *   with
   *   | Ok () ->
   *     print_endline "Scheduled successfully";
   *     Sched_ver_history.Print.debug_print_sched_ver_history sched_ver_history
   *   | Error () -> print_endline "Scheduling failed" ); *)
  print_newline ()

(* let debug_time_pattern_matching_date_time_seq () =
 *   print_endline "Debug print for Time_pattern.matching_date_time_seq";
 *   let pattern =
 *     let open Daypack_lib.Time_pattern in
 *     {
 *       years = [];
 *       months = [ `Jun ];
 *       month_days = [];
 *       weekdays = [];
 *       hours = [ 11 ];
 *       minutes = [ 0 ];
 *       seconds = [];
 *       unix_seconds = [];
 *     }
 *   in
 *   let search_years_ahead = 100 in
 *   Daypack_lib.Time_pattern.Print.debug_print_time_pattern pattern;
 *   let s =
 *     Daypack_lib.Time_pattern.Single_pattern.matching_date_time_seq
 *       (Years_ahead_start_unix_second
 *          {
 *            search_using_tz_offset_s = None;
 *            start = Time.Current.cur_unix_second ();
 *            search_years_ahead;
 *          })
 *       pattern
 *     |> Result.get_ok
 *   in
 *   s
 *   |> OSeq.take 10
 *   |> OSeq.iteri (fun i x ->
 *       Printf.printf "iter : %d\n" i;
 *       print_endline "  =====";
 *       Printf.printf "  %s\n"
 *         (Time.To_string.yyyymondd_hhmmss_string_of_date_time x)) *)

let debug_time_pattern_matching_time_slots () =
  print_endline "Debug print for Time_pattern.matching_time_slots";
  let date_time =
    Time.Current.cur_date_time ~tz_offset_s_of_date_time:None |> Result.get_ok
  in
  let start = Time.Date_time.to_unix_second date_time |> Result.get_ok in
  let end_exc =
    Time.Date_time.to_unix_second { date_time with year = date_time.year + 100 }
    |> Result.get_ok
  in
  let time_slots = [ (start, end_exc) ] in
  let pattern =
    let open Daypack_lib.Time_pattern in
    {
      years = [];
      months = [ `Feb ];
      month_days = [ 29 ];
      weekdays = [];
      hours = [];
      minutes = [];
      seconds = [];
      unix_seconds = [];
    }
  in
  Daypack_lib.Time_pattern.Print.debug_print_time_pattern pattern;
  let s =
    Daypack_lib.Time_pattern.Single_pattern.matching_time_slots
      (Time_slots { search_using_tz_offset_s = None; time_slots })
      pattern
    |> Result.get_ok
  in
  s
  |> OSeq.take 30
  |> OSeq.iteri (fun i (start, end_exc) ->
      Printf.printf "iter : %d\n" i;
      Printf.printf "  [%s, %s)\n"
        ( Time.To_string.yyyymmdd_hhmmss_string_of_unix_second
            ~display_using_tz_offset_s:None start
          |> Result.get_ok )
        ( Time.To_string.yyyymmdd_hhmmss_string_of_unix_second
            ~display_using_tz_offset_s:None end_exc
          |> Result.get_ok ))

let debug_time_range_pattern_matching_time_slots () =
  print_endline
    "Debug print for Time_pattern.matching_time_slots_time_range_pattern";
  let date_time =
    Time.Current.cur_date_time ~tz_offset_s_of_date_time:None |> Result.get_ok
  in
  let start = Time.Date_time.to_unix_second date_time |> Result.get_ok in
  let end_exc =
    Time.Date_time.to_unix_second { date_time with year = date_time.year + 1 }
    |> Result.get_ok
  in
  let time_slots = [ (start, end_exc) ] in
  let pattern =
    let open Daypack_lib.Time_pattern in
    `Range_inc
      ( {
        years = [];
        months = [ `Feb ];
        month_days = [];
        weekdays = [];
        hours = [ 13 ];
        minutes = [];
        seconds = [];
        unix_seconds = [];
      },
        {
          years = [];
          months = [ `Feb ];
          month_days = [];
          weekdays = [];
          hours = [ 13 ];
          minutes = [];
          seconds = [];
          unix_seconds = [];
        } )
  in
  Daypack_lib.Time_pattern.Print.debug_print_time_range_pattern pattern;
  let s =
    Daypack_lib.Time_pattern.Range_pattern.matching_time_slots
      (Time_slots { search_using_tz_offset_s = None; time_slots })
      pattern
    |> Result.get_ok
  in
  s
  |> OSeq.take 30
  |> OSeq.iteri (fun i (start, end_exc) ->
      Printf.printf "iter : %d\n" i;
      Printf.printf "  [%s, %s)\n"
        ( Time.To_string.yyyymmdd_hhmmss_string_of_unix_second
            ~display_using_tz_offset_s:None start
          |> Result.get_ok )
        ( Time.To_string.yyyymmdd_hhmmss_string_of_unix_second
            ~display_using_tz_offset_s:None end_exc
          |> Result.get_ok ))

let debug_time_expr_matching_time_slots () =
  print_endline "Debug print for Time_pattern.matching_time_slots_time_expr";
  (* let date_time =
   *   Time.Current.cur_date_time ~tz_offset_s_of_date_time:None |> Result.get_ok
   * in *)
  let search_using_tz_offset_s = Ptime_clock.current_tz_offset_s () in
  let display_using_tz_offset_s = search_using_tz_offset_s in
  let search_param =
    Daypack_lib.Search_param.Years_ahead_start_unix_second
      {
        search_using_tz_offset_s;
        start = Daypack_lib.Time.Current.cur_unix_second ();
        search_years_ahead = 100;
      }
  in
  let s =
    match Daypack_lib.Time_expr.of_string "6pm >> 9am" with
    | Error msg -> failwith (Printf.sprintf "Error: %s" msg)
    | Ok e ->
      e
      |> Daypack_lib.Time_expr.matching_time_slots search_param
      |> Result.get_ok
  in
  s
  |> OSeq.take 30
  |> Seq.iter (fun (x, y) ->
      let x =
        Daypack_lib.Time.To_string.yyyymondd_hhmmss_string_of_unix_second
          ~display_using_tz_offset_s x
        |> Result.get_ok
      in
      let y =
        Daypack_lib.Time.To_string.yyyymondd_hhmmss_string_of_unix_second
          ~display_using_tz_offset_s y
        |> Result.get_ok
      in
      Printf.printf "[%s, %s)\n" x y)

let debug_time_profile_matching_time_slots_of_periods () =
  print_endline "Debug print for Time_profile.matching_time_slots_of_periods";
  let start = Time.Current.cur_unix_second () in
  let end_exc = Int64.add start (Int64.mul 10_000L 360L) in
  let periods =
    let open Time_pattern in
    [
      ( {
        years = [];
        months = [];
        month_days = [];
        weekdays = [];
        hours = [];
        minutes = [];
        seconds = [];
        unix_seconds = [];
      },
        {
          years = [];
          months = [];
          month_days = [];
          weekdays = [];
          hours = [];
          minutes = [];
          seconds = [];
          unix_seconds = [];
        } );
    ]
  in
  let s = Time_profile.matching_time_slots_of_periods ~start ~end_exc periods in
  s
  |> OSeq.take 10
  |> OSeq.iteri (fun i (start, end_exc) ->
      Printf.printf "iter : %d\n" i;
      Printf.printf "  [%s, %s)\n"
        ( Time.To_string.yyyymmdd_hhmmss_string_of_unix_second
            ~display_using_tz_offset_s:None start
          |> Result.get_ok )
        ( Time.To_string.yyyymmdd_hhmmss_string_of_unix_second
            ~display_using_tz_offset_s:None end_exc
          |> Result.get_ok ))

(* let debug_time_pattern_next_match_tm () =
 *   print_endline "Debug print for Time_pattern.next_match_tm";
 *   let tm =
 *     ref
 *       (\* (Some
 *        *    Unix.
 *        *      {
 *        *        tm_sec = 0;
 *        *        tm_min = 0;
 *        *        tm_hour = 0;
 *        *        tm_mday = 1;
 *        *        tm_mon = 0;
 *        *        tm_year = 0;
 *        *        tm_wday = 0;
 *        *        tm_yday = 0;
 *        *        tm_isdst = false;
 *        *      }) *\)
 *       (Unix.time () |> Unix.gmtime |> Option.some)
 *   in
 *   let normalize_dir = `Start in
 *   let pattern =
 *     let open Daypack_lib.Time_pattern in
 *     { year = None; mon = None; day = None; hour = None; min = None }
 *     |> normalize_pattern normalize_dir
 *   in
 *   let search_years_ahead = 100 in
 *   Daypack_lib.Time_pattern.Print.debug_print_pattern pattern;
 *   for i = 0 to 10 do
 *     Printf.printf "iter : %d\n" i;
 *     match !tm with
 *     | Some x ->
 *       print_endline "  =====";
 *       Printf.printf "  tm_sec : %d\n" x.tm_sec;
 *       Printf.printf "  tm_min : %d\n" x.tm_min;
 *       Printf.printf "  tm_hour : %d\n" x.tm_hour;
 *       Printf.printf "  tm_mday : %d\n" x.tm_mday;
 *       Printf.printf "  tm_mon : %d\n" x.tm_mon;
 *       Printf.printf "  tm_year : %d\n" x.tm_year;
 *       Printf.printf "  tm_wday : %d\n" x.tm_wday;
 *       Printf.printf "  tm_yday : %d\n" x.tm_yday;
 *       tm :=
 *         Daypack_lib.Time_pattern.next_match_tm ~normalize_dir
 *           ~search_years_ahead pattern x
 *     | None -> print_endline "nothing"
 *   done *)

(* let debug_time_pattern_next_match_int64 () =
 *   print_endline "Debug print for Time_pattern.next_match_int64";
 *   let time =
 *     ref
 *       (Some
 *          ( Unix.
 *              {
 *                tm_sec = 0;
 *                tm_min = 0;
 *                tm_hour = 0;
 *                tm_mday = 1;
 *                tm_mon = 0;
 *                tm_year = 0;
 *                tm_wday = 0;
 *                tm_yday = 0;
 *                tm_isdst = false;
 *              }
 *            |> Daypack_lib.Time.tm_to_time ))
 *       (\* (Unix.time () |> Unix.gmtime |> Daypack_lib.Time.tm_to_time |> Option.some) *\)
 *   in
 *   let normalize_dir = `Start in
 *   let pattern =
 *     let open Daypack_lib.Time_pattern in
 *     { year = None; mon = Some 1; day = None; hour = None; min = None }
 *     |> normalize_pattern normalize_dir
 *   in
 *   let search_years_ahead = 2 in
 *   let time_slots = [ (\* (0L, 36_347_213L) *\) ] in
 *   Daypack_lib.Time_pattern.Print.debug_print_pattern pattern;
 *   for i = 0 to 10 do
 *     Printf.printf "iter : %d\n" i;
 *     match !time with
 *     | Some x ->
 *       print_endline "  =====";
 *       Printf.printf "  time : %Ld\n" x;
 *       time :=
 *         Daypack_lib.Time_pattern.next_match_int64 ~time_slots ~normalize_dir
 *           ~search_years_ahead pattern x
 *     | None -> print_endline "nothing"
 *   done *)

(* let () = debug_single_task_seg_shift (); print_newline () *)

(* let () =
 *   debug_single_task_seg_shift_rev ();
 *   print_newline () *)

(* let () =
 *   debug_multi_task_segs_shift ();
 *   print_newline () *)

(* let () =
 *   debug_single_task_seg_single_split ();
 *   print_newline () *)

(* let () =
 *   debug_single_task_seg_multi_splits_exact ();
 *   print_newline () *)

(* let () =
 *   debug_single_task_seg_multi_splits_max ();
 *   print_newline () *)

(* let () =
 *   debug_single_task_seg_multi_splits_exact_shift ();
 *   print_newline () *)

(* let () =
 *   debug_single_task_seg_multi_splits_max_shift ();
 *   print_newline () *)

(* let () =
 *   debug_multi_tasks_interleave ();
 *   print_newline () *)

(* let () =
 *   debug_single_task_seg_multi_even_splits ();
 *   print_newline () *)

(* let () =
 *   debug_slice_time_slots_start ();
 *   print_newline () *)

(* let () =
 *   debug_slice_time_slots_end_exc ();
 *   print_newline () *)

(* let () =
 *   debug_slice_time_slots_start_rev ();
 *   print_newline () *)

(* let () =
 *   debug_slice_time_slots_end_exc_rev ();
 *   print_newline () *)

(* let () =
 *   debug_normalize_time_slots ();
 *   print_newline () *)

(* let () =
 *   debug_invert_time_slots ();
 *   print_newline () *)

(* let () =
 *   debug_relative_complement_time_slots ();
 *   print_newline () *)

(* let () =
 *   debug_inter_time_slots ();
 *   print_newline () *)

(* let () =
 *   debug_union_time_slots ();
 *   print_newline () *)

(* let () =
 *   debug_sched_backtracking_search_pending ();
 *   print_newline () *)

(* let () =
 *   debug_sched_agenda_range ();
 *   print_newline () *)

(* let () =
 *   debug_sched_usage_simulation ();
 *   print_newline () *)

(* let () =
 *   debug_time_pattern_matching_tm_seq ();
 *   print_newline () *)

(* let () =
 *   debug_time_pattern_matching_time_slots ();
 *   print_newline () *)

(* let () =
 *   debug_time_range_pattern_matching_time_slots ();
 *   print_newline () *)

let () =
  debug_time_expr_matching_time_slots ();
  print_newline ()

(* let () =
 *   debug_time_pattern_next_match_tm ();
 *   print_newline () *)

(* let () =
 *   debug_time_pattern_next_match_int64 ();
 *   print_newline () *)

(* let () =
 *   debug_time_profile_matching_time_slots_of_periods ();
 *   print_newline () *)

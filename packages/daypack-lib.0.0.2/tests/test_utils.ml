module Print_utils = struct
  let small_nat = QCheck.Print.int

  let int64 = Int64.to_string

  let int64_set s =
    s |> Daypack_lib.Int64_set.to_seq |> List.of_seq |> QCheck.Print.list int64

  let int64_int64_option =
    QCheck.Print.pair Int64.to_string (QCheck.Print.option Int64.to_string)

  let int64_int64_option_set s =
    s
    |> Daypack_lib.Int64_int64_option_set.to_seq
    |> List.of_seq
    |> QCheck.Print.list int64_int64_option

  let time_slot = QCheck.Print.pair int64 int64

  let time_slots = QCheck.Print.list time_slot

  let task_inst_id = Daypack_lib.Task.Id.string_of_task_inst_id

  let task_seg_id = Daypack_lib.Task.Id.string_of_task_seg_id

  let task_seg_id_set s =
    s
    |> Daypack_lib.Task_seg_id_set.to_seq
    |> List.of_seq
    |> QCheck.Print.list Daypack_lib.Task.Id.string_of_task_seg_id

  let task_seg = QCheck.Print.(pair task_seg_id int64)

  let task_segs = QCheck.Print.list QCheck.Print.(pair task_seg_id int64)

  let task_seg_place = QCheck.Print.triple task_seg_id int64 int64

  let task_seg_places s =
    s
    |> Daypack_lib.Task_seg_place_set.to_seq
    |> List.of_seq
    |> QCheck.Print.list task_seg_place

  let task_seg_place_map m =
    m
    |> Daypack_lib.Int64_map.to_seq
    |> List.of_seq
    |> QCheck.Print.list (fun (start, task_seg_ids') ->
        Printf.sprintf "%Ld, %s" start
          ( task_seg_ids'
            |> Daypack_lib.Task_seg_id_set.to_seq
            |> Seq.map task_seg_id
            |> List.of_seq
            |> String.concat "," ))

  let progress = Daypack_lib.Task.To_string.debug_string_of_progress
end

let nz_small_nat_gen = QCheck.Gen.(map (( + ) 1) small_nat)

let nz_small_nat = QCheck.make nz_small_nat_gen

let int64_bound_gen bound =
  let open QCheck.Gen in
  map
    (fun (pos, x) ->
       x |> max 0L |> min bound |> fun x -> if pos then x else Int64.mul (-1L) x)
    (pair bool ui64)

let pos_int64_bound_gen bound =
  QCheck.Gen.(map (fun x -> x |> max 0L |> min bound) ui64)

let nz_pos_int64_bound_gen bound =
  QCheck.Gen.(map (fun x -> x |> max 1L |> min bound) ui64)

let small_pos_int64_gen = pos_int64_bound_gen 100L

let small_nz_pos_int64_gen = nz_pos_int64_bound_gen 100L

let int64_gen = int64_bound_gen (Int64.sub Int64.max_int 1L)

let pos_int64_gen = pos_int64_bound_gen (Int64.sub Int64.max_int 1L)

let pos_int64 = QCheck.make ~print:Print_utils.int64 pos_int64_gen

let small_pos_int64 = QCheck.make ~print:Print_utils.int64 small_pos_int64_gen

let small_nz_pos_int64 =
  QCheck.make ~print:Print_utils.int64 small_nz_pos_int64_gen

let nz_pos_int64_gen =
  QCheck.Gen.map (Int64.add 1L)
    (pos_int64_bound_gen (Int64.sub Int64.max_int 1L))

let nz_pos_int64 = QCheck.make ~print:Print_utils.int64 nz_pos_int64_gen

let pos_int64_int64_option_bound_gen bound =
  QCheck.Gen.(pair (pos_int64_bound_gen bound) (opt (pos_int64_bound_gen bound)))

let nz_pos_int64_int64_option_bound_gen bound =
  let open QCheck.Gen in
  pair (nz_pos_int64_bound_gen bound) (opt (nz_pos_int64_bound_gen bound))

let small_pos_int64_int64_option_gen =
  QCheck.Gen.(pair small_pos_int64_gen (opt small_pos_int64_gen))

let small_nz_pos_int64_int64_option_gen =
  QCheck.Gen.(pair small_nz_pos_int64_gen (opt small_nz_pos_int64_gen))

let pos_int64_int64_option_gen =
  QCheck.Gen.(pair pos_int64_gen (opt pos_int64_gen))

let pos_int64_int64_option =
  QCheck.make ~print:Print_utils.int64_int64_option pos_int64_int64_option_gen

let small_pos_int64_int64_option =
  QCheck.make ~print:Print_utils.int64_int64_option
    small_pos_int64_int64_option_gen

let small_nz_pos_int64_int64_option =
  QCheck.make ~print:Print_utils.int64_int64_option
    small_nz_pos_int64_int64_option_gen

let nz_pos_int64_int64_option_gen =
  nz_pos_int64_int64_option_bound_gen (Int64.sub Int64.max_int 1L)

let nz_pos_int64_int64_option =
  QCheck.make ~print:Print_utils.int64_int64_option
    nz_pos_int64_int64_option_gen

let tiny_sorted_time_slots_gen =
  let open QCheck.Gen in
  map
    (fun (start, sizes_and_gaps) ->
       sizes_and_gaps
       |> List.fold_left
         (fun (last_end_exc, acc) (size, gap) ->
            let start =
              match last_end_exc with
              | None -> start
              | Some x -> Int64.add x gap
            in
            let end_exc = Int64.add start size in
            (Some end_exc, (start, end_exc) :: acc))
         (None, [])
       |> fun (_, l) -> List.rev l)
    (pair (int64_bound_gen 10_000L)
       (list_size (int_bound 5)
          (pair (pos_int64_bound_gen 20L) (pos_int64_bound_gen 20L))))

let tiny_sorted_time_slots =
  QCheck.make ~print:Print_utils.time_slots tiny_sorted_time_slots_gen

let sorted_time_slots_maybe_gaps_gen =
  let open QCheck.Gen in
  map
    (fun (start, sizes_and_gaps) ->
       sizes_and_gaps
       |> List.fold_left
         (fun (last_end_exc, acc) (size, gap) ->
            let start =
              match last_end_exc with
              | None -> start
              | Some x -> Int64.add x (Int64.of_int gap)
            in
            let end_exc = Int64.add start (Int64.of_int size) in
            (Some end_exc, (start, end_exc) :: acc))
         (None, [])
       |> fun (_, l) -> List.rev l)
    (pair int64_gen
       (list_size (int_bound 1000) (pair nz_small_nat_gen small_nat)))

let sorted_time_slots_maybe_gaps =
  QCheck.make ~print:Print_utils.time_slots sorted_time_slots_maybe_gaps_gen

let sorted_time_slots_with_gaps_gen =
  let open QCheck.Gen in
  map
    (fun (start, sizes_and_gaps) ->
       sizes_and_gaps
       |> List.fold_left
         (fun (last_end_exc, acc) (size, gap) ->
            let start =
              match last_end_exc with
              | None -> start
              | Some x -> Int64.add x (Int64.of_int gap)
            in
            let end_exc = Int64.add start (Int64.of_int size) in
            (Some end_exc, (start, end_exc) :: acc))
         (None, [])
       |> fun (_, l) -> List.rev l)
    (pair int64_gen
       (list_size (int_bound 1000) (pair nz_small_nat_gen nz_small_nat_gen)))

let sorted_time_slots_with_gaps =
  QCheck.make ~print:Print_utils.time_slots sorted_time_slots_with_gaps_gen

let sorted_time_slots_with_overlaps_gen =
  let open QCheck.Gen in
  map
    (fun (start, sizes_and_gaps) ->
       sizes_and_gaps
       |> List.fold_left
         (fun (last_start_and_size, acc) (size, gap) ->
            let start, size =
              match last_start_and_size with
              | None -> (start, Int64.of_int size)
              | Some (last_start, last_size) ->
                let start = Int64.add last_start (Int64.of_int gap) in
                let size =
                  if start = last_start then
                    Int64.add last_size (Int64.of_int size)
                  else Int64.of_int size
                in
                (start, size)
            in
            let end_exc = Int64.add start size in
            (Some (start, size), (start, end_exc) :: acc))
         (None, [])
       |> fun (_, l) -> List.rev l)
    (pair int64_gen
       (list_size (int_bound 1000) (pair nz_small_nat_gen small_nat)))

let sorted_time_slots_with_overlaps =
  QCheck.make ~print:Print_utils.time_slots sorted_time_slots_with_overlaps_gen

let tiny_time_slots_gen =
  let open QCheck.Gen in
  map
    (List.map (fun (start, size) -> (start, Int64.add start size)))
    (list_size (int_bound 5)
       (pair (int64_bound_gen 10_000L) (pos_int64_bound_gen 20L)))

let tiny_time_slots =
  QCheck.make ~print:Print_utils.time_slots tiny_time_slots_gen

let time_slots_gen =
  let open QCheck.Gen in
  map
    (List.map (fun (start, size) ->
         (start, Int64.add start (Int64.of_int size))))
    (list_size (int_bound 100) (pair (int64_bound_gen 100_000L) small_nat))

let time_slots = QCheck.make ~print:Print_utils.time_slots time_slots_gen

let weekday_gen : Daypack_lib.Time.weekday QCheck.Gen.t =
  QCheck.Gen.(oneofl [ `Sun; `Mon; `Tue; `Wed; `Thu; `Fri; `Sat ])

let month_gen : Daypack_lib.Time.month QCheck.Gen.t =
  let open QCheck.Gen in
  oneofl
    [ `Jan; `Feb; `Mar; `Apr; `May; `Jun; `Jul; `Aug; `Sep; `Oct; `Nov; `Dec ]

let month_days_gen : int list QCheck.Gen.t =
  QCheck.Gen.(list_size (int_bound 10) (int_range 1 32))

let month_days =
  QCheck.make
    ~print:Daypack_lib.Time_pattern.To_string.debug_string_of_month_days
    month_days_gen

let weekdays_gen : Daypack_lib.Time.weekday list QCheck.Gen.t =
  QCheck.Gen.(list_size (int_bound 10) weekday_gen)

let weekdays =
  QCheck.make ~print:Daypack_lib.Time_pattern.To_string.debug_string_of_weekdays
    weekdays_gen

let time_pattern_gen : Daypack_lib.Time_pattern.time_pattern QCheck.Gen.t =
  let open QCheck.Gen in
  map
    (fun (years, months, month_days, (weekdays, hours, minutes, seconds)) ->
       let open Daypack_lib.Time_pattern in
       {
         years;
         months;
         month_days;
         weekdays;
         hours;
         minutes;
         seconds;
         unix_seconds = [];
       })
    (quad
       (list_size (int_bound 5) (int_range 1980 2100))
       (list_size (int_bound 5) month_gen)
       month_days_gen
       (quad weekdays_gen
          (list_size (int_bound 5) (int_bound 24))
          (list_size (int_bound 5) (int_bound 60))
          (list_size (int_bound 5) (int_bound 60))))

let time_pattern =
  QCheck.make
    ~print:Daypack_lib.Time_pattern.To_string.debug_string_of_time_pattern
    time_pattern_gen

let time_profile_gen =
  let open QCheck.Gen in
  pair (map string_of_int int)
    (map
       (fun periods -> Daypack_lib.Time_profile.{ periods })
       (list_size (int_bound 100) (pair time_pattern_gen time_pattern_gen)))

let time_profile_store_gen =
  let open QCheck.Gen in
  map Daypack_lib.Time_profile_store.of_profile_list
    (list_size (int_bound 20) time_profile_gen)

let time_profile_store =
  QCheck.make
    ~print:
      Daypack_lib.Time_profile_store.To_string
      .debug_string_of_time_profile_store time_profile_store_gen

let task_seg_id_gen =
  let open QCheck.Gen in
  map
    (fun ((id1, id2, id3, id4), id5) -> (id1, id2, id3, id4, id5))
    (pair
       (quad pos_int64_gen pos_int64_gen pos_int64_gen pos_int64_gen)
       (opt pos_int64_gen))

let task_seg_id = QCheck.make task_seg_id_gen

let task_seg_id_set_gen =
  let open QCheck.Gen in
  map
    (fun l -> l |> List.to_seq |> Daypack_lib.Task_seg_id_set.of_seq)
    (list_size (int_bound 20) task_seg_id_gen)

let task_seg_id_set = QCheck.make task_seg_id_set_gen

let task_seg_size_gen = nz_pos_int64_bound_gen 20L

let task_seg_gen =
  let open QCheck in
  Gen.(pair task_seg_id_gen task_seg_size_gen)

let task_seg_sizes_gen =
  let open QCheck in
  Gen.(list_size (int_bound 5) task_seg_size_gen)

let task_segs_gen =
  let open QCheck in
  Gen.(list_size (int_bound 5) task_seg_gen)

let task_seg = QCheck.(make ~print:Print_utils.task_seg task_seg_gen)

let task_segs = QCheck.(make ~print:Print_utils.task_segs task_segs_gen)

let task_inst_id_gen =
  QCheck.Gen.(triple pos_int64_gen pos_int64_gen pos_int64_gen)

let task_inst_id = QCheck.make task_inst_id_gen

let task_inst_data_gen =
  let open QCheck.Gen in
  oneof
    [
      return Daypack_lib.Task.{ task_inst_type = Reminder };
      map
        (fun quota ->
           let open Daypack_lib.Task in
           { task_inst_type = Reminder_quota_counting { quota } })
        pos_int64_gen;
      return Daypack_lib.Task.{ task_inst_type = Passing };
    ]

let task_inst_gen = QCheck.Gen.(pair task_inst_id_gen task_inst_data_gen)

let task_inst =
  let open QCheck in
  make ~print:Daypack_lib.Task.To_string.debug_string_of_task_inst task_inst_gen

let split_count_gen =
  let open QCheck.Gen in
  oneof
    [
      map
        (fun x -> Daypack_lib.Sched_req_data_unit_skeleton.Max_split x)
        pos_int64_gen;
      map
        (fun x -> Daypack_lib.Sched_req_data_unit_skeleton.Exact_split x)
        pos_int64_gen;
    ]

let sched_req_template_data_unit_gen =
  let open QCheck.Gen in
  oneof
    [
      map2
        (fun task_seg_related_data start ->
           Daypack_lib.Sched_req_data_unit_skeleton.Fixed
             { task_seg_related_data; start })
        task_seg_size_gen pos_int64_gen;
      map3
        (fun task_seg_related_data_list time_slots incre ->
           Daypack_lib.Sched_req_data_unit_skeleton.Shift
             { task_seg_related_data_list; time_slots; incre })
        task_seg_sizes_gen tiny_time_slots_gen small_nz_pos_int64_gen;
      map3
        (fun task_seg_related_data time_slots
          (incre, split_count, min_seg_size, offset) ->
          let max_seg_size =
            Option.map (fun x -> Int64.add min_seg_size x) offset
          in
          Daypack_lib.Sched_req_data_unit_skeleton.Split_and_shift
            {
              task_seg_related_data;
              time_slots;
              incre;
              split_count;
              min_seg_size;
              max_seg_size;
            })
        task_seg_size_gen tiny_time_slots_gen
        (quad small_nz_pos_int64_gen split_count_gen small_pos_int64_gen
           (QCheck.Gen.opt small_pos_int64_gen));
      map3
        (fun task_seg_related_data time_slots (buckets, incre) ->
           Daypack_lib.Sched_req_data_unit_skeleton.Split_even
             { task_seg_related_data; time_slots; buckets; incre })
        task_seg_size_gen tiny_time_slots_gen
        (pair tiny_time_slots_gen small_pos_int64_gen);
      map3
        (fun task_seg_related_data_list time_slots interval_size ->
           Daypack_lib.Sched_req_data_unit_skeleton.Time_share
             { task_seg_related_data_list; time_slots; interval_size })
        task_seg_sizes_gen tiny_time_slots_gen small_pos_int64_gen;
      map3
        (fun task_seg_related_data target (time_slots, incre) ->
           Daypack_lib.Sched_req_data_unit_skeleton.Push_toward
             { task_seg_related_data; target; time_slots; incre })
        task_seg_size_gen pos_int64_gen
        (pair tiny_time_slots_gen small_pos_int64_gen);
    ]

let sched_req_template_gen =
  QCheck.Gen.(list_size (int_bound 10) sched_req_template_data_unit_gen)

let sched_req_data_unit_gen :
  Daypack_lib.Sched_req.sched_req_data_unit QCheck.Gen.t =
  let open QCheck.Gen in
  map2
    (fun task_inst_id sched_req_template ->
       Daypack_lib.Sched_req_data_unit_skeleton.map
         ~f_data:(fun task_seg_size -> (task_inst_id, task_seg_size))
         ~f_time:(fun x -> x)
         ~f_time_slot:(fun x -> x)
         sched_req_template)
    task_inst_id_gen sched_req_template_data_unit_gen

let sched_req_gen : Daypack_lib.Sched_req.sched_req QCheck.Gen.t =
  let open QCheck.Gen in
  pair pos_int64_gen (list_size (int_bound 2) sched_req_data_unit_gen)

let sched_req =
  QCheck.make ~print:Daypack_lib.Sched_req.To_string.debug_string_of_sched_req
    sched_req_gen

let sched_req_record_data_unit_gen :
  Daypack_lib.Sched_req.sched_req_record_data_unit QCheck.Gen.t =
  let open QCheck.Gen in
  map2
    (fun task_seg_id sched_req_template ->
       Daypack_lib.Sched_req_data_unit_skeleton.map
         ~f_data:(fun task_seg_size -> (task_seg_id, task_seg_size))
         ~f_time:(fun x -> x)
         ~f_time_slot:(fun x -> x)
         sched_req_template)
    task_seg_id_gen sched_req_template_data_unit_gen

let sched_req_record_gen : Daypack_lib.Sched_req.sched_req_record QCheck.Gen.t =
  let open QCheck.Gen in
  pair pos_int64_gen (list_size (int_bound 2) sched_req_record_data_unit_gen)

let sched_req_record =
  QCheck.make
    ~print:Daypack_lib.Sched_req.To_string.debug_string_of_sched_req_record
    sched_req_record_gen

let arith_seq_gen =
  let open QCheck.Gen in
  map3
    (fun start offset diff ->
       let open Daypack_lib.Task in
       { start; end_exc = Option.map (fun x -> Int64.add start x) offset; diff })
    pos_int64_gen (opt small_pos_int64_gen) small_nz_pos_int64_gen

let arith_seq =
  QCheck.make ~print:Daypack_lib.Task.To_string.debug_string_of_arith_seq
    arith_seq_gen

let recur_data_gen =
  let open QCheck.Gen in
  map2
    (fun task_inst_data sched_req_template ->
       Daypack_lib.Task.{ task_inst_data; sched_req_template })
    task_inst_data_gen sched_req_template_gen

let recur_type_gen =
  let open QCheck.Gen in
  map2
    (fun arith_seq recur_data ->
       Daypack_lib.Task.Arithemtic_seq (arith_seq, recur_data))
    arith_seq_gen recur_data_gen

let recur_gen =
  let open QCheck.Gen in
  map2
    (fun time_slots recur_type ->
       Daypack_lib.Task.{ excluded_time_slots = time_slots; recur_type })
    tiny_sorted_time_slots_gen recur_type_gen

let task_type_gen =
  let open QCheck.Gen in
  oneof
    [
      return Daypack_lib.Task.One_off;
      map (fun recur -> Daypack_lib.Task.Recurring recur) recur_gen;
    ]

let task_id_gen =
  QCheck.Gen.map2 (fun id1 id2 -> (id1, id2)) pos_int64_gen pos_int64_gen

let task_id = QCheck.make task_id_gen

let task_data_gen =
  let open QCheck.Gen in
  map3
    (fun splittable parallelizable (task_type, name) ->
       Daypack_lib.Task.{ splittable; parallelizable; task_type; name })
    bool bool
    (pair task_type_gen string_readable)

let task_gen = QCheck.Gen.(pair task_id_gen task_data_gen)

let task =
  QCheck.make ~print:Daypack_lib.Task.To_string.debug_string_of_task task_gen

let pos_int64_set_gen =
  let open QCheck.Gen in
  map
    (fun l -> Daypack_lib.Int64_set.of_list l)
    (list_size (int_bound 100) pos_int64_gen)

let pos_int64_set = QCheck.make ~print:Print_utils.int64_set pos_int64_set_gen

let pos_int64_int64_option_set_gen =
  let open QCheck.Gen in
  map
    (fun l -> Daypack_lib.Int64_int64_option_set.of_list l)
    (list_size (int_bound 100) pos_int64_int64_option_gen)

let pos_int64_int64_option_set =
  QCheck.make ~print:Print_utils.int64_int64_option_set
    pos_int64_int64_option_set_gen

let task_seg_place_gen =
  let open QCheck.Gen in
  map3
    (fun task_seg_id start offset ->
       let end_exc = Int64.add start offset in
       (task_seg_id, start, end_exc))
    task_seg_id_gen
    (pos_int64_bound_gen 100_000L)
    (pos_int64_bound_gen 100L)

let task_seg_place =
  QCheck.make ~print:Print_utils.task_seg_place task_seg_place_gen

let task_seg_places_gen =
  let open QCheck.Gen in
  map
    (fun l -> Daypack_lib.Task_seg_place_set.of_list l)
    (list_size (int_bound 10) task_seg_place_gen)

let task_seg_places =
  QCheck.make ~print:Print_utils.task_seg_places task_seg_places_gen

let task_seg_place_map_gen =
  let open QCheck.Gen in
  map
    (fun l ->
       ( l |> List.to_seq |> Daypack_lib.Int64_map.of_seq
         : Daypack_lib.Sched.task_seg_place_map ))
    (list_size (int_bound 10) (pair small_nz_pos_int64_gen task_seg_id_set_gen))

let task_seg_place_map =
  QCheck.make ~print:Print_utils.task_seg_place_map task_seg_place_map_gen

let progress_gen =
  let open QCheck.Gen in
  map
    (fun chunks ->
       let open Daypack_lib.Task in
       {
         chunks =
           chunks
           |> List.map (fun (x, y) ->
               ( Daypack_lib.Misc_utils.int32_int32_of_int64 x,
                 Daypack_lib.Misc_utils.int32_int32_of_int64 y ))
           |> Daypack_lib.Int64_int64_set.Deserialize.unpack;
       })
    tiny_sorted_time_slots_gen

let progress = QCheck.make ~print:Print_utils.progress progress_gen

(*$
  let get_gen_name ~name = Printf.sprintf "%s_gen" name in

  let print_store_gen ~name ~f_of_seq ~inner_typ_gen =
    Printf.printf "let %s =\n" (get_gen_name ~name);
    Printf.printf "let open QCheck.Gen in\n";
    Printf.printf "map\n";
    Printf.printf "(fun l -> l\n";
    Printf.printf "|> List.to_seq\n";
    Printf.printf "|> %s\n" f_of_seq;
    Printf.printf ")\n";
    Printf.printf "(list_size (int_bound 20) %s)\n" inner_typ_gen
  in

  let print_store_arbitrary ~name ~f_to_seq ~inner_typ_print =
    let gen_name = get_gen_name ~name in
    Printf.printf "let %s =\n" name;
    Printf.printf "QCheck.make\n";
    Printf.printf "~print:(fun s -> s\n";
    Printf.printf "|> %s\n" f_to_seq;
    Printf.printf "|> List.of_seq\n";
    Printf.printf "|> QCheck.Print.list %s\n" inner_typ_print;
    Printf.printf ")\n";
    Printf.printf "%s\n" gen_name
  in

  let store_list =
    [
      ( "task_store",
        "Daypack_lib.Task_id_map.of_seq",
        "Daypack_lib.Task_id_map.to_seq",
        "task_gen",
        "Daypack_lib.Task.To_string.debug_string_of_task" );
      ( "task_inst_store",
        "Daypack_lib.Task_inst_id_map.of_seq",
        "Daypack_lib.Task_inst_id_map.to_seq",
        "task_inst_gen",
        "Daypack_lib.Task.To_string.debug_string_of_task_inst" );
      ( "task_seg_store",
        "Daypack_lib.Task_seg_id_map.of_seq",
        "Daypack_lib.Task_seg_id_map.to_seq",
        "task_seg_gen",
        "Daypack_lib.Task.To_string.debug_string_of_task_seg" );
      ( "sched_req_store",
        "Daypack_lib.Sched_req_id_map.of_seq",
        "Daypack_lib.Sched_req_id_map.to_seq",
        "sched_req_gen",
        "Daypack_lib.Sched_req.To_string.debug_string_of_sched_req" );
      ( "sched_req_record_store",
        "Daypack_lib.Sched_req_id_map.of_seq",
        "Daypack_lib.Sched_req_id_map.to_seq",
        "sched_req_record_gen",
        "Daypack_lib.Sched_req.To_string.debug_string_of_sched_req_record" );
      ( "quota",
        "Daypack_lib.Task_inst_id_map.of_seq",
        "Daypack_lib.Task_inst_id_map.to_seq",
        "(pair task_inst_id_gen pos_int64_gen)",
        "(QCheck.Print.pair Daypack_lib.Task.Id.string_of_task_inst_id \
         Print_utils.int64)" );
      ( "user_id_to_task_ids",
        "Daypack_lib.User_id_map.of_seq",
        "Daypack_lib.User_id_map.to_seq",
        "(pair pos_int64_gen pos_int64_set_gen)",
        "(QCheck.Print.pair Daypack_lib.Task.Id.string_of_user_id \
         Print_utils.int64_set)" );
      ( "task_id_to_task_inst_ids",
        "Daypack_lib.Task_id_map.of_seq",
        "Daypack_lib.Task_id_map.to_seq",
        "(pair task_id_gen pos_int64_set_gen)",
        "(QCheck.Print.pair Daypack_lib.Task.Id.string_of_task_id \
         Print_utils.int64_set)" );
      ( "task_inst_id_to_task_seg_ids",
        "Daypack_lib.Task_inst_id_map.of_seq",
        "Daypack_lib.Task_inst_id_map.to_seq",
        "(pair task_inst_id_gen pos_int64_int64_option_set_gen)",
        "(QCheck.Print.pair Daypack_lib.Task.Id.string_of_task_inst_id \
         Print_utils.int64_int64_option_set)" );
      ( "indexed_by_task_seg_id",
        "Daypack_lib.Task_seg_id_map.of_seq",
        "Daypack_lib.Task_seg_id_map.to_seq",
        "(pair task_seg_id_gen (pair pos_int64_gen pos_int64_gen))",
        "(QCheck.Print.pair Print_utils.task_seg_id Print_utils.time_slot)" );
      ( "indexed_by_start",
        "Daypack_lib.Int64_map.of_seq",
        "Daypack_lib.Int64_map.to_seq",
        "(pair pos_int64_gen task_seg_id_set_gen)",
        "(QCheck.Print.pair Print_utils.int64 Print_utils.task_seg_id_set)" );
      ( "indexed_by_end_exc",
        "Daypack_lib.Int64_map.of_seq",
        "Daypack_lib.Int64_map.to_seq",
        "(pair pos_int64_gen task_seg_id_set_gen)",
        "(QCheck.Print.pair Print_utils.int64 Print_utils.task_seg_id_set)" );
      ( "task_seg_id_to_progress",
        "Daypack_lib.Task_seg_id_map.of_seq",
        "Daypack_lib.Task_seg_id_map.to_seq",
        "(pair task_seg_id_gen progress_gen)",
        "(QCheck.Print.pair Print_utils.task_seg_id Print_utils.progress)" );
      ( "task_inst_id_to_progress",
        "Daypack_lib.Task_inst_id_map.of_seq",
        "Daypack_lib.Task_inst_id_map.to_seq",
        "(pair task_inst_id_gen progress_gen)",
        "(QCheck.Print.pair Print_utils.task_inst_id Print_utils.progress)" );
    ]
  in

  List.iter
    (fun (name, f_of_seq, f_to_seq, inner_typ_gen, inner_typ_print) ->
       print_store_gen ~name ~f_of_seq ~inner_typ_gen;
       print_store_arbitrary ~name ~f_to_seq ~inner_typ_print)
    store_list
*)

let task_store_gen =
  let open QCheck.Gen in
  map
    (fun l -> l |> List.to_seq |> Daypack_lib.Task_id_map.of_seq)
    (list_size (int_bound 20) task_gen)

let task_store =
  QCheck.make
    ~print:(fun s ->
        s
        |> Daypack_lib.Task_id_map.to_seq
        |> List.of_seq
        |> QCheck.Print.list Daypack_lib.Task.To_string.debug_string_of_task)
    task_store_gen

let task_inst_store_gen =
  let open QCheck.Gen in
  map
    (fun l -> l |> List.to_seq |> Daypack_lib.Task_inst_id_map.of_seq)
    (list_size (int_bound 20) task_inst_gen)

let task_inst_store =
  QCheck.make
    ~print:(fun s ->
        s
        |> Daypack_lib.Task_inst_id_map.to_seq
        |> List.of_seq
        |> QCheck.Print.list Daypack_lib.Task.To_string.debug_string_of_task_inst)
    task_inst_store_gen

let task_seg_store_gen =
  let open QCheck.Gen in
  map
    (fun l -> l |> List.to_seq |> Daypack_lib.Task_seg_id_map.of_seq)
    (list_size (int_bound 20) task_seg_gen)

let task_seg_store =
  QCheck.make
    ~print:(fun s ->
        s
        |> Daypack_lib.Task_seg_id_map.to_seq
        |> List.of_seq
        |> QCheck.Print.list Daypack_lib.Task.To_string.debug_string_of_task_seg)
    task_seg_store_gen

let sched_req_store_gen =
  let open QCheck.Gen in
  map
    (fun l -> l |> List.to_seq |> Daypack_lib.Sched_req_id_map.of_seq)
    (list_size (int_bound 20) sched_req_gen)

let sched_req_store =
  QCheck.make
    ~print:(fun s ->
        s
        |> Daypack_lib.Sched_req_id_map.to_seq
        |> List.of_seq
        |> QCheck.Print.list
          Daypack_lib.Sched_req.To_string.debug_string_of_sched_req)
    sched_req_store_gen

let sched_req_record_store_gen =
  let open QCheck.Gen in
  map
    (fun l -> l |> List.to_seq |> Daypack_lib.Sched_req_id_map.of_seq)
    (list_size (int_bound 20) sched_req_record_gen)

let sched_req_record_store =
  QCheck.make
    ~print:(fun s ->
        s
        |> Daypack_lib.Sched_req_id_map.to_seq
        |> List.of_seq
        |> QCheck.Print.list
          Daypack_lib.Sched_req.To_string.debug_string_of_sched_req_record)
    sched_req_record_store_gen

let quota_gen =
  let open QCheck.Gen in
  map
    (fun l -> l |> List.to_seq |> Daypack_lib.Task_inst_id_map.of_seq)
    (list_size (int_bound 20) (pair task_inst_id_gen pos_int64_gen))

let quota =
  QCheck.make
    ~print:(fun s ->
        s
        |> Daypack_lib.Task_inst_id_map.to_seq
        |> List.of_seq
        |> QCheck.Print.list
          (QCheck.Print.pair Daypack_lib.Task.Id.string_of_task_inst_id
             Print_utils.int64))
    quota_gen

let user_id_to_task_ids_gen =
  let open QCheck.Gen in
  map
    (fun l -> l |> List.to_seq |> Daypack_lib.User_id_map.of_seq)
    (list_size (int_bound 20) (pair pos_int64_gen pos_int64_set_gen))

let user_id_to_task_ids =
  QCheck.make
    ~print:(fun s ->
        s
        |> Daypack_lib.User_id_map.to_seq
        |> List.of_seq
        |> QCheck.Print.list
          (QCheck.Print.pair Daypack_lib.Task.Id.string_of_user_id
             Print_utils.int64_set))
    user_id_to_task_ids_gen

let task_id_to_task_inst_ids_gen =
  let open QCheck.Gen in
  map
    (fun l -> l |> List.to_seq |> Daypack_lib.Task_id_map.of_seq)
    (list_size (int_bound 20) (pair task_id_gen pos_int64_set_gen))

let task_id_to_task_inst_ids =
  QCheck.make
    ~print:(fun s ->
        s
        |> Daypack_lib.Task_id_map.to_seq
        |> List.of_seq
        |> QCheck.Print.list
          (QCheck.Print.pair Daypack_lib.Task.Id.string_of_task_id
             Print_utils.int64_set))
    task_id_to_task_inst_ids_gen

let task_inst_id_to_task_seg_ids_gen =
  let open QCheck.Gen in
  map
    (fun l -> l |> List.to_seq |> Daypack_lib.Task_inst_id_map.of_seq)
    (list_size (int_bound 20)
       (pair task_inst_id_gen pos_int64_int64_option_set_gen))

let task_inst_id_to_task_seg_ids =
  QCheck.make
    ~print:(fun s ->
        s
        |> Daypack_lib.Task_inst_id_map.to_seq
        |> List.of_seq
        |> QCheck.Print.list
          (QCheck.Print.pair Daypack_lib.Task.Id.string_of_task_inst_id
             Print_utils.int64_int64_option_set))
    task_inst_id_to_task_seg_ids_gen

let indexed_by_task_seg_id_gen =
  let open QCheck.Gen in
  map
    (fun l -> l |> List.to_seq |> Daypack_lib.Task_seg_id_map.of_seq)
    (list_size (int_bound 20)
       (pair task_seg_id_gen (pair pos_int64_gen pos_int64_gen)))

let indexed_by_task_seg_id =
  QCheck.make
    ~print:(fun s ->
        s
        |> Daypack_lib.Task_seg_id_map.to_seq
        |> List.of_seq
        |> QCheck.Print.list
          (QCheck.Print.pair Print_utils.task_seg_id Print_utils.time_slot))
    indexed_by_task_seg_id_gen

let indexed_by_start_gen =
  let open QCheck.Gen in
  map
    (fun l -> l |> List.to_seq |> Daypack_lib.Int64_map.of_seq)
    (list_size (int_bound 20) (pair pos_int64_gen task_seg_id_set_gen))

let indexed_by_start =
  QCheck.make
    ~print:(fun s ->
        s
        |> Daypack_lib.Int64_map.to_seq
        |> List.of_seq
        |> QCheck.Print.list
          (QCheck.Print.pair Print_utils.int64 Print_utils.task_seg_id_set))
    indexed_by_start_gen

let indexed_by_end_exc_gen =
  let open QCheck.Gen in
  map
    (fun l -> l |> List.to_seq |> Daypack_lib.Int64_map.of_seq)
    (list_size (int_bound 20) (pair pos_int64_gen task_seg_id_set_gen))

let indexed_by_end_exc =
  QCheck.make
    ~print:(fun s ->
        s
        |> Daypack_lib.Int64_map.to_seq
        |> List.of_seq
        |> QCheck.Print.list
          (QCheck.Print.pair Print_utils.int64 Print_utils.task_seg_id_set))
    indexed_by_end_exc_gen

let task_seg_id_to_progress_gen =
  let open QCheck.Gen in
  map
    (fun l -> l |> List.to_seq |> Daypack_lib.Task_seg_id_map.of_seq)
    (list_size (int_bound 20) (pair task_seg_id_gen progress_gen))

let task_seg_id_to_progress =
  QCheck.make
    ~print:(fun s ->
        s
        |> Daypack_lib.Task_seg_id_map.to_seq
        |> List.of_seq
        |> QCheck.Print.list
          (QCheck.Print.pair Print_utils.task_seg_id Print_utils.progress))
    task_seg_id_to_progress_gen

let task_inst_id_to_progress_gen =
  let open QCheck.Gen in
  map
    (fun l -> l |> List.to_seq |> Daypack_lib.Task_inst_id_map.of_seq)
    (list_size (int_bound 20) (pair task_inst_id_gen progress_gen))

let task_inst_id_to_progress =
  QCheck.make
    ~print:(fun s ->
        s
        |> Daypack_lib.Task_inst_id_map.to_seq
        |> List.of_seq
        |> QCheck.Print.list
          (QCheck.Print.pair Print_utils.task_inst_id Print_utils.progress))
    task_inst_id_to_progress_gen

(*$*)

let store_gen =
  let open QCheck.Gen in
  map
    (fun ( (task_uncompleted_store, task_completed_store, task_discarded_store),
           ( task_inst_uncompleted_store,
             task_inst_completed_store,
             task_inst_discarded_store ),
           ( task_seg_uncompleted_store,
             task_seg_completed_store,
             task_seg_discarded_store ),
           ( user_id_to_task_ids,
             task_id_to_task_inst_ids,
             task_inst_id_to_task_seg_ids,
             ( sched_req_ids,
               sched_req_pending_store,
               sched_req_discarded_store,
               ( sched_req_record_store,
                 quota,
                 task_seg_id_to_progress,
                 task_inst_id_to_progress ) ) ) ) ->
      let open Daypack_lib.Sched in
      {
        task_uncompleted_store;
        task_completed_store;
        task_discarded_store;
        task_inst_uncompleted_store;
        task_inst_completed_store;
        task_inst_discarded_store;
        task_seg_uncompleted_store;
        task_seg_completed_store;
        task_seg_discarded_store;
        user_id_to_task_ids;
        task_id_to_task_inst_ids;
        task_inst_id_to_task_seg_ids;
        sched_req_ids;
        sched_req_pending_store;
        sched_req_discarded_store;
        sched_req_record_store;
        quota;
        task_seg_id_to_progress;
        task_inst_id_to_progress;
      })
    (quad
       (triple task_store_gen task_store_gen task_store_gen)
       (triple task_inst_store_gen task_inst_store_gen task_inst_store_gen)
       (triple task_seg_store_gen task_seg_store_gen task_seg_store_gen)
       (quad user_id_to_task_ids_gen task_id_to_task_inst_ids_gen
          task_inst_id_to_task_seg_ids_gen
          (quad pos_int64_set_gen sched_req_store_gen sched_req_store_gen
             (quad sched_req_record_store_gen quota_gen
                task_seg_id_to_progress_gen task_inst_id_to_progress_gen))))

let agenda_gen =
  let open QCheck.Gen in
  map3
    (fun indexed_by_task_seg_id indexed_by_start indexed_by_end_exc ->
       let open Daypack_lib.Sched in
       { indexed_by_task_seg_id; indexed_by_start; indexed_by_end_exc })
    indexed_by_task_seg_id_gen indexed_by_start_gen indexed_by_end_exc_gen

let sched_gen =
  QCheck.Gen.map3
    (fun sid store agenda -> (sid, Daypack_lib.Sched.{ store; agenda }))
    nz_small_nat_gen store_gen agenda_gen

let sched =
  QCheck.make ~print:Daypack_lib.Sched.To_string.debug_string_of_sched sched_gen

let sched_ver_history_gen =
  QCheck.Gen.map Daypack_lib.Sched_ver_history.of_sched_list
    QCheck.Gen.(list_size (int_range 1 10) sched_gen)

let sched_ver_history =
  QCheck.make
    ~print:
      Daypack_lib.Sched_ver_history.To_string.debug_string_of_sched_ver_history
    sched_ver_history_gen

let date_time_testable : (module Alcotest.TESTABLE) =
  ( module struct
    type t = Daypack_lib.Time.Date_time.t

    let pp =
      Fmt.using Daypack_lib.Time.To_string.yyyymondd_hhmmss_string_of_date_time
        Fmt.string

    let equal = ( = )
  end )

let time_pattern_testable : (module Alcotest.TESTABLE) =
  ( module struct
    type t = Daypack_lib.Time_pattern.time_pattern

    let pp =
      Fmt.using Daypack_lib.Time_pattern.To_string.debug_string_of_time_pattern
        Fmt.string

    let equal = ( = )
  end )

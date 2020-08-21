open Int64_utils

let single_task_seg_shift ~incre ~cur_pos ~(task_seg : Task.task_seg)
    (time_slots : Time_slot.t Seq.t) : Task.task_seg_place Seq.t =
  let rec aux incre cur_pos ((task_seg_id, task_seg_size) as task_seg)
      time_slots =
    let time_slots = Time_slots.Slice.slice ~start:cur_pos time_slots in
    match time_slots () with
    | Seq.Nil -> Seq.empty
    | Seq.Cons ((start, end_exc), slots) ->
      if end_exc -^ start >= task_seg_size then
        (* if time slot is large enough *)
        fun () ->
          Seq.Cons
            ( (task_seg_id, start, start +^ task_seg_size),
              aux incre (start +^ incre) task_seg time_slots )
      else
        (* not big enough, move to next time slot *)
        aux incre cur_pos task_seg slots
  in
  assert (incre > 0L);
  aux incre cur_pos task_seg time_slots

let single_task_seg_shift_rev ~incre ~cur_end_pos_exc
    ~(task_seg : Task.task_seg) (time_slots : Time_slot.t Seq.t) :
  ('a * int64 * int64) Seq.t =
  let rec aux incre cur_end_pos_exc ((task_seg_id, task_seg_size) as task_seg)
      time_slots =
    let time_slots =
      Time_slots.Slice.slice_rev ~end_exc:cur_end_pos_exc time_slots
    in
    match time_slots () with
    | Seq.Nil -> Seq.Nil
    | Seq.Cons ((start, end_exc), slots) ->
      if end_exc -^ start >= task_seg_size then
        (* if time slot is large enough *)
        Seq.Cons
          ( (task_seg_id, end_exc -^ task_seg_size, end_exc),
            fun () -> aux incre (end_exc -^ incre) task_seg time_slots )
      else
        (* not big enough, move to next time slot *)
        aux incre cur_end_pos_exc task_seg slots
  in
  assert (incre > 0L);
  let time_slots = time_slots |> List.of_seq |> List.rev |> List.to_seq in
  fun () -> aux incre cur_end_pos_exc task_seg time_slots

let multi_task_segs_shift ~incre ~(task_segs : Task.task_seg list)
    (time_slots : Time_slot.t Seq.t) : Task.task_seg_place list Seq.t =
  assert (incre > 0L);
  match task_segs with
  | [] -> Seq.empty
  | _ ->
    List.fold_left
      (fun places_seq task_seg ->
         Seq.flat_map
           (fun places ->
              match places with
              | [] ->
                single_task_seg_shift ~incre ~cur_pos:0L ~task_seg time_slots
                |> Seq.map (fun x -> [ x ])
              | (last_id, last_start, last_end_exc) :: pos_s ->
                let time_slots =
                  Time_slots.Slice.slice ~start:last_end_exc time_slots
                in
                (* costruct next shifter which begins at last end_exc position *)
                single_task_seg_shift ~incre ~cur_pos:last_end_exc ~task_seg
                  time_slots
                (* chain the list to the shifter *)
                |> Seq.map (fun (id, start, end_exc) ->
                    (id, start, end_exc)
                    :: (last_id, last_start, last_end_exc)
                    :: pos_s))
           places_seq)
      (Seq.return []) task_segs
    |> Seq.map List.rev

let single_task_seg_single_split ~min_seg_size ~max_seg_size ~cur_split_pos
    ~(task_seg : Task.task_seg) : (Task.task_seg * Task.task_seg) Seq.t =
  let rec aux min_seg_size max_seg_size cur_split_pos
      ((task_seg_id, task_seg_size) as task_seg) =
    if cur_split_pos >= task_seg_size then Seq.empty
    else
      let l_split_size = cur_split_pos -^ 0L in
      let r_split_size = task_seg_size -^ cur_split_pos in
      if
        l_split_size < min_seg_size
        || r_split_size < min_seg_size
        || max_seg_size < l_split_size
        || max_seg_size < r_split_size
      then aux min_seg_size max_seg_size (Int64.succ cur_split_pos) task_seg
      else fun () ->
        Seq.Cons
          ( ( (task_seg_id, l_split_size),
              (Task.Id.succ_task_seg_sub_id task_seg_id, r_split_size) ),
            aux min_seg_size max_seg_size (Int64.succ cur_split_pos) task_seg )
  in
  let _, task_seg_size = task_seg in
  assert (min_seg_size > 0L);
  assert (max_seg_size > 0L);
  assert (cur_split_pos >= 0L);
  assert (task_seg_size > 0L);
  let task_seg = Task.Id.init_task_seg_sub_id task_seg in
  aux min_seg_size max_seg_size cur_split_pos task_seg

let single_task_seg_multi_splits_exact ~min_seg_size ~max_seg_size
    ~(split_count : int64) ~(task_seg : Task.task_seg) :
  Task.task_seg list Seq.t =
  let _, task_seg_size = task_seg in
  assert (min_seg_size > 0L);
  Option.iter (fun max_seg_size -> assert (max_seg_size > 0L)) max_seg_size;
  assert (task_seg_size > 0L);
  Seq.fold_left
    (fun splits_seq _ ->
       Seq.flat_map
         (fun splits ->
            match splits with
            | [] ->
              if min_seg_size <= task_seg_size then Seq.return [ task_seg ]
              else Seq.empty
            | first :: rest ->
              let splits_with_first_sub_splits =
                single_task_seg_single_split ~min_seg_size
                  ~max_seg_size:task_seg_size ~cur_split_pos:0L ~task_seg:first
                |> Seq.map (fun (s1, s2) -> s2 :: s1 :: rest)
              in
              splits_with_first_sub_splits)
         splits_seq)
    (Seq.return [])
    (Seq_utils.zero_to_n_inc_int64 split_count)
  |> (fun s ->
      match max_seg_size with
      | None -> s
      | Some max_seg_size ->
        Seq.filter
          (fun l -> List.for_all (fun (_, s) -> s <= max_seg_size) l)
          s)
  |> Seq.map List.rev

let single_task_seg_multi_splits_max ~min_seg_size ~max_seg_size
    ~(split_count : int64) ~(task_seg : Task.task_seg) :
  Task.task_seg list Seq.t =
  Seq.flat_map
    (fun split_count ->
       single_task_seg_multi_splits_exact ~min_seg_size ~max_seg_size
         ~split_count ~task_seg)
    (Seq_utils.zero_to_n_inc_int64 split_count)

let single_task_seg_multi_splits_exact_shift ~min_seg_size ~max_seg_size
    ~split_count ~(incre : int64) ~(task_seg : Task.task_seg)
    (time_slots : Time_slot.t Seq.t) : Task.task_seg_place list Seq.t =
  single_task_seg_multi_splits_exact ~min_seg_size ~max_seg_size ~split_count
    ~task_seg
  |> Seq.flat_map (fun task_segs ->
      multi_task_segs_shift ~incre ~task_segs time_slots)

let single_task_seg_multi_splits_max_shift ~min_seg_size ~max_seg_size
    ~split_count ~(incre : int64) ~(task_seg : Task.task_seg)
    (time_slots : Time_slot.t Seq.t) : Task.task_seg_place list Seq.t =
  single_task_seg_multi_splits_max ~min_seg_size ~max_seg_size ~split_count
    ~task_seg
  |> Seq.flat_map (fun task_segs ->
      multi_task_segs_shift ~incre ~task_segs time_slots)

let multi_task_segs_interleave ~interval_size ~(task_segs : Task.task_seg list)
    (time_slots : Time_slot.t Seq.t) : Task.task_seg_place Seq.t =
  assert (interval_size > 0L);
  let quota =
    List.fold_left
      (fun m ((id1, id2, id3, id4, _), len) ->
         Task_seg_id_map.add (id1, id2, id3, id4, None) len m)
      Task_seg_id_map.empty task_segs
  in
  match task_segs with
  | [] -> Seq.empty
  | _ ->
    let max_round_count =
      let max_len =
        List.fold_left (fun acc (_, len) -> max acc len) 0L task_segs
      in
      max_len /^ interval_size
    in
    let max_seg_count =
      Seq.fold_left
        (fun acc (start, end_exc) ->
           acc +^ ((end_exc -^ start) /^ interval_size))
        0L time_slots
      |> Int64.to_int
    in
    let time_slots_chunked =
      Time_slots.chunk ~chunk_size:interval_size ~drop_partial:true time_slots
    in
    let task_segs =
      Seq_utils.zero_to_n_exc_int64 max_round_count
      |> Seq.flat_map (fun round ->
          quota
          |> Task_seg_id_map.to_seq
          (* get task segments usable in this round *)
          |> Seq.filter (fun (_id, len) ->
              let quota_left = len -^ (round *^ interval_size) in
              quota_left >= interval_size)
          (* update id based on round, drop length *)
          |> Seq.map (fun ((id1, id2, id3, id4, _), _) ->
              (id1, id2, id3, id4, Some round)))
      |> OSeq.take max_seg_count
    in
    OSeq.map2
      (fun id (start, end_exc) -> (id, start, end_exc))
      task_segs time_slots_chunked

let single_task_seg_multi_even_splits ~incre ~(task_seg : Task.task_seg)
    ~(buckets : Time_slot.t list) ~(usable_time_slots : Time_slot.t Seq.t) :
  Task.task_seg_place list Seq.t =
  let rec aux task_seg_size n buckets =
    (* try to find maximum number of buckets to fit into *)
    if n = 0L then (None, [])
    else
      let bucket_count = List.length buckets in
      let seg_part_size = Int64_utils.div_round_up task_seg_size n in
      let usable_buckets =
        buckets
        |> List.filter (fun bucket_parts ->
            List.for_all
              (fun (start, end_exc) -> end_exc -^ start >= seg_part_size)
              bucket_parts)
      in
      let usable_bucket_count = List.length usable_buckets in
      if usable_bucket_count > 0 && usable_bucket_count = bucket_count then
        (Some seg_part_size, usable_buckets)
      else aux task_seg_size (Int64.pred n) usable_buckets
  in
  let (id1, id2, id3, id4, _), task_seg_size = task_seg in
  let possibly_usable_buckets =
    buckets
    |> List.map (fun bucket ->
        Time_slots.inter (Seq.return bucket) usable_time_slots |> List.of_seq)
  in
  let possibly_usable_bucket_count =
    List.length possibly_usable_buckets |> Int64.of_int
  in
  match
    aux task_seg_size possibly_usable_bucket_count possibly_usable_buckets
  with
  | None, _ -> Seq.empty
  | Some task_seg_part_size, l ->
    l
    |> List.to_seq
    |> OSeq.mapi (fun i bucket -> (Int64.of_int i, List.to_seq bucket))
    |> Seq.fold_left
      (fun places_seq (bucket_id, bucket) ->
         let id = (id1, id2, id3, id4, Some bucket_id) in
         let task_seg = (id, task_seg_part_size) in
         Seq.flat_map
           (fun places ->
              single_task_seg_shift ~incre ~cur_pos:0L ~task_seg bucket
              |> Seq.map (fun place -> place :: places))
           places_seq)
      (Seq.return [])
    |> Seq.map List.rev

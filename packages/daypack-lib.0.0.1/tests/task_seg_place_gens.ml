open Test_utils

module Qc = struct
  let single_task_seg_shift_consistent =
    QCheck.Test.make ~count:10_000 ~name:"single_task_seg_shift_consistent"
      QCheck.(
        triple nz_pos_int64 pos_int64
          (pair (pair task_seg_id small_pos_int64) tiny_sorted_time_slots))
      (fun (incre, cur_pos, ((task_seg_id, task_seg_size), time_slots)) ->
         let time_slots = List.to_seq time_slots in
         Daypack_lib.Task_seg_place_gens.single_task_seg_shift ~incre ~cur_pos
           ~task_seg:(task_seg_id, task_seg_size)
           time_slots
         |> OSeq.for_all (fun (id, start, end_exc) ->
             task_seg_id = id && Int64.sub end_exc start = task_seg_size))

  let single_task_seg_shift_sorted_and_uniq =
    QCheck.Test.make ~count:10_000 ~name:"single_task_seg_shift_sorted_and_uniq"
      QCheck.(
        triple nz_pos_int64 pos_int64
          (pair (pair task_seg_id small_pos_int64) tiny_sorted_time_slots))
      (fun (incre, cur_pos, ((task_seg_id, task_seg_size), time_slots)) ->
         let time_slots = List.to_seq time_slots in
         let l =
           Daypack_lib.Task_seg_place_gens.single_task_seg_shift ~incre ~cur_pos
             ~task_seg:(task_seg_id, task_seg_size)
             time_slots
           |> List.of_seq
         in
         List.sort_uniq compare l = l)

  let single_task_seg_shift_rev_consistent =
    QCheck.Test.make ~count:10_000 ~name:"single_task_seg_shift_rev_consistent"
      QCheck.(
        triple nz_pos_int64 pos_int64
          (pair (pair task_seg_id small_pos_int64) tiny_sorted_time_slots))
      (fun (incre, cur_end_pos_exc, ((task_seg_id, task_seg_size), time_slots)) ->
         let time_slots = List.to_seq time_slots in
         Daypack_lib.Task_seg_place_gens.single_task_seg_shift_rev ~incre
           ~cur_end_pos_exc
           ~task_seg:(task_seg_id, task_seg_size)
           time_slots
         |> OSeq.for_all (fun (id, start, end_exc) ->
             task_seg_id = id && Int64.sub end_exc start = task_seg_size))

  let single_task_seg_shift_rev_sorted_and_uniq =
    QCheck.Test.make ~count:10_000
      ~name:"single_task_seg_shift_rev_sorted_and_uniq"
      QCheck.(
        triple nz_pos_int64 pos_int64
          (pair (pair task_seg_id small_pos_int64) tiny_sorted_time_slots))
      (fun (incre, cur_end_pos_exc, ((task_seg_id, task_seg_size), time_slots)) ->
         let time_slots = List.to_seq time_slots in
         let l =
           Daypack_lib.Task_seg_place_gens.single_task_seg_shift_rev ~incre
             ~cur_end_pos_exc
             ~task_seg:(task_seg_id, task_seg_size)
             time_slots
           |> List.of_seq
         in
         List.sort_uniq (fun x y -> compare y x) l = l)

  let multi_task_segs_shift_uniq =
    QCheck.Test.make ~count:1000 ~name:"multi_task_segs_shift_uniq"
      QCheck.(triple nz_pos_int64 task_segs tiny_sorted_time_slots)
      (fun (incre, task_segs, time_slots) ->
         let time_slots = List.to_seq time_slots in
         let l =
           Daypack_lib.Task_seg_place_gens.multi_task_segs_shift ~incre
             ~task_segs time_slots
           |> List.of_seq
         in
         List.length (List.sort_uniq compare l) = List.length l)

  let single_task_seg_single_split_consistent =
    QCheck.Test.make ~count:1000 ~name:"single_task_seg_single_split_consistent"
      QCheck.(quad nz_pos_int64 nz_pos_int64 nz_pos_int64 task_seg)
      (fun (min_seg_size, offset, cur_split_pos, task_seg) ->
         let _, task_seg_size = task_seg in
         let cur_split_pos = Int64.rem cur_split_pos task_seg_size in
         let max_seg_size = max Int64.max_int (Int64.add min_seg_size offset) in
         Daypack_lib.Task_seg_place_gens.single_task_seg_single_split
           ~min_seg_size ~max_seg_size ~cur_split_pos ~task_seg
         |> OSeq.for_all (fun ((_, s1), (_, s2)) ->
             (min_seg_size <= s1 && min_seg_size <= s2)
             && (s1 <= max_seg_size && s2 <= max_seg_size)
             && Int64.add s1 s2 = task_seg_size))

  let single_task_seg_multi_splits_exact_consistent =
    QCheck.Test.make ~count:1000
      ~name:"single_task_seg_multi_splits_exact_consistent"
      QCheck.(
        quad nz_pos_int64 (pair nz_pos_int64 bool) small_pos_int64 task_seg)
      (fun (min_seg_size, (offset, test_max), split_count, task_seg) ->
         let (id1, id2, id3, id4, _), task_seg_size = task_seg in
         let split_count = Int64.rem split_count task_seg_size in
         let max_seg_size =
           if test_max then
             Some (max Int64.max_int (Int64.add min_seg_size offset))
           else None
         in
         Daypack_lib.Task_seg_place_gens.single_task_seg_multi_splits_exact
           ~min_seg_size ~max_seg_size ~split_count ~task_seg
         |> OSeq.for_all (fun l ->
             List.for_all
               (fun ((id1', id2', id3', id4', _), _) ->
                  (id1, id2, id3, id4) = (id1', id2', id3', id4'))
               l
             && List.for_all (fun (_, s) -> min_seg_size <= s) l
             && ( match max_seg_size with
                 | None -> true
                 | Some max_seg_size ->
                   List.for_all (fun (_, s) -> s <= max_seg_size) l )
             && List.length l = Int64.to_int split_count + 1
             && List.fold_left (fun acc (_, s) -> Int64.add acc s) 0L l
                = task_seg_size))

  let single_task_seg_multi_splits_max_consistent =
    QCheck.Test.make ~count:1000
      ~name:"single_task_seg_multi_splits_max_consistent"
      QCheck.(
        quad nz_pos_int64 (pair nz_pos_int64 bool) small_pos_int64 task_seg)
      (fun (min_seg_size, (offset, test_max), split_count, task_seg) ->
         let (id1, id2, id3, id4, _), task_seg_size = task_seg in
         let split_count = Int64.rem split_count task_seg_size in
         let max_seg_size =
           if test_max then
             Some (max Int64.max_int (Int64.add min_seg_size offset))
           else None
         in
         Daypack_lib.Task_seg_place_gens.single_task_seg_multi_splits_max
           ~min_seg_size ~max_seg_size ~split_count ~task_seg
         |> OSeq.for_all (fun l ->
             List.for_all
               (fun ((id1', id2', id3', id4', _), _) ->
                  (id1, id2, id3, id4) = (id1', id2', id3', id4'))
               l
             && List.for_all (fun (_, s) -> min_seg_size <= s) l
             && ( match max_seg_size with
                 | None -> true
                 | Some max_seg_size ->
                   List.for_all (fun (_, s) -> s <= max_seg_size) l )
             && List.length l <= Int64.to_int split_count + 1
             && List.fold_left (fun acc (_, s) -> Int64.add acc s) 0L l
                = task_seg_size))

  let single_task_seg_multi_splits_exact_shift_consistent =
    QCheck.Test.make ~count:1000
      ~name:"single_task_seg_multi_splits_exact_shift_consistent"
      QCheck.(
        quad nz_pos_int64
          (triple nz_pos_int64 bool nz_pos_int64)
          small_pos_int64
          (pair task_seg tiny_sorted_time_slots))
      (fun ( min_seg_size,
             (offset, test_max, incre),
             split_count,
             (task_seg, time_slots) ) ->
        let (id1, id2, id3, id4, _), task_seg_size = task_seg in
        let split_count = Int64.rem split_count task_seg_size in
        let max_seg_size =
          if test_max then
            Some (max Int64.max_int (Int64.add min_seg_size offset))
          else None
        in
        Daypack_lib.Task_seg_place_gens.single_task_seg_multi_splits_exact_shift
          ~min_seg_size ~max_seg_size ~split_count ~incre ~task_seg
          (List.to_seq time_slots)
        |> OSeq.for_all (fun l ->
            List.length l = Int64.to_int split_count + 1
            && List.for_all
              (fun ((id1', id2', id3', id4', _), start, end_exc) ->
                 let size = Int64.sub end_exc start in
                 (id1, id2, id3, id4) = (id1', id2', id3', id4')
                 && min_seg_size <= size
                 &&
                 match max_seg_size with
                 | None -> true
                 | Some max_seg_size -> size <= max_seg_size)
              l))

  let single_task_seg_multi_splits_max_shift_consistent =
    QCheck.Test.make ~count:1000
      ~name:"single_task_seg_multi_splits_max_shift_consistent"
      QCheck.(
        quad nz_pos_int64
          (triple nz_pos_int64 bool nz_pos_int64)
          small_pos_int64
          (pair task_seg tiny_sorted_time_slots))
      (fun ( min_seg_size,
             (offset, test_max, incre),
             split_count,
             (task_seg, time_slots) ) ->
        let (id1, id2, id3, id4, _), task_seg_size = task_seg in
        let split_count = Int64.rem split_count task_seg_size in
        let max_seg_size =
          if test_max then
            Some (max Int64.max_int (Int64.add min_seg_size offset))
          else None
        in
        Daypack_lib.Task_seg_place_gens.single_task_seg_multi_splits_max_shift
          ~min_seg_size ~max_seg_size ~split_count ~incre ~task_seg
          (List.to_seq time_slots)
        |> OSeq.for_all (fun l ->
            List.length l <= Int64.to_int split_count + 1
            && List.for_all
              (fun ((id1', id2', id3', id4', _), start, end_exc) ->
                 let size = Int64.sub end_exc start in
                 (id1, id2, id3, id4) = (id1', id2', id3', id4')
                 && min_seg_size <= size
                 &&
                 match max_seg_size with
                 | None -> true
                 | Some max_seg_size -> size <= max_seg_size)
              l))

  let suite =
    [
      single_task_seg_shift_consistent;
      single_task_seg_shift_sorted_and_uniq;
      single_task_seg_shift_rev_consistent;
      single_task_seg_shift_rev_sorted_and_uniq;
      multi_task_segs_shift_uniq;
      single_task_seg_single_split_consistent;
      single_task_seg_multi_splits_exact_consistent;
      single_task_seg_multi_splits_max_consistent;
      single_task_seg_multi_splits_exact_shift_consistent;
      single_task_seg_multi_splits_max_shift_consistent;
    ]
end

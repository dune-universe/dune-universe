include Map_utils.Make (Int64_map)
module Int64_bucketed = Map_utils.Make_bucketed (Int64_map) (Int64_set)
module Task_seg_place_bucketed =
  Map_utils.Make_bucketed (Int64_map) (Task_seg_place_set)
module Task_seg_id_bucketed =
  Map_utils.Make_bucketed (Int64_map) (Task_seg_id_set)

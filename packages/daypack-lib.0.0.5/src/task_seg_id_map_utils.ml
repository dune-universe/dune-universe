include Map_utils.Make (Task_seg_id_map)
module Int64_bucketed = Map_utils.Make_bucketed (Task_seg_id_map) (Int64_set)

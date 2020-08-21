include Map_utils.Make (Task_inst_id_map)
module Int64_bucketed = Map_utils.Make_bucketed (Task_inst_id_map) (Int64_set)
module Int64_int64_option_bucketed =
  Map_utils.Make_bucketed (Task_inst_id_map) (Int64_int64_option_set)

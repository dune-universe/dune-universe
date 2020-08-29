include Map_utils.Make (User_id_map)
module Int64_bucketed = Map_utils.Make_bucketed (User_id_map) (Int64_set)

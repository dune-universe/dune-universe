include Set.Make (Int64)

module Serialize = struct
  let pack (set : t) : (int32 * int32) list =
    set |> to_seq |> Seq.map Misc_utils.int32_int32_of_int64 |> List.of_seq
end

module Deserialize = struct
  let unpack (l : (int32 * int32) list) : t =
    l |> List.to_seq |> Seq.map Misc_utils.int64_of_int32_int32 |> of_seq
end

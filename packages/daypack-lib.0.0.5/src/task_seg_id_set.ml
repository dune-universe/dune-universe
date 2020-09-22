include Set.Make (struct
    type t = Task.task_seg_id

    let compare = compare
  end)

module Serialize = struct
  let pack (t : t) : Task_t.task_seg_id list =
    t |> to_seq |> Seq.map Task.Serialize.pack_task_seg_id |> List.of_seq
end

module Deserialize = struct
  let unpack (l : Task_t.task_seg_id list) : t =
    l |> List.to_seq |> Seq.map Task.Deserialize.unpack_task_seg_id |> of_seq
end

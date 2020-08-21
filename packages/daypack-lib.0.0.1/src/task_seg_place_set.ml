include Set.Make (struct
    type t = Task.task_seg_place

    let compare (task_seg_id1, start1, end_exc1) (task_seg_id2, start2, end_exc2)
      =
      match Time_slot.compare (start1, end_exc1) (start2, end_exc2) with
      | 0 -> compare task_seg_id1 task_seg_id2
      | n -> n
  end)

module Serialize = struct
  let pack (t : t) : Task.task_seg_place list = t |> to_seq |> List.of_seq
end

module Deserialize = struct
  let unpack (l : Task.task_seg_place list) : t = l |> List.to_seq |> of_seq
end

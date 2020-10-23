include Map.Make (struct
    type t = Task.task_seg_id

    let compare = compare
  end)

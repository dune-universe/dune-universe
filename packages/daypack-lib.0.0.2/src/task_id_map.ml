include Map.Make (struct
    type t = Task.task_id

    let compare = compare
  end)

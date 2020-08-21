include Map.Make (struct
    type t = Task.task_inst_id

    let compare = compare
  end)

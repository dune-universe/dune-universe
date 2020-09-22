include Map.Make (struct
    type t = Task.user_id

    let compare = compare
  end)

include Map.Make (struct
    type t = string * string

    let compare = compare
  end)

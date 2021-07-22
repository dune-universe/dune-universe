include Set.Make (struct
  type t = Drop.t

  let compare x y = compare (Drop.index x) (Drop.index y)
end)

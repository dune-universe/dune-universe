include Domain.Make(struct
  type t = Box.t
  let man = Box.manager_alloc ()
end)

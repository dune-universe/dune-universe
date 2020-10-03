include Domain.Make(struct
  type t = Polka.strict Polka.t
  let man = Polka.manager_alloc_strict()
end)

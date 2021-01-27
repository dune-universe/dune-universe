include Domain.Make(struct
            type t = Polka.strict Polka.t
            let man = Polka.manager_alloc_strict() end)

let set_diff a b =
  let work acc a c =
    let neg_c = Linconsext.neg c in
    let a' = Abstractext.filter_lincons man a c
    and s = Abstractext.filter_lincons man a neg_c in
    if Abstractext.is_bottom man s then a,acc
    else a',(s::acc)
  in
  Linconsext.array_fold
    (fun (_,acc) c ->
      if Linconsext.get_typ c = Apron.Lincons1.EQ then
        let c1,c2 = Linconsext.spliteq c in
        let a',acc' = work acc a c1 in
        work acc' a' c2
      else work acc a c
    ) (a,[]) (A.to_lincons_array man b)

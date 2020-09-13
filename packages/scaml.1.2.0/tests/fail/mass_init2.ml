open SCaml

let vote (k : string) bigmap = 
  let bigmap' = match BigMap.get k bigmap with
    | None -> BigMap.update k (Some (Nat 1)) bigmap
    | Some n -> BigMap.update k (Some (n +^ Nat 1)) bigmap
  in
  [], bigmap'

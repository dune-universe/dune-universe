open SCaml

type storage = 
  { first : nat
  ; last : nat
  ; map  : (nat, string) map
  }

let [@entry] add s storage =
  let last = storage.last +^ Nat 1 in
  let map = Map.update last (Some s) storage.map in
  [], { storage with last; map }

let [@entry] pop () storage =
  let first = storage.first in
  let last = storage.last in
  if first = last then assert false;
  let map = Map.update first None storage.map in
  let first = first +^ Nat 1 in
  [], { storage with first; map }

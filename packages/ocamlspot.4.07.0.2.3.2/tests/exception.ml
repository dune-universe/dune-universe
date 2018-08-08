exception (* E => *) E (* <= E *)
let _ = raise E (* ? E *)

let _ = try raise E (* ? E *) with
  | E (* ? E *) -> assert false
  | Target.E (* ? Target.E *) -> assert false

exception X = E (* ? E *) (* bug 090818 *)

module M = struct
  exception (* EE => *) EE (* <= EE *)
end

let _ = raise M.EE (* ? EE *)
let _ = raise (Failure "x") (* predefind *)

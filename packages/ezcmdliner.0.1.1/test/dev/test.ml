(** Coucou *)
type t = {
  foo : int; [@default 42] (** prout *)
  bar : bool;
  goo : string list; [@sep ':']
}
[@@deriving cmdliner]
[@@xrefs [`Main]]
[@@envs ["PLOP", Some "It rules!", None]]
[@@version "4.2"]


let f t = t.foo + 1

let _ = cmdliner f

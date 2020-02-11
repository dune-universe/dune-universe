open Hashcons

module Hash_consed = struct

  let table =
    create 128

  let iter f env hcx =
    f env hcx.node

  let map f env hcx =
    hashcons table (f env hcx.node)

end

type expr =
  expr_node hash_consed

and expr_node =
  | EConst of int
  | EAdd of expr * expr
  [@@deriving visitors { name = "iter"; variety = "iter" },
              visitors { name = "map" ; variety = "map"  }]

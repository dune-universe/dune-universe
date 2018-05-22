
let core_type t =
  let loc = t.Parsetree.ptyp_loc in
  match t with
  | [%type: [%t? itype] -> [%t? otype]] ->
     (
       match Madcast.derive itype otype with
       | [cast] -> cast
       | [] ->
          Ppx_deriving.(raise_errorf ~loc
                          "No cast found for %s -> %s"
                          (string_of_core_type itype)
                          (string_of_core_type otype))
       | _ ->
          Ppx_deriving.(raise_errorf ~loc
                          "Several casts found for %s -> %s"
                          (string_of_core_type itype)
                          (string_of_core_type otype))
     )
  | _ ->
     Ppx_deriving.(raise_errorf ~loc
                     "Expected an arrow type, got %s"
                     (string_of_core_type t))

let () = Ppx_deriving.(register (create "madcast" ~core_type ()))

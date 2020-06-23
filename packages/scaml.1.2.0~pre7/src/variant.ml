open Spotlib.Spot

let variant_type tyenv ty constrs = 
  let consts, non_consts =
    List.partition (fun constr -> constr.Types.cstr_arity = 0) constrs
  in
  let consts = match consts with
    | [] -> None
    | _::_ -> 
        let names = List.map (fun c -> c.Types.cstr_name) consts in
        Some names
  in
  let non_consts =
    List.map (fun constr ->
        let ty_args, ty_res = Ctype.instance_constructor constr in
        Ctype.unify tyenv ty ty_res; (* XXX should succeed *)
        (constr.cstr_name, ty_args)
      ) non_consts
  in
  consts, non_consts

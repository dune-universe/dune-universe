class orakuda = object
  inherit P_sprintf.sprintf as super

  (*  [%name "hello"] ==> {name|hello|name}
      [%name {|hello|}] ==> {name|hello|name}
  *)
  method !expr e0 = match e0 with
    | { pexp_desc = Pexp_extension ( {txt=("fmt"|"qq"|"qx"|"m"|"s" as name)},
                                     PStr [ { pstr_desc= Pstr_eval (e, _) } ] ) } ->
        begin match e.pexp_desc with
        | Pexp_constant (Pconst_string (s, (Some "" | None))) ->
            super#expr { e with pexp_desc = Pexp_constant (Pconst_string (s, Some name)) }
        | Pexp_constant (Pconst_string (_s, Some _x)) -> super#expr e0
        | _ -> super#expr e0
        end
    | _ -> super#expr e0

end

let mapper = Ast_mapper_class_405.to_mapper (new orakuda)

include Ppxx.Ppx.Make(struct
    let name = "ppx_orakuda"
    let options = []
    let make_mapper _ _ = mapper
  end)

let () = register ()

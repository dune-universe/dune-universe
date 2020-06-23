open Tools
open Parsetree
open Ast_mapper

let expr m e = 
  let es =
    List.filter_map (fun a ->
        if a.attr_name.txt <> "scaml.replace" then None
        else
          match a.attr_payload with
          | PStr [si] ->
              begin match si.pstr_desc with
              | Pstr_eval (e, _) -> Some e
              | _ -> None
              end
          | _ ->
              errorf_attribute ~loc:a.attr_loc "Attribute [@scaml.replace e] takes one expression."
      ) e.pexp_attributes
  in
  match es with
  | [] -> default_mapper.expr m e
  | _::_::_ -> errorf_attribute ~loc:e.pexp_loc "At most one [@scaml.replace e] is allowed for each expression"
  | [e] -> m.expr m e

let mapper = { default_mapper with expr }

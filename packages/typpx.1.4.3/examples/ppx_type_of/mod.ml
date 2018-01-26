open Asttypes
open Typedtree

module MapArg : TypedtreeMap.MapArgument = struct
  include TypedtreeMap.DefaultMapArgument

  let enter_expression e = match e.exp_desc with
    | Texp_apply ({ exp_desc= Texp_ident(Pdot(Pident {Ident.name= "Ppx_type_of"}, "type_of", _), _, _) }, args) ->
        begin match args with
        | [Nolabel, Some ({ exp_type= ty } as _a) ] ->
            let s =
              Printtyp.reset ();
              Printtyp.mark_loops ty;
              Format.asprintf "%a" Printtyp.type_expr ty
            in
            let name =
              { e with exp_desc = Texp_constant (Const_string (s, None)) }
            in
            name

        | _ -> assert false (* better error handling required *)
        end
    | _ -> e
        
  let leave_expression e = e
end

module Map = TypedtreeMap.MakeMap(MapArg)

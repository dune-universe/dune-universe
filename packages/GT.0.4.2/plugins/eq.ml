(** {i Eq} plugin: receive another value as inherited attribute and test for equality.

    Very similar to {!Compare} plugin.

    For type declaration [type ('a,'b,...) typ = ...] it will create a transformation
    function with type

    [('a -> 'a -> bool) ->
     ('b -> 'b -> bool) -> ... -> ('a,'b,...) typ -> bool ]

    Inherited attribute' is the same as argument, synthetized attribute is {!GT.comparison}.
*)

open GTCommon

let trait_name = "eq"

module Make(AstHelpers : GTHELPERS_sig.S) = struct

let trait_name = trait_name

module C = Compare.Make(AstHelpers)

open AstHelpers

class g initial_args tdecls = object(self: 'self)
  inherit C.g initial_args tdecls

  method! trait_name = trait_name

  method! syn_of_param ~loc s = Typ.sprintf ~loc "bool"
  method! syn_of_main ~loc ?in_class tdecl = self#syn_of_param ~loc "dummy"

  method! on_different_constructors ~loc is_poly other_name cname arg_typs =
    Exp.let_ ~loc [Pat.any ~loc, Exp.ident ~loc other_name]
      (Exp.false_ ~loc)
  method! chain_exprs ~loc e1 e2 =
    Exp.app_list ~loc (Exp.ident ~loc "&&") [ e1; e2 ]
  method! chain_init ~loc = Exp.true_ ~loc

end

let create = (new g :> C.P.plugin_constructor)
end

let register () =
  Expander.register_plugin trait_name (module Make: Plugin_intf.MAKE)

let () = register ()

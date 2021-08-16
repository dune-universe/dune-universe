(*
 * Generic transformers: plugins.
 * Copyright (C) 2016-2021
 *   Dmitrii Kosarev aka Kakadu
 * St.Petersburg State University, JetBrains Research
 *)

(** {i Enum} plugin: converts constructor name to integer.
    Constructor arguments are not taken to account.

    Synthetized attribute is [int].

    Inherited attributes' type (both default and for type parameters) is [unit].

    For type declaration [type ('a,'b,...) typ = ...] it will create transformation
    function with type

    [('a -> int) -> ('b -> int) -> ... -> ('a,'b,...) typ -> int]

    See also: {!Compare} plugin.
  *)

open Base
open Ppxlib
open GTCommon
open HelpersBase

let trait_name = "enum"

module Make(AstHelpers : GTHELPERS_sig.S) = struct

let trait_name = trait_name

module P = Plugin.Make(AstHelpers)

exception Found of int
open AstHelpers

let default_index = 0

class g args tdecls = object(self)
  inherit [loc, Exp.t, Typ.t, type_arg, Ctf.t, Cf.t, Str.t, Sig.t] Plugin_intf.typ_g
  inherit P.generator args tdecls
  inherit P.no_inherit_arg args tdecls

  method trait_name = trait_name
  method inh_of_main ~loc _tdecl           = Typ.ident ~loc "unit"
  method syn_of_main ~loc ?in_class _tdecl = Typ.ident ~loc "int"

  method syn_of_param ~loc _           = Typ.ident ~loc "int"
  method inh_of_param ~loc tdecl _name = self#inh_of_main ~loc tdecl

  (* TODO: copy-paste from show. Maybe refactor to separate class? *)
  method plugin_class_params ~loc typs ~typname =
    (List.map typs ~f:Typ.from_caml) @
    [ Typ.var ~loc @@ Naming.make_extra_param typname ]

  method private find_right_one ~loc constr_info tdecl =
    match tdecl.ptype_kind, constr_info with
    | Ptype_open,_ -> failwith "Open types can't be enumerable"
    | Ptype_record _, _ -> Exp.int_const ~loc default_index
    | Ptype_abstract, Some (`Poly s) -> (
        match tdecl.ptype_manifest with
        | Some {ptyp_desc = Ptyp_variant (rows,_,labs)} -> (
          try
            (* Format.printf "There are %d rows\n%!" (List.length rows); *)
            List.iteri rows ~f:(fun i -> function
            | { prf_desc = Rtag ({txt},_,_)} when Stdlib.( txt = s) -> raise (Found (HelpersBase.hash_variant s))
            | _ -> ()
            );
            failwiths "Plugin passed a constructor `%s` that isn't present" s
          with Found i -> Exp.int_const ~loc i
        )
        | _ -> assert false
    )
    | _, None
    | Ptype_abstract, Some (`Normal _)
    | Ptype_variant _, Some (`Poly _) -> failwith "should not happen?"
    | Ptype_variant cds, Some (`Normal s) ->
      try
        List.iteri cds ~f:(fun i -> function
        | { pcd_name = {txt}} when String.equal txt s -> raise (Found i)
        | _ -> ()
        );
        failwiths "Plugin passed a constructor `%s` that isn't present" s
      with Found i -> Exp.int_const ~loc i


  (* Adapted to generate only single method per constructor definition *)
  method on_tuple_constr ~loc ~is_self_rec ~mutual_decls ~inhe tdecl constr_info _ts =
    self#find_right_one ~loc constr_info tdecl

  method! on_record_constr ~loc ~is_self_rec ~mutual_decls ~inhe tdecl constr_info bindings labs =
    assert Int.(List.length labs > 0);
    self#find_right_one ~loc (Some constr_info) tdecl

  method on_record_declaration ~loc ~is_self_rec ~mutual_decls tdecl labs =
    [ Cf.method_concrete ~loc (Naming.meth_name_for_record tdecl) @@
      Exp.fun_ ~loc (Pat.unit ~loc) @@
      Exp.fun_ ~loc (Pat.any ~loc) @@
      Exp.int_const ~loc 0
    ]

  method! make_inh ~loc =
    (Pat.unit ~loc, Exp.unit ~loc)
end

let create = (new g :> P.plugin_constructor)

end

let register () =
  Expander.register_plugin trait_name (module Make: Plugin_intf.MAKE)

let () = register ()

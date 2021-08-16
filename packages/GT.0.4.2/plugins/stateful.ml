(*
 * Generic transformers: plugins.
 * Copyright (C) 2016-2019
 *   Dmitrii Kosarev aka Kakadu
 * St.Petersburg State University, JetBrains Research
 *)

(** {i Stateful} plugin: functors + inherited value
    to make decisions about how to map values.

    Behave the same as {!Eval} trait but can may return modified state.

    Inherited attributes' type (both default and for type parameters) is ['env].

    Synthetized attributes' type (both default and for type parameters) is ['env * _ t].

    For type declaration [type ('a,'b,...) typ = ...] it will create transformation
    function with type

    [('env -> 'a -> 'env * 'a2) ->
     ('env -> 'b -> 'env * 'b2) -> ... ->
     'env -> ('a,'b,...) typ -> 'env * ('a2, 'b2, ...) typ ]

  *)

open Base
open Ppxlib
open Printf
open GTCommon
open HelpersBase

let trait_name = "stateful"

module Make(AstHelpers : GTHELPERS_sig.S) = struct

module G = Gmap.Make(AstHelpers)
module P = Plugin.Make(AstHelpers)

let trait_name = trait_name
open AstHelpers

class g initial_args tdecls = object(self: 'self)
  (* TODO: maybe do not inherit from gmap a.k.a. functor *)
  inherit G.g initial_args tdecls as super
  inherit P.with_inherited_attr initial_args tdecls

  method trait_name = trait_name

  method! inh_of_main ~loc _tdecl = Typ.var ~loc "env"
  method! syn_of_param ~loc s =
    Typ.tuple ~loc [Typ.var ~loc "env"; Typ.var ~loc @@ Gmap.param_name_mangler s]
  method inh_of_param ~loc tdecl _name = Typ.var ~loc "env"

  method! syn_of_main ~loc ?in_class tdecl =
    let in_class = match in_class with
      | None -> false
      | Some b -> b
    in
    Typ.tuple ~loc [self#inh_of_main ~loc tdecl; super#syn_of_main ~loc ~in_class tdecl]

  method plugin_class_params ~loc typs ~typname =
    super#plugin_class_params ~loc typs ~typname @
    [ Typ.var ~loc "env"]

  method on_tuple_constr ~loc ~is_self_rec ~mutual_decls ~inhe tdecl constr_info ts =
    let c = match constr_info with
          | Some (`Normal s) -> Exp.construct ~loc (lident s)
          | Some (`Poly s)   -> Exp.variant ~loc s
          | None ->
              assert (List.length ts >=2);
              Exp.tuple ~loc
    in
    match ts with
    | [] -> Exp.tuple ~loc [ inhe; c [] ]
    | ts ->
         let res_var_name = sprintf "%s_rez" in
         let ys = List.mapi ~f:(fun n x -> (n,x)) ts in
         List.fold_right ys
           ~init:(Exp.tuple ~loc [ Exp.sprintf ~loc "env%d" (List.length ys)
                                 ; c @@
                                   List.map ts
                                     ~f:(fun (n,t) -> Exp.ident ~loc @@ res_var_name n)
                                 ]
                 )
           ~f:(fun (i,(name,typ)) acc ->
               Exp.let_one ~loc
                 (Pat.tuple ~loc [ Pat.sprintf ~loc "env%d" (i+1)
                                 ; Pat.sprintf ~loc "%s" @@ res_var_name name])
                 (self#app_transformation_expr ~loc
                    (self#do_typ_gen ~loc ~is_self_rec ~mutual_decls tdecl typ)
                    (if i=0 then inhe else Exp.sprintf ~loc "env%d" i)
                    (Exp.ident ~loc name)
                 )
                 acc
             )


  method! on_record_declaration ~loc ~is_self_rec ~mutual_decls tdecl labs =
    (* TODO: *)
    failwith "not implemented"
end

let create = (new g :> P.plugin_constructor)

end

let register () =
  Expander.register_plugin trait_name (module Make: Plugin_intf.MAKE)

let () = register ()

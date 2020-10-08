(*
 * Generic transformers: plugins.
 * Copyright (C) 2016-2019
 *   Dmitrii Kosarev aka Kakadu
 * St.Petersburg State University, JetBrains Research
 *)

(** {i Stateful} plugin: functors + inherited value
    to make decisions about how to map values.

    Essentially, is a {!Gmap} trait with polymorphic inherited attributes.

    Is a simplified version of {!Stateful} trait: doesn't allow to pass modified
    environment through transformation.

    Inherited attributes' type (both default and for type parameters) is ['env].

    Synthetized attributes' type (both default and for type parameters) is [ _ t].

    For type declaration [type ('a,'b,...) typ = ...] it will create transformation
    function with type

    [('env -> 'a ->  'a2) ->
     ('env -> 'b ->  'b2) -> ... ->
     'env -> ('a,'b,...) typ -> ('a2, 'b2, ...) typ ]
  *)

open Base
open Ppxlib
open Printf
open GTCommon
open HelpersBase

let trait_name = "eval"

let param_name_mangler = sprintf "%s_2"


module Make(AstHelpers : GTHELPERS_sig.S) = struct

module G = Gmap.Make(AstHelpers)
module P = Plugin.Make(AstHelpers)

let trait_name = trait_name

open AstHelpers

class g initial_args tdecls = object(self: 'self)
  inherit G.g initial_args tdecls as super
  inherit P.with_inherited_attr initial_args tdecls as super2

  method trait_name = trait_name

  method! inh_of_main ~loc _tdecl = Typ.var ~loc "env"
  method inh_of_param ~loc tdecl _name = Typ.var ~loc "env"

  method! make_typ_of_class_argument: 'a . loc:loc -> type_declaration ->
    (Typ.t -> 'a -> 'a) ->
    string -> (('a -> 'a) -> 'a -> 'a) -> 'a -> 'a =
    fun ~loc tdecl chain name k ->
      let subj_t = Typ.var ~loc name in
      let syn_t = self#syn_of_param ~loc name in
      let inh_t = self#inh_of_main ~loc tdecl in
      k @@ chain (Typ.arrow ~loc inh_t @@ Typ.arrow ~loc subj_t syn_t)

  method! app_transformation_expr ~loc trf inh subj =
    Exp.app_list ~loc trf [ inh; subj ]

  method plugin_class_params ~loc (typs: Ppxlib.core_type list) ~typname =
    super#plugin_class_params ~loc typs ~typname @
    [ Typ.var ~loc "env"]

  method! extra_class_sig_members tdecl =
    let loc = loc_from_caml tdecl.ptype_loc in
    let wrap =
      if is_polyvariant_tdecl tdecl
      then Typ.openize ~loc
      else (fun ?as_ x -> x)
    in
    [ Ctf.constraint_ ~loc
        (Typ.var ~loc @@ Naming.make_extra_param tdecl.ptype_name.txt)
        (wrap @@ Typ.constr ~loc (Lident tdecl.ptype_name.txt) @@
         map_type_param_names tdecl.ptype_params
           ~f:(fun s -> Typ.var ~loc s)
        )
    ; let syn = sprintf "syn_%s" tdecl.ptype_name.txt in
      Ctf.constraint_ ~loc
        (Typ.var ~loc @@ syn)
        (self#hack ~loc param_name_mangler syn tdecl)
    ]

  method! extra_class_str_members tdecl =
    let loc = loc_from_caml tdecl.ptype_loc in
    let wrap =
      if is_polyvariant_tdecl tdecl
      then Typ.openize ~loc
      else (fun ?as_ x -> x)
    in
    [ Cf.constraint_ ~loc
        (Typ.var ~loc @@ Naming.make_extra_param tdecl.ptype_name.txt)
        (wrap @@ Typ.constr ~loc (Lident tdecl.ptype_name.txt) @@
         map_type_param_names tdecl.ptype_params
           ~f:(fun s -> Typ.var ~loc s)
        )
    ; let syn = sprintf "syn_%s" tdecl.ptype_name.txt in
      Cf.constraint_ ~loc
        (Typ.var ~loc @@ syn)
        (self#hack ~loc param_name_mangler syn tdecl)
    ]

  (* very similar as gmap but uses significant inherited attribute *)
  (* TODO: refactor somehow ??? *)
  method! on_record_declaration ~loc ~is_self_rec ~mutual_decls tdecl labs =
    let pat = Pat.record ~loc @@
      List.map labs ~f:(fun l ->
          (Lident l.pld_name.txt, Pat.var ~loc l.pld_name.txt)
        )
    in
    let methname = sprintf "do_%s" tdecl.ptype_name.txt in
    [ Cf.method_concrete ~loc methname @@
      Exp.fun_ ~loc (Pat.sprintf ~loc "env") @@
      Exp.fun_ ~loc pat @@
      Exp.record ~loc @@ List.map labs
        ~f:(fun {pld_name; pld_type} ->
            lident pld_name.txt,
            self#app_transformation_expr ~loc
              (self#do_typ_gen ~loc ~is_self_rec ~mutual_decls tdecl pld_type)
              (Exp.ident ~loc "env")
              (Exp.ident ~loc pld_name.txt)
          )
    ]

end

let create = (new g :> P.plugin_constructor)

end

let register () =
  Expander.register_plugin trait_name (module Make: Plugin_intf.MAKE)

let () = register ()

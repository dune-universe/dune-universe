(*
 * Generic transformers: plugins.
 * Copyright (C) 2016-2019
 *   Dmitrii Kosarev aka Kakadu
 * St.Petersburg State University, JetBrains Research
 *)

(** {i Foldl} plugin: fold all values in a type.

    Essentially is a stub that chains inherited attribute thorough all values
    in the value

    For type declaration [type ('a,'b,...) typ = ...] it will create a transformation
    function with type

    [('s -> 'a -> 's) ->
     ('s -> 'b -> 's) ->
     ... ->
     's -> ('a,'b,...) typ -> 's ]
*)

open Base
open Ppxlib
open Printf
open GTCommon
open HelpersBase

let trait_name = "foldl"

module Make(AstHelpers : GTHELPERS_sig.S) = struct
open AstHelpers
module P = Plugin.Make(AstHelpers)

let trait_name = trait_name
let make_dest_param_names ps =
  map_type_param_names ps ~f:(Printf.sprintf "%s_2")

class g initial_args tdecls = object(self: 'self)
  inherit P.with_inherited_attr initial_args tdecls

  method trait_name = trait_name

  method syn_of_param ~loc s = Typ.var ~loc "syn"
  method inh_of_main  ~loc tdecl = self#syn_of_main ~loc tdecl
  method syn_of_main  ~loc ?in_class tdecl = self#syn_of_param ~loc "dummy"

  method inh_of_param ~loc tdecl _ = self#syn_of_param ~loc "dummy"

  method plugin_class_params ~loc typs ~typname =
    (List.map typs ~f:Typ.from_caml) @
    [ Typ.var ~loc "syn"
    ; Typ.var ~loc @@ Naming.make_extra_param typname ]

  (* new type of trasfomation function is 'syn -> old_type *)
  method! make_typ_of_class_argument: 'a . loc:loc -> type_declaration ->
    (Typ.t -> 'a -> 'a) ->
    string -> (('a -> 'a) -> 'a -> 'a) -> 'a -> 'a =
    fun ~loc tdecl chain name k ->
      let subj_t = Typ.var ~loc name in
      let syn_t = self#syn_of_param ~loc name in
      let inh_t = self#inh_of_param ~loc tdecl name in
      k @@ chain (Typ.arrow ~loc inh_t @@ Typ.arrow ~loc subj_t syn_t)

  method join_args ~loc do_typ ~init (xs: (string * core_type) list) =
    List.fold_left ~f:(fun acc (name,typ) ->
        Exp.app_list ~loc
          (do_typ typ)
          [ acc; Exp.sprintf ~loc "%s" name]
        )
        ~init
        xs

  method on_tuple_constr ~loc ~is_self_rec ~mutual_decls ~inhe tdecl constr_info args =
    self#join_args ~loc ~init:inhe
        (self#do_typ_gen ~loc ~is_self_rec ~mutual_decls tdecl)
        args

  method on_record_declaration ~loc ~is_self_rec ~mutual_decls tdecl labs =
    (* TODO: introduce fresh pattern names here *)
    let pat = Pat.record ~loc @@
      List.map labs ~f:(fun l ->
          (Lident l.pld_name.txt, Pat.var ~loc l.pld_name.txt)
        )
    in
    let methname = sprintf "do_%s" tdecl.ptype_name.txt in
    [ Cf.method_concrete ~loc methname @@
      Exp.fun_list ~loc
        [ Pat.sprintf ~loc "inh"; pat] @@
        self#join_args ~loc ~init:(Exp.ident ~loc "inh")
          (self#do_typ_gen ~loc ~is_self_rec ~mutual_decls tdecl)
          (List.map labs ~f:(fun l -> (l.pld_name.txt, l.pld_type)))

    ]

  method! on_record_constr ~loc ~is_self_rec ~mutual_decls ~inhe tdecl _info bindings labs =
    assert Int.(List.length labs > 0);

    Exp.fun_list ~loc (List.map bindings ~f:(fun (s,_,_) -> Pat.sprintf ~loc "%s" s)) @@
    self#join_args ~loc ~init:inhe
      (self#do_typ_gen ~loc ~is_self_rec ~mutual_decls tdecl)
      (List.map bindings ~f:(fun (name,_,typ) -> (name, typ)))

end
let create = (new g :> P.plugin_constructor)

end

let register () =
  Expander.register_plugin trait_name (module Make: Plugin_intf.MAKE)

let () = register ()

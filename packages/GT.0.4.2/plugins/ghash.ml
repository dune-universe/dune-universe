(** {i Hash} plugin.

    For type declaration [type ('a,'b,...) typ = ...] it will create a transformation
    function with type

    [(GT.H.t -> 'a -> GT.H.t * 'a) ->
     (GT.H.t -> 'b -> GT.H.t * 'b) -> ... ->
     GT.H.t -> ('a,'b,...) typ -> GT.H.t * ('a,'b,...) typ ]

    which takes a value and hashtable (possibly on-empty) and returns hash concsed
    representation of the value with (maybe) update hashtable.
*)

(*
 * OCanren: syntax extension.
 * Copyright (C) 2016-2017
 *   Dmitrii Kosarev a.k.a. Kakadu
 * St.Petersburg University, JetBrains Research
 *)

open Base
open Ppxlib
open Printf
open GTCommon

let trait_name = "hash"

module Make(AstHelpers : GTHELPERS_sig.S) = struct

module P = Plugin.Make(AstHelpers)

let trait_name = trait_name

open AstHelpers

let ht_typ ~loc =
  Typ.of_longident ~loc (Ldot (Ldot (Lident "GT","H"), "t"))

(* The class representing a plugin for hashconsing. It accepts
   [initial_args] which may care additional arguments specific to plugin
   and [tdecls] -- type declartion that should be processed.

   During code generate phase all plugins require access to type declarations
   declared mutually. That's why we pass type declaration to plugin's
   constructor and not to generation method
*)
class g initial_args tdecls = object(self: 'self)
  inherit P.with_inherited_attr initial_args tdecls

  method trait_name = trait_name

  (* Default inherited attribute is a predefined in GT type of hash table *)
  method inh_of_main ~loc _tdecl = ht_typ ~loc
  (* Inherited attribute for parameter is the same as default one*)
  method inh_of_param ~loc _tdecl _name = ht_typ ~loc

  (* The synthesized attribute of hashconsing is a tuple of new value and
     a new hash table *)
  method syn_of_param ~loc s =
    Typ.tuple ~loc
      [ ht_typ ~loc
      ; Typ.var ~loc s
      ]
  (* The same for default synthsized attribute *)
  method syn_of_main ~loc ?in_class tdecl =
    Typ.tuple ~loc
      [ ht_typ ~loc
      ; Typ.use_tdecl tdecl
      ]


  (* Type parameters of the class are type parameters of type being processed
     plus extra parameter to support polymorphic variants *)
  method plugin_class_params ~loc (typs: Ppxlib.core_type list) ~typname =
    (* the same as in 'show' plugin *)
    (List.map typs ~f:Typ.from_caml) @
    [ Typ.var ~loc @@ Naming.make_extra_param typname ]

  (* method [on_tuple_constr ~loc ~is_self_rec ~mutual_decls ~inhe tdecl cinfo ts]
     receive expression fo rinherited attribute in [inhe],
     the name of constructor (algebrain or polyvariant) in [cinfo]
     and parameters' type in [ts]
  *)
  method on_tuple_constr ~loc ~is_self_rec ~mutual_decls ~inhe tdecl constr_info ts =
    let c = match constr_info with
      | Some (`Normal s) -> Exp.construct ~loc (lident s)
      | Some (`Poly s)   -> Exp.variant ~loc s
      | None -> assert (List.length ts >= 2);
                Exp.tuple ~loc
    in
    match ts with
    | [] ->
      (* without argument we simply return a hash and unchanged value *)
      Exp.tuple ~loc [ inhe; c [] ]
    | ts ->
      (* Constructor with arguments gives oppotunite to save some memory *)
      let res_var_name = sprintf "%s_rez" in
      let argcount = List.length ts in
      (* a shortcut for hashconsing function *)
      let hfhc =
        Exp.field ~loc
          (Exp.of_longident ~loc (Ldot (Lident "GT", "hf")))
          (Lident "hc")
      in
      (* We fold argument and construct a new has and a new argument
        on every step
      *)
      List.fold_right
        (List.mapi ~f:(fun n x -> (n,x)) ts)
        ~init:(
          (* After folding we hashcons constructor of hashconsed arguments *)
          Exp.app_list ~loc hfhc
            [ Exp.sprintf ~loc "ht%d" argcount
            ; c @@
              List.map ts
                ~f:(fun (name,_) -> Exp.ident ~loc @@ res_var_name name)
            ]
        )
        ~f:(fun (i,(name,typ)) acc ->
            (* for every argument we constuctr a pair of new hash and
              new hashconsed argument *)
            Exp.let_one ~loc
              (Pat.tuple ~loc [ Pat.sprintf ~loc "ht%d" (i+1)
                              ; Pat.sprintf ~loc "%s" @@ res_var_name name])
              (* to call transformation for argument we use a method from
                base class
              *)
              (self#app_transformation_expr ~loc
                (* transformation is being generated from the type of argument *)
                (self#do_typ_gen ~loc ~is_self_rec ~mutual_decls tdecl typ)
                (* inherited argument to use *)
                (if i=0 then inhe else Exp.sprintf ~loc "ht%d" i)
                (* the subject of transformation *)
                (Exp.ident ~loc name)
              )
              acc
          )

  method on_record_declaration ~loc ~is_self_rec ~mutual_decls tdecl labs =
    (* TODO: *)
    failwith "not implemented"
end
let create = (new g :> P.plugin_constructor)

end

let register () =
  Expander.register_plugin trait_name (module Make: Plugin_intf.MAKE)

let () = register ()

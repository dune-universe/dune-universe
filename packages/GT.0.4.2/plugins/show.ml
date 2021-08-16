(*
 * Generic transformers: plugins.
 * Copyright (C) 2016-2019
 *   Dmitrii Kosarev aka Kakadu
 * St.Petersburg State University, JetBrains Research
 *)

(** {i Show} plugin: converts value to a string.

    Synthetized attributes' type (both default and for type parameters) is [string].

    Inherited attributes' type (both default and for type parameters) is [unit].

    For type declaration [type ('a,'b,...) typ = ...] it will create transformation
    function with type

    [('a -> string) -> ('b -> string) -> ... -> ('a,'b,...) typ -> string]

    See also: {!Fmt} plugin.
  *)

open Base
open Ppxlib
open Printf
open GTCommon
open HelpersBase

let trait_name = "show"

module Make(AstHelpers : GTHELPERS_sig.S) = struct

let trait_name = trait_name

module P = Plugin.Make(AstHelpers)
open AstHelpers


let app_format_sprintf ~loc arg =
  Exp.app ~loc
    (Exp.of_longident ~loc (Ldot(Lident "Format", "sprintf")))
    arg

class g args tdecls = object(self)
  inherit [loc, Exp.t, Typ.t, type_arg, Ctf.t, Cf.t, Str.t, Sig.t] Plugin_intf.typ_g
  inherit P.generator args tdecls
  inherit P.no_inherit_arg args tdecls

  method trait_name = trait_name
  method inh_of_main ~loc _tdecl           = Typ.ident ~loc "unit"
  method syn_of_main ~loc ?in_class _tdecl = Typ.ident ~loc "string"

  method syn_of_param ~loc _           = Typ.ident ~loc "string"
  method inh_of_param ~loc tdecl _name = self#inh_of_main ~loc tdecl

  method plugin_class_params ~loc typs ~typname =
    (List.map typs ~f:Typ.from_caml) @
    [ Typ.var ~loc @@ Naming.make_extra_param typname ]

  (* Adapted to generate only single method per constructor definition *)
  method on_tuple_constr ~loc ~is_self_rec ~mutual_decls ~inhe tdecl constr_info ts =
    let constr_name = match constr_info with
      | Some (`Poly s) -> sprintf "`%s" s
      | Some (`Normal s) -> sprintf "%s" s
      | None -> ""
    in

    let names = List.map ts ~f:fst in

    Exp.let_ ~loc [Pat.unit ~loc, inhe] @@
    if List.length ts = 0
    then Exp.string_const ~loc constr_name
    else
      List.fold_left ts
        ~f:(fun acc (name, typ) ->
            Exp.app ~loc acc
              (self#app_transformation_expr ~loc
                (self#do_typ_gen ~loc ~is_self_rec ~mutual_decls tdecl typ)
                (Exp.unit ~loc)
                (Exp.ident ~loc name)
              )
          )
        ~init:Exp.(app ~loc
                    (of_longident ~loc (Ldot(Lident "Printf", "sprintf"))) @@

                  let fmt = String.concat ~sep:", " @@ List.map names
                      ~f:(fun _ -> "%s")
                  in
                  Exp.string_const ~loc @@ Printf.sprintf "%s%s(%s)"
                    constr_name
                    (if List.is_empty ts || Option.is_none constr_info then "" else " ")
                    fmt
                  )

  method! on_record_constr ~loc ~is_self_rec ~mutual_decls ~inhe tdecl info bindings labs =
    assert Int.(List.length labs > 0);

    let constr_name = match info with
      | `Poly s -> sprintf "`%s" s
      | `Normal s -> s
    in

    if List.length bindings = 0
    then failwith "Record constructors can't have empty label list"
    else
      List.fold_left bindings
        ~f:(fun acc (ident, labname, typ) ->
          Exp.app ~loc acc @@
            self#app_transformation_expr ~loc
              (self#do_typ_gen ~loc ~is_self_rec ~mutual_decls tdecl typ)
              (Exp.unit ~loc)
              (Exp.ident ~loc ident)
        )
        ~init:Exp.(app ~loc
                    (of_longident ~loc (Ldot(Lident "Printf", "sprintf"))) @@

                  let fmt = String.concat ~sep:", " @@ List.map bindings
                      ~f:(fun (_,lab,_) -> Printf.sprintf "%s=%%s" lab)
                  in
                  Exp.string_const ~loc @@ Printf.sprintf "%s {%s}" constr_name fmt
                 )

  method on_record_declaration ~loc ~is_self_rec ~mutual_decls tdecl labs =
    let pat = Pat.record ~loc @@
      List.map labs ~f:(fun l ->
          (Lident l.pld_name.txt, Pat.var ~loc l.pld_name.txt)
        )
    in
    let fmt = List.fold_left labs ~init:""
        ~f:(fun acc x ->
            sprintf "%s %s=%%s;" acc x.pld_name.txt
          )
    in
    [ Cf.method_concrete ~loc (Naming.meth_name_for_record tdecl) @@
      Exp.fun_ ~loc (Pat.unit ~loc) @@
      Exp.fun_ ~loc pat @@
      List.fold_left labs
            ~f:(fun acc {pld_name; pld_type} ->
                Exp.app ~loc acc
                  (self#app_transformation_expr ~loc
                     (self#do_typ_gen ~loc ~is_self_rec ~mutual_decls tdecl pld_type)
                     (Exp.unit ~loc)
                     (Exp.ident ~loc pld_name.txt)
                  )
              )
            ~init:(app_format_sprintf ~loc @@
                   Exp.string_const ~loc @@ sprintf "{%s }" fmt
                  )
    ]

  method treat_type_specially t =
    Option.map ~f:(fun _ ->
      let loc = loc_from_caml t.ptyp_loc in
      Exp.fun_ ~loc (Pat.unit ~loc) @@
      Exp.fun_ ~loc (Pat.any  ~loc) @@
      Exp.string_const ~loc "\"<opaque>\""
      ) @@
    List.find t.ptyp_attributes ~f:(fun {attr_name={txt}} -> String.equal txt "opaque")

  method! make_inh ~loc = 
    (Pat.unit ~loc, Exp.unit ~loc)
end

let create = (new g :> P.plugin_constructor)

end

let register () =
  Expander.register_plugin trait_name (module Make: Plugin_intf.MAKE)

let () = register ()

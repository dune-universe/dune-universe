(*
 * Generic Transformers: `format` plugin.
 * Copyright (C) 2016-2020
 *   Dmitrii Kosarev a.k.a Kakadu
 * St.Petersburg State University, JetBrains Research
 *)

(** {i Format} module: pretty-prints a value to {!Format.formatter} using {!Format} module.

    For type declaration [type ('a,'b,...) typ = ...] it will create a transformation
    function with type

    [(Format.formatter -> 'a -> unit) -> (Format.formatter -> 'b -> unit) -> ... ->
     Format.formatter -> ('a,'b,...) typ -> unit ]

    Inherited attributes' type (both default and for type parameters) is [Format.formatter].
    Synthesized attributes' type (both default and for type parameters) is [unit].
*)

open Base
open Ppxlib
open Printf
open GTCommon
open HelpersBase

let trait_name = "fmt"

module Make(AstHelpers : GTHELPERS_sig.S) = struct

let trait_name = trait_name

module P = Plugin.Make(AstHelpers)
open AstHelpers

let app_format_fprintf ~loc efmtr efmts =
  Exp.app_list ~loc
    Exp.(of_longident ~loc (Ldot(Lident "Format", "fprintf")) )
    [ efmtr; efmts ]

class g args tdecls = object(self)
  inherit [loc, Exp.t, Typ.t, type_arg, Ctf.t, Cf.t, Str.t, Sig.t] Plugin_intf.typ_g
  inherit P.generator args tdecls
  inherit P.with_inherited_attr args tdecls

  method trait_name = trait_name
  method inh_of_main ~loc _tdecl =
    Typ.of_longident ~loc (Ldot (Lident "Format", "formatter"))
  method syn_of_main ~loc ?in_class _tdecl = Typ.ident ~loc "unit"

  method syn_of_param ~loc _     = Typ.ident ~loc "unit"
  method inh_of_param ~loc tdecl _name = self#inh_of_main ~loc tdecl

  method plugin_class_params ~loc typs ~typname =
    List.map typs ~f:Typ.from_caml  @
    [ Typ.var ~loc @@ Naming.make_extra_param typname ]

  (* Adapted to generate only single method per constructor definition *)
  method on_tuple_constr ~loc ~is_self_rec ~mutual_decls ~inhe tdecl constr_info ts =
    let constr_name = match constr_info with
      | Some (`Poly s) -> sprintf "`%s " s
      | Some (`Normal s) -> sprintf "%s " s
      | None -> ""
    in

    let fmt = List.map ts ~f:(fun _ -> "%a") |> String.concat ~sep:",@,@ " in
    let fmt = sprintf "%s@[(@,%s@,)@]" constr_name fmt in

    if List.length ts = 0
    then app_format_fprintf ~loc inhe @@ Exp.string_const ~loc constr_name
    else
      List.fold_left ts
        ~f:(fun acc (name, typ) ->
            Exp.app_list ~loc acc
              [ self#do_typ_gen ~loc ~is_self_rec ~mutual_decls tdecl typ
              ; Exp.ident ~loc name
              ]
          )
        ~init:(app_format_fprintf ~loc inhe @@
                Exp.string_const ~loc fmt
              )


  method on_record_declaration ~loc ~is_self_rec ~mutual_decls tdecl labs =
    let pat = Pat.record ~loc @@
      List.map labs ~f:(fun l ->
          (Lident l.pld_name.txt, Pat.var ~loc l.pld_name.txt)
        )
    in
    let methname = sprintf "do_%s" tdecl.ptype_name.txt in
    let fmt = List.fold_left labs ~init:""
        ~f:(fun acc x ->
            sprintf "%s@,@ @,@[%s@,=@,%%a;@]" acc x.pld_name.txt
          )
    in
    let fmt_name = gen_symbol ~prefix:"fmt" () in
    [ Cf.method_concrete ~loc methname @@
      Exp.fun_ ~loc (Pat.sprintf "%s" ~loc fmt_name) @@
      Exp.fun_ ~loc pat @@
      List.fold_left labs
            ~f:(fun acc {pld_name; pld_type} ->
                Exp.app_list ~loc acc
                  [ self#do_typ_gen ~loc ~is_self_rec ~mutual_decls tdecl pld_type
                  ; Exp.ident ~loc pld_name.txt
                  ]
              )
            ~init:(app_format_fprintf ~loc (Exp.sprintf "%s" ~loc fmt_name) @@
                   Exp.string_const ~loc @@ sprintf "{@[<hov>%s@]@ }@," fmt
                  )
    ]

  method! on_record_constr ~loc ~is_self_rec ~mutual_decls ~inhe tdecl info bindings labs =
    let cname = match info with
      | `Normal s -> s
      | `Poly s -> s
    in
    let fmt = List.fold_left labs ~init:""
        ~f:(fun acc l ->
            sprintf "%s@,@ @,@[%s@,=@,%%a;@]" acc l.pld_name.txt
          )
    in
    List.fold_left bindings
      ~f:(fun acc (name, _, typ) ->
        Exp.app_list ~loc acc
          [ self#do_typ_gen ~loc ~is_self_rec ~mutual_decls tdecl typ
          ; Exp.ident ~loc name
          ]
      )
      ~init:(app_format_fprintf ~loc inhe @@
        Exp.string_const ~loc @@ sprintf "%s {@[<hov>%s@]@ }@," cname fmt
      )


end

let create = (new g :> P.plugin_constructor)

end

let register () =
  Expander.register_plugin trait_name (module Make: Plugin_intf.MAKE)

let () = register ()

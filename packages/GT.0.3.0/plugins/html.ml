(*
 * Generic transformers: plugins.
 * Copyright (C) 2016-2019
 *   Dmitrii Kosarev aka Kakadu
 * St.Petersburg State University, JetBrains Research
 *)

(** {i Html} module: converts a value to its html represenation (work in progress).
*)

(*
    For type declaration [type ('a,'b,...) typ = ...] it will create a transformation
    function with type

    [('a -> HTML.er) -> ('b -> HTML.er) -> ... ->
     ('a,'b,...) typ -> HTML.er ]

    Inherited attributes' type (both default and for type parameters) are absent.

    Synthesized attributes' type (both default and for type parameters) is [HTML.er].
*)

open Base
open Ppxlib
open Printf
open GTCommon
open HelpersBase

let trait_name = "html"

module Make(AstHelpers : GTHELPERS_sig.S) = struct

let trait_name = trait_name

module P = Plugin.Make(AstHelpers)
open AstHelpers

let app_format_sprintf ~loc arg =
  Exp.app ~loc
    (Exp.of_longident ~loc (Ldot(Lident "Format", "sprintf")))
    arg

module H = struct
  type elt = Exp.t
  let wrap ~loc s = Exp.of_longident ~loc (Ldot (Lident "HTML", s))
  let pcdata ~loc s = Exp.(app ~loc (wrap ~loc "string") (string_const ~loc s))
  let div ~loc xs =
    Exp.app ~loc (wrap ~loc "list") @@
    Exp.list ~loc xs

  let to_list_e ~loc xs =
    List.fold_right xs ~init:(Exp.construct ~loc (lident "[]") [])
      ~f:(fun x acc ->
          Exp.app_list ~loc (Exp.of_longident ~loc (Ldot (Lident "List", "cons"))) [x; acc]
        )

  let li ~loc xs =
    Exp.app ~loc (wrap ~loc "li") @@ Exp.app ~loc (wrap ~loc "seq") @@ to_list_e ~loc xs
  (* let ol ~loc xs =
   *   Exp.app ~loc (wrap ~loc "ol") @@ Exp.app ~loc (wrap ~loc "seq") @@ to_list_e ~loc xs *)
  let ul ~loc xs =
    Exp.app ~loc (wrap ~loc "ul") @@ Exp.app ~loc (wrap ~loc "seq") @@ to_list_e ~loc xs
  let checkbox ~loc name =
    let open Exp in
    app ~loc
      (app_lab ~loc (wrap ~loc "input")
         "attrs" (string_const ~loc @@ Printf.sprintf "type=\"checkbox\" id=\"%s\"" name))
      (app ~loc (wrap ~loc "unit") (unit ~loc))
end

class g args tdecls = object(self)
  inherit [loc, Exp.t, Typ.t, type_arg, Ctf.t, Cf.t, Str.t, Sig.t] Plugin_intf.typ_g
  inherit P.generator args tdecls
  inherit P.no_inherit_arg args tdecls

  method trait_name = trait_name
  method inh_of_main ~loc _tdecl = Typ.ident ~loc "unit"
  method syn_of_main ~loc ?in_class _tdecl = self#syn_of_param ~loc "dummy"

  method syn_of_param ~loc _     =
    Typ.constr ~loc (Ldot (Lident "HTML", "er")) []

  method inh_of_param ~loc tdecl _name = self#inh_of_main ~loc tdecl

  method plugin_class_params ~loc (typs: Ppxlib.core_type list) ~typname =
    (* the same as in 'show' plugin *)
    (List.map typs ~f:Typ.from_caml) @
    [ Typ.var ~loc @@ Naming.make_extra_param typname ]

  method on_tuple_constr ~loc ~is_self_rec ~mutual_decls ~inhe tdecl constr_info ts =
    let constr_name = match constr_info with
      | Some (`Poly s) -> sprintf "`%s" s
      | Some (`Normal s) -> s
      | None -> ""
    in

    if List.length ts = 0
    then H.(ul ~loc [pcdata ~loc constr_name])
    else
        H.ul ~loc @@ (
           (H.li ~loc [H.pcdata ~loc constr_name]) ::
           (List.map ts ~f:(fun (name, typ) ->
              H.li ~loc
                [ self#app_transformation_expr ~loc
                    (self#do_typ_gen ~loc ~is_self_rec ~mutual_decls tdecl typ)
                    (Exp.unit ~loc)
                    (Exp.ident ~loc name)
                ]
              ))
         )

  method on_record_declaration ~loc ~is_self_rec ~mutual_decls tdecl labs =
    let pat = Pat.record ~loc @@
      List.map labs ~f:(fun l ->
          (Lident l.pld_name.txt, Pat.var ~loc l.pld_name.txt)
        )
    in
    let methname = sprintf "do_%s" tdecl.ptype_name.txt in
    [ Cf.method_concrete ~loc methname @@
      Exp.fun_ ~loc (Pat.unit ~loc) @@
      Exp.fun_ ~loc pat @@

      let ds = List.map labs
          ~f:(fun {pld_name; pld_type} ->
            H.li ~loc
              [ H.pcdata ~loc pld_name.txt
              ; self#app_transformation_expr ~loc
                  (self#do_typ_gen ~loc ~is_self_rec ~mutual_decls tdecl pld_type)
                  (Exp.unit ~loc)
                  (Exp.ident ~loc pld_name.txt)
              ]
            )
      in
      H.ul ~loc @@ (H.pcdata ~loc tdecl.ptype_name.txt) :: ds
    ]

  method! on_record_constr: loc:loc ->
    is_self_rec:(core_type -> [ `Nonrecursive | `Nonregular | `Regular ]) ->
    mutual_decls:type_declaration list ->
    inhe:Exp.t ->
    _ ->
    [ `Normal of string | `Poly of string ] ->
    (string * _ * core_type) list ->
    label_declaration list ->
    Exp.t = fun  ~loc ~is_self_rec ~mutual_decls ~inhe tdecl info bindings labs ->
    let constr_name = match info with
      | `Poly s -> sprintf "`%s" s
      | `Normal s -> s
    in


    let open H  in
    ul ~loc @@
    [pcdata ~loc constr_name] @
    List.map bindings
      ~f:(fun (pname, lname, typ) ->
          H.li ~loc
              [ H.pcdata ~loc lname
              ; self#app_transformation_expr ~loc
                  (self#do_typ_gen ~loc ~is_self_rec ~mutual_decls tdecl typ)
                  (Exp.unit ~loc)
                  (Exp.ident ~loc pname)
              ]
        )


end

let create = (new g :> P.plugin_constructor)

end

let register () =
  Expander.register_plugin trait_name (module Make: Plugin_intf.MAKE)

let () = register ()

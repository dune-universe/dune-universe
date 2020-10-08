(*
 * Generic Transformers PPX syntax extension.
 * Copyright (C) 2016-2019
 *   Dmitrii Kosarev aka Kakadu
 * St.Petersburg State University, JetBrains Research
 *)

open Base
open Ppxlib
open Ppxlib.Ast_builder.Default
open HelpersBase
open Printf
open Naming
let (@@) = Caml.(@@)

type config_plugin = Skip | Use of Plugin_intf.plugin_args

let registered_plugins
  : (string * (module Plugin_intf.MAKE)) list ref =
  ref []

let get_registered_plugins () = List.map ~f:fst !registered_plugins
let register_plugin name m =
  let p = (name, m) in
  registered_plugins := p :: !registered_plugins;
  ()

module Make(AstHelpers : GTHELPERS_sig.S) = struct

open AstHelpers

let prepare_patt_match ~loc ?else_case what constructors make_rhs =
  let on_alg cdts =
    let k cs = Exp.match_ ~loc what cs in
    k @@
    List.map cdts ~f:(fun cd ->
      match cd.pcd_args with
      | Pcstr_record ls ->
        let names = List.map ls ~f:(fun _ -> gen_symbol ()) in
        case
          ~lhs:(Pat.constr_record ~loc cd.pcd_name.txt @@
            List.map2_exn ls names
              ~f:(fun l s -> (l.pld_name.txt, Pat.var ~loc s))
          )
          ~rhs:(make_rhs cd names)
      | Pcstr_tuple args ->
            let names = List.map args ~f:(fun _ -> gen_symbol ()) in
            case
              ~lhs:(Pat.constr ~loc cd.pcd_name.txt @@
                    List.map ~f:(Pat.var ~loc) names
                   )
              ~rhs:(make_rhs cd names)
      )
    @
    (match else_case with
     | None -> []
     | Some f ->
       let name = gen_symbol ~prefix:"else" () in
       [case ~lhs:(Pat.sprintf ~loc "%s" name) ~rhs:(f name)]
    )
  in
  let on_poly cs =
    assert false
  in
  match constructors with
  | `Algebraic cdts -> on_alg cdts
  | `PolyVar cs -> on_poly cs

let prepare_patt_match_poly ~loc what rows labels ~onrow ~onlabel ~oninherit =
  let k cs = Exp.match_ ~loc what cs in
  let rs =
    List.map rows ~f:(function
        | Rtag (lab, _, args) ->
          let args = match args with
            | [t] -> unfold_tuple t
            | [] -> []
            | _ -> failwith "we don't support conjunction types"
          in
          let names = List.map args ~f:(fun _ -> gen_symbol ~prefix:"_" ()) in
          let lhs = Pat.variant ~loc  lab.txt @@
            List.map ~f:(fun s -> Pat.var ~loc s) names
          in
          case ~lhs
            ~rhs:(onrow lab @@ List.zip_exn names args)
        | Rinherit typ ->
          match typ.ptyp_desc with
          | Ptyp_constr({txt;_},ts) ->
            let newname = "subj" in
            let lhs = Pat.alias ~loc (Pat.type_ ~loc txt) newname in
            case ~lhs ~rhs:(oninherit ts txt newname)
          | _ -> failwith "this inherit field isn't supported"

      )
  in
  let ls = match labels with
    | None -> []
    | Some ls -> List.map ls ~f:(fun lab ->
        let newname = "subj" in
        let lhs = Pat.alias ~loc (Pat.type_ ~loc (Lident lab)) newname in
        case ~lhs ~rhs:(onlabel lab newname)
      )
  in
  k @@ rs@ls

let params_of_interface_class ~loc params =
  (* actual params depend on sort of type.
     2 + 3*params_count + 1 (for polyvar subtyping)
  *)
  (List.concat @@ map_type_param_names params
     ~f:(fun s ->
         [ named_type_arg ~loc ("i"^s)
         ; named_type_arg ~loc s
         ; named_type_arg ~loc ("s"^s) ]
       )
  )
  @ [ named_type_arg ~loc "inh"
    ; named_type_arg ~loc Naming.extra_param_name
    ; named_type_arg ~loc "syn"
    ]

let make_interface_class_sig ~loc tdecl =
  let name = tdecl.ptype_name in

  let k fields =
    [ Sig.class_ ~loc ~virt:true
        ~name:(class_name_for_typ name.txt)
        ~params:(params_of_interface_class ~loc tdecl.ptype_params)
        fields
    ]
  in
  let on_constructor pcd_args pcd_name =
    let methname = Naming.meth_name_for_constructor pcd_name.txt in
    let typs = match pcd_args with
      | Pcstr_record ls -> List.map ls ~f:(fun x -> x.pld_type)
      | Pcstr_tuple ts -> ts
    in
    Ctf.method_ ~loc methname ~virt:true @@
    Typ.chain_arrow ~loc @@
      [ Typ.var ~loc "inh"
      ; Typ.var ~loc "extra" ] @
      (List.map typs ~f:Typ.from_caml) @
      [ Typ.var ~loc "syn" ]
  in
  visit_typedecl ~loc tdecl
    ~onrecord:(fun _labels ->
        (* almost the same as plugin#get_class_sig*)
          k [ Ctf.method_ ~loc (Naming.meth_name_for_record tdecl) ~virt:true @@
              Typ.chain_arrow ~loc @@
              let open Typ in
              [ var ~loc "inh"
              ; use_tdecl tdecl
              ; var ~loc "syn" ]
            ]
      )
    ~onabstract:(fun () ->
        (* For purely abstract type we can only generate interface
           (we don't know methods)  *)
        []
      )
    ~onvariant:(fun cds ->
      k @@ List.map cds ~f:(fun cd -> on_constructor cd.pcd_args cd.pcd_name)
    )
    ~onmanifest:(fun typ ->
        let wrap name params =
          let inh_params =
            List.concat_map params ~f:(fun typ ->
                [ map_core_type typ ~onvar:(fun n -> Some (ptyp_var ~loc:typ.ptyp_loc ("i"^n)) )
                ; typ
                ; map_core_type typ ~onvar:(fun n -> Some (ptyp_var ~loc:typ.ptyp_loc ("s"^n)) )
                ]
              )
            |> List.map ~f:Typ.from_caml
          in
          let inh_params =
              inh_params @
              [ Typ.var ~loc "inh"
              ; Typ.var ~loc Naming.extra_param_name
              ; Typ.var ~loc "syn"
              ]
          in

          [ Ctf.inherit_ ~loc @@
            Cty.constr ~loc (map_longident ~f:class_name_for_typ name)
              inh_params
          ]
        in

        let rec helper typ = match typ.ptyp_desc with
          | Ptyp_constr ({txt;loc}, params) ->
            (* a type alias on toplevel *)
            k @@ wrap txt params
          | Ptyp_var name ->
            let new_lident = Ldot (Lident "GT", "free") in
            let open Ppxlib.Ast_builder.Default in
            let loc = typ.ptyp_loc in
            helper @@ ptyp_constr ~loc (Located.mk ~loc new_lident) [ptyp_var ~loc name]
          | Ptyp_tuple ts ->
            (* let's say we have predefined aliases for now *)
            let new_lident = Ldot (Lident "GT", sprintf "tuple%d" @@ List.length ts) in
            let open Ppxlib.Ast_builder.Default in
            let loc = typ.ptyp_loc in
            helper @@ ptyp_constr ~loc (Located.mk ~loc new_lident) ts
          | Ptyp_alias (typ, new_name) ->
            let loc = typ.ptyp_loc in
            map_core_type typ ~onvar:(fun as_ ->
                let params = List.map ~f:fst tdecl.ptype_params in
                let open Ppxlib.Ast_builder.Default in
                if String.equal as_ new_name
                then Some (ptyp_constr ~loc (Located.lident ~loc name.txt) params)
                else Some (ptyp_var ~loc as_)
              ) |> helper
          | Ptyp_variant (rows,_,labels) ->
              (* rows go to virtual methods. label goes to inherit fields *)
              let meths =
                List.concat_map rows ~f:(fun rf ->
                  match rf.prf_desc with
                  | Rtag (lab, _, args)  ->
                    let args = match args with
                      | [] -> []
                      | [{ptyp_desc=Ptyp_tuple ts; _}] -> ts
                      | [t] -> [t]
                      | _ -> failwith "conjective not supported"
                    in
                    let methname = Naming.meth_of_constr lab.txt in
                    let ts =
                      let open Typ in
                      [ var ~loc "inh"
                      ; var ~loc "extra" ] @
                      (List.map ~f:from_caml args) @
                      [ var ~loc "syn" ]
                      |> chain_arrow ~loc
                    in
                    [ Ctf.method_ ~loc ~virt:true methname ts ]
                | Rinherit typ -> match typ.ptyp_desc with
                  | Ptyp_constr ({txt;loc}, params) ->
                     wrap txt params
                  | _ -> assert false
                )
              in
              k meths
          | Ptyp_extension _ -> not_implemented "extensions in types not implemented: %s"
                                  (string_of_core_type typ)
          |  _ -> failwith " not implemented"
          in
          let toplevel typ = match typ.ptyp_desc with
          | Ptyp_tuple _
          | Ptyp_var _ ->
              k  @@ [ on_constructor (Pcstr_tuple []) @@
                      Located.mk ~loc:typ.ptyp_loc @@ String.uppercase tdecl.ptype_name.txt ]
          | _ -> helper typ
          in
          toplevel typ
    )

let inherit_iface_class ~loc name params =
  let inh_params =
    List.concat_map params ~f:(fun typ ->
        [ map_core_type typ ~onvar:(fun n -> Some (ptyp_var ~loc:typ.ptyp_loc ("i"^n) ))
        ; typ
        ; map_core_type typ ~onvar:(fun n -> Some (ptyp_var ~loc:typ.ptyp_loc ("s"^n) ))
        ]
      )
    |> List.map ~f:Typ.from_caml
  in
  let inh_params =
    inh_params @
    [ Typ.var ~loc "inh"
    ; Typ.var ~loc "extra"
    ; Typ.var ~loc "syn"
    ]
  in

  Cf.inherit_ ~loc @@
  Cl.constr ~loc (map_longident ~f:class_name_for_typ name)
    inh_params

let make_interface_class ~loc tdecl =
  let params = List.map ~f:fst tdecl.ptype_params in
  let name = tdecl.ptype_name in

  let ans ?(is_poly=false) fields =
    class_declaration ~loc ~name:(class_name_for_typ name.txt) fields
      ~virt:true
      ~params:(params_of_interface_class ~loc tdecl.ptype_params)
  in
  let on_constructor pcd_args pcd_name =
    let methname = Naming.meth_of_constr pcd_name.txt in
    let typs = match pcd_args with
      | Pcstr_record ls -> List.map ls ~f:(fun x -> x.pld_type)
      | Pcstr_tuple ts -> ts
    in
    Cf.method_virtual ~loc methname @@
      Typ.(List.fold_right typs ~init:(var ~loc "syn")
          ~f:(fun t -> arrow ~loc (from_caml t))
      |> (arrow ~loc (var ~loc "extra"))
      |> (arrow ~loc (var ~loc "inh"))
        )
  in
  visit_typedecl ~loc tdecl
    ~onopen:(fun () -> ans [])
    ~onrecord:(fun _ ->
        ans
          [ Cf.method_virtual ~loc (sprintf "do_%s" tdecl.ptype_name.txt) @@
            Typ.(arrow ~loc (var ~loc "inh") @@
                 arrow ~loc (use_tdecl tdecl)
                   (var ~loc "syn")
                )
          ]
      )
    ~onvariant:(fun cds ->
      ans @@
      List.map cds ~f:(fun cd -> on_constructor cd.pcd_args cd.pcd_name)
    )
    ~onmanifest:(fun typ ->
        let wrap ?(is_poly=false) name params =
          [inherit_iface_class ~loc name params]
        in

        let rec helper typ = match typ with
          | _ -> match typ.ptyp_desc with
          | Ptyp_var name ->
            let new_lident = Ldot (Lident "GT", "free") in
            let open Ppxlib.Ast_builder.Default in
            let loc = typ.ptyp_loc in
            helper @@ ptyp_constr ~loc (Located.mk ~loc new_lident) [ptyp_var ~loc name]

          | Ptyp_constr ({txt;loc}, params) ->
            (* a type alias on toplevel *)
            ans @@ wrap txt params
          | Ptyp_tuple ts ->
            (* let's say we have predefined aliases for now *)
            let new_lident = Ldot (Lident "GT", sprintf "tuple%d" @@ List.length ts)
            in
            let open Ppxlib.Ast_builder.Default in
            let loc = typ.ptyp_loc in
            helper @@ ptyp_constr ~loc (Located.mk ~loc new_lident) ts
          | Ptyp_alias (typ, new_name) ->
            let loc = typ.ptyp_loc in
            map_core_type typ ~onvar:(fun as_ ->
                let open Ppxlib.Ast_builder.Default in
                if String.equal as_ new_name
                then Some (ptyp_constr ~loc (Located.lident ~loc name.txt) params)
                else Some (ptyp_var ~loc as_)
              ) |> helper
          | Ptyp_variant (rows,_,labels) ->
              (* rows go to virtual methods. label goes to inherit fields *)
            ans ~is_poly:true @@
              List.concat_map rows ~f:(fun rf ->
                match rf.prf_desc with
                | Rtag (lab, _, []) ->
                    let methname = sprintf "c_%s" lab.txt in
                    [ Cf.method_virtual ~loc methname @@
                      Typ.( var ~loc "syn"
                            |> (arrow ~loc @@
                                var ~loc "extra")
                            |> arrow ~loc (var ~loc "inh")
                          )
                    ]
                | Rtag (lab, _, [typ]) ->
                      (* print_endline "HERE"; *)
                      let args = match typ.ptyp_desc with
                        | Ptyp_tuple ts -> ts
                        | _ -> [typ]
                      in
                      let methname = sprintf "c_%s" lab.txt in
                      [ Cf.method_virtual ~loc methname @@
                          let open Typ in
                          (List.fold_right args ~init:(var ~loc "syn")
                             ~f:(fun t -> arrow ~loc (from_caml t))
                           |> (arrow ~loc @@
                               var ~loc "extra")
                            |> (arrow ~loc (var ~loc "inh"))
                          )
                      ]
                | Rtag (_, _, _) -> failwith "Can't deal with conjunctive types"
                | Rinherit typ -> match typ.ptyp_desc with
                      | Ptyp_constr ({txt;loc}, params) ->
                        wrap ~is_poly:true txt params
                      | _ -> assert false
                )
          | Ptyp_extension _ -> not_implemented "extensions in types `%s`"
                                  (string_of_core_type typ)
          | _ -> failwith "not implemented "
          in
          let toplevel typ = match typ.ptyp_desc with
          | Ptyp_tuple _
          | Ptyp_var _ ->
              ans @@ [ on_constructor (Pcstr_tuple []) @@
                       Located.mk ~loc:typ.ptyp_loc @@ String.uppercase tdecl.ptype_name.txt ]
          | _ -> helper typ
          in
          toplevel typ
    )

let wildcard_tdecl td =
  let loc = loc_from_caml td.ptype_loc in
  Typ.constr ~loc
    (Lident td.ptype_name.txt) @@
  List.map td.ptype_params ~f:(fun _ -> Typ.any ~loc)

let make_gcata_typ ~loc tdecl =
  let on_alias_or_abstract () =
    let args = map_type_param_names tdecl.ptype_params ~f:(fun name ->
        [ Typ.any ~loc
        ; Typ.var ~loc name
        ; Typ.var ~loc @@ "s"^name ]
      ) |> List.concat
    in
    let args = args @ [Typ.var ~loc "inh"; Typ.use_tdecl tdecl; Typ.var ~loc "syn" ]
    in
    Typ.class_ ~loc (Lident(class_name_for_typ tdecl.ptype_name.txt)) args
  in
  let tr_t =
    visit_typedecl ~loc tdecl
      ~onabstract:(fun () -> on_alias_or_abstract () )
      ~onrecord:(fun _ ->
          Typ.object_ ~loc Open @@
          [ sprintf "do_%s" tdecl.ptype_name.txt,
            Typ.(chain_arrow ~loc
              [var ~loc "inh"; use_tdecl tdecl; var ~loc "syn"]
                )
          ]
        )
      ~onvariant:(fun cds ->
          Typ.object_ ~loc Open @@
          List.map cds
            ~f:(fun cd ->
                let typs = match cd.pcd_args with
                  | Pcstr_record ls -> List.map ls ~f:(fun x -> x.pld_type)
                  | Pcstr_tuple ts -> ts
                in
                let new_ts =
                  let open Typ in
                  [ var ~loc "inh"
                  ; use_tdecl tdecl ] @
                  (List.map typs ~f:Typ.from_caml) @
                  [Typ.var ~loc "syn"]
                in
                (Naming.meth_of_constr cd.pcd_name.txt, Typ.chain_arrow ~loc new_ts)
              )
        )
      ~onmanifest:(fun t ->
          let rec helper typ =
            match typ.ptyp_desc with
            | Ptyp_constr (_,_) -> on_alias_or_abstract ()
            | Ptyp_var name ->
              let new_lident = Ldot (Lident "GT", "free") in
              let open Ppxlib.Ast_builder.Default in
              let loc = typ.ptyp_loc in
              helper @@ ptyp_constr ~loc (Located.mk ~loc new_lident) [ptyp_var ~loc name]
            | Ptyp_variant (rows,_flg,_) ->
              let params = map_type_param_names tdecl.ptype_params
                  ~f:(fun s ->
                    [Typ.any ~loc; Typ.var ~loc s; Typ.var ~loc @@ "s"^s ]
                  )
              in
              Typ.class_ ~loc
                (Lident (class_name_for_typ tdecl.ptype_name.txt))
                (List.concat params @
                 Typ.[ var ~loc "inh"
                     ; openize_poly ~loc @@
                       Typ.constr ~loc (Lident tdecl.ptype_name.txt) @@
                       map_type_param_names tdecl.ptype_params ~f:(Typ.var ~loc)
                     ; var ~loc "syn" ])
            | Ptyp_tuple ts ->
              helper @@ constr_of_tuple ~loc:t.ptyp_loc ts
            | _ -> failwith "Unsupported case during generate of the type of gcata"
          in
          helper t
        )
  in
  let subj_t = Typ.use_tdecl tdecl in
  Typ.(chain_arrow ~loc [ tr_t; var ~loc "inh"; subj_t; var ~loc "syn" ])

let make_gcata_sig ~loc ?(shortname=false) tdecl =
  let wrap () =
    let type_ = make_gcata_typ ~loc tdecl in
    let name = if shortname then "gcata"
      else Printf.sprintf "gcata_%s" tdecl.ptype_name.txt
    in
    [ Sig.value ~loc ~name type_ ]
  in
  visit_typedecl ~loc tdecl
    ~onrecord:(fun _ -> wrap ())
    ~onvariant:(fun _ -> wrap ())
    ~onmanifest:(fun _ -> wrap ())
    ~onabstract:(fun () -> [])

let make_gcata_str ~loc tdecl =
  let gcata_pat =
     Pat.var ~loc (sprintf "gcata_%s" tdecl.ptype_name.txt)
  in
  let ans k =
    let tr =
      let wrap  t =
        if is_polyvariant_tdecl tdecl
        then openize_poly ~loc t
        else t
      in
      (* let tr = Pat.var ~loc "tr" in
       * if not (is_polyvariant_tdecl tdecl)
       * then tr
       * else *)
        Pat.constraint_ ~loc (Pat.var ~loc "tr") @@
        Typ.class_ ~loc
          (Lident (Naming.class_name_for_typ tdecl.ptype_name.txt))
          (let param_names = List.mapi tdecl.ptype_params
            ~f:(fun i _ -> gen_symbol ~prefix:(sprintf "typ%d" i)  ())
          in
          let typ_self =
            Typ.constr ~loc (Lident tdecl.ptype_name.txt) @@
            List.map param_names ~f:(Typ.var ~loc)
          in
          List.concat_map param_names ~f:(fun name ->
               Typ.[any ~loc; var ~loc name; any ~loc ]
             )
           @ [Typ.any ~loc; wrap @@ typ_self; Typ.any ~loc]
          )
    in
    Str.single_value ~loc
      gcata_pat
      (Exp.fun_list ~loc Pat.[tr; var ~loc "inh"; var ~loc "subj"]
         k)
  in


  let match_and_openize ~loc ident type_lident =
    let new_name = "foo" in
    Exp.match_ ~loc ident
      [ case ~lhs:(Pat.alias ~loc (Pat.type_ ~loc type_lident) new_name)
          ~rhs:(Exp.ident ~loc new_name)
      ]
  in
  let onvariant cds =
    ans @@ prepare_patt_match ~loc (Exp.ident ~loc "subj") (`Algebraic cds)
      (fun cd names ->
        (* TODO: Subj ident has to be passed as an argument *)
        let subj = "subj" in
        List.fold_left ("inh"::subj::names)
            ~init:(Exp.send ~loc (Exp.ident ~loc "tr")
                    (Naming.meth_of_constr cd.pcd_name.txt))
            ~f:(fun acc arg -> Exp.app ~loc acc (Exp.ident ~loc arg))
    )
  in
  visit_typedecl ~loc tdecl
    ~onopen:(fun () ->
        ans @@ Exp.failwith_ ~loc "Extensible types not yet supported")
    ~onrecord:(fun _labels ->
        let methname = sprintf "do_%s" tdecl.ptype_name.txt in
        ans @@ Exp.(app_list ~loc
                  (send ~loc (ident ~loc "tr") methname)
                  [ident ~loc "inh"; ident ~loc "subj"])
      )
    ~onmanifest:(fun typ ->
        let rec helper t = match t.ptyp_desc with
        | Ptyp_alias (t,_) -> helper t
        | Ptyp_var name ->
          let new_lident = Ldot (Lident "GT", "free") in
          let open Ppxlib.Ast_builder.Default in
          let loc = typ.ptyp_loc in
          helper @@ ptyp_constr ~loc (Located.mk ~loc new_lident) [ptyp_var ~loc name]

        | Ptyp_constr ({txt},_) ->
          Str.single_value ~loc
            gcata_pat
            (Exp.of_longident ~loc @@
             map_longident txt ~f:Naming.gcata_name_for_typ)
        | Ptyp_tuple ts ->
          (* let's say we have predefined aliases for now *)
          helper @@ constr_of_tuple ~loc:t.ptyp_loc ts
        | Ptyp_variant (rows,_,maybe_labels) ->
          let subj_s = "subj" in
          ans @@ prepare_patt_match_poly ~loc (Exp.ident ~loc subj_s)
            (List.map rows ~f:(fun {prf_desc} -> prf_desc))
            maybe_labels
            ~onrow:(fun cname names ->
                List.fold_left
                  ~init:(Exp.send ~loc (Exp.ident ~loc "tr") ("c_" ^ cname.txt))
                  ~f:(Exp.app ~loc)
                  ((Exp.ident ~loc "inh") ::
                   (match_and_openize ~loc (Exp.ident ~loc subj_s)
                      (Lident tdecl.ptype_name.txt ))
                                            ::
                   (List.map ~f:(fun (s,_) -> Exp.ident ~loc s) names)
                  )
              )
            ~onlabel:(fun label patname -> failwith "not implemented")
            ~oninherit:(fun params cident patname ->
                Exp.app_list ~loc
                  (Exp.of_longident ~loc  @@
                   map_longident cident ~f:(gcata_name_for_typ))
                  (List.map ["tr";"inh";patname] ~f:(Exp.sprintf ~loc "%s"))
              )
        | Ptyp_object (_,_) -> failwith "not implemented: object types"
        | Ptyp_class (_,_) -> failwith "not implemented: class types"
        | Ptyp_package _ -> failwith "not implemented: package types"
        | Ptyp_extension _ -> failwith "not implemented: extension types"
        | Ptyp_arrow _ -> failwith "not implemented: arrow types"
        | Ptyp_any -> failwith "not implemented: wildcard types (but it should be easy to rewrite)"
        | Ptyp_poly (_,_) -> failwith "not implemented: existential types"
        in

        let toplevel t = match t.ptyp_desc with
        | Ptyp_var _
        | Ptyp_tuple _ ->
            ans @@
            Exp.app_list ~loc
              (Exp.send ~loc (Exp.ident ~loc "tr")
                  (Naming.meth_of_constr (String.uppercase tdecl.ptype_name.txt)))
              (List.map ~f:(Exp.ident ~loc) ["inh"; "subj"])
        | _ -> helper t
        in
        toplevel typ
      )
    ~onvariant

(* create opened renaming for polymorphic variant *)
(* seems that we don't need it no more *)
let make_heading_gen ~loc wrap tdecl = []


let collect_plugins_str ~loc tdecl all_tdecls plugins : Str.t list =
  let wrap p tdecl =
    p#eta_and_exp
      ~center:(Exp.sprintf ~loc "%s_%s" p#trait_name tdecl.ptype_name.txt)
      tdecl
  in
  let plugin_fields =
    List.map plugins ~f:(fun p ->
        Cf.method_concrete ~loc p#trait_name @@
        if p#need_inh_attr
        then
          Exp.sprintf ~loc "%s" @@
          Naming.trf_function p#trait_name tdecl.ptype_name.txt
        else wrap p tdecl
      )
  in

  let tname = tdecl.ptype_name.txt in

  (* The pack itself *)
  let gcata_ident = Exp.sprintf ~loc "gcata_%s" tname in
  (Str.single_value ~loc (Pat.sprintf ~loc "%s" tname) @@
  Exp.record ~loc
    [ Ldot (lident "GT", "gcata"), gcata_ident
    ; Ldot (lident "GT", "fix"),
      if List.length all_tdecls > 1
      then Exp.sprintf ~loc "%s" @@ Naming.make_fix_name all_tdecls
      else
        Exp.fun_ ~loc (Pat.var ~loc "eta") @@
        Exp.app_list ~loc
          (Exp.of_longident ~loc (Ldot (Lident "GT", "transform_gc")))
          [ gcata_ident; Exp.sprintf ~loc "eta"]
    ; Ldot (lident "GT", "plugins"), Exp.object_ ~loc @@ class_structure
        ~self:(Pat.any ~loc) ~fields:plugin_fields
    ])
  :: (List.filter_map plugins ~f:(fun p ->
      (* also we generate transformation function with unit preapplied
         Because we seems to need them in case of abstract type in the interface
      *)
      if p#need_inh_attr then None
      else
        let fname = Naming.trf_function p#trait_name tdecl.ptype_name.txt in
        Option.some @@
        Str.single_value ~loc
          (Pat.sprintf ~loc "%s" fname)
          (wrap p tdecl)
    ))

let rename_params tdecl =
  let loc = tdecl.ptype_loc in
  visit_typedecl ~loc tdecl
    ~onmanifest:(fun typ ->
        let names = (map_type_param_names tdecl.ptype_params ~f:id) in
        let (r_names,new_manifest) =
          List.fold_left ~init:([],typ) names
            ~f:(fun (ns,acc) name ->
                if String.equal name Naming.self_typ_param_name
                then
                  let n_new = Naming.self_typ_param_name ^ "__new" in
                  let t2 = map_core_type acc ~onvar:(fun s ->
                      (* Caml.Printf.printf "cmp '%s' and '%s' = %b\n%!" s name (String.equal s name ); *)
                      if String.equal s name then Some(ptyp_var ~loc n_new)
                      else None)
                  in
                  (n_new::ns, t2)
                else (name::ns,acc)
              )
        in
        { tdecl with ptype_params = List.map2_exn tdecl.ptype_params
                         (List.rev r_names)
                         ~f:(fun (_,v) s -> (ptyp_var ~loc s, v))
                     ; ptype_manifest = Some new_manifest
        }
      )
    ~onvariant:(fun _ -> tdecl)
    ~onabstract:(fun _ -> tdecl)
    ~onrecord:(fun _ -> tdecl)
    (* TODO: Implement general case about renaming of paramters *)

module G = Graph.Persistent.Digraph.Concrete(String)
module T = Graph.Topological.Make(G)
module SM = Caml.Map.Make(String)

let topsort_tdecls tdecls =
  (* TODO: we need topological sorting because in case
   *   type y = int x
   *   type 'a x = ....
   * we need to declare class for x before class for y
   * due to inheritance
  *)
  let name_map =
    List.fold_left ~init:SM.empty tdecls
      ~f:(fun acc tdecl ->
          match tdecl with
          | {ptype_name} -> SM.add ptype_name.txt tdecl acc
        )
  in
  let g = List.fold_left ~init:G.empty tdecls
      ~f:(fun acc tdecl ->
          let acc = G.add_vertex acc tdecl.ptype_name.txt in
          let info = visit_typedecl ~loc:tdecl.ptype_loc tdecl
            ~onrecord:(fun _ -> None)
            ~onvariant:(fun _ -> None)
            ~onabstract:(fun _ -> None)
            ~onopen:(fun _ -> None)
            ~onmanifest:(fun typ ->
                match typ.ptyp_desc with
                | Ptyp_constr ({txt=Lident s},_) -> Some s
                | _ -> None
              )
          in
          match info with
          | None -> acc
          | Some s -> begin
              match SM.find s name_map with
              | exception Caml.Not_found -> acc
              |  _ -> G.add_edge acc s tdecl.ptype_name.txt
            end
        )
  in
  let tdecls_new =
    T.fold (fun s acc -> (SM.find s name_map) :: acc) g []
    |> List.rev
  in
  assert (List.length tdecls = List.length tdecls_new);
  tdecls_new


(* for structures *)
let do_typ ~loc sis plugins is_rec tdecl =
  let (_:bool) = is_rec in
  let tdecl = rename_params tdecl in
  let intf_class = Str.of_class_declarations ~loc [make_interface_class ~loc tdecl] in
  let gcata = make_gcata_str ~loc tdecl in

  let plugins = List.map plugins ~f:(fun p -> p [tdecl]) in
  List.concat
    [ sis
    ; [intf_class; gcata]
    (* ; indexes_str ~loc plugins [tdecl] *)
    ; List.concat_map plugins ~f:(fun g -> g#do_single ~loc ~is_rec tdecl)
    ; collect_plugins_str ~loc tdecl [tdecl] plugins
    ]

let fix_typ ~loc tdecls =
  let idx = ref (0) in
  let next () = Int.incr idx; !idx in

  let arr3 a b c = Typ.arrow ~loc  a (Typ.arrow ~loc b c) in
  let tup ~loc  xs =
    match xs with
    | [] -> failwith "bad arguemnt"
    | [x] -> x
    | xs -> Typ.tuple ~loc xs
  in

  let ys = List.map tdecls ~f:(fun tdecl ->
      let ps = List.map tdecl.ptype_params ~f:(fun _  -> sprintf "a%d" (next ())) in
      let subj_t =
        Typ.constr ~loc (Lident tdecl.ptype_name.txt) @@
        (List.map ps ~f:(Typ.var ~loc))
      in
      let inhs = List.map ps ~f:(sprintf "%s_i") in
      let syns = List.map ps ~f:(sprintf "%s_s") in

      let main_inh = Typ.var ~loc @@ sprintf "inh%d" (next ()) in
      let main_syn = Typ.var ~loc @@ sprintf "syn%d" (next ()) in

      let cls =
        let args =
          List.map3_exn inhs ps syns ~f:(fun i p s -> [i;p;s])
          |> List.concat
          |> List.map ~f:(Typ.var ~loc)
          |> (fun xs -> xs @ [ main_inh
                             ; (if is_polyvariant_tdecl tdecl
                                then openize_poly ~loc else id)
                                 subj_t
                             ; main_syn])
        in
        Typ.class_ ~loc (Lident (Naming.class_name_for_typ tdecl.ptype_name.txt))
          args
      in
      let trf = arr3 main_inh subj_t main_syn in
      (trf, cls)
    )
  in

  let mutuals = tup ~loc @@  List.map ys ~f:fst in
  List.fold_right
    ~init:(tup ~loc @@ List.map ys ~f:fst)
    ys
    ~f:(fun (_trf, cls) acc ->
        Typ.arrow ~loc
          (Typ.arrow ~loc mutuals cls)
          acc
      )


let fix_sig ~loc tdecls =
  Sig.value ~loc
    ~name:(sprintf "%s" @@ Naming.make_fix_name tdecls)
    (fix_typ ~loc tdecls)

let fix_str ~loc tdecls =
  value_binding ~loc
    ~pat:(Pat.sprintf ~loc "%s" @@ Naming.make_fix_name tdecls)
    ~expr:Exp.(
        fun_list ~loc (List.map tdecls ~f:(fun {ptype_name} ->
            Pat.sprintf ~loc "%s0" ptype_name.txt
          )) @@

        Exp.let_ ~loc ~rec_:true
          (List.map tdecls ~f:(fun tdecl ->
               let e k = Exp.fun_list ~loc
                   (map_type_param_names tdecl.ptype_params
                      ~f:(fun txt -> Pat.sprintf ~loc "f%s" txt))
                   k
               in
               let inhsubj k =
                 Exp.fun_list ~loc
                   [ Pat.var ~loc "inh";  Pat.var ~loc "subj"]
                   k
               in
               let gc  =
                 Exp.app_list ~loc
                   (Exp.sprintf ~loc "gcata_%s" tdecl.ptype_name.txt)
                   [ Exp.app_list ~loc
                       (Exp.sprintf ~loc "%s0" tdecl.ptype_name.txt)
                       ((Exp.tuple ~loc @@
                         List.map tdecls ~f:(fun {ptype_name={txt}} ->
                             Exp.sprintf ~loc "trait%s" txt
                           ))
                        ::
                        (map_type_param_names tdecl.ptype_params
                           ~f:(fun txt -> Exp.sprintf ~loc "f%s" txt))
                        (* @
                         * [Exp.app_list ~loc
                         *    (Exp.sprintf ~loc "trait%s" tdecl.ptype_name.txt)
                         *    (map_type_param_names tdecl.ptype_params
                         *       ~f:(fun txt -> Exp.sprintf ~loc "f%s" txt))
                         * ] *)
                       )
                   ; Exp.ident ~loc "inh"
                   ; Exp.ident ~loc "subj"
                   ]

               in
               (Pat.sprintf ~loc "trait%s" tdecl.ptype_name.txt,
                e @@ inhsubj @@ gc)
             )) @@
        Exp.tuple ~loc
          (List.map tdecls ~f:(fun {ptype_name} ->
               Exp.sprintf ~loc "trait%s" ptype_name.txt
             ))
      )
  |> Str.of_vb ~loc ~rec_flag:Nonrecursive |> List.return

let collect_plugins_sig ~loc tdecls plugins =
  List.concat_map tdecls ~f:(fun tdecl ->
      let wrap () =
        [Sig.value ~loc ~name:tdecl.ptype_name.txt @@
         Typ.constr ~loc (Ldot (lident "GT", "t"))
           [ make_gcata_typ ~loc tdecl
           ; Typ.object_ ~loc Closed @@ List.map plugins ~f:(fun p ->
                 (p#trait_name, p#make_final_trans_function_typ ~loc tdecl)
               )
           (* ; make_gcata_typ ~loc tdecl *)
           ; fix_typ ~loc tdecls
           ]]
      in
      visit_typedecl ~loc tdecl
        ~onabstract:(fun _ -> [] )
        ~onmanifest:(fun _ -> wrap ())
        ~onvariant:(fun _ -> wrap ())
        ~onrecord:(fun _ -> wrap ())
    )

let do_mutual_types ~loc sis plugins tdecls =
  let tdecls = topsort_tdecls tdecls in
  let classes, catas =
    let all =
      List.map tdecls ~f:(fun tdecl ->
        (make_interface_class ~loc tdecl, make_gcata_str ~loc tdecl)
      )
    in
    (List.map ~f:fst all, List.map ~f:snd all)
  in

  let plugins = List.map plugins ~f:(fun p -> p tdecls) in
  List.concat
    [ sis
    ; List.map classes ~f:(fun c -> Str.of_class_declarations ~loc [c])
    ; catas
    ; fix_str ~loc tdecls
    ; List.concat_map plugins ~f:(fun g -> g#do_mutuals ~loc ~is_rec:true tdecls)
    ; List.concat_map tdecls ~f:(fun tdecl -> collect_plugins_str ~loc tdecl tdecls plugins)
    ]

(* for signatures *)
let do_typ_sig ~loc sis plugins is_rec tdecl =
  let plugins = List.map plugins ~f:(fun p -> p [tdecl]) in
  let intf_class = make_interface_class_sig ~loc tdecl in
  let gcata = make_gcata_sig ~loc tdecl in

  (* Pprintast.signature_item Format.std_formatter @@
   *   psig_type ~loc:tdecl.ptype_loc Nonrecursive [tdecl]; *)
  List.concat
    [ sis
    ; intf_class
    ; gcata
    ; List.concat_map plugins ~f:(fun g -> g#do_single_sig ~loc ~is_rec tdecl)
    ; collect_plugins_sig ~loc [tdecl] plugins
    ]

let do_mutual_types_sig ~loc sis plugins tdecls =
  (* TODO: it could be a bug with topological sorting here *)
  sis @
  List.concat_map tdecls ~f:(fun tdecl ->
      List.concat
        [ make_interface_class_sig ~loc tdecl
        ; make_gcata_sig ~loc tdecl
        ]
    ) @
  [ fix_sig ~loc tdecls ] @
  List.concat_map plugins ~f:(fun p -> (p tdecls)#do_mutuals_sigs ~loc ~is_rec:true) @
  (* (List.concat_map tdecls ~f:(fun tdecl ->
   *      List.concat_map plugins ~f:(fun p ->
   *          collect_plugins_sig ~loc tdecl (p tdecls))
   *    )
   * ) @ *)
  (collect_plugins_sig ~loc tdecls (List.map plugins ~f:(fun p -> p tdecls))) @
  []
  (* TODO: collect plugins for mutual types *)
  (* List.concat_map ~f:(do_typ_sig ~loc [] plugins true) tdecls *)

let wrap_plugin name = function
  | Skip -> id
  | Use args ->
    match List.Assoc.find !registered_plugins name ~equal:String.equal with
      | Some m ->
        let module F = (val m : Plugin_intf.MAKE) in
        let module P = F(AstHelpers) in
        List.cons @@ P.create args
      | None -> failwithf "Plugin '%s' is not registered" name ()


(* let sig_type_decl ~loc ~path si
 *     ?(use_show=skip) ?(use_gmap=skip) ?(use_foldl=skip) ?(use_show_type=skip)
 *     ?(use_compare=skip) ?(use_eq=skip)
 *     (rec_flag, tdls) =
 *   let plugins =
 *     wrap_plugin "show"        use_show  @@
 *     wrap_plugin "compare"     use_compare @@
 *     wrap_plugin "gmap"        use_gmap  @@
 *     wrap_plugin "foldl"       use_foldl @@
 *     wrap_plugin "show_typed"  use_show_type @@
 *     wrap_plugin "eq"          use_eq @@
 *     []
 *   in
 *   match rec_flag, tdls with
 *   | recursive, []      -> []
 *   | recursive, [tdecl] -> do_typ_sig si ~loc plugins true tdecl
 *   | recursive, ts      -> do_mutal_types_sig ~loc plugins ts
 *   | nonrecursive, tdls ->
 *       list.concat_map ~f:(do_typ_sig ~loc si plugins false) tdls *)

let sig_type_decl_many_plugins ~loc si plugins_info declaration =
  let plugins =
    List.fold_left plugins_info ~init:[]
      ~f:(fun acc (name,args) ->
          wrap_plugin name args acc
        )
  in
  let declaration =
    (fst declaration,  List.map ~f:rename_params (snd declaration))
  in

  match declaration with
  | Recursive, []      -> []
  | Recursive, [tdecl] -> do_typ_sig si ~loc plugins true tdecl
  | Recursive, ts      ->
    (* Stdio.printf "Got %d declarations\n%!" (List.length ts); *)
    do_mutual_types_sig ~loc si plugins ts
  | Nonrecursive, tdls ->
      List.concat_map ~f:(do_typ_sig ~loc si plugins false) tdls

let str_type_decl_many_plugins ~loc si plugins_info declaration =
  let plugins =
    List.fold_left plugins_info ~init:[]
      ~f:(fun acc (name,args) ->
          wrap_plugin name args acc
        )
  in
  match declaration with
  | Recursive, []      -> []
  | Recursive, [tdecl] -> do_typ         ~loc si plugins true tdecl
  | Recursive, ts      -> do_mutual_types ~loc si plugins ts
  | Nonrecursive, decls ->
      List.concat_map ~f:(do_typ ~loc si plugins false) decls

let str_type_ext_many_plugins ~loc si plugins_info extension =
  []
end


(* part of old implementation where we are trying to collect all values in t
 * first-class module. but we decided to roll back because we can't write
 * generic function to access it *)
let name_fcm_mt tdecl = sprintf "mt_%s" tdecl.ptype_name.txt
(* let gather_module_str tdecl plugins =
 *   let loc = tdecl.ptype_loc in
 *
 *   let body = [%stri let gcata =
 *                       [%e exp.sprintf "gcata_%s" tdecl.ptype_name.txt] ] ::[]
 *   in
 *   let body = list.fold_left ~init:body plugins
 *       ~f:(fun acc p ->
 *         let expr = exp.sprintf ~loc "%s" @@ p#make_trans_function_name tdecl in
 *         str.single_value ~loc (pat.of_string ~loc p#plugin_name) expr
 *         :: acc
 *       )
 *   in
 *   let expr = exp.pack_with_constraint ~loc
 *       (mod.structure ~loc @@ list.rev body)
 *       (located.lident ~loc (name_fcm_mt tdecl))
 *   in
 *   str.single_value ~loc (pat.sprintf "%s" tdecl.ptype_name.txt) expr *)

(* let make_fcm_sig ~loc tdecl plugins =
 *   let fields = list.concat_map plugins ~f:(fun p ->
 *       let name  = p#plugin_name in
 *       let type_ = p#make_trans_function_typ tdecl in
 *       [sig.value ~loc ~name type_ ]
 *     )
 *   in
 *   mty.signature ~loc ((make_gcata_sig ~shortname:true ~loc tdecl) :: fields )
 *
 * let prepare_mt ~loc tdecl plugins =
 *     let name = located.mk ~loc @@ sprintf "mt_%s" tdecl.ptype_name.txt in
 *     let type_ = some (make_fcm_sig ~loc tdecl plugins) in
 *     str.modtype ~loc (module_type_declaration ~loc ~name ~type_) *)

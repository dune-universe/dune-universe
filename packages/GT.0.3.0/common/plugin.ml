(*
 * Generic Transformers PPX syntax extension.
 * Copyright (C) 2016-2019
 *   Dmitrii Kosarev aka Kakadu
 * St.Petersburg State University, JetBrains Research
 *)

(** A few base classes for plugins with virtual methods to be implemented.

    See {!Plugin_intf} for complete description of all valuable methods
  *)

open Base
open Ppxlib
open Printf
open Asttypes
open HelpersBase

module Make(AstHelpers : GTHELPERS_sig.S) = struct

open AstHelpers
module Intf = Plugin_intf.Make(AstHelpers)

type plugin_constructor =
  Plugin_intf.plugin_args -> Ppxlib.type_declaration list ->
    (loc, Exp.t, Typ.t, type_arg, Ctf.t, Cf.t, Str.t, Sig.t) Plugin_intf.typ_g

let prepare_patt_match_poly ~loc what rows labels ~onrow ~onlabel ~oninherit =
  let rs =
    List.map rows ~f:(function
        | Rtag (lab, _, args) ->
          let args = match args with
            | [t] -> unfold_tuple t
            | [] -> []
            | _ -> failwith "we don't support conjunction types"
          in
          let names = List.map args ~f:(fun _ -> gen_symbol ~prefix:"_" ()) in
          let lhs = Pat.variant ~loc lab.txt @@ List.map ~f:(Pat.var ~loc) names in
          case ~lhs ~rhs:(onrow lab @@ List.zip_exn names args)
        | Rinherit typ ->
          match typ.ptyp_desc with
          | Ptyp_constr({txt;_},ts) ->
            let newname = "subj" in
            let lhs = Pat.alias ~loc (Pat.type_ ~loc txt) newname
            in
            case ~lhs ~rhs:(oninherit ts txt newname)
          | _ -> failwith "this inherit field isn't supported"

      )
  in
  let for_labels = match labels with
    | None -> []
    | Some ls -> List.map ls ~f:(fun lab ->
        let newname = "subj" in
        let lhs = Pat.alias ~loc (Pat.type_ ~loc (Lident lab) ) newname
        in
        case ~lhs ~rhs:(onlabel lab newname)
      )
  in
  Exp.match_ ~loc what (rs @ for_labels)


(** Base class for all plugins. Implements {!Plugin_intf.typ_g} interface

    Is subclassed by {!with_inherited_attr} and {!no_inherite_arg}. Use them for
    convenience.
*)
class virtual generator initial_args tdecls = object(self: 'self)
  inherit Intf.g

  method tdecls = tdecls

  method plugin_name = self#trait_name
  (* parse arguments like { _1=<expr>; ...; _N=<expr>; ...} *)
  val reinterpreted_args =
    let check_name s =
      try Caml.Scanf.sscanf s "_%d" Option.some
      with Caml.Scanf.Scan_failure _ ->  None
    in
    List.fold_left initial_args ~init:[]
      ~f:(fun acc (lident,expr) ->
          match lident with
          | Lident s -> Option.value_map (check_name s) ~default:acc
                          ~f:(fun n -> (n,expr) :: acc)
          | _ -> acc
        )

  method show_args =
    (* Stdio.printf "showing %d args\n%!" (List.length reinterpreted_args); *)
    List.iter reinterpreted_args ~f:(fun (k,e) ->
        Format.printf "%d -> %a\n%!" k Pprintast.expression e
      )

  method extra_class_sig_members tdecl =
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
    ]

  method extra_class_str_members tdecl =
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
    ]

  method cur_name tdecl = tdecl.ptype_name.txt

  (* preparing class of transformation for [tdecl] *)
  method make_class ~loc ~is_rec tdecl =
    let cur_name = self#cur_name tdecl in
    let mutual_decls =
      List.filter self#tdecls ~f:(fun {ptype_name={txt}} ->
          String.(<>) txt tdecl.ptype_name.txt
        )
    in
    let k fields =
      let inh_params =
        prepare_param_triples ~loc
          ~inh:(self#inh_of_param tdecl)
          ~syn:self#syn_of_param
          ~default_syn:(self#syn_of_main ~loc ~in_class:true tdecl)
          ~default_inh:(self#inh_of_main ~loc tdecl)
          ~extra:(Typ.var ~loc @@
                  sprintf "%s_%s" Naming.extra_param_name tdecl.ptype_name.txt)
          (map_type_param_names tdecl.ptype_params ~f:id)
      in
      self#wrap_class_definition ~loc mutual_decls tdecl ~inh_params fields
    in

    let is_self_rec t =
      if not is_rec then `Nonrecursive else
      match t.ptyp_desc with
      | Ptyp_constr ({txt=Lident s}, params)
        when String.equal s cur_name &&
             List.length params = List.length tdecl.ptype_params ->
        let wrap old next = match old,next with
          | `Nonregular,_ -> `Nonregular
          | _,`Nonregular -> `Nonregular
          | `Regular,`Regular -> `Regular
        in
        ((List.fold2_exn params tdecl.ptype_params
          ~init:`Regular
          ~f:(fun acc inst_par (formal_par,_) ->
              match inst_par.ptyp_desc, formal_par.ptyp_desc with
              | (Ptyp_var s1, Ptyp_var s2) when String.equal s1 s2 -> wrap acc `Regular
              | (_, Ptyp_var _) -> `Nonregular
              | (_, Ptyp_any) -> `Nonregular (* TODO: think again about this *)
              | _ when 0=compare_core_type inst_par formal_par -> wrap acc `Regular
              | _ -> `Nonregular
            ))
          :> [ `Regular | `Nonregular | `Nonrecursive])
      | _ -> `Nonrecursive
    in
    self#got_typedecl ~loc ~is_self_rec ~mutual_decls
      tdecl k

  method prepare_fa_args ~loc tdecl =
    map_type_param_names tdecl.ptype_params ~f:(Pat.sprintf ~loc "f%s")

  method wrap_class_definition ~loc ~inh_params mutual_decls tdecl fields =
    let cur_name = self#cur_name tdecl in
    (* inherit class_t and prepare to put other members *)

    let mutual_decls = self#tdecls in
    let is_mutal = (List.length mutual_decls > 1) in
    Str.class_single ~loc
      ~params:(self#plugin_class_params_tdecl tdecl)
      ~name:(self#make_class_name ~is_mutal tdecl)
      ~virt:false
      ~wrap:(fun body ->
          (* constructor arguments are *)
          let names =
            (if is_mutal then []
             else [Pat.var ~loc @@ self#self_arg_name tdecl.ptype_name.txt ])
            |> (fun tl -> self#prepare_fa_args ~loc tdecl @ tl )
            |> (fun ps ->
                match mutual_decls with
                | [] -> failwith "Should not happen"
                | [_] -> ps
                | tdecls ->
                  (* we don't need self transformation for *)
                  (Pat.alias ~loc
                     (Pat.tuple ~loc @@
                      List.map self#tdecls ~f:(fun {ptype_name={txt=name}} ->
                          Pat.var ~loc @@
                          if String.equal name tdecl.ptype_name.txt
                          then sprintf "fself_%s" name
                          else Naming.for_ self#trait_name name
                        ))
                     Naming.mutuals_pack
                  ) :: ps
              )
          in
          Cl.fun_list ~loc names body
      )
      @@
      [ let parent_name = Naming.class_name_for_typ cur_name in
        Cf.inherit_ ~loc (Cl.constr ~loc (Lident parent_name) inh_params)
      ] @ (self#extra_class_str_members tdecl) @ fields


  method virtual make_typ_of_class_argument: 'a . loc:loc -> type_declaration ->
    (Typ.t -> 'a -> 'a) ->
    string -> (('a -> 'a) -> 'a -> 'a) -> 'a -> 'a

  (* next method should be synchronized with prepare_fa_args *)
  (* method prepare_fa_arg_types ~loc tdecl =
   *   let names = map_type_param_names tdecl.ptype_params ~f:id  in
   *   List.map names
   *     ~f:(fun name ->
   *         self#make_typ_of_class_argument
   *           ~loc
   *           tdecl
   *           name
   *           (fun x -> x)
   *       ) *)

  method class_constructor_sig ~loc ?(a_stub=false) tdecl : Typ.t =
    let tl =
      Typ.arrow ~loc
        (self#make_typ_of_self_trf ~loc ~in_class:true tdecl) @@
      Typ.constr ~loc
        (Lident (self#make_class_name ~is_mutal:a_stub tdecl))
        (List.map (self#plugin_class_params_tdecl tdecl) ~f:(Typ.of_type_arg ~loc))
    in
    let funcs_for_args =
      let names = map_type_param_names tdecl.ptype_params ~f:id in
      List.fold_left names
        ~init:id
        ~f:(fun acc name ->
            self#make_typ_of_class_argument ~loc tdecl (Typ.arrow ~loc) name
              (fun f arg -> acc @@ f arg)
          )
        tl
    in
    let ans =
      if not a_stub
      then funcs_for_args
      else
        Typ.arrow ~loc
          (Typ.tuple ~loc @@ List.map self#tdecls ~f:(fun tdecl ->
               self#long_trans_function_typ ~loc tdecl
               (* TODO: rename *)
             ))
          tl
    in
    ans

  (* signature for a plugin class *)
  method make_class_sig ~loc ?(a_stub=false) ~is_rec tdecl =
    let k fields =
      [ Sig.class_ ~loc
          ~params:(self#plugin_class_params_tdecl tdecl)
          ~name:(self#make_class_name ~is_mutal:a_stub tdecl)
          ~virt:false
          ~wrap:(fun sign ->
              let funcs_for_args =
                let names = map_type_param_names tdecl.ptype_params ~f:id in
                List.fold_left names
                  ~init:id
                  ~f:(fun acc name ->
                      self#make_typ_of_class_argument ~loc tdecl (Cty.arrow ~loc) name
                        (fun f arg -> acc @@ f arg)
                    )
                  (if a_stub then sign
                   else
                     let for_self = self#make_typ_of_self_trf ~loc ~in_class:true tdecl in
                     Cty.arrow ~loc for_self sign)
              in

              funcs_for_args
              |> (fun tl ->
                  if not a_stub then tl
                  else
                    Cty.arrow ~loc
                      (Typ.tuple ~loc @@ List.map self#tdecls ~f:(fun tdecl ->
                           self#long_trans_function_typ ~loc tdecl
                             (* TODO: rename *)
                         ))
                      tl
                )
            )
          ((self#extra_class_sig_members tdecl) @ fields)
      ]
    in
    let on_constructor pcd_args pcd_name =
      let methname = Naming.meth_of_constr pcd_name.txt in
      let typs = match pcd_args with
        | Pcstr_record ls -> List.map ls ~f:(fun x -> x.pld_type)
        | Pcstr_tuple ts -> ts
      in
      let new_ts =
        let open Typ in
        [ self#inh_of_main ~loc tdecl
        ; var ~loc @@ Printf.sprintf "extra_%s" tdecl.ptype_name.txt ] @
        (List.map typs ~f:Typ.from_caml) @
        [ self#syn_of_main ~loc ~in_class:true tdecl ]
        (* There changing default_syn to 'extra can introduce problems *)
      in

      Ctf.method_ ~loc ~virt:false methname @@
      Typ.chain_arrow ~loc new_ts
    in

    visit_typedecl ~loc tdecl
      ~onabstract:(fun () -> [])
      ~onrecord:(fun _fields ->
          k [ Ctf.method_ ~loc (Naming.meth_name_for_record tdecl) ~virt:false @@
              Typ.chain_arrow ~loc @@
              let open Typ in
              [ self#inh_of_main ~loc tdecl
              ; var ~loc @@ Printf.sprintf "extra_%s" tdecl.ptype_name.txt ] @
              [ self#syn_of_main ~loc tdecl ]
            ]
        )
      ~onvariant:(fun cds ->
          k @@ List.map cds ~f:(fun cd -> on_constructor cd.pcd_args cd.pcd_name)
        )
      ~onmanifest:(fun typ ->
        let rec helper typ =
          match typ.ptyp_desc with
          | Ptyp_var name -> (* antiphantom types *)
            let new_lident = Ldot (Lident "GT", "free") in
            let open Ppxlib.Ast_builder.Default in
            let loc = typ.ptyp_loc in
            helper @@ ptyp_constr ~loc (Located.mk ~loc new_lident) [ptyp_var ~loc name]

          | Ptyp_alias (t, aname) ->
            let loc = t.ptyp_loc in
            map_core_type t ~onvar:(fun as_ ->
              let open Ppxlib.Ast_builder.Default in
              if String.equal as_ aname
              then Option.some @@
                ptyp_constr ~loc (Located.lident ~loc tdecl.ptype_name.txt) @@
                List.map tdecl.ptype_params ~f:(fun (t,_) -> t)
              else Option.some @@ ptyp_var ~loc as_
              ) |> helper
          | Ptyp_constr (cid, params) ->
            (* there for type 'a list = ('a,'a list) alist
             * we inherit plugin class for base type, for example (gmap):
             *  inherit ('a,'a2,'a list,'a2 list) gmap_alist
             **)
            k [Ctf.inherit_ ~loc @@ Cty.constr ~loc
                 (map_longident cid.txt
                    ~f:(Naming.trait_class_name_for_typ ~trait:self#trait_name))
                 (self#final_typ_params_for_alias ~loc tdecl params)
              ]
          | Ptyp_tuple ts ->
            (* let's say we have predefined aliases for now *)
            helper @@ constr_of_tuple ~loc:typ.ptyp_loc ts
          | Ptyp_variant (rows,_,_) ->
              let rr = List.map rows ~f:(fun rf -> match rf.prf_desc with
              | Rinherit typ ->
                  with_constr_typ typ
                    ~ok:(fun cid params ->
                        Ctf.inherit_ ~loc @@
                        Cty.constr ~loc
                          (map_longident cid.txt
                             ~f:(Naming.trait_class_name_for_typ ~trait:self#plugin_name))
                          (self#final_typ_params_for_alias ~loc tdecl params)
                     )
                     ~fail:(fun () -> assert false)
              | Rtag (lab, _, typs) -> begin
                  Ctf.method_ ~loc (sprintf "c_%s" lab.txt) ~virt:false @@
                  match typs with
                  | [] ->
                    Typ.(chain_arrow ~loc
                           [ self#inh_of_main ~loc tdecl
                           ; var ~loc @@ Printf.sprintf "extra_%s" tdecl.ptype_name.txt
                           ; self#syn_of_main ~loc ~in_class:true tdecl
                           ]
                        )
                  | [t] ->
                      Typ.(chain_arrow ~loc @@
                             [ self#inh_of_main ~loc tdecl
                             ; var ~loc @@ Printf.sprintf "extra_%s" tdecl.ptype_name.txt ] @
                             (List.map ~f:Typ.from_caml @@ unfold_tuple t) @
                             [self#syn_of_main ~loc ~in_class:true tdecl]
                          )
                  | typs ->
                      Typ.(chain_arrow ~loc @@
                             [ self#inh_of_main ~loc tdecl
                             ; var ~loc @@ Printf.sprintf "extra_%s" tdecl.ptype_name.txt ] @
                             (List.map ~f:Typ.from_caml typs) @
                             [self#syn_of_main ~loc ~in_class:true tdecl]
                          )
                end
              )
              in
              k @@ rr
        | _ -> assert false
        in
        let toplevel typ = match typ.ptyp_desc with
        | Ptyp_tuple _ ->
            let new_ts =
              let open Typ in
              [ self#inh_of_main ~loc tdecl
              ; var ~loc @@ Printf.sprintf "extra_%s" tdecl.ptype_name.txt ] @
              [ self#syn_of_main ~loc ~in_class:true tdecl ]
            in

            k @@
              [ Ctf.method_ ~loc ~virt:false
                  (Naming.meth_name_for_constructor (String.uppercase tdecl.ptype_name.txt)) @@
                  Typ.chain_arrow ~loc new_ts
              ]
        | Ptyp_var _ ->
            let open Ppxlib.Ast_builder.Default in
            k @@ [ on_constructor (Pcstr_tuple []) @@
                   Located.mk ~loc:typ.ptyp_loc @@ String.uppercase tdecl.ptype_name.txt ]
        | _ -> helper typ
        in
        toplevel typ
    )

  method make_inherit_args_for_alias ~loc ~is_self_rec tdecl do_typ cid cparams =
    let args =
      List.mapi cparams ~f:(fun i t ->
          (* Stdio.printf "checking for arg with index (%d+1)\n%!" i; *)
          match List.Assoc.find reinterpreted_args ~equal:Int.equal (i+1) with
          | Some e -> Exp.from_caml e
          | None   -> do_typ ~loc t
        )
    in
    (* for typ aliases we can cheat because first argument of constructor of type
               on rhs is self transformer function *)
    args


  (* When we got declaration of type alias via type application *)
  method got_constr ~loc ~is_self_rec ?(fix_self_app=id) tdecl mutual_decls
      do_typ cid cparams k =
    (* It seems that we can't filter mutal decls because we need to preserve an order *)
    let mutal_names = List.map mutual_decls ~f:(fun t -> t.ptype_name.txt) in
    let ans args : Cf.t list =
      [ let typ_params = self#final_typ_params_for_alias ~loc tdecl cparams in
        let args =
          (match cid.txt with
          | Lident s when List.mem mutal_names s ~equal:String.equal ->
            (* Only Lident because we ignore types with same name but from another module *)

            [Exp.sprintf ~loc "%s" Naming.mutuals_pack]
          | _ ->
            []
            (* [Exp.of_longident ~loc @@
             *  map_longident ~f:(fun for_ -> self#fix_func_name ~for_ ()) cid.txt] *)
          ) @ args
        in
        match cid.txt with
        | Lident s when List.mem mutal_names s ~equal:String.equal ->
          Cf.inherit_ ~loc @@ Cl.apply ~loc
            (Cl.constr ~loc
               (lident @@ Naming.make_stub_class_name ~plugin:self#plugin_name s)
               typ_params
            )
            [Exp.ident ~loc Naming.mutuals_pack]
        | _ ->
          Cf.inherit_ ~loc @@ Cl.apply ~loc
            (Cl.constr ~loc
               (map_longident cid.txt
                  ~f:(fun s ->
                      Naming.trait_class_name_for_typ ~trait:self#plugin_name s
                    ))
               typ_params)
            args
      ]
    in

    let class_args =
      (self#make_inherit_args_for_alias ~loc ~is_self_rec tdecl do_typ cid cparams)
      @ [fix_self_app @@ Exp.ident ~loc (self#self_arg_name tdecl.ptype_name.txt)]
    in
    k @@ ans class_args


  method got_polyvar ~loc ~is_self_rec ~mutual_decls tdecl do_typ rows k =
    List.concat_map rows ~f:(function
    | Rinherit typ ->
        with_constr_typ typ
            ~fail:(fun () -> failwith "type is not a constructor")
            ~ok:(fun cid params ->
                (* Hypothesis: it's almost a type alias *)
                self#got_constr ~loc ~is_self_rec tdecl mutual_decls do_typ cid params k
                  ~fix_self_app:(fun eself ->
                      self#abstract_trf ~loc (fun einh esubj ->
                          match typ.ptyp_desc with
                          | Ptyp_constr ({txt},_) ->
                            (* TODO: refactoring. we inented special function for this *)
                            Exp.match_ ~loc esubj
                              [case
                                 ~lhs:(Pat.alias ~loc (Pat.type_ ~loc txt) "subj")
                                 ~rhs:(self#app_transformation_expr ~loc eself einh
                                         (Exp.ident ~loc "subj"))
                              ]
                          | _ -> failwith "should not happen"
                        )
                    )
            )
    (* TODO: Do something with copy paste. *)
    (* tag by default have 1 argument which is a tuple instead of many arguments *)
    | Rtag (constr_name, _, []) ->
      k [
        let inhname = gen_symbol ~prefix:"inh_" () in
        Cf.method_concrete ~loc (Naming.meth_name_for_constructor constr_name.txt) @@
        Exp.fun_ ~loc (Pat.sprintf "%s" ~loc inhname) @@
        Exp.fun_ ~loc (Pat.any ~loc) @@
        self#on_tuple_constr ~loc ~is_self_rec ~mutual_decls ~inhe:(Exp.ident ~loc inhname)
          tdecl (Option.some @@ `Poly constr_name.txt) []
      ]
    | Rtag (constr_name, _, [arg]) ->
      k [
        let inhname = gen_symbol ~prefix:"inh_" () in
        let bindings = List.map (unfold_tuple arg) ~f:(fun ts -> gen_symbol (), ts) in
        Cf.method_concrete ~loc (Naming.meth_name_for_constructor constr_name.txt) @@
        Exp.fun_ ~loc (Pat.sprintf "%s" ~loc inhname) @@
        Exp.fun_ ~loc (Pat.any ~loc) @@
        Exp.fun_list ~loc (List.map bindings ~f:(fun (s,_) -> Pat.var ~loc s)) @@
        self#on_tuple_constr ~loc ~is_self_rec ~mutual_decls ~inhe:(Exp.ident ~loc inhname)
          tdecl (Option.some @@ `Poly constr_name.txt) bindings
      ]
    | Rtag (constr_name, _, args) ->
      (* Hypothesis: it's almost the same as constructor with a tuple of types  *)
      failwith "conjunction types are not supported but"
    )

  method got_typedecl ~loc ~is_self_rec ~mutual_decls tdecl (k: Cf.t list -> _) =
    k @@
    visit_typedecl ~loc tdecl
    ~onmanifest:(fun typ ->
        let rec helper typ =
          match typ.ptyp_desc with
          | Ptyp_var name -> (* antiphantom types *)
            let new_lident = Ldot (Lident "GT", "free") in
            let open Ppxlib.Ast_builder.Default in
            let loc = typ.ptyp_loc in
            helper @@ ptyp_constr ~loc (Located.mk ~loc new_lident) [ptyp_var ~loc name]
          | Ptyp_alias (t, aname) ->
            let open Ppxlib.Ast_builder.Default in
            let loc = tdecl.ptype_loc in
            map_core_type t ~onvar:(fun as_ ->
              if String.equal as_ aname
              then Option.some @@
                ptyp_constr ~loc:t.ptyp_loc
                  (Located.lident ~loc tdecl.ptype_name.txt)
                  (List.map tdecl.ptype_params ~f:fst)
              else Option.some @@ ptyp_var ~loc as_
              ) |> helper
          | Ptyp_constr (cid, params) ->
              self#got_constr ~loc ~is_self_rec tdecl mutual_decls
                (self#do_typ_gen ~mutual_decls ~is_self_rec tdecl)
                cid params (fun x -> x)

          | Ptyp_tuple ts ->
            (* let's say we have predefined aliases for now *)
            helper @@ constr_of_tuple ~loc:typ.ptyp_loc ts
          | Ptyp_variant (rows,_,_) ->
            self#got_polyvar ~loc tdecl (self#do_typ_gen ~mutual_decls ~is_self_rec tdecl)
              ~is_self_rec ~mutual_decls
              (List.map rows ~f:(fun {prf_desc} -> prf_desc))
              (fun x -> x)
          | Ptyp_object (_,_) -> failwith "not implemented: object types"
          | Ptyp_class (_,_) -> failwith "not implemented: class types"
          | Ptyp_package _ -> failwith "not implemented: package types"
          | Ptyp_extension _ -> failwith "not implemented: extension types"
          | Ptyp_arrow _ -> failwith "not implemented: arrow types"
          | Ptyp_any -> failwith "not implemented: wildcard types (but it should be easy to rewrite)"
          | Ptyp_poly (_,_) -> failwith "not implemented: existential types"
        in

        let toplevel typ = match typ.ptyp_desc with
        | Ptyp_var name -> (* antiphantom types *)
            (* let inhname = gen_symbol ~prefix:"inh_" () in *)
            (* let argname = gen_symbol ~prefix:"arg_" () in *)
            [ Cf.method_concrete ~loc
              (Naming.meth_name_for_constructor (String.uppercase tdecl.ptype_name.txt)) @@
              (self#do_typ_gen ~loc ~mutual_decls ~is_self_rec tdecl typ)
            ]
        | Ptyp_tuple ts ->
          begin
            let inhname = gen_symbol ~prefix:"inh_" () in
            let loc = loc_from_caml tdecl.ptype_loc in
            let bindings = List.map ts ~f:(fun ts -> gen_symbol (), ts) in
            let bind_pats = List.map bindings ~f:(fun (s,_) -> Pat.var ~loc s) in
            (* We don't need bind_pats, we cat patternmatch original value which is wildcarded for now *)
            [ Cf.method_concrete ~loc (Naming.meth_name_for_constructor (String.uppercase tdecl.ptype_name.txt)) @@
              Exp.fun_ ~loc (Pat.sprintf "%s" ~loc inhname) @@
              Exp.fun_ ~loc (Pat.tuple ~loc @@ bind_pats) @@
              self#on_tuple_constr ~loc ~mutual_decls ~is_self_rec
                ~inhe:(Exp.ident ~loc inhname)
                tdecl None bindings]
          end
        | Ptyp_object (_,_) -> failwith "not implemented: object types"
        | Ptyp_class (_,_) -> failwith "not implemented: class types"
        | Ptyp_package _ -> failwith "not implemented: package types"
        | Ptyp_extension _ -> failwith "not implemented: extension types"
        | Ptyp_arrow _ -> failwith "not implemented: arrow types"
        | Ptyp_any -> failwith "not implemented: wildcard types (but it should be easy to rewrite)"
        | Ptyp_poly (_,_) -> failwith "not implemented: existential types"
        | _ -> helper typ
        in
        toplevel typ
    )
    ~onvariant:(fun cds -> self#on_variant ~loc ~mutual_decls ~is_self_rec tdecl cds id)
    ~onrecord:(self#on_record_declaration ~loc ~is_self_rec ~mutual_decls tdecl)
    ~onopen:(fun () -> [])

  method virtual on_record_declaration: loc:loc ->
    is_self_rec:(core_type -> [ `Nonrecursive | `Nonregular | `Regular ]) ->
    mutual_decls:(type_declaration list) ->
    type_declaration ->
    label_declaration list ->
    Cf.t list

  method make_typ_of_mutal_trf ~loc mutal_tdecl (k: Typ.t -> _) : Typ.t =
    let subj_t = Typ.use_tdecl mutal_tdecl in
    k Typ.(arrow ~loc subj_t (self#syn_of_main ~loc mutal_tdecl))

    (* k @@ Typ.from_caml [%type: ([%t subj_t] -> [%t self#syn_of_main ~loc mutal_tdecl]) ] *)


  (* val name : <typeof fa> -> ... -> <typeof fz> ->
                     <this type we are generating here>
  *)

  (* method make_RHS_typ_of_transformation ~loc ?subj_t ?syn_t tdecl =
   *   let subj_t = Option.value subj_t
   *       ~default:(Typ.use_tdecl tdecl) in
   *   let syn_t  = Option.value syn_t ~default:(self#syn_of_main ~loc tdecl) in
   *   Typ.arrow ~loc subj_t syn_t *)

  (* method chain_inh_syn ~loc ~inh_t ~syn_t subj_t =
   *   [%type: [%t inh_t] -> [%t subj_t] -> [%t syn_t] ] *)

  method wrap_tr_function_typ (typ: core_type) = typ

  method virtual long_trans_function_typ: loc:loc -> type_declaration -> Typ.t

  method make_trans_function_typ ~loc tdecl =
    let type_ = self#make_RHS_typ_of_transformation ~loc tdecl in
    let names = map_type_param_names tdecl.ptype_params ~f:id in
    List.fold_left names
      ~init:id
      ~f:(fun acc name ->
          self#make_typ_of_class_argument ~loc tdecl (Typ.arrow ~loc) name
            (fun f arg -> acc @@ f arg)
        )
      type_

  method make_trans_function_name tdecl =
    sprintf "%s_%s" self#plugin_name tdecl.ptype_name.txt

  (* method make_trf_init_typ ~loc tdecl =
   *   Typ.arrow ~loc (Typ.access2 ~loc (sprintf "For_%s" self#trait_name) "fn") @@
   *   self#make_trans_function_typ ~loc tdecl *)

  (* generate obly for this type *)
  method make_trans_functions_sig: loc:loc ->
    is_rec:bool -> type_declaration  -> Sig.t list
    = fun ~loc ~is_rec tdecl ->
      (* we skip initial functions in the interface *)
      List.concat
        [ [Sig.value ~loc
             ~name:(Naming.trf_function self#trait_name tdecl.ptype_name.txt)
             (self#make_final_trans_function_typ ~loc tdecl)]
        ]

  method make_class_name ?(is_mutal=false) tdecl =
    sprintf "%s%s"
      (Naming.trait_class_name_for_typ ~trait:self#plugin_name
         tdecl.ptype_name.txt)
      (if is_mutal then "_stub" else "")

  method apply_fas_in_new_object ~loc tdecl =
    (* very similar to self#make_inherit_args_for_alias but the latter
     * applies `fself` by default. Need to refactor and remove this function *)
    map_type_param_names tdecl.ptype_params ~f:(Exp.sprintf ~loc "f%s")

  (* only for non-recursive types *)
  method virtual make_trans_function_body: loc:loc -> ?rec_typenames: string list ->
    string -> type_declaration -> Exp.t

  method is_combinatorial tdecl =
    (* We are allowed to use combinatorial interface for types which are type abbreviations *)
    (* let cmb_attr = List.find tdecl.ptype_attributes
          ~f:(fun {attr_name={txt}} -> String.equal txt "combinatorial")
    in *)
    if (* Option.is_some cmb_attr &&*) Caml.(=) tdecl.ptype_kind Ptype_abstract
      && not (is_polyvariant_tdecl tdecl) && not (is_tuple_tdecl tdecl)
    then match tdecl.ptype_manifest with
          | Some t -> Some t
          | None -> None
    else None

  method make_trans_functions : loc:loc -> is_rec:bool -> Str.t list
    = fun ~loc ~is_rec ->
      (* we will generate mutally recursive showers here

         (* n functions like *)
         let show0_typ1 = ...

         let show_typ1 = #5 (fix ....)
      *)
      let mutal_names = List.map self#tdecls ~f:(fun {ptype_name={txt}} -> txt) in
      let intials =
        if List.length self#tdecls > 1
        then
          List.map self#tdecls ~f:(fun tdecl ->
              value_binding ~loc
                ~pat:(Pat.sprintf ~loc "%s" @@
                      Naming.init_trf_function self#trait_name tdecl.ptype_name.txt)
                ~expr:(
                  let class_name = self#make_class_name
                      ~is_mutal:(not (List.is_empty mutal_names))
                      tdecl
                  in
                  Exp.new_ ~loc (Lident class_name)
                )
            )
        else []
      in

      let knots =
        match self#tdecls with
        | [] -> []
        | [tdecl] ->
          [ value_binding ~loc
              ~pat:(Pat.sprintf ~loc "%s" @@
                    Naming.trf_function self#trait_name tdecl.ptype_name.txt)
              ~expr:(
                Exp.fun_list ~loc
                      (map_type_param_names tdecl.ptype_params
                        ~f:(fun txt -> Pat.sprintf ~loc "f%s" txt))
                @@
                match self#is_combinatorial tdecl with
                | Some typ ->
                    self#do_typ_gen ~loc ~mutual_decls:[tdecl] ~is_self_rec:(fun _ -> `Nonrecursive) tdecl typ
                | None ->
                    self#make_trans_function_body ~loc
                      (self#make_class_name ~is_mutal:false tdecl)
                      tdecl
              )]
        | tdecls ->
          List.mapi tdecls ~f:(fun n {ptype_name={txt}} ->
              value_binding ~loc
                ~pat:(Pat.sprintf ~loc "%s" @@ Naming.trf_function self#trait_name txt)
                ~expr:(
                  Exp.fun_ ~loc (Pat.var ~loc "eta") @@
                  Exp.let_ ~loc ~rec_:false
                    [ Pat.tuple ~loc @@ List.mapi tdecls ~f:(fun i _ ->
                          if i=n then Pat.var ~loc "f"
                          else Pat.any ~loc
                        )
                    , Exp.app_list ~loc
                          (Exp.ident ~loc @@ Naming.make_fix_name tdecls)
                          (List.map tdecls ~f:(fun {ptype_name={txt}} ->
                               Exp.sprintf ~loc "%s" @@
                               Naming.init_trf_function self#trait_name txt
                             ))
                    ]
                    Exp.(app ~loc (sprintf ~loc "f") (sprintf ~loc "eta"))
                )
            )
      in
      (* TODO: insert Recursive only if type is recursive *)
      List.map ~f:(Str.of_vb ~loc ~rec_flag:Recursive) (intials @ knots)

  method fix_func_name ?for_ () =
    match for_ with
    | None -> Naming.fix_func_name_tdecls self#plugin_name self#tdecls
    | Some for_ -> Naming.fix_func_name ~for_ self#plugin_name

  method do_single_sig ~loc ~is_rec tdecl =
    List.concat
      [ self#make_class_sig ~loc ~is_rec tdecl
      (* Need to fix drawing a signature by specializing for show|gmap case *)
      ; self#make_trans_functions_sig ~loc ~is_rec tdecl
      ]

  method do_single ~loc ~is_rec tdecl =
    List.concat
      [ [ ((self#make_class ~loc ~is_rec tdecl) : Str.t ) ]
      ; self#make_trans_functions ~loc ~is_rec
      ]

  method final_typ_params_for_alias ~loc tdecl rhs =
    self#alias_inherit_type_params ~loc tdecl rhs @
    []

  method alias_inherit_type_params ~loc tdecl rhs_args =
    self#plugin_class_params ~loc rhs_args ~typname:tdecl.ptype_name.txt

  method do_mutuals_sigs ~loc ~is_rec =
    List.concat
      [ List.concat_map self#tdecls ~f:(fun tdecl ->
            List.concat
              [ self#make_class_sig ~loc ~is_rec ~a_stub:true tdecl
              ; self#make_class_sig ~loc ~is_rec tdecl
              ]
          )
      ; List.concat_map self#tdecls ~f:(fun tdecl ->
            (* Need to fix drawing a signature by specializing for show|gmap case *)
            self#make_trans_functions_sig ~loc ~is_rec tdecl
          )
      ]

  method do_mutuals ~loc ~is_rec tdecls : Str.t list =
    (* for mutal recursion we need to generate two classes, one transformation
       function many structures, fixpoint, etc. *)
    List.concat
      [ List.map tdecls ~f:(self#make_class ~loc ~is_rec:true)
      ; self#make_trans_functions ~loc ~is_rec
      ; self#make_shortend_class ~loc tdecls
      ]

  method simple_trf_funcs ~loc tdecl : Typ.t -> Typ.t =
    let names = map_type_param_names tdecl.ptype_params ~f:id in
    List.fold_left names
      ~init:id
      ~f:(fun acc name ->
          self#make_typ_of_class_argument ~loc tdecl
            (Typ.arrow ~loc) name
            (fun f arg -> acc @@ f arg)
        )

  method plugin_class_params_tdecl tdecl =
    let params_typs =
      self#plugin_class_params ~loc:(loc_from_caml tdecl.ptype_loc)
        ~typname:tdecl.ptype_name.txt
        (List.map ~f:fst tdecl.ptype_params)
    in
    (* FIXME: dirty hacks *)
    List.map params_typs ~f:Typ.to_type_arg_exn

  method make_shortend_class_sig ~loc =
    List.map self#tdecls ~f:(fun tdecl ->
        let typname = tdecl.ptype_name.txt in
        let class_name =
          Naming.trait_class_name_for_typ ~trait:self#plugin_name typname
        in
        let params = self#plugin_class_params_tdecl tdecl in
        let stub_name = "asdf" in
        Sig.class_ ~loc ~name:class_name
          ~params
          [ Ctf.inherit_ ~loc @@ Cty.arrow ~loc
              (Typ.unit ~loc)
              (Cty.constr ~loc (Lident stub_name) @@
               List.map ~f:(Typ.of_type_arg ~loc) params)
          ]
      )


  (* shortened class only used for mutally recursive declarations *)
  method make_shortend_class ~loc tdecls =
    List.map tdecls ~f:(fun tdecl ->
        let typname = tdecl.ptype_name.txt in
        let class_name =
          Naming.trait_class_name_for_typ ~trait:self#plugin_name typname
        in
        let stub_name = Naming.stub_class_name ~plugin:self#plugin_name tdecl in
        (* maybe it should be called proto *)
        let mut_funcs =
          Exp.tuple ~loc @@
          List.map tdecls ~f:(fun {ptype_name={txt}} ->
              Exp.ident ~loc @@ Naming.trf_function self#plugin_name txt
            )
        in

        let params = self#plugin_class_params_tdecl tdecl in
        Str.class_single ~loc ~name:class_name
          ~wrap:(fun cl ->
              Cl.fun_ ~loc
                (* (Pat.var ~loc @@ self#self_arg_name tdecl.ptype_name.txt) @@ *)
                (Pat.any ~loc) @@
              Cl.fun_list ~loc (self#prepare_fa_args ~loc tdecl) cl
            )
          ~params
          [ Cf.inherit_ ~loc @@ Cl.apply ~loc
              (Cl.constr ~loc (Lident stub_name) @@
               List.map ~f:(Typ.of_type_arg ~loc) params)
              (mut_funcs ::
               (self#apply_fas_in_new_object ~loc tdecl))
          ]
      )

  method virtual on_record_constr: loc:loc ->
    is_self_rec:(core_type -> [ `Nonrecursive | `Nonregular | `Regular ]) ->
    mutual_decls:type_declaration list ->
    inhe:Exp.t ->
    type_declaration ->
    [ `Normal of string | `Poly of string ] ->
    (* pattern variable, label name, typ of label *)
    (* TODO: Replace next to arguments by single of type (string * label_declaration) list *)
    (string * string * core_type) list ->
    label_declaration list ->
    Exp.t

  method virtual on_tuple_constr : loc:loc ->
    is_self_rec:(core_type -> [ `Nonrecursive | `Nonregular | `Regular ]) ->
    mutual_decls:type_declaration list ->
    inhe:Exp.t ->
    type_declaration ->
    [ `Normal of string | `Poly of string ] option ->
    (string * core_type) list ->
    Exp.t

  method on_variant ~loc tdecl ~mutual_decls ~is_self_rec cds k =
    k @@
    List.map cds ~f:(fun cd ->
        match cd.pcd_args with
        | Pcstr_tuple ts ->
          let inhname = gen_symbol ~prefix:"inh_" () in
          let loc = loc_from_caml cd.pcd_loc in
          let bindings = List.map ts ~f:(fun ts -> gen_symbol (), ts) in
          let bind_pats = List.map bindings ~f:(fun (s,_) -> Pat.var ~loc s) in
          Cf.method_concrete ~loc (Naming.meth_name_for_constructor cd.pcd_name.txt) @@
          Exp.fun_ ~loc (Pat.sprintf "%s" ~loc inhname) @@
          Exp.fun_ ~loc (Pat.any ~loc) @@
          Exp.fun_list ~loc bind_pats @@
          self#on_tuple_constr ~loc ~mutual_decls ~is_self_rec
            ~inhe:(Exp.ident ~loc inhname)
            tdecl (Option.some @@ `Normal cd.pcd_name.txt) bindings
        | Pcstr_record ls ->
            let inhname = gen_symbol ~prefix:"inh_" () in
            let loc = loc_from_caml cd.pcd_loc in
            let bindings =
              List.map ls ~f:(fun l -> gen_symbol (), l.pld_name.txt, l.pld_type)
            in
            let bind_pats = List.map bindings ~f:(fun (s,_,_) -> Pat.var ~loc s) in
            Cf.method_concrete ~loc (Naming.meth_name_for_constructor cd.pcd_name.txt) @@
            Exp.fun_ ~loc (Pat.sprintf "%s" ~loc inhname) @@
            Exp.fun_ ~loc (Pat.any ~loc) @@
            Exp.fun_list ~loc bind_pats @@
            self#on_record_constr ~loc ~mutual_decls ~is_self_rec
              ~inhe:(Exp.ident ~loc inhname)
              tdecl
              (`Normal cd.pcd_name.txt)
              bindings
              ls
      )

  (* should be overriden in show_typed *)
  method generate_for_variable ~loc varname =
    Exp.sprintf ~loc "f%s" varname

  (* required only for show_typed true *)
  method eta_and_exp ~center tdecl =
    let loc = loc_from_caml tdecl.ptype_loc in
    let fs = map_type_param_names tdecl.ptype_params ~f:(sprintf "f%s") in
    let ans =
      List.fold_left fs ~init:center
        ~f:(fun acc name ->
            Exp.app ~loc
              acc
              (Exp.app ~loc
                 (Exp.of_longident ~loc (Ldot (Lident "GT", "lift")))
                 (Exp.ident ~loc name)))
    in
    (* extra unit instead of inherited attribute *)
    let ans = Exp.app_list ~loc ans [Exp.unit ~loc; Exp.ident ~loc "subj"] in
    let ans = Exp.fun_ ~loc (Pat.var ~loc "subj") ans in
    List.fold_right fs ~init:ans
      ~f:(fun name acc -> Exp.fun_ ~loc (Pat.var ~loc name) acc)

  (* method do_typext_str ~loc ({ptyext_path } as extension) =
   *   let clas =
   *     let is_self_rec _ = false in
   *     let cds = List.map extension.ptyext_constructors
   *         ~f:(fun ec ->
   *             match ec.pext_kind with
   *             | Pext_rebind _ -> failwith ""
   *             | Pext_decl (args, _) ->
   *               Ast_builder.Default.constructor_declaration
   *                 ~loc:extension.ptyext_path.loc ~res:None
   *                 ~name:(ec.pext_name) ~args
   *           )
   *     in
   *     let tdecl =
   *       let open Ast_builder.Default in
   *       type_declaration ~loc:extension.ptyext_path.loc
   *         ~name:(Located.map Longident.last_exn extension.ptyext_path)
   *         ~params:extension.ptyext_params
   *         ~private_:Public ~manifest:None ~cstrs:[]
   *         ~kind:(Ptype_variant cds)
   *     in
   *     let fields = self#on_variant ~loc ~is_self_rec ~mutual_decls:[]
   *         tdecl
   *         cds
   *         id
   *     in
   *     let extra_path s = map_longident extension.ptyext_path.txt ~f:(fun _ -> s) in
   *     let inh_params =
   *       prepare_param_triples ~loc
   *         ~inh:(fun ~loc -> self#inh_of_param tdecl)
   *         ~syn:self#syn_of_param
   *         ~default_syn:(self#syn_of_main ~loc ~extra_path tdecl)
   *         ~default_inh:(self#inh_of_main ~loc tdecl)
   *         ~extra:(Typ.var ~loc @@
   *                 sprintf "%s_%s" Naming.extra_param_name tdecl.ptype_name.txt)
   *         (map_type_param_names tdecl.ptype_params ~f:id)
   *     in
   *     let parent_plugin_impl =
   *       let params =
   *         self# final_typ_params_for_alias ~loc tdecl
   *           (List.map ~f:fst tdecl.ptype_params)
   *       in
   *       Cf.inherit_ ~loc @@
   *       Cl.apply ~loc
   *         (Cl.constr ~loc (extra_path (self#make_class_name tdecl)) params)
   *         (Exp.sprintf ~loc "%s" Naming.self_arg_name :: (self#apply_fas_in_new_object ~loc tdecl))
   *         (\* TODO: check that apply_fas_... is what we need *\)
   *     in
   *     (\* TODO: It seems that we don't need to inherit interface class for extensible types
   *      * because type parameters are no changing but it require some work to disable this
   *      * generation. So it is postponed *\)
   *     self#wrap_class_definition ~loc [] tdecl ~inh_params
   *       ((self#extra_class_str_members tdecl) @ parent_plugin_impl :: fields)
   *   in
   *   [ clas ] *)

  method self_arg_name cname  =
    sprintf "%s_%s" Naming.self_arg_name cname

  (* TODO: maybe we can buble from the botton not whole expression  but mayse eitehr
     full expression or not yet applied expression to attribute and subject. That will
     allow to implement sprintf effectively *)
  (* TODO: decide expression of which type should be returned here *)
  (* do_type_gen will return an expression which after being applied
   * to inherited attribute and subject will return synthetized one
   *)
  method do_typ_gen ~loc ~mutual_decls ~is_self_rec tdecl t : Exp.t =
    let mutual_names = List.map mutual_decls ~f:(fun t -> t.ptype_name.txt) in

    let on_constr params helper typname =
        self#abstract_trf ~loc (fun einh esubj ->
            self#fancy_app ~loc
              (List.fold_left params
                 ~init:(
                   Exp.(app ~loc
                          (access ~loc "GT" self#plugin_name)
                          (access ~loc "GT" typname)
                       )
                 )
                 ~f:(fun left typ ->
                     (* TODO: copy-paste with polyvariants *)
                     let arg = helper ~loc typ in
                     let arg =
                       if self#need_inh_attr
                       then arg
                       else Exp.app ~loc arg (Exp.unit ~loc)
                     in
                     self#compose_apply_transformations ~loc ~left arg typ
                   )
              )
              einh esubj
          )

    in
    let rec helper ~loc t =
      match t.ptyp_desc with
      | Ptyp_var s -> self#generate_for_variable ~loc s
      | Ptyp_arrow (_,t1,t2) ->
        on_constr [t1;t2] helper "arrow"
      | Ptyp_tuple ts ->
          let inh_name = gen_symbol () in
          let inhe = Exp.ident ~loc inh_name in
          let bindings = List.map ts ~f:(fun ts -> gen_symbol (), ts) in
          Exp.fun_ ~loc (Pat.var ~loc inh_name) @@
          Exp.fun_ ~loc (Pat.tuple ~loc @@
                         List.map bindings ~f:(fun (name,_) -> Pat.var ~loc name)
                        ) @@
          self#on_tuple_constr ~loc ~is_self_rec ~mutual_decls ~inhe tdecl None @@
          bindings

        (* self#abstract_trf ~loc (fun einh esubj ->
            self#fancy_app ~loc
              (List.fold_left params
                 ~init:(
                   Exp.(app ~loc
                          (of_longident ~loc @@ Ldot (Lident "GT", self#plugin_name))
                          (of_longident ~loc @@
                           Ldot (Lident "GT", Printf.sprintf "tuple%d" (List.length params)))
                       )
                 )
                 ~f:(fun left typ ->
                     (* TODO: copy-paste with polyvariants *)
                     let arg = helper ~loc typ in
                     let arg =
                       if self#need_inh_attr
                       then arg
                       else Exp.app ~loc arg (Exp.unit ~loc)
                     in
                     self#compose_apply_transformations ~loc ~left arg typ
                   )
              )
              einh esubj
          ) *)
      | Ptyp_constr ({txt},params) -> begin
          match is_self_rec t with
          | `Regular ->
            let cname =
              let helper = function Lident s -> s | Ldot (_,s) -> s | _ -> assert false in
              helper txt
            in
            Exp.ident ~loc (self#self_arg_name cname)
          | `Nonregular -> failwith "non-regular types are not supported"
          | `Nonrecursive -> begin
              (* it is not a recursion but it can be a mutual recursion *)
              match txt with
              | Lident s when List.mem mutual_names s ~equal:String.equal ->
                (* we should use local trf object *)
                let args = List.map params
                    ~f:(self#do_typ_gen ~loc ~is_self_rec ~mutual_decls tdecl)
                in
                Exp.( app_list ~loc (ident ~loc @@ Naming.for_ self#trait_name s) args )
              | _ ->
                let init =
                  Exp.(app ~loc
                         (access ~loc "GT" self#plugin_name)
                         (of_longident ~loc txt)
                      )
                in
                self#abstract_trf ~loc (fun einh esubj ->
                    self#fancy_app ~loc
                      (List.fold_left params
                         ~init
                         ~f:(fun left typ ->
                             (* TODO: copy-paste with constructors  *)
                             let arg = helper ~loc typ in
                             let arg =
                               if self#need_inh_attr
                               then arg
                               else Exp.app ~loc arg (Exp.unit ~loc) in
                             self#compose_apply_transformations ~loc ~left arg typ
                           )
                      )
                      einh esubj
                  )
            end
        end

      | Ptyp_variant (rows, _, maybe_labels) -> begin
            let oninherit ~loc einh esubj typs cident varname =
              self#fancy_app ~loc
                (Exp.app_list ~loc
                   Exp.(app ~loc
                          (Exp.access ~loc "GT" self#plugin_name)
                          (of_longident ~loc cident)
                       )
                   (List.map typs ~f:(fun typ ->
                        let arg = helper ~loc typ in
                        if self#need_inh_attr
                        then arg
                        else Exp.app ~loc arg (Exp.unit ~loc)
                      ))
                )
                einh (Exp.ident ~loc varname)
            in
            let onrow ~inhe lab bindings =
              (* let inh_name = gen_symbol ~prefix:"inh_" () in *)
              (* (if self#need_inh_attr then Exp.fun_ ~loc (Pat.var ~loc inh_name)
               else Fn.id) @@
              Exp.fun_list ~loc (List.map bindings ~f:(fun (s,_) -> Pat.var ~loc s)) @@ *)
              Exp.app_list ~loc
                (self#on_tuple_constr ~loc ~is_self_rec ~mutual_decls:mutual_decls
                   ~inhe
                   tdecl
                   (Option.some @@ `Poly lab.txt)
                   bindings)
              @@
              []
              (* List.map bindings ~f:(fun (s,_) -> Exp.ident ~loc s) *)
            in
            self#abstract_trf ~loc (fun einh esubj ->
              prepare_patt_match_poly ~loc esubj
                (List.map rows ~f:(fun {prf_desc} -> prf_desc))
                maybe_labels
                ~onrow:(onrow ~inhe:einh)
                ~onlabel:(fun _ _ -> Exp.assert_false ~loc)
                ~oninherit:(oninherit ~loc einh esubj)
            )
          end
        | _ -> failwith "unsupported case in do_typ_gen"
    in
    match self#treat_type_specially t with
    | None -> helper ~loc t
    | Some e -> e

  (* should be used only in concrete plugins  *)
  method treat_type_specially t = None

  (* may be the same as fancy_app *)
  method virtual app_transformation_expr: loc:loc ->
    Exp.t -> Exp.t -> Exp.t -> Exp.t

  method virtual abstract_trf: loc:loc -> (Exp.t -> Exp.t -> Exp.t) -> Exp.t

  (* [fancy_app ~loc e inh subj] will either apply twice or skip application
   * of inherited attribute *)
  method virtual fancy_app: loc:loc -> Exp.t -> Exp.t -> Exp.t -> Exp.t
  method virtual app_gcata: loc:loc -> Exp.t -> Exp.t
  method virtual make_typ_of_self_trf:
    loc:loc -> ?in_class:bool -> type_declaration -> Typ.t

  (* method virtual inh_of_main : loc:loc -> Ppxlib.type_declaration -> Typ.t *)

  method virtual make_RHS_typ_of_transformation: loc:AstHelpers.loc ->
    ?subj_t:Typ.t -> ?syn_t:Typ.t -> type_declaration -> Typ.t

  method compose_apply_transformations ~loc ~left right typ : Exp.t =
    Exp.app ~loc left right

  (* Is not very composable but this is olny difference between plugins now *)
  method virtual need_inh_attr : bool

end

(* ******************************************************************************* *)


(** Base plugin class where transformation functions doesn't use inherited
    attribute.
    See {!Show} and {!Gmap} plugin for examples.
  *)
class virtual no_inherit_arg0 args _tdecls = object(self: 'self)
  inherit generator args _tdecls

  method virtual plugin_name: string
  method virtual syn_of_main : loc:loc ->
      ?in_class:bool ->
      Ppxlib.type_declaration -> Typ.t
  method virtual inh_of_main : loc:loc -> Ppxlib.type_declaration -> Typ.t
  method virtual syn_of_param: loc:loc -> string -> Typ.t
  method virtual inh_of_param: loc:loc -> type_declaration -> string -> Typ.t
  method virtual make_trans_function_typ: loc:loc -> type_declaration -> Typ.t

  method use_tdecl = Typ.use_tdecl
  method need_inh_attr = false

  (* almost the same as `make_typ_of_class_argument` *)
  method make_typ_of_self_trf ~loc ?(in_class=false) tdecl =
    let openize_poly typ = Typ.from_caml typ in
    let subj_t = openize_poly @@ using_type ~typename:tdecl.ptype_name.txt tdecl in
    let syn_t  = self#syn_of_main ~loc ~in_class tdecl in

    let ans = Typ.(arrow ~loc subj_t @@ syn_t) in
    if self#need_inh_attr
    then Typ.arrow ~loc (self#inh_of_main ~loc tdecl) ans
    else Typ.arrow ~loc (Typ.unit ~loc) ans

  (* val name: <fa> -> <fb> -> ... -> <fz> -> <_not_ this>
   *   fot a type ('a,'b,....'z) being generated
   **)
  (* method make_typ_of_class_argument: 'a . loc:loc -> type_declaration ->
   *   (Typ.t -> 'a -> 'a) ->
   *   string -> (('a -> 'a) -> 'a -> 'a) -> 'a -> 'a =
   *   fun ~loc tdecl chain name k ->
   *     let subj_t = Typ.var ~loc name in
   *     let syn_t = self#syn_of_param ~loc name in
   *     k @@ (fun arg -> chain (Typ.arrow ~loc subj_t syn_t) arg) *)

  method make_typ_of_class_argument: 'a . loc:loc -> type_declaration ->
    (Typ.t -> 'a -> 'a) ->
    string -> (('a -> 'a) -> 'a -> 'a) -> 'a -> 'a =
    fun ~loc tdecl chain name k ->
      let inh_t = self#inh_of_param ~loc tdecl name in
      let subj_t = Typ.var ~loc name in
      let syn_t = self#syn_of_param ~loc name in
      k @@ (fun arg -> chain (Typ.arrow ~loc inh_t @@ Typ.arrow ~loc subj_t syn_t) arg)

  method make_RHS_typ_of_transformation ~loc ?subj_t ?syn_t tdecl =
    let subj_t = Option.value subj_t
        ~default:(Typ.use_tdecl tdecl) in
    let syn_t  = Option.value syn_t ~default:(self#syn_of_main ~loc tdecl) in
    Typ.arrow ~loc subj_t syn_t

  method app_gcata ~loc egcata =
    Exp.app ~loc egcata (Exp.unit ~loc)

  method on_record_constr: loc:loc ->
    is_self_rec:(core_type -> [ `Nonrecursive | `Nonregular | `Regular ]) ->
    mutual_decls:type_declaration list ->
    inhe:Exp.t ->
    type_declaration ->
    [ `Normal of string | `Poly of string ] ->
    (string * string * core_type) list ->
    label_declaration list ->
    Exp.t = fun  ~loc ~is_self_rec ~mutual_decls ~inhe _ _ _ ->
    failwithf "handling record constructors in plugin `%s`" self#plugin_name ()

  method wrap_tr_function_str ~loc (tdecl: type_declaration) make_gcata_of_class =
    (* [%expr fun the_init subj -> GT.fix0 (fun self -> [%e body]) the_init subj] *)
    let body = make_gcata_of_class in
    Exp.fun_list ~loc [ Pat.sprintf ~loc "inh0"; Pat.sprintf ~loc "subj"] @@
    Exp.app_list ~loc
      (Exp.of_longident ~loc (Ldot (Lident "GT", "transform_gc")))
      [ Exp.sprintf ~loc "gcata_%s" tdecl.ptype_name.txt (* TODO: name *)
      ; body
      ; Exp.sprintf ~loc "inh0"
      ; Exp.sprintf ~loc "subj"
      ]

end

(** Base plugin class where transformation functions receive inherited attribute for
    type parameter *)
class virtual with_inherited_attr args _tdecls = object(self: 'self)
  inherit no_inherit_arg0 args _tdecls as super

  method! need_inh_attr = true

  (* method! make_typ_of_self_trf ~loc tdecl =
   *   Typ.arrow ~loc (self#inh_of_main ~loc tdecl) (super#make_typ_of_self_trf ~loc tdecl) *)

  (* method long_typ_of_self_trf ~loc tdecl = self#make_typ_of_self_trf ~loc tdecl *)

  (* val name: <fa> -> <fb> -> ... -> <fz> -> <_not_ this>
   *   fot a type ('a,'b,....'z) being generated
   **)

  method! make_typ_of_class_argument: 'a . loc:loc -> type_declaration ->
    (Typ.t -> 'a -> 'a) ->
    string -> (('a -> 'a) -> 'a -> 'a) -> 'a -> 'a =
    fun ~loc tdecl chain name k ->
      let inh_t = self#inh_of_param ~loc tdecl name in
      let subj_t = Typ.var ~loc name in
      let syn_t = self#syn_of_param ~loc name in
      k @@ (fun arg -> chain (Typ.arrow ~loc inh_t @@ Typ.arrow ~loc subj_t syn_t) arg)

  method! make_RHS_typ_of_transformation ~loc ?subj_t ?syn_t tdecl =
    let subj_t = Option.value subj_t ~default:(Typ.use_tdecl tdecl) in
    let syn_t  = Option.value syn_t  ~default:(self#syn_of_main ~loc tdecl) in
    Typ.arrow ~loc (self#inh_of_main ~loc tdecl)
      (super#make_RHS_typ_of_transformation ~loc ~subj_t ~syn_t tdecl)

  method abstract_trf ~loc k =
    let inh  = gen_symbol ~prefix:"inh_"  () in
    let subj = gen_symbol ~prefix:"subj_" () in
    Exp.fun_list ~loc [ Pat.var ~loc inh ; Pat.var ~loc subj ]  @@
    k (Exp.ident ~loc inh) (Exp.ident ~loc subj)

  (* method fancy_abstract_trf ~loc k =
   *   Exp.fun_list ~loc [ Pat.sprintf ~loc "inh"; Pat.sprintf ~loc "subj" ]  @@
   *   k (Exp.ident ~loc "inh") (Exp.ident ~loc "subj") *)

  method app_transformation_expr ~loc trf inh subj =
    (* we ignore inherited argument by default *)
    Exp.app_list ~loc trf [inh; subj]

  method fancy_app ~loc trf (inh: Exp.t) subj =
    Exp.app_list ~loc trf [inh; subj]

  method! app_gcata ~loc egcata = egcata

  (* let <plugin-name> fa ... fz = <this body> *)
  method make_trans_function_body ~loc ?(rec_typenames=[]) class_name tdecl =
    self#wrap_tr_function_str ~loc tdecl
      (  Exp.app_list ~loc (Exp.new_ ~loc @@ Lident class_name) @@
         ( (* (Exp.sprintf ~loc "%s" "call")
           * :: *) (self#apply_fas_in_new_object ~loc tdecl)
         )
      )

  method app_extra_unit ~(loc: loc) (e: Exp.t) = e

  method long_trans_function_typ ~(loc:loc) (tdecl: type_declaration) : Typ.t =
    self#make_trans_function_typ ~loc tdecl

  method make_final_trans_function_typ ~loc tdecl =
    self#make_trans_function_typ ~loc tdecl

end


(** Base plugin class where transformation functions doesn't use inherited
    attribute.
    See {!Show} and {!Gmap} plugin for examples.
  *)
class virtual no_inherit_arg args _tdecls = object(self: 'self)
  inherit no_inherit_arg0 args _tdecls

  method! need_inh_attr = false

  (* method long_typ_of_self_trf ~loc tdecl =
   *   (\* almost copy-paste of inherit_arg0 class*\)
   *   let openize_poly typ = Typ.from_caml typ in
   *   let subj_t = openize_poly @@ using_type ~typename:tdecl.ptype_name.txt tdecl in
   *   let syn_t  = self#syn_of_main ~loc tdecl in
   *   Typ.(arrow ~loc subj_t @@ syn_t) *)

  (* let <plugin-name> fa ... fz = <this body> *)
  method make_trans_function_body ~loc ?(rec_typenames=[]) class_name tdecl =
    self#wrap_tr_function_str ~loc tdecl
      (  Exp.app_list ~loc (Exp.new_ ~loc @@ Lident class_name) @@
         ( (* ((Exp.sprintf ~loc "%s" "call")) :: *)
           List.map rec_typenames ~f:(fun name ->
               Exp.fun_ ~loc (Pat.unit ~loc) @@
               Exp.sprintf ~loc "%s_%s" self#plugin_name name)
          @ (self#apply_fas_in_new_object ~loc tdecl)
         )
      )


  method fancy_app ~loc trf (inh: Exp.t) subj = Exp.app ~loc trf subj

  method abstract_trf ~loc k =
    Exp.fun_list ~loc [ Pat.unit ~loc; Pat.sprintf ~loc "subj" ]  @@
    k (Exp.unit ~loc) (Exp.ident ~loc "subj")

  method! make_RHS_typ_of_transformation ~loc ?subj_t ?syn_t tdecl =
    let subj_t = Option.value subj_t
        ~default:(Typ.use_tdecl tdecl) in
    let syn_t  = Option.value syn_t ~default:(self#syn_of_main ~loc tdecl) in
    Typ.arrow ~loc subj_t syn_t

  (* method compose_apply_transformations ~loc ~left right (typ:core_type) =
   *   (\* Exp.app ~loc left (Exp.fun_ ~loc (Pat.unit ~loc) right) *\)
   *   (\* Exp.app ~loc  (Exp.app ~loc left @@ Exp.unit ~loc) right *\)
   *     Exp.app ~loc left  (Exp.app ~loc right @@ Exp.unit ~loc) *)

  method app_extra_unit ~loc e = Exp.app ~loc e (Exp.unit ~loc)

  method long_trans_function_typ ~loc tdecl =
    let type_ = self#make_RHS_typ_of_transformation ~loc tdecl in
    let type_ = Typ.arrow ~loc (Typ.ident ~loc "unit") type_ in
    let names = map_type_param_names tdecl.ptype_params ~f:id in

    List.fold_right names ~init:type_
      ~f:(fun name acc ->
          let for_arg =
            Typ.(arrow ~loc (ident ~loc "unit") @@
                 arrow ~loc (var ~loc name) (self#syn_of_param ~loc name)
                )
          in
          Typ.arrow ~loc for_arg acc
        )

  (* (\* val name: <fa> -> <fb> -> ... -> <fz> -> <_not_ this>
   *  *   fot a type ('a,'b,....'z) being generated
   *  **\)
   * method! make_typ_of_class_argument: 'a . loc:loc -> type_declaration ->
   *   (Typ.t -> 'a -> 'a) ->
   *   string -> (('a -> 'a) -> 'a -> 'a) -> 'a -> 'a =
   *   fun ~loc tdecl chain name k ->
   *     let subj_t = Typ.var ~loc name in
   *     let syn_t = self#syn_of_param ~loc name in
   *     k @@ (fun arg -> chain (Typ.arrow ~loc subj_t syn_t) arg) *)

  method make_final_trans_function_typ ~loc tdecl =
    let make_arg ~loc td chain name k =
      let subj_t = Typ.var ~loc name in
      let syn_t = self#syn_of_param ~loc name in
      k @@ (fun arg -> chain (Typ.arrow ~loc subj_t syn_t) arg)
    in
    let type_ = self#make_RHS_typ_of_transformation ~loc tdecl in
    let names = map_type_param_names tdecl.ptype_params ~f:id in
    List.fold_left names
      ~init:id
      ~f:(fun acc name ->
          make_arg ~loc tdecl (Typ.arrow ~loc) name
            (fun f arg -> acc @@ f arg)
        )
      type_

  method app_transformation_expr ~loc trf (inh: Exp.t) subj =
    (* we ignore inherited argument by default *)
    Exp.app_list ~loc trf [inh; subj]

  (* method wrap_tr_function_str ~loc tdecl make_new_obj =
   *   Exp.fun_ ~loc (Pat.sprintf ~loc "subj") @@
   *   Exp.app_list ~loc
   *     (Exp.of_longident ~loc (Ldot (Lident "GT", "transform0_gc")) )
   *     [ Exp.sprintf ~loc "gcata_%s" tdecl.ptype_name.txt (\* TODO: name *\)
   *     ; make_new_obj
   *     ; Exp.sprintf ~loc "subj"
   *     ] *)

end
(*
class index_result = object
  method index_functor tdecls =
    assert (List.length tdecls > 0);
    let name = (List.hd_exn tdecls).ptype_name.txt in
    sprintf "Index_%s" name
  method index_modtyp_name tdecls =
    assert (List.length tdecls > 0);
    let name = (List.hd_exn tdecls).ptype_name.txt in
    sprintf "IndexResult_%s" name
end
class index_result2 = object
  method index_functor tdecls =
    assert (List.length tdecls > 0);
    let name = (List.hd_exn tdecls).ptype_name.txt in
    sprintf "Index2_%s" name
  method index_modtyp_name tdecls =
    assert (List.length tdecls > 0);
    let name = (List.hd_exn tdecls).ptype_name.txt in
    sprintf "IndexResult2_%s" name
end
                      *)
end

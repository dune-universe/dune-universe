open Ppxlib
open Base
open GTCommon
open HelpersBase
open Ppxlib.Ast_builder.Default
let (@@) = Caml.(@@)

let nolabelize xs = List.map ~f:(fun x -> Asttypes.Nolabel,x) xs
let invariantize types = List.map types ~f:(fun x -> x,Asttypes.Invariant)

type loc = Location.t
let noloc = Location.none
let loc_from_caml l = l

type type_arg = core_type
let named_type_arg ~loc s = ptyp_var ~loc s
let typ_arg_of_core_type t = t

let lid ?(loc=Location.none) txt = { txt; loc }
let mknoloc txt = lid txt
let pexp_pair ?(loc=Location.none) a b = pexp_tuple ~loc [a; b]

let const_string ?wtf s = Pconst_string (s, wtf)

type lab_decl = label_declaration
let lab_decl ~loc name mut type_ =
  label_declaration ~loc ~name:(Located.mk ~loc name)
    ~mutable_:(if mut then Mutable else Immutable)
    ~type_

type case = Ppxlib.case
let case  ~lhs ~rhs = case ~lhs ~rhs ~guard:None

module Pat = struct
  type t = pattern

  let any ~loc = ppat_any ~loc
  let unit ~loc = [%pat? () ]
  let of_longident ~loc lident =
    let helper = function
      | Lident s -> ppat_var ~loc (Located.mk ~loc s)
      | Ldot (Lident l, r) as p -> ppat_construct ~loc (Located.mk ~loc p) None
      | _ -> assert false
    in
    helper lident

  let access2 ~loc m n = of_longident ~loc (Ldot (Lident m, n))
  let constraint_ ~loc = ppat_constraint ~loc

  let constr      ~loc lident ps =
    let lident = Located.lident ~loc lident in
    ppat_construct ~loc lident @@
    match ps with
    | [] -> None
    | _  -> Some (ppat_tuple ~loc ps)
  let variant ~loc l ps =
    ppat_variant ~loc l @@
    match ps with
    | [] -> None
    | _  -> Some (ppat_tuple ~loc ps)

  let tuple ~loc = ppat_tuple ~loc
  let var ~loc s = ppat_var ~loc (Located.mk ~loc s)
  let of_string ~loc s = var ~loc s
  let sprintf ~loc fmt = Printf.ksprintf (of_string ~loc) fmt
  let alias ~loc p s   = ppat_alias ~loc p (lid ~loc s)
  let type_ ~loc lident = ppat_type ~loc (Located.mk ~loc lident)
  let record ~loc ps =
    ppat_record ~loc (List.map ~f:(fun (l,t) -> (Located.mk ~loc l, t)) ps) Closed
  let record1 ~loc name =
    match name with
    | Lident name ->
      record ~loc [(Lident name, var ~loc name)]
    | Ldot (Lident p, name) as i ->
      record ~loc [(i, var ~loc name)]
    | _ -> failwith "not implemented"
  let constr_record      ~loc lident ps =
    constr ~loc lident [record ~loc (List.map ~f:(fun (l,x) -> (Lident l, x)) ps)]

end

let use_new_type ~loc name e =
  pexp_newtype ~loc (Located.mk ~loc name) e

module Exp = struct
  type t = expression
  let from_caml e = e

  let ident ~loc s = pexp_ident ~loc @@ Located.lident ~loc s
  let of_longident ~loc l = pexp_ident ~loc (Located.mk ~loc l)
  let sprintf ~loc fmt = Printf.ksprintf (ident ~loc) fmt

  let unit ~loc = [%expr ()]
  let uid  ~loc = assert false
  let lid = ident
  let access ~loc mname iname =
    assert (Char.is_uppercase mname.[0]);
    let lident = Ldot (Lident mname, iname) in
    if Char.is_uppercase iname.[0]
    then pexp_construct ~loc (Located.mk ~loc lident) None
    else of_longident ~loc lident

  let constant ~loc = pexp_constant ~loc
  let int_const ~loc n = constant ~loc (Pconst_integer (Int.to_string n, None))
  let string_const ~loc s = constant ~loc (Pconst_string (s, None))

  let app ~loc l r = pexp_apply ~loc l [(Nolabel, r)]
  let app_lab ~loc l lab r = pexp_apply ~loc l [(Labelled lab, r)]
  let app_list ~loc e xs = pexp_apply ~loc e (nolabelize xs)
  (* let apply1 ~loc ?(label=Nolabel) f arg = pexp_apply ~loc f [label,arg] *)
  let field ~loc t lident =
    pexp_field ~loc t (Located.mk ~loc lident)

  let acc ~loc l r = pexp_field ~loc l (Located.mk ~loc r)
  let acc_list ~loc l xs = assert false
  let fun_ ~loc = pexp_fun ~loc Nolabel None

  let fun_list_l ~loc args e =
    if List.is_empty args then e
    else List.fold_right args
        ~init:e
        ~f:(fun (l, opt) -> pexp_fun ~loc (Optional l) (Some opt) (Pat.var ~loc l))

  let fun_list ~loc args e =
    if List.is_empty args then e
    else List.fold_right args
        ~init:e
        ~f:(fun arg -> pexp_fun ~loc Nolabel None arg)

  let case ?guard lhs rhs = Ast_builder.Default.case ~lhs ~rhs ~guard

  let record ~loc ts =
    pexp_record ~loc (List.map ts ~f:(fun (l,r) -> (Located.mk ~loc l, r))) None
  let record1 ~loc lident expr = record ~loc [lident,expr]

  let construct ~loc lident xs =
    pexp_construct ~loc (Located.mk ~loc lident) @@
    match xs with
    | [] -> None
    | xs -> Some (pexp_tuple ~loc xs)

  let variant ~loc e ts =
    match ts with
    | [] -> pexp_variant ~loc e None
    | _  -> pexp_variant ~loc e (Some (pexp_tuple ~loc ts))


  let match_ ~loc = pexp_match ~loc
  let new_ ~loc s = pexp_new ~loc (Located.mk ~loc s)
  let object_ ~loc = pexp_object ~loc
  let tuple ~loc = pexp_tuple ~loc
  let maybe_tuple ~loc xs =
    match xs with
    | [] -> None
    | [x] -> Some x
    | _   -> Some (tuple ~loc xs)

  let send ~loc obj s = pexp_send ~loc obj (Located.mk ~loc s)
  let letmodule ~loc = pexp_letmodule ~loc
  let pack_with_constraint ~loc me typname =
    pexp_constraint ~loc (pexp_pack ~loc me) @@
    ptyp_package ~loc (typname, [])

  let let_one ~loc ?(rec_=false) pat expr ewhere =
    pexp_let ~loc (if rec_ then Recursive else Nonrecursive)
      [value_binding ~loc ~pat ~expr] ewhere
  let let_ ~loc ?(rec_=false) ps =
    pexp_let ~loc (if rec_ then Recursive else Nonrecursive)
      (List.map ps ~f:(fun (pat,expr) -> value_binding ~loc ~pat ~expr))

  let assert_false ~loc = [%expr assert false]
  let objmagic_unit ~loc = [%expr Obj.magic ()]
  let failwith_ ~loc s = app ~loc [%expr failwith] (string_const ~loc s)
  let true_  ~loc = [%expr true]
  let false_ ~loc = [%expr false]
  let list ~loc xs =
    List.fold_right xs
      ~f:(fun e acc -> construct ~loc (lident "::") [e; acc])
      ~init:(construct ~loc (lident "[]") [])

  let constraint_ ~loc e t = pexp_constraint ~loc e t
end

module Typ = struct


  type t = Ppxlib.core_type
  let constr ~loc lident = ptyp_constr ~loc (Located.mk ~loc lident)

  let of_type_arg ~loc typ = {typ with ptyp_loc = loc}
  let from_caml typ = typ
  let use_tdecl tdecl =
    let loc = tdecl.ptype_loc in
    ptyp_constr ~loc (Located.lident ~loc tdecl.ptype_name.txt) @@
    (List.map ~f:fst tdecl.ptype_params)

  let of_longident ~loc lident = ptyp_constr ~loc (Located.mk ~loc lident) []
  let access2 ~loc mname tname =
    assert (Char.is_uppercase mname.[0]);
    let lident = Ldot (Lident mname, tname) in
    of_longident ~loc lident

  let ident  ~loc s = ptyp_constr ~loc (Located.lident ~loc s) []
  let sprintf ~loc fmt = Printf.ksprintf (ident ~loc) fmt
  let string ~loc = ptyp_constr ~loc (Located.mk ~loc @@ Lident "string") []
  let unit ~loc = ptyp_constr ~loc (Located.mk ~loc @@ Lident "unit") []
  let pair ~loc l r = ptyp_tuple ~loc [l;r]

  let var ~loc s = ptyp_var ~loc s
  let any ~loc = ptyp_any ~loc
  let unit ~loc  = [%type: unit]
  (* let ground  ~loc s = constr ~loc (Located.mk ~loc s) [] *)
  let class_  ~loc = ptyp_class ~loc
  let object_ ~loc flg xs =
    ptyp_object ~loc
    (List.map xs ~f:(fun (l,r) -> { pof_desc = Otag (Located.mk ~loc l, r); pof_loc=loc; pof_attributes=[]}) )
    flg
  let package ~loc lident =
    ptyp_package ~loc (lident, [])
  let arrow ~loc l r =
    ptyp_arrow ~loc Nolabel l r
  let tuple ~loc ts =
    let () = assert (List.length ts > 1) in
    ptyp_tuple ~loc ts

  let class_ ~loc lident args =
    ptyp_class ~loc (Located.mk ~loc lident) args
  let chain_arrow ~loc = function
    | [] -> failwith "list can't be empty"
    | xs ->
      let revxs = List.rev xs in
      List.fold_left (List.tl_exn revxs) ~init:(List.hd_exn revxs)
        ~f:(fun acc t -> arrow ~loc t acc)

  let variant ~loc ?(is_open=false) fields =
    ptyp_variant ~loc fields (if is_open then Open else Closed) None

  let variant_of_t ~loc t = [%type: [> [%t t] ] ]
  let alias ~loc t s = ptyp_alias ~loc t s
  let poly ~loc names t = ptyp_poly ~loc (List.map names ~f:(Located.mk ~loc)) t

  let map ~onvar t = HelpersBase.map_core_type ~onvar t

  let openize ~loc ?as_ t =
    let ans = variant_of_t ~loc t in
    match as_ with
    | Some name -> alias ~loc ans name
    | None ->  ans

  let to_type_arg x = Some x
  let to_type_arg_exn = Fn.id
end[@@warning "-32"]

type nonrec class_declaration = class_declaration
let class_declaration ~loc ~name ?(virt=false) ?(wrap=(fun x -> x)) ~params fields =
  let open Ast_builder.Default in
  let virt = if virt then Virtual else Concrete in
  let params = invariantize params in
  let pat = [%pat? _] in
  Ast_helper.Ci.mk ~loc ~virt ~params (Located.mk ~loc name) @@
  wrap (Ast_helper.Cl.structure ~loc (Ast_helper.Cstr.mk pat fields))

type nonrec type_kind =
  | Ptype_abstract
  | Ptype_record of lab_decl list

type nonrec type_declaration = type_declaration
let type_declaration ~loc ~name ~params ~manifest ~kind =
  type_declaration ~loc ~name:(Located.mk ~loc name)
    ~params:(List.map params ~f:(fun p -> (p,Invariant)))
    ~cstrs:[]
    ~private_:Public
    ~manifest
    ~kind:(match kind with
        | Ptype_abstract -> Parsetree.Ptype_abstract
        | Ptype_record ls -> Parsetree.Ptype_record ls
      )

module Str = struct

  type t = structure_item
  let single_class ~loc ?(virt=Asttypes.Virtual) ?(pat=[%pat? _])
      ?(wrap= (fun x -> x)) ~name ~params body =
    pstr_class [Ast_helper.Ci.mk ~virt ~params (Located.mk ~loc name) @@
                wrap (Ast_helper.Cl.structure (Ast_helper.Cstr.mk pat body))
  ]

  let of_class_declarations = pstr_class
  let of_tdecls ~loc decl = Ast_helper.Str.type_ ~loc Recursive [decl]

  let tdecl ~loc ~name ~params typ =
    let params = List.map ~f:(fun s -> Typ.var ~loc s, Invariant) params in
    pstr_type ~loc Recursive @@
    [ Ast_builder.Default.type_declaration ~loc
        ~name:(Located.mk ~loc name)
        ~params
        ~manifest:(Some typ)
        ~kind:Ptype_abstract
        ~cstrs:[] ~private_:Public
    ]

  let tdecl_record ~loc ~name ~params labels =
    let params = List.map ~f:(fun s -> Typ.var ~loc s, Invariant) params in
    pstr_type ~loc Nonrecursive @@
    [ Ast_builder.Default.type_declaration ~loc
        ~name:(Located.mk ~loc name)
        ~params
        ~manifest:None
        ~kind:(Ptype_record labels)
        ~cstrs:[] ~private_:Public
    ]

  (* make value have a default re4cursive flag *)
  let class_single ~loc ~name ?(virt=false) ?(wrap=(fun x -> x)) ~params fields =
    let open Ast_builder.Default in
    let virt = if virt then Virtual else Concrete in
    let params = invariantize params in
    let pat = [%pat? _] in
    pstr_class ~loc
      [ Ast_helper.Ci.mk ~loc ~virt ~params (Located.mk ~loc name) @@
        wrap (Ast_helper.Cl.structure ~loc (Ast_helper.Cstr.mk pat fields))
      ]

  let value ~loc ?(flag=Nonrecursive) decls =
    pstr_value ~loc flag decls
  let single_value ~loc pat expr =
    let flag = Nonrecursive in
    pstr_value ~loc flag [value_binding ~loc ~pat ~expr]
  let values ~loc ?(rec_flag=Recursive) vbs =
    pstr_value ~loc rec_flag vbs
  let of_vb ~loc ?(rec_flag=Recursive) vb = pstr_value ~loc rec_flag [vb]

  let tdecl_abstr ~loc name params =
    pstr_type ~loc Recursive @@
    [ Ast_builder.Default.type_declaration ~loc
        ~name:(Located.mk ~loc name)
        ~params:(List.map params ~f:(function
            | None -> (ptyp_any ~loc, Invariant)
            | Some s -> (ptyp_var ~loc s, Invariant) ))
        ~cstrs:[]
        ~kind:Ptype_abstract
        ~private_:Public
        ~manifest:None
    ]

  let functor1 ~loc name ~param sigs strs =
    pstr_module ~loc @@ module_binding ~loc ~name:(Located.mk ~loc name)
      ~expr:(pmod_functor ~loc (Located.mk ~loc param)
               (Option.some @@
                pmty_signature ~loc sigs) @@
             pmod_structure ~loc strs
            )

  let simple_gadt : loc:loc -> name:string -> params_count:int -> (string * Typ.t) list -> t =
    fun ~loc ~name ~params_count xs ->
    pstr_type ~loc Recursive
      [ Ast_builder.Default.type_declaration ~loc
          ~name:(Located.mk ~loc name)
          ~params:(List.init params_count ~f:(fun _ -> (ptyp_any ~loc, Invariant)))
          ~cstrs:[]
          ~private_:Public
          ~manifest:None
          ~kind:(Ptype_variant (List.map xs ~f:(fun (name,typ) ->
              constructor_declaration ~loc ~name:(Located.mk ~loc name) ~args:(Pcstr_tuple [])
                ~res:(Some typ)
            )))
      ]

  let module_ ~loc name me =
    pstr_module ~loc @@ module_binding ~loc ~name:(Located.mk ~loc name) ~expr:me
  let modtype ~loc = pstr_modtype ~loc

  let include_ ~loc me =
    pstr_include ~loc @@ include_infos ~loc me
end

module Me = struct
  type t = module_expr
  let structure ~loc sis = pmod_structure ~loc sis
  let ident ~loc lident = pmod_ident ~loc (Located.mk ~loc lident)
  let apply ~loc = pmod_apply ~loc
  let functor_ ~loc name = pmod_functor ~loc (Located.mk ~loc name)
end

module Mt = struct
  type t = module_type
  let ident ~loc lident = pmty_ident ~loc (Located.mk ~loc lident)
  let signature ~loc = pmty_signature ~loc
  let functor_ ~loc argname argt t = pmty_functor ~loc (Located.mk ~loc argname) argt t
  let with_ ~loc = pmty_with ~loc
end

type nonrec module_declaration = module_declaration
type nonrec module_type_declaration = module_type_declaration

let module_declaration ~loc ~name type_ =
  module_declaration ~loc ~name:(Located.mk ~loc name) ~type_

let module_type_declaration ~loc ~name type_ =
  module_type_declaration ~loc ~name:(Located.mk ~loc name) ~type_

module Sig = struct
  type t = signature_item
  let of_tdecls ~loc decl = Ast_helper.Sig.type_ ~loc Recursive [decl]
  let class_ ~loc  ~name ~params ?(virt=false)
      ?(wrap= (fun x -> x)) fields =
    let virt = if virt then Virtual else Concrete in
    let params = invariantize params in
    psig_class ~loc [class_infos ~loc ~name:(Located.mk ~loc name) ~virt ~params
                       ~expr:(wrap (pcty_signature ~loc @@
                                    class_signature ~self:[%type: _] ~fields))
                    ]

  let value ~loc ~name type_ =
    psig_value ~loc @@
    let prim = [] in
    value_description ~loc ~name:(Located.mk ~loc name) ~type_ ~prim

  let tdecl_abstr ~loc name params =
    psig_type ~loc Recursive @@
    [ Ast_builder.Default.type_declaration ~loc
        ~name:(Located.mk ~loc name)
        ~params:(List.map params ~f:(function
            | None -> (ptyp_any ~loc, Invariant)
            | Some s -> (ptyp_var ~loc s, Invariant) ))
        ~cstrs:[]
        ~kind:Ptype_abstract
        ~private_:Public
        ~manifest:None
    ]

  let functor1 ~loc name ~param sigs strs =
    psig_module ~loc @@
    Ast_builder.Default.module_declaration ~loc ~name:(Located.mk ~loc name)
      ~type_:(pmty_functor ~loc (Located.mk ~loc param)
               (Option.some @@
                pmty_signature ~loc sigs) @@
              pmty_signature ~loc strs
            )

  let simple_gadt : loc:loc -> name:string -> params_count:int -> (string * Typ.t) list -> t =
    fun ~loc ~name ~params_count xs ->
    psig_type ~loc Recursive
      [ Ast_builder.Default.type_declaration ~loc
          ~name:(Located.mk ~loc name)
          ~params:(List.init params_count ~f:(fun _ -> (ptyp_any ~loc, Invariant)))
          ~cstrs:[]
          ~private_:Public
          ~manifest:None
          ~kind:(Ptype_variant (List.map xs ~f:(fun (name,typ) ->
              constructor_declaration ~loc ~name:(Located.mk ~loc name) ~args:(Pcstr_tuple [])
                ~res:(Some typ)
            )))
      ]

  let module_ ~loc md = psig_module ~loc md
  let modtype ~loc = psig_modtype ~loc
end

module WC = struct
  type t = Ppxlib.with_constraint

  (* There is no helper functions in Ast_builder *)
  let typ ~loc ~params name typ =
    Pwith_type (Located.mk ~loc (Lident name),
                Ast_builder.Default.type_declaration ~loc
                  ~name:(Located.mk ~loc name)
                  ~params:(List.map params ~f:(fun s -> (Typ.var ~loc s, Invariant)))
                  ~private_:Public ~cstrs:[]
                  ~kind:Ptype_abstract
                  ~manifest:(Some typ)
               )
end

module Vb = struct
  type t = Ppxlib.value_binding
end
let value_binding = value_binding

module Cf = struct
  type t = class_field
  let constraint_ ~loc t1 t2 =
    pcf_constraint ~loc (t1,t2)
  let inherit_ ~loc ?(as_=None) cl_expr =
    let (_: string option) = as_ in
    let flg = Fresh in
    pcf_inherit ~loc flg  cl_expr @@ Option.map ~f:(fun s -> Located.mk ~loc s) as_
  let method_ ~loc name ?(flg=Public) kind =
    pcf_method ~loc (Located.mk ~loc name, flg, kind)
  let method_concrete ~loc name (* ?(flg=Public) ?(over_flg=Fresh) *) e =
    method_ ~loc name ~flg:Public (Cfk_concrete (Fresh, e))
  let method_virtual ~loc name (* ?(flg=Public) *) typ =
    method_ ~loc name ~flg:Public (Cfk_virtual typ)
end

module Ctf = struct
  type t = class_type_field
  let method_ ~loc ?(virt=false) name kind =
    let flg = Public in
    let virt_flg = if virt then Virtual else Concrete in
    pctf_method ~loc (Located.mk ~loc name, flg, virt_flg, kind)
  let inherit_ ~loc = pctf_inherit ~loc
  let constraint_ ~loc l r = pctf_constraint ~loc (l,r)
end

module Cty = struct
  (* include Ast_helper.Cty *)
  type t = class_type
  let arrow ~loc  l r =
    Ast_helper.Cty.arrow ~loc Nolabel l r
  let constr ~loc lident ts =
    pcty_constr ~loc (Located.mk ~loc lident) ts
end

module Cl = struct
  open Ast_helper
  include Cl

  type t = class_expr
  let fun_list ~loc args e =
    if List.is_empty args then e else
    List.fold_right args ~init:e
      ~f:(fun arg acc -> Cl.fun_ ~loc Asttypes.Nolabel None arg acc)

  let apply ~loc e args =
    if List.is_empty args then e else Cl.apply ~loc e (nolabelize args)

  let fun_ ~loc = pcl_fun ~loc Nolabel None

  let constr ~loc (lid: longident) ts =
    pcl_constr ~loc (Located.mk ~loc lid) ts
  let structure ~loc = pcl_structure ~loc
  let let_ ~loc ?(flg=Nonrecursive) = Cl.let_ ~loc flg
end


(*
module Cstr = struct
  let mk ~self fields = class_structure ~self ~fields
end*)

type class_structure = Ppxlib.class_structure
let class_structure = Ast_builder.Default.class_structure

open Parsetree

let openize_helper ~is_open ~loc typ =
  let loc = typ.ptyp_loc in
  Typ.variant ~loc ~is_open [{prf_desc= Rinherit typ; prf_loc=loc; prf_attributes=[] }]

let openize_poly ~loc = openize_helper ~is_open:true ~loc
let closize_poly ~loc = openize_helper ~is_open:false ~loc

let map_type_param_names ~f ps =
  List.map ps ~f:(fun (t,_) ->
    match t.ptyp_desc with
    | Ptyp_var name -> f name
    | _ -> failwith "bad argument of map_type_param_names")

let prepare_param_triples ~loc
    ~extra
    ?(inh=fun ~loc s -> Typ.var ~loc @@ "i"^s)
    ?(syn=fun ~loc s -> Typ.var ~loc @@ "s"^s)
    ?(default_inh=[%type: 'inh])
    ?(default_syn=[%type: 'syn])
    names =
  let ps = List.concat_map names ~f:(fun n ->
    [inh ~loc n; Typ.var ~loc n; syn ~loc n]
  )
  in
  ps @ [ default_inh; extra; default_syn ]


(* let params_obj ?(loc=Location.none)
 *     ?(inh=fun s -> Typ.var @@ "i"^s) ?(syn=fun s -> Typ.var @@ "s"^s) root_type =
 *   (\* converts 'a, 'b to
 *      < a: 'ia -> 'a -> 'sa ; b: 'ib -> 'b -> 'sb >
 *    *\)
 *   let f (t,_) = arr_of_param ~inh ~syn t in
 *   ptyp_object ~loc (List.map ~f root_type.ptype_params) Asttypes.Closed *)

let inh_syn_ts ?(loc=Location.none) () = [ [%type: 'inh]; [%type: 'syn] ]
(* Used when we need to check that type we working on references himself in
  it's body *)
let are_the_same (typ: core_type) (tdecl: type_declaration) =
  (match typ.ptyp_desc with
  | Ptyp_constr ({txt=Longident.Lident xxx},_) ->
    let b = (String.equal xxx tdecl.ptype_name.txt) in
    b
  | _ ->
    false
  )

let typ_vars_of_typ t =
  let o = object
    inherit [string list] Ast_traverse.fold as super
    method! core_type_desc t acc  =
      match t with
      | Ptyp_var s -> s::acc
      | _ -> super#core_type_desc t acc
  end
  in
  List.remove_consecutive_duplicates ~equal:String.equal @@ o#core_type t []

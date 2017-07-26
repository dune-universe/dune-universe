open Ppx_core
open Ast_builder.Default

module Attrs = struct
  let no_hashing =
    Attribute.declare "hash.no_hashing"
      Attribute.Context.label_declaration
      Ast_pattern.(pstr nil)
      ()
end

let str_attributes = [
  Attribute.T Attrs.no_hashing;
]

(* Generate code to compute hash values of type [t] in folding style, following the structure of
   the type.  Incorporate all structure when computing hash values, to maximise hash
   quality. Don't attempt to detect/avoid cycles - just loop. *)

let hash_state_t ~loc =
  [%type: Ppx_hash_lib.Std.Hash.state]

let hash_fold_type ~loc ty =
  [%type: [%t hash_state_t ~loc] -> [%t ty] -> [%t hash_state_t ~loc]]

let hash_type ~loc ty =
  [%type: [%t ty] -> Ppx_hash_lib.Std.Hash.hash_value]

(* Note: The argument [hsv] "hash state value" should not be duplicated either in this
   generator code, or in the generated code. *)

let hash_fold_int hsv ~loc i =
  [%expr Ppx_hash_lib.Std.Hash.fold_int [%e hsv] [%e eint ~loc i]]

let special_case_types_named_t = function
  | `hash_fold -> false
  | `hash -> true

let hash_fold_ tn =
  match tn with
  | "t" when special_case_types_named_t `hash_fold -> "hash_fold"
  | _ -> "hash_fold_" ^ tn

let hash_ tn =
  match tn with
  | "t" when special_case_types_named_t `hash -> "hash"
  | _ -> "hash_" ^ tn

(** renames [x] avoiding collision with [type_name] *)
let rigid_type_var ~type_name x =
  let prefix = "rigid_" in
  if String.equal x type_name || String.is_prefix x ~prefix
  then prefix ^ x ^ "_of_type_" ^ type_name
  else x

let make_type_rigid ~type_name =
  let map = object
    inherit Ast_traverse.map as super
    method! core_type ty =
      let ptyp_desc =
        let () = (* making sure [type_name] is the only free type variable *)
          match ty.ptyp_desc with
          | Ptyp_constr (name, _args) ->
            (match name.txt with
             | Ldot _ | Lapply _ -> ()
             | Lident name ->
               if (not (String.equal name type_name)) then
                 Location.raise_errorf ~loc:ty.ptyp_loc
                   "ppx_hash: make_type_rigid: unexpected type %S. expected to only find %S"
                   (string_of_core_type ty)
                   type_name;
               ())
          | _ -> ()
        in
        match ty.ptyp_desc with
        | Ptyp_var s ->
          Ptyp_constr (Located.lident ~loc:ty.ptyp_loc (rigid_type_var ~type_name s), [])
        | desc -> super#core_type_desc desc
      in
      { ty with ptyp_desc }
  end in
  map#core_type

(**
   In this comment we ignore any free variables that have the form "*.hash_fold*" or
   "_hash_fold_". So, a term that only refers to such variables will be called "closed".

   Scope-correctness of the terms produced by the below mutually-recursive group
   can be checked by keeping in mind the following convention:

   - Anything called [hsv] is a closed term or the variable [hsv]
   - [value] is allowed to have arbitrary free variables
   - The resulting term will have at most the free variables found in [value] or [hsv] (in
   particular, functions that don't accept a [value] will produce terms that have the same
   free variables as [hsv]).
*)

(* The only names we assume to be in scope are [hash_fold_<TY>]
   So we are sure [tp_name] (which start with an [_]) will not capture them.a *)
let tp_name n = Printf.sprintf "_hash_fold_%s" n

let with_tuple loc value xs f =
  let names = List.mapi ~f:(fun i t -> Printf.sprintf "e%d" i, t) xs in
  let pattern =
    let l = List.map ~f:(fun (n, _) -> pvar ~loc n) names in
    ppat_tuple ~loc l
  in
  let e = f (List.map ~f:(fun (n,t) -> (evar ~loc n, t)) names) in
  let binding  = value_binding ~loc ~pat:pattern ~expr:value in
  pexp_let ~loc Nonrecursive [binding] e

let rec hash_applied hsv ty value =
  let loc = ty.ptyp_loc in
  match ty.ptyp_desc with
  | Ptyp_constr (name, ta) ->
    let args = List.map ta ~f:(hash_fold_of_ty_fun ~type_constraint:false) in
    type_constr_conv ~loc name ~f:hash_fold_ (args @ [hsv; value])
  | _ -> assert false

and hash_fold_of_tuple hsv loc tys value =
  with_tuple loc value tys (fun elems1 ->
    List.fold_left elems1 ~init:hsv ~f:(fun hsv (v, t) ->
      hash_fold_of_ty hsv t v))

and hash_variant hsv loc row_fields value =
  let map = function
    | Rtag (cnstr, _attrs, true, _) | Rtag (cnstr, _attrs, _, []) ->
       case ~guard:None
         ~lhs:(ppat_variant ~loc cnstr None)
         ~rhs:(hash_fold_int hsv ~loc (Ocaml_common.Btype.hash_variant cnstr))
    | Rtag (cnstr, _attrs, false, tp :: _) ->
       let v = "_v" in
       let hsv = hash_fold_int hsv ~loc (Ocaml_common.Btype.hash_variant cnstr) in
       let body = hash_fold_of_ty hsv tp (evar ~loc v) in
       case ~guard:None
         ~lhs:(ppat_variant ~loc cnstr (Some (pvar ~loc v)))
         ~rhs:body
    | Rinherit ({ ptyp_desc = Ptyp_constr (id, _); _ } as ty) ->
      (* Generated code from..
         type 'a id = 'a [@@deriving hash]
         type t = [ `a | [ `b ] id ] [@@deriving hash]
         doesn't compile: Also see the "sadly" note in: ppx_compare_expander.ml *)
       let v = "_v" in
       case ~guard:None
         ~lhs:(ppat_alias ~loc (ppat_type ~loc id) (Located.mk ~loc v))
         ~rhs:(hash_applied hsv ty (evar ~loc v))
    | Rinherit ty ->
       let s = string_of_core_type ty in
       Location.raise_errorf ~loc "ppx_hash: impossible variant case: %s" s
  in
  pexp_match ~loc value (List.map ~f:map row_fields)

and branch_of_sum hsv ~loc cd =
  match cd.pcd_args with
  | Pcstr_tuple [] ->
     let pcnstr = pconstruct cd None in
     case ~guard:None ~lhs:pcnstr ~rhs:hsv
  | Pcstr_tuple tps ->
     let ids_ty =
       List.mapi tps ~f:(fun i ty -> (Printf.sprintf "_a%d" i, ty))
     in
     let lpatt = List.map ids_ty ~f:(fun (l,_ty) -> pvar ~loc l) |> ppat_tuple ~loc
     and body =
       List.fold_left ids_ty ~init:hsv ~f:(fun hsv (l,ty) -> hash_fold_of_ty hsv ty (evar ~loc l))
     in
     case ~guard:None
       ~lhs:(pconstruct cd (Some lpatt))
       ~rhs:body
  | Pcstr_record lds ->
    let arg = "_ir" in
    let pat = pvar ~loc arg in
    let v = evar ~loc arg in
    let body = hash_fold_of_record hsv loc lds v in
    case ~guard:None
      ~lhs:(pconstruct cd (Some pat))
      ~rhs:body

and branches_of_sum hsv = function
  | [cd] ->
    (* this is an optimization: we don't need to mix in the constructor tag if the type
       only has one constructor *)
     let loc = cd.pcd_loc in
     [branch_of_sum hsv ~loc cd]
  | cds ->
     List.mapi cds ~f:(fun i cd ->
       let loc = cd.pcd_loc in
       let hsv = hash_fold_int hsv ~loc i in
       branch_of_sum hsv ~loc cd
     )

and hash_sum hsv loc cds value =
  pexp_match ~loc value (branches_of_sum hsv cds)

and hash_fold_of_ty hsv ty value =
  let loc = ty.ptyp_loc in
  match ty.ptyp_desc with
  | Ptyp_constr _ -> hash_applied hsv ty value
  | Ptyp_tuple tys -> hash_fold_of_tuple hsv loc tys value
  | Ptyp_var name -> eapply ~loc (evar ~loc (tp_name name)) [hsv; value]
  | Ptyp_arrow _ ->
     Location.raise_errorf ~loc "ppx_hash: functions can not be hashed."
  | Ptyp_variant (row_fields, Closed, None) ->
     hash_variant hsv loc row_fields value
  | _ ->
     let s = string_of_core_type ty in
     Location.raise_errorf ~loc "ppx_hash: unsupported type: %s" s

and hash_fold_of_ty_fun ~type_constraint ty =
  let loc = ty.ptyp_loc in
  let hsv = "hsv" in
  let arg = "arg" in
  let maybe_constrained_arg =
    if type_constraint then
      ppat_constraint ~loc (pvar ~loc arg) ty
    else
      pvar ~loc arg
  in
  [%expr
      fun [%p pvar ~loc hsv] [%p maybe_constrained_arg] ->
        [%e hash_fold_of_ty (evar ~loc hsv) ty (evar ~loc arg) ]
  ]

and hash_fold_of_record hsv _loc lds value =
  let is_evar = function
    | { pexp_desc = Pexp_ident _; _ } -> true
    | _                               -> false
  in
  assert (is_evar value);
  List.fold_left lds ~init:hsv ~f:(fun hsv ld ->
    let loc = ld.pld_loc in
    let label = Located.map lident ld.pld_name in
    let field_handling =
      match
        ld.pld_mutable,
        Attribute.get Attrs.no_hashing ld
      with
      | Mutable, None
        -> `error "require [@no_hashing] on mutable record field"
      | (Mutable | Immutable), Some () ->
         `skip
      | Immutable, None ->
         `incorporate
    in
    match field_handling with
    | `error s -> Location.raise_errorf ~loc "ppx_hash: %s" s
    | `incorporate -> hash_fold_of_ty hsv ld.pld_type (pexp_field ~loc value label)
    | `skip -> hsv
  )

let hash_fold_of_abstract hsv loc type_name value =
  let str =
    Printf.sprintf
      "hash called on the type %s, which is abstract in an implementation."
      type_name
  in
  [%expr
   let _ = [%e hsv] in
   let _ = [%e value] in
   failwith [%e estring ~loc str]
  ]

let hash_of_ty_fun ~type_constraint ty =
  let loc = ty.ptyp_loc in
  let arg = "arg" in
  let maybe_constrained_arg =
    if type_constraint then
      ppat_constraint ~loc (pvar ~loc arg) ty
    else
      pvar ~loc arg
  in
  [%expr
    fun [%p maybe_constrained_arg] ->
      Ppx_hash_lib.Std.Hash.get_hash_value
        [%e hash_fold_of_ty [%expr Ppx_hash_lib.Std.Hash.create ()] ty (evar ~loc arg)]
  ]

let hash_structure_item_of_td td =
  let loc = td.ptype_loc in
  match td.ptype_params with
  | _::_ -> []
  | [] -> [
    let bnd = pvar ~loc (hash_ td.ptype_name.txt) in
    let typ = combinator_type_of_type_declaration td ~f:hash_type in
    let pat = ppat_constraint ~loc bnd typ in
    let expr =
      hash_of_ty_fun ~type_constraint:false
        { ptyp_loc = loc;
          ptyp_attributes = [];
          ptyp_desc =
            Ptyp_constr ({ loc; txt = Lident td.ptype_name.txt }, []); }
    in
    value_binding ~loc ~pat ~expr
  ]

let hash_fold_structure_item_of_td td =
  let loc = td.ptype_loc in
  let hsv = "hsv" in
  let arg = "arg" in
  let body =
    let hsv = evar ~loc hsv in
    let v      = evar ~loc arg in
    match td.ptype_kind with
    | Ptype_variant cds -> hash_sum       hsv loc cds v
    | Ptype_record  lds -> hash_fold_of_record hsv loc lds v
    | Ptype_open ->
       Location.raise_errorf ~loc
         "ppx_hash: open types are not supported"
    | Ptype_abstract ->
       match td.ptype_manifest with
       | None -> hash_fold_of_abstract hsv loc td.ptype_name.txt v
       | Some ty ->
          match ty.ptyp_desc with
          | Ptyp_variant (_, Open, _) | Ptyp_variant (_, Closed, Some (_ :: _)) ->
             Location.raise_errorf ~loc:ty.ptyp_loc
               "ppx_hash: cannot hash open polymorphic variant types"
          | Ptyp_variant (row_fields, _, _) ->
             hash_variant hsv loc row_fields v
          | _ ->
             hash_fold_of_ty hsv ty v
  in
  let vars = List.map td.ptype_params ~f:(fun p -> (get_type_param_name p).txt) in
  let extra_names = List.map vars ~f:tp_name in
  let patts = List.map (extra_names @ [ hsv; arg ]) ~f:(pvar ~loc) in
  let bnd = pvar ~loc (hash_fold_ td.ptype_name.txt) in
  let scheme = combinator_type_of_type_declaration td ~f:hash_fold_type in
  let pat = ppat_constraint ~loc bnd (ptyp_poly ~loc vars scheme) in
  let expr = eabstract ~loc patts body in
  let use_rigid_variables = match td.ptype_kind with | Ptype_variant _ -> true | _ -> false in
  let expr =
    if use_rigid_variables then
      let type_name = td.ptype_name.txt in
      List.fold_right vars ~f:(fun s -> pexp_newtype ~loc (rigid_type_var ~type_name s))
        ~init:(pexp_constraint ~loc expr (make_type_rigid ~type_name scheme))
    else
      expr
  in
  value_binding ~loc ~pat ~expr

let str_type_decl ~loc ~path:_ (rec_flag, tds) =
  let rec_flag = really_recursive rec_flag tds in
  let hash_fold_definitions =
    let bindings = List.map ~f:hash_fold_structure_item_of_td tds in
    pstr_value ~loc rec_flag bindings
  in
  let hash_definition_bindings =
    List.concat (List.map ~f:hash_structure_item_of_td tds)
  in
  match hash_definition_bindings with
  | [] -> [ hash_fold_definitions ]
  | bindings ->
    let hash_definitions = pstr_value ~loc Nonrecursive bindings in
    [ hash_fold_definitions; hash_definitions ]

let sig_type_decl ~loc:_ ~path:_ (_rec_flag, tds) =
  List.concat (List.map tds ~f:(fun td ->
    let monomorphic = List.is_empty td.ptype_params in
    let definition ~f_type ~f_name =
      let type_ =
        combinator_type_of_type_declaration td ~f:f_type
      in
      let name =
        let tn = td.ptype_name.txt in
        f_name tn
      in
      let loc = td.ptype_loc in
      psig_value ~loc (
        value_description ~loc ~name:{ td.ptype_name with txt = name }
          ~type_ ~prim:[])
    in
    List.concat [
      [                     definition ~f_type:hash_fold_type ~f_name:hash_fold_];
      (if monomorphic then [definition ~f_type:hash_type      ~f_name:hash_     ] else []);
    ]))

let hash_fold_core_type ty = hash_fold_of_ty_fun ~type_constraint:true ty
let hash_core_type ty = hash_of_ty_fun ~type_constraint:true ty

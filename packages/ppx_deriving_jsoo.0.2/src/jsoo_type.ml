open Ppxlib
open Ast_builder.Default
open Common

type ('a, 'b) ctt =
  | CT of 'a
  | TT of 'b

let core_of_param = List.map fst
let core_tail ~loc ptyp = match ptyp.ptyp_desc with
  | Ptyp_constr (_, l) -> l
  | _ -> Location.raise_errorf ~loc "core type is not a constr"

let typ_constr ~loc s l = ptyp_constr ~loc (mkl ~loc (Longident.parse s)) l
let ctyp_constr ~loc s l = pcty_constr ~loc (mkl ~loc (Longident.parse s)) l

let rec typ_constr_list ~loc = function
  | [] -> assert false
  | [h] -> typ_constr ~loc h []
  | h :: t -> typ_constr ~loc h [typ_constr_list ~loc t]

let ml_type_to_class_type ?(opt="optdef") ?(number=false) s l = match s, l with
  | "unit", _ -> "unit"
  | "int", [] | "int32", [] | "Int32.t", [] | "Int.t", [] ->
    if number then js_mod "number" else "int"
  | "int64", [] | "Int64.t", [] | "nativeint", [] | "Nativeint.t", [] -> js_mod "bigInt"
  | "float", [] | "Float.t", [] -> js_mod "number"
  | "string", [] | "String.t", [] | "char", [] | "Char.t", []
    -> js_mod "js_string"
  | "bytes", [] | "Bytes.t", [] -> jsoo_mod "Typed_array.arrayBuffer"
  | "list", [_] | "array", [_] | "List.t", [_] | "Array.t", [_] -> js_mod "js_array"
  | "option", [_] | "Option.t", [_] -> js_mod opt
  | "Unsafe.any", [] | "any", [] | "Js.Unsafe.any", [] | "Js_of_ocaml.Js.Unsafe.any", []
    -> js_mod "Unsafe.any"
  | s, _ -> if s = "t" then jsoo else s ^ _jsoo

let prop ~loc core = typ_constr ~loc (js_mod "prop") [core]
let readonly_prop ~loc core = typ_constr ~loc (js_mod "readonly_prop") [core]
let case_prop ~loc core = typ_constr ~loc (js_mod "case_prop") [core]
let meth ~loc core = typ_constr ~loc (js_mod "meth") [core]
let constr ~loc core = typ_constr ~loc (js_mod "constr") [core]

let js_monad ~loc = function
    | TT c -> c
    | CT c -> typ_constr ~loc (js_mod "t") [c]

let fold_ctt ?number ~ctt ~tt name core = match name with
  | "unit" | "option" | "int"  | "int32" | "Int.t" | "Int32.t"
  | "any" | "Unsafe.any" | "Js.Unsafe.any" | "Js_of_ocaml.Js.Unsafe.any"
  | "meth" when number <> Some true -> TT (tt core)
  | _ -> CT (ctt core)

let mk_ctt_core ?number name core = fold_ctt ?number ~ctt:(fun x -> x) ~tt:(fun x -> x) name core

let map_acc f g end_ start l =
  let l, acc = List.fold_left (fun (l, acc) x ->
      let x, acc2 = f x in
      g l x, acc @ acc2) (start, []) l in
  List.rev (end_ l), acc

let map_acc0 f l =
  let l, acc = List.fold_left (fun (l, acc) x ->
      let x, acc2 = f x in
      x :: l, acc @ acc2) ([], []) l in
  List.rev l, acc

let typ_to_ctyp ~loc c = match c with
  | CT {ptyp_desc=Ptyp_constr (lloc, l); _} -> pcty_constr ~loc lloc l
  | _ -> Location.raise_errorf ~loc "cannot write this core type to class type constr"

let assoc_type ?(assoc=false) name l =
  if not assoc then None
  else if name = "list" || name = "List.t" then
    match l with
    | [ {ptyp_desc = Ptyp_tuple [ c1; c2 ]; _} ] ->
      begin match c1.ptyp_desc with
        | Ptyp_constr ({txt; _}, []) ->
          let cons = Longident.name txt in
          if cons = "string" || cons = "String.t" then Some c2
          else None
        | _ -> None
      end
    | _ -> None
  else None

let rec type_of_core ~loc ~params ~name ?(is_meth=false) ?meth_start ?meth_callback ?callback ?opt ?number ?array_tup ?assoc c =
  let {ca_opt; ca_ignore; ca_number; ca_assoc; ca_array; ca_meth_cb; ca_cb; ca_case; _} =
    core_attributes ?opt ?number ?array_tup ?callback ?meth_callback ?assoc c.ptyp_attributes in
  if ca_ignore then TT c, []
  else
    match c.ptyp_desc with
    | Ptyp_any -> TT (ptyp_any ~loc), []
    | Ptyp_var v -> TT (ptyp_var ~loc v), []
    | Ptyp_tuple [] -> TT (typ_constr ~loc "unit" []), []
    | Ptyp_tuple [h] -> type_of_core ~loc ~params ~name ~is_meth ?opt:ca_opt ?number:ca_number h
    | Ptyp_tuple l ->
      if ca_array = Some true then
        TT (typ_constr_list ~loc [js_mod "t"; js_mod "js_array"; js_mod "Unsafe.any"]), []
      else
        let expr, acc = tuple_type ~loc ~params ~name l in
        let tuple_name = get_tuple_name name in
        debug ~v:2 "\tadd tuple class type %s" tuple_name;
        let ct = class_infos ~loc
            ~virt:Concrete ~params ~name:(mkl ~loc tuple_name) ~expr in
        CT (typ_constr ~loc tuple_name (core_of_param params)), acc @ [ ct ]
    | Ptyp_arrow (_a, c1, c2) ->
      arrow_type ~loc ~params ~name ~is_meth ?meth_start ?meth_callback:ca_meth_cb ?callback:ca_cb (c1, c2)
    | Ptyp_variant (l, _, _) ->
      let expr, acc = variant_type ~case:ca_case ~loc ~params ~name l in
      let variant_name = get_tuple_name name in
      debug ~v:2 "\tadd variant class type %s" variant_name;
      let ct = class_infos ~loc
          ~virt:Concrete ~params ~name:(mkl ~loc variant_name) ~expr in
      CT (typ_constr ~loc variant_name (core_of_param params)), acc @ [ ct ]
    | Ptyp_constr ({txt; _}, [c]) when Longident.name txt = "ref" || Longident.name txt = "Lazy.t" ->
      type_of_core ~loc ~params ~name ~is_meth ?opt:ca_opt ?number:ca_number c
    | Ptyp_constr ({txt; _}, l) ->
      let type_name = Longident.name txt in
      begin match assoc_type ?assoc:ca_assoc type_name l with
        | None ->
          let c, acc = map_acc0 (monad_of_core ~loc ~params ~name) l in
          let c = mk_ctt_core ?number:ca_number type_name @@
            typ_constr ~loc (ml_type_to_class_type ?opt:ca_opt ?number:ca_number type_name c) c in
          if is_meth then TT (meth ~loc (js_monad ~loc c)), acc
          else c, acc
        | Some c ->
          let c, acc = monad_of_core ~loc ~name ~params c in
          TT (typ_constr ~loc (js_mod "Table.t") [ c ]), acc
      end
    | _ -> Location.raise_errorf ~loc "type not handled"

and monad_of_core ~loc ~params ~name ?is_meth ?meth_start ?meth_callback
    ?callback ?opt ?array_tup ?number ?assoc c =
  let c, acc = type_of_core ~loc ~params ~name ?is_meth ?meth_start
      ?meth_callback ?callback ?opt ?array_tup ?number ?assoc c in
  js_monad ~loc c, acc

and tuple_type ~loc ~params ~name ?is_meth l =
    let fields, acc =
      map_acc
        (monad_of_core ~loc ~params ~name ?is_meth)
        (fun (fields, i) c ->
           let f = pctf_method ~loc (
               (mkl ~loc @@ "_" ^ string_of_int i), Public, Concrete,
               readonly_prop ~loc c) in
           f :: fields, i+1) fst ([], 0) l in
    pcty_signature ~loc @@ class_signature ~self:(ptyp_any ~loc) ~fields, acc

and arrow_type ~loc ~params ~name ?is_meth ?(meth_start=false) ?(meth_callback=false) ?(callback=false) (c1, c2) =
  let c2, acc2 = monad_of_core ~loc ~params ~name ?is_meth c2 in
  match meth_start, meth_callback, c1 with
  | true, _, {ptyp_desc = Ptyp_constr ({txt; _}, _); _} when Longident.name txt = "unit" ->
    TT c2, acc2
  | _, true, _ ->
    let c1, acc1 = monad_of_core ~loc ~params ~name c1 in
    TT (typ_constr ~loc (js_mod "meth_callback") [c1; c2]), acc1 @ acc2
  | _ ->
    let c1, acc1 = monad_of_core ~loc ~params ~name c1 in
    let c = ptyp_arrow ~loc Nolabel c1 c2 in
    let c = if callback then typ_constr ~loc (js_mod "callback") [c] else c in
    TT c, acc1 @ acc2

and variant_type ?(case=false) ~loc ~params ~name l =
  let c_name, c_prop =
    if case then (fun txt -> name ^ "_" ^ txt), case_prop
    else (fun txt -> txt), (fun ~loc c -> readonly_prop ~loc (typ_constr ~loc (js_mod "optdef") [c])) in
  let fields, acc =
    map_acc0
      (fun rf ->
         match rf.prf_desc with
         | Rtag ({txt; _}, _, []) ->
           pctf_method ~loc (
             mkl ~loc (field_name ~case (c_name txt)), Public, Concrete,
             c_prop ~loc (typ_constr ~loc "unit" [])), []
         | Rtag ({txt; _}, _, (h :: _)) ->
           let c, acc = monad_of_core ~loc ~params ~name h in
           pctf_method ~loc (
             mkl ~loc (field_name ~case (c_name txt)), Public, Concrete,
             c_prop ~loc c), acc
         | Rinherit c ->
           let c, acc = type_of_core ~loc ~params ~name c in
           pctf_inherit ~loc (typ_to_ctyp ~loc c), acc) l in
  pcty_signature ~loc @@ class_signature ~self:(ptyp_any ~loc) ~fields, acc

let field_of_label_declaration ~loc ~params pld =
  let name = pld.pld_name.txt in
  debug ~v:2 "\t\tlabel field %s" name;
  let {fa_meth; fa_meth_cb; fa_cb; fa_prop; fa_opt; fa_key; fa_ignore;
       fa_array; fa_number; fa_assoc; fa_case; _} =
    field_attributes ~key:name pld.pld_attributes in
  let name_js = field_name fa_key in
  match fa_case, pld.pld_type.ptyp_desc with
  | true, Ptyp_variant (l, _, _) ->
    let expr, acc = variant_type ~case:true ~loc ~params ~name l in
    let ct = class_infos ~loc
        ~virt:Concrete ~params ~name:(mkl ~loc name) ~expr in
    pctf_inherit ~loc
      (pcty_constr ~loc (mkl ~loc @@ Longident.parse name)
         (core_of_param params)), acc @ [ ct ]
  | _ ->
    let c, acc =
      if fa_ignore then pld.pld_type, []
      else
        monad_of_core ~loc ~params ~name ~is_meth:fa_meth ~meth_start:fa_meth
          ?opt:fa_opt ?meth_callback:fa_meth_cb ?callback:fa_cb ?array_tup:fa_array
          ?number:fa_number ?assoc:fa_assoc pld.pld_type in
    let prop = match pld.pld_type.ptyp_desc with
      | Ptyp_constr ({txt; _}, _) when Longident.name txt = "ref" -> Some "prop"
      | _ -> fa_prop in
    let c = match prop with None -> c | Some prop -> typ_constr ~loc (js_mod prop) [c] in
    pctf_method ~loc (mkl ~loc name_js, Public, Concrete, c), acc

let field_of_constructor_declaration ?(case=false) ~loc ~params ~name pcd =
  (match pcd.pcd_res with
   | Some _ -> Location.raise_errorf ~loc "GADT not handled"
   | None -> ());
  let name = if case then name ^ "_" ^ pcd.pcd_name.txt else pcd.pcd_name.txt in
  debug ~v:2 "\t\tconstructor field %s" name;
  let {fa_meth_cb; fa_cb; fa_prop; fa_key; fa_array; fa_number; fa_assoc; _} =
    field_attributes ~key:name pcd.pcd_attributes in
  let name_js = field_name ~case fa_key in
  let desc, record = match pcd.pcd_args with
    | Pcstr_tuple l ->
      type_of_core ~loc ~params ~name ?meth_callback:fa_meth_cb ?callback:fa_cb
        ?array_tup:fa_array ?number:fa_number ?assoc:fa_assoc (ptyp_tuple ~loc l)
    | Pcstr_record l ->
      let name = "_" ^ pcd.pcd_name.txt ^ _jsoo in
      debug ~v:2 "\tadd record class type %s" name;
      let fields, acc = map_acc0 (field_of_label_declaration ~loc ~params) l in
      let expr = pcty_signature ~loc @@ class_signature ~self:(ptyp_any ~loc) ~fields in
      let ct = class_infos ~loc
          ~virt:Concrete ~params ~name:(mkl ~loc name) ~expr in
      CT (typ_constr ~loc name (core_of_param params)), acc @ [ ct ] in
  let c = js_monad ~loc desc in
  let c = match case, fa_prop with
    | true, _ -> case_prop ~loc c
    | _, None -> typ_constr ~loc (js_mod "optdef") [c]
    | _, Some prop -> typ_constr ~loc (js_mod prop) [typ_constr ~loc (js_mod "optdef") [c]] in
  pctf_method ~loc (mkl ~loc name_js, Public, Concrete, c), record

let rec declaration_of_manifest ?(case=false) ~loc ~params ~name c =
  let {fa_meth_cb; fa_cb; fa_opt; fa_ignore; fa_array; fa_number; fa_assoc; _} =
    field_attributes ~key:name c.ptyp_attributes in
  if fa_ignore then TT c, []
  else
    match c.ptyp_desc with
    | Ptyp_constr ({txt; _}, [c]) when Longident.name txt = "ref" ->
      declaration_of_manifest ~loc ~params ~name c
    | Ptyp_constr ({txt; _}, l) ->
      let name = Longident.name txt in
      debug ~v:2 "\tmanifest constr %s" name;
      let s_js = ml_type_to_class_type name ?number:fa_number l in
      let fields, acc = map_acc0
          (monad_of_core ~loc ~params ~name ?opt:fa_opt ?array_tup:fa_array ?assoc:fa_assoc) l in
      fold_ctt ?number:fa_number name fields
        ~ctt:(fun fields -> ctyp_constr ~loc s_js fields)
        ~tt:(fun fields -> typ_constr ~loc s_js fields), acc
    | Ptyp_tuple [] -> TT (typ_constr ~loc "unit" []), []
    | Ptyp_tuple [h] -> declaration_of_manifest ~loc ~params ~name h
    | Ptyp_tuple l ->
      debug ~v:2 "\tmanifest tuple";
      if fa_array = Some true then
        CT (ctyp_constr ~loc (js_mod "js_array") [typ_constr ~loc (js_mod "Unsafe.any") []]), []
      else
        let c, acc = tuple_type ~loc ~params ~name l in
        CT c, acc
    | Ptyp_arrow _ ->
      debug ~v:2 "\tmanifest arrow";
      let c, acc = monad_of_core ~loc ~params ~name ?meth_callback:fa_meth_cb ?callback:fa_cb ?opt:fa_opt c in
      TT c, acc
    | Ptyp_variant (l, _, _) ->
      debug ~v:2 "\tmanifest variant";
      let c, acc = variant_type ~case ~loc ~params ~name l in
      CT c, acc
    | _ -> Location.raise_errorf ~loc "cannot derive jsoo of this manifest"

let declaration_of_type_kind ?(case=false) ~loc t =
  let name, params = t.ptype_name.txt, t.ptype_params in
  match t.ptype_kind, t.ptype_manifest with
  | Ptype_abstract, None -> Location.raise_errorf ~loc "cannot derive jsoo of abstract type"
  | Ptype_abstract, Some manifest ->
    debug ~v:2 "\tjsoo type from manifest";
    declaration_of_manifest ~case ~loc ~params ~name manifest
  | Ptype_open, _ -> Location.raise_errorf ~loc "cannot derive jsoo of open type"
  | Ptype_variant l, _ ->
    debug ~v:2 "\tjsoo type from variant";
    let fields, acc = map_acc0 (field_of_constructor_declaration ~case ~loc ~params ~name) l in
    CT (pcty_signature ~loc @@ class_signature ~self:(ptyp_any ~loc) ~fields), acc
  | Ptype_record l, _ ->
    debug ~v:2 "\tjsoo type from record";
    let fields, acc = map_acc0 (field_of_label_declaration  ~loc ~params) l in
    CT (pcty_signature ~loc @@ class_signature ~self:(ptyp_any ~loc) ~fields), acc

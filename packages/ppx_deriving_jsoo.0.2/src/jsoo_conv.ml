open Ppxlib
open Ast_builder.Default
open Common

let esome ~loc e =
  pexp_construct ~loc (mkl ~loc @@ Lident "Some") (Some (e ~loc))
let psome ~loc e =
  ppat_construct ~loc (mkl ~loc @@ Lident "Some") (Some (e ~loc))

let pexp_fun ~loc p e = pexp_fun ~loc Nolabel None p e

let i_project ~loc n i =
  pexp_fun ~loc (
    ppat_tuple ~loc (List.init n (fun j -> if j = i then pvar ~loc "x" else ppat_any ~loc)))
    (evar ~loc "x")

let pexp_fun0 ~loc v expr =
  pexp_fun ~loc (pvar ~loc v) expr

let id_fun ?typ loc =
  let pat = match typ with
    | None -> pvar ~loc "x"
    | Some t -> ppat_constraint ~loc (pvar ~loc "x") (
        ptyp_constr ~loc (llid ~loc t) []) in
  pexp_fun ~loc pat (evar ~loc "x")

let rec pexp_fun_rec ~loc expr = function
  | [] -> expr
  | h :: t -> pexp_fun0 ~loc h (pexp_fun_rec ~loc expr t)

let pexp_funs ~loc n expr =
  let l = List.init n (fun _ -> new_var ()) in
  let rec aux = function
    | [] -> expr (List.map (evar ~loc) l)
    | h :: t -> pexp_fun0 ~loc h (aux t) in
  aux l

let rec eapplys_expr ~loc l var = match l with
  | [] -> var
  | h :: t -> eapply ~loc h [eapplys_expr ~loc t var]

let rec eapplys ~loc l var = match l with
  | [] -> var
  | h :: t -> eapply ~loc (evar ~loc h) [eapplys ~loc t var]

let pexp_fun1 ~loc expr =
  let var = new_var () in
  pexp_fun0 ~loc var (expr ~loc (evar ~loc var))

let param_name ~loc p = match p.ptyp_desc with
  | Ptyp_var x -> x
  | _ -> Location.raise_errorf ~loc "parameter is not a variable"

let rec add_params_fun ~loc expr = function
  | [] -> expr
  | ({ptyp_desc = Ptyp_var x; _}, _) :: t ->
    pexp_fun ~loc (ppat_tuple ~loc [
        pvar ~loc ("_" ^ x ^ _to_jsoo); pvar ~loc ("_" ^ x ^ _of_jsoo)])
      (add_params_fun expr ~loc t)
  | _ :: t -> add_params_fun ~loc expr t

type ('a, 'b) conv0 = {e_to : 'a; e_of : 'b; e_acc : (conv * (string * string)) list}
and conv = (expression, expression) conv0

let mconv ?(acc=[]) eto eof = {e_of=eof; e_to=eto; e_acc = acc}

let conv_map f l =
  let _, l_to, l_of, l_acc = List.fold_left (fun (i, acc_to, acc_of, acc) x ->
      let cv = f i x in
      i+1, cv.e_to :: acc_to, cv.e_of :: acc_of, cv.e_acc :: acc) (0, [],[],[]) l in
  List.rev l_to, List.rev l_of, List.flatten (List.rev l_acc)

let ml_type_to_conv ~loc ?(opt="optdef") ?(number=false) s l = match s, l with
  | "unit", [] -> mconv (pexp_fun0 ~loc "_" (eunit ~loc)) (pexp_fun0 ~loc "_" (eunit ~loc))
  | "int", [] | "Int.t", [] ->
    if number then
      mconv
        (pexp_fun1 ~loc (eapplys [js_mod "number_of_float"; "float_of_int"]))
        (pexp_fun1 ~loc (eapplys ["int_of_float"; js_mod "float_of_number"]))
    else
      mconv (id_fun ~typ:"int" loc) (id_fun ~typ:"int" loc)
  | "int32", [] | "Int32.t", [] ->
    if number then
      mconv
        (pexp_fun1 ~loc (eapplys [js_mod "number_of_float"; "Int32.to_float"]))
        (pexp_fun1 ~loc (eapplys ["Int32.of_float"; js_mod "float_of_number"]))
    else
      mconv (evar ~loc "Int32.to_int") (evar ~loc "Int32.of_int")
  | "int64", [] | "Int64.t", [] ->
    mconv
      (evar ~loc (js_mod "BigInt.of_int64"))
      (pexp_fun1 ~loc (eapplys ["Int64.of_string"; js_mod "BigInt.to_string"]))
  | "nativeint", [] | "Nativeint.t", [] ->
    mconv (evar ~loc (js_mod "BigInt.of_native"))
      (pexp_fun1 ~loc (eapplys ["Nativeint.of_string"; js_mod "BigInt.to_string"]))
  | "float", [] | "Float.t", [] ->
    mconv (evar ~loc (js_mod "number_of_float")) (evar ~loc (js_mod "float_of_number"))
  | "string", [] | "String.t", [] ->
    mconv (evar ~loc (js_mod "string")) (evar ~loc (js_mod "to_string"))
  | "char", [] | "Char.t", [] ->
    mconv
      (pexp_fun1 ~loc (eapplys_expr [
           evar ~loc (js_mod "string");
           eapply ~loc (evar ~loc "String.make") [eint ~loc 0]]))
      (pexp_fun1 ~loc (fun ~loc var -> eapply ~loc (eapplys ~loc ["String.get"; js_mod "to_string"] var) [
           eint ~loc 0]))
  | "bytes", [] | "Bytes.t", [] ->
    mconv
      (pexp_fun1 ~loc (eapplys [jsoo_mod "Typed_array.Bigstring.to_arrayBuffer"; "Bigstring.of_bytes"]))
      (pexp_fun1 ~loc (eapplys ["Bigstring.to_bytes"; jsoo_mod "Typed_array.Bigstring.of_arrayBuffer"]))
  | "array", [_]  | "Array.t", [_] ->
    mconv
      (pexp_fun1 ~loc (eapplys [js_mod "of_arrayf"; "fst"]))
      (pexp_fun1 ~loc (eapplys [js_mod "to_arrayf"; "snd"]))
  | "list", [_] | "List.t", [_] ->
    mconv
      (pexp_fun1 ~loc (eapplys [js_mod "of_listf"; "fst"]))
      (pexp_fun1 ~loc (eapplys [js_mod "to_listf"; "snd"]))
  | "option", [_] | "Option.t", [_] ->
    mconv
      (pexp_fun1 ~loc (eapplys [js_mod opt; "fst"]))
      (pexp_fun1 ~loc (eapplys [js_mod ("to_" ^ opt); "snd"]))
  | "Unsafe.any", [] | "any", [] | "Js.Unsafe.any", [] | "Js_of_ocaml.Js.Unsafe.any", []
    -> mconv (id_fun ~typ:(js_mod "Unsafe.any") loc) (id_fun ~typ:(js_mod "Unsafe.any") loc)
  | "ref", [_] ->
    mconv
      (pexp_fun ~loc (ppat_tuple ~loc [pvar ~loc "f"; ppat_any ~loc])
         (pexp_fun0 ~loc "x" (eapply ~loc (evar ~loc "f") [
              pexp_field ~loc (evar ~loc "x") (llid ~loc "contents")])))
      (pexp_fun ~loc (ppat_tuple ~loc [ppat_any ~loc; pvar ~loc "f"])
         (pexp_fun0 ~loc "x" (eapply ~loc (evar ~loc "ref") [
              eapply ~loc (evar ~loc "f") [(evar ~loc "x")] ])))
  | "Lazy.t", [_] ->
    mconv
      (pexp_fun ~loc (ppat_tuple ~loc [pvar ~loc "f"; ppat_any ~loc])
         (pexp_fun0 ~loc "x" (eapply ~loc (evar ~loc "f") [
              eapply ~loc (evar ~loc "Lazy.force") [(evar ~loc "x")] ])))
      (pexp_fun ~loc (ppat_tuple ~loc [ppat_any ~loc; pvar ~loc "f"])
         (pexp_fun0 ~loc "x" (eapply ~loc (evar ~loc "Lazy.from_val") [
              eapply ~loc (evar ~loc "f") [(evar ~loc "x")] ])))
  | _ ->
    let to_jsoo, of_jsoo =
      if s = "t" then evar ~loc to_jsoo, evar ~loc of_jsoo
      else evar ~loc (s ^ "_" ^ to_jsoo), evar ~loc (s ^ "_" ^ of_jsoo) in
    mconv to_jsoo of_jsoo

let arg_array ~loc l = pexp_array ~loc l

let add_expr0 ~loc ~name (e_to, e_of) =
  let name_to, name_of = jsoo_name_to name, jsoo_name_of name in
  let v_to = value_binding ~loc ~pat:(pvar ~loc name_to) ~expr:e_to in
  let v_of = value_binding ~loc ~pat:(pvar ~loc name_of) ~expr:e_of in
  [v_to; v_of], (name_to, name_of)

let acc_expr ~loc ~name ~params cv =
  let name_to, name_of = jsoo_name_to name, jsoo_name_of name in
  let acc = cv.e_acc in
  let cv = mconv
      (add_params_fun ~loc cv.e_to params) (add_params_fun ~loc cv.e_of params) in
  let acc = acc @ [cv, (name_to, name_of)] in
  let e_to, e_of = match params with
    | [] -> evar ~loc name_to, evar ~loc name_of
    | _ ->
      let params = List.map (fun c ->
            let s = param_name ~loc c in
            pexp_tuple ~loc [evar ~loc ("_" ^ s ^ _to_jsoo); evar ~loc ("_" ^ s ^ _of_jsoo)])
          (Jsoo_type.core_of_param params) in
      eapply ~loc (evar ~loc name_to) params,
      eapply ~loc (evar ~loc name_of) params in
  mconv ~acc e_to e_of

type remember = (
  ?obj_to:expression ->
  expression ->
  add:value_binding list ->
  ([ `RCase of string loc * expression
   | `RVal of string loc * [ `Case | `Readonly | `Readwrite | `Writeonly | `Optdef ] *
              override_flag * expression ],
   expression -> expression) conv0) list * string list

let remember_table : (string, remember) Hashtbl.t =
  Hashtbl.create 1024

let remember_fields id fields =
  Hashtbl.add remember_table id fields

let get_remember_fields id = Hashtbl.find_opt remember_table id

module FDMap = Map.Make(String)

let partition_rows l =
  snd @@ List.split @@ FDMap.bindings @@
  List.fold_left (fun acc r -> match r with
      | `RVal (id, k, b, e) -> FDMap.add (Ppx_js.unescape id.txt) (Ppx_js.Val (id, k, b, e)) acc
      | `RCase (id,e) -> let txt = Ppx_js.unescape id.txt in
        match FDMap.find_opt txt acc with
        | Some (Ppx_js.Cases (ids, es)) ->
          FDMap.add txt (Ppx_js.Cases (ids @ [id], es @ [e])) acc
        | _ -> FDMap.add txt (Ppx_js.Cases ([id], [e])) acc) FDMap.empty l

let rec expr_of_core ~loc ~params ~name ?opt ?meth_callback ?callback ?array_tup ?number ?assoc c =
  let {ca_opt; ca_ignore; ca_number; ca_array; ca_cb; ca_meth_cb; ca_assoc; ca_case; _} =
    core_attributes ?opt ?meth_callback ?callback ?array_tup ?number ?assoc c.ptyp_attributes in
  if ca_ignore then mconv (id_fun loc) (id_fun loc)
  else match c.ptyp_desc with
    | Ptyp_any -> mconv (id_fun loc) (id_fun loc)
    | Ptyp_var v ->
      mconv (evar ~loc ("_" ^ v ^ "_" ^ to_jsoo)) (evar ~loc ("_" ^ v ^ "_" ^ of_jsoo))
    | Ptyp_constr ({txt; _}, l) ->
      let type_name = Longident.name txt in
      begin match Jsoo_type.assoc_type ?assoc:ca_assoc type_name l with
        | None ->
          let cv = ml_type_to_conv ~loc ?opt:ca_opt ?number:ca_number type_name l in
          let v, acc = List.split @@ List.map (fun c ->
              let cv = expr_of_core ~loc ~params ~name c in
              pexp_tuple ~loc [cv.e_to; cv.e_of], cv.e_acc) l in
          mconv ~acc:(List.flatten acc) (eapply ~loc cv.e_to v) (eapply ~loc cv.e_of v)
        | Some c ->
          let cv = expr_of_core ~loc ~params ~name c in
          mconv ~acc:cv.e_acc
            (eapply ~loc (evar ~loc (js_mod "Table.makef")) [cv.e_to])
            (eapply ~loc (evar ~loc (js_mod "Table.itemsf")) [cv.e_of])
      end
    | Ptyp_arrow (_, c1, c2) ->
      function_expr ~loc ~params ~name ?meth_callback:ca_meth_cb ?callback:ca_cb (c1, c2)
    | Ptyp_tuple l ->
      if ca_array = Some true then
        array_tuple_expr ~loc ~params ~name l
      else
        let cv1 = tuple_expr ~loc ~params ~name l in
        let tuple_name = get_tuple_name name in
        acc_expr ~loc ~name:tuple_name ~params cv1
    | Ptyp_variant (l, _, _) ->
      let cv1 = variant_expr ~case:ca_case ~loc ~params ~name l in
      let variant_name = get_variant_name name in
      acc_expr ~loc ~name:variant_name ~params cv1
    | _ -> Location.raise_errorf ~loc "core type not handled (only _, 'a, constr, arrow and tuple)"

and arrows_to_array ~loc ~params ~name ?opt ?(meth_start=false) ?(callback_start=false) c =
  match c.ptyp_desc with
  | Ptyp_arrow (_a, c1, c2) ->
    let v = Ppx_js.Arg.make1 () in
    let cv = expr_of_core ~loc ~params ~name ?opt c1 in
    let es_to, es_of, vs_to, vs_of, e_to_end, e_of_end, acc =
      arrows_to_array ~loc ~params ~name ?opt c2 in
    begin match meth_start, callback_start, c1 with
      | _, true, {ptyp_desc = Ptyp_constr ({txt; _}, _); _} when Longident.name txt = "unit" ->
        es_to, eapply ~loc cv.e_of [evar ~loc (Ppx_js.Arg.name v)] :: es_of,
        v :: vs_to, v :: vs_of, e_to_end, e_of_end, acc @ cv.e_acc
      | true, _, {ptyp_desc = Ptyp_constr ({txt; _}, _); _} when Longident.name txt = "unit" ->
        es_to, eunit ~loc :: es_of,
        v :: vs_to, vs_of, e_to_end, e_of_end, acc @ cv.e_acc
      | _ ->
        eapply ~loc cv.e_to [evar ~loc (Ppx_js.Arg.name v)] :: es_to,
        eapply ~loc cv.e_of [evar ~loc (Ppx_js.Arg.name v)] :: es_of,
        v :: vs_to, v :: vs_of, e_to_end, e_of_end, acc @ cv.e_acc
    end
  | _ ->
    let cv = expr_of_core ~loc ~params ~name ?opt c in
    [], [], [], [], cv.e_to, cv.e_of, cv.e_acc

and callback_expr ~loc ~params ~name ?opt (c1, c2) =
  let cv1 = expr_of_core ~loc ~params ~name c1 in
  let cv2 = expr_of_core ~loc ~params ~name c2 in
  let f, x = new_var (), new_var () in
  let e_to = pexp_fun0 ~loc f (eapply ~loc (evar ~loc (js_mod "wrap_callback")) [
      pexp_fun0 ~loc x (eapply ~loc cv2.e_to [
          eapply ~loc (evar ~loc f) [eapply ~loc cv1.e_of [evar ~loc x]]])
    ]) in
  let es_to, _, vs_to, _, _, e_of, acc =
    arrows_to_array ~loc ~params ~name ?opt ~callback_start:true (ptyp_arrow ~loc Nolabel c1 c2) in
  let es_to = List.map (fun v -> eapply ~loc (evar ~loc (js_mod "Unsafe.inject")) [v]) es_to in
  let vs = List.map Ppx_js.Arg.name vs_to in
  let e_of =
    pexp_fun0 ~loc f (pexp_fun_rec ~loc (eapply ~loc e_of [
        eapply ~loc (evar ~loc (js_mod "Unsafe.fun_call")) [
          evar ~loc f; arg_array ~loc es_to]])
        vs) in
  mconv ~acc:(acc @ cv1.e_acc @ cv2.e_acc) e_to e_of

and meth_callback_expr ~loc ~params ~name ?opt (c1, c2) =
  let cv1 = expr_of_core ~loc ~params ~name c1 in
  let cv2 = expr_of_core ~loc ~params ~name c2 in
  let f, x = new_var (), new_var () in
  let e_to = pexp_fun0 ~loc f (eapply ~loc (evar ~loc (js_mod "wrap_meth_callback")) [
      pexp_fun0 ~loc x (eapply ~loc cv2.e_to [
          eapply ~loc (evar ~loc f) [eapply ~loc cv1.e_of [evar ~loc x]]])
    ]) in
  let es_to, _, vs_to, _, _, e_of, acc =
    arrows_to_array ~loc ~params ~name ?opt (ptyp_arrow ~loc Nolabel c1 c2) in
  let es_to = List.map (fun v -> eapply ~loc (evar ~loc (js_mod "Unsafe.inject")) [v]) es_to in
  let vs = List.map Ppx_js.Arg.name vs_to in
  let this_to, es_to = match es_to with [] -> assert false | h :: t -> h, t in
  let e_of =
    pexp_fun0 ~loc f (pexp_fun_rec ~loc (eapply ~loc e_of [
        eapply ~loc (evar ~loc (js_mod "Unsafe.call")) [
          evar ~loc f; this_to; arg_array ~loc es_to]])
        vs) in
  mconv ~acc:(acc @ cv1.e_acc @ cv2.e_acc) e_to e_of

and function_expr ~loc ~params ~name ?opt ?(meth_callback=false) ?(callback=false) (c1, c2) =
  if callback then callback_expr ~loc ~params ~name ?opt (c1, c2)
  else if meth_callback then meth_callback_expr ~loc ~params ~name ?opt (c1, c2)
  else
    let cv1 = expr_of_core ~loc ~params ~name c1 in
    let cv2 = expr_of_core ~loc ~params ~name c2 in
    let f, x = new_var (), new_var () in
    mconv ~acc:(cv1.e_acc @ cv2.e_acc)
      (pexp_fun0 ~loc f (pexp_fun0 ~loc x (eapply ~loc cv2.e_to [
           eapply ~loc (evar ~loc f) [eapply ~loc cv1.e_of [evar ~loc x]]])))
      (pexp_fun0 ~loc f (pexp_fun0 ~loc x (eapply ~loc cv2.e_of [
           eapply ~loc (evar ~loc f) [eapply ~loc cv1.e_to [evar ~loc x]]])))

and tuple_expr ~loc ~params ~name ?callback ?meth_callback ?number ?assoc = function
  | [] -> mconv (pexp_fun0 ~loc "_" (eunit ~loc)) (pexp_fun0 ~loc "_" (eunit ~loc))
  | [h] -> expr_of_core ~loc ~params ~name ?callback ?meth_callback ?number ?assoc h
  | l ->
    let n = List.length l in
    let obj = new_var () in
    let fields_to, fields_of, acc = conv_map (fun i c ->
        let cv = expr_of_core ~loc ~params ~name c in
        let name_js = "_" ^ string_of_int i in
        mconv ~acc:cv.e_acc
          (Ppx_js.Val (mkl ~loc name_js, `Readonly, Fresh, eapply ~loc cv.e_to [
               eapply ~loc (i_project ~loc n i) [evar ~loc obj] ]))
          (eapply ~loc cv.e_of [Ppx_js.prop_get ~loc (evar ~loc obj) name_js]))
        l in
    mconv ~acc
      (pexp_fun0 ~loc obj @@ Ppx_js.literal_object (pvar ~loc "this") fields_to)
      (pexp_fun0 ~loc obj @@ pexp_tuple ~loc fields_of)

and array_tuple_expr ~loc ~params ~name ?number l =
  let es_to, es_of, acc = conv_map (fun _ c -> expr_of_core ~loc ~params ~name ?number c) l in
  let e_to =
    let vs = List.init (List.length l) (fun _ -> new_var ()) in
    pexp_fun ~loc (ppat_tuple ~loc (List.map (pvar ~loc) vs)) @@
    eapply ~loc (evar ~loc (js_mod "array")) [
      pexp_array ~loc (List.map2 (fun v e_to ->
          eapply ~loc (evar ~loc (js_mod "Unsafe.inject")) [
            eapply ~loc e_to [evar ~loc v]]) vs es_to)] in
  let e_of =
    let a = new_var () in
    pexp_fun ~loc (pvar ~loc a) @@
    pexp_let ~loc Nonrecursive [
      value_binding ~loc ~pat:(pvar ~loc a)
        ~expr:(eapply ~loc (evar ~loc (js_mod "to_array")) [evar ~loc a])] @@
    pexp_tuple ~loc (List.mapi (fun i e_of ->
        eapply ~loc e_of [
          eapply ~loc (evar ~loc (js_mod "Unsafe.coerce")) [
            eapply ~loc (evar ~loc "Array.get") [ evar ~loc a; eint ~loc i ] ] ]) es_of) in
  mconv ~acc e_to e_of

and case_expr ?(case_=false) ?obj_to ~loc
    ~name ?name_js
    ~add ?prop ?(kind=`Construct) ?(local=false)
    (obj : expression) cv var =
  let obj_to = match obj_to with None -> obj | Some o -> o in
  let name_js = match name_js with None -> field_name ~case:case_ name | Some n -> n in
  let lhs_to = match kind with
    | `Construct ->
      if local then
        ppat_alias ~loc (
          ppat_construct ~loc (llid ~loc name)
            (Option.map (fun _ -> ppat_any ~loc) var))
          (mkl ~loc (Option.get var))
      else
        ppat_construct ~loc (llid ~loc name)
          (Option.map (pvar ~loc) var)
    | `Variant -> ppat_variant ~loc name (Option.map (pvar ~loc) var) in
  let rhs_of = match kind with
    | `Construct ->
      pexp_construct ~loc (llid ~loc name) (Option.map (evar ~loc) var)
    | `Variant -> pexp_variant ~loc name (Option.map (evar ~loc) var) in
  let e_to = pexp_match ~loc obj_to [
      case ~guard:None
        ~lhs:lhs_to
        ~rhs:(eapply ~loc (evar ~loc (js_mod "def")) [
            eapply ~loc cv.e_to [
              Option.fold ~none:(eunit ~loc) ~some:(fun v -> evar ~loc v) var ] ]);
      case ~guard:None
        ~lhs:(ppat_any ~loc) ~rhs:(evar ~loc (js_mod "undefined"))
    ] in
  let e_of expr = pexp_match ~loc (eapply ~loc (evar ~loc (js_mod "Optdef.to_option")) [
      Ppx_js.prop_try ~loc obj name_js cv.e_of]) [
      case ~guard:None
        ~lhs:(psome ~loc (Option.fold ~none:ppat_any ~some:(fun v ~loc ->
            if local then ppat_construct ~loc
                (llid ~loc name) (Some (pvar ~loc v))
            else pvar ~loc v) var))
        ~rhs:rhs_of;
      case ~guard:None ~lhs:(ppat_any ~loc) ~rhs:expr;
    ] in
  let e_to, e_of = match add with [] -> e_to, e_of | _ ->
    pexp_let ~loc Nonrecursive add e_to, (fun expr -> pexp_let ~loc Nonrecursive add (e_of expr)) in
  if case_ then
    mconv ~acc:cv.e_acc (`RCase (mkl ~loc name_js, e_to)) e_of
  else
    mconv ~acc:cv.e_acc (`RVal (mkl ~loc name_js, prop_kind prop, Fresh, e_to)) e_of

and row_expr ?(case=false) ~loc ~params ~name rf =
  let c_name =
    if case then (fun txt -> field_name ~case name ^ "_" ^ txt)
    else (fun txt -> field_name ~case txt) in
  match rf.prf_desc with
  | Rtag ({txt; _}, _, []) ->
    let cv = mconv (pexp_fun0 ~loc "_" (eunit ~loc)) (pexp_fun0 ~loc "_" (eunit ~loc)) in
    let name_js = c_name txt in
    [ fun ?obj_to obj -> case_expr ~case_:case ~loc ~name:txt ~name_js
        ~kind:`Variant ?obj_to obj cv None ]
  | Rtag ({txt; _}, _, (h :: _)) ->
    let name_js = c_name txt in
    let cv = expr_of_core ~loc ~params ~name h in
    [ fun ?obj_to obj -> case_expr ~case_:case ~loc ~name:txt ~name_js ~kind:`Variant ?obj_to obj cv
        (Some (new_var ())) ]
  | Rinherit c ->  match c.ptyp_desc with
    | Ptyp_constr ({txt; _}, l) ->
      begin match get_remember_fields (Longident.name txt) with
        | Some (fs, ps) ->
          let conv, _acc_expr = List.split @@ List.map2 (fun v c ->
              let cv = expr_of_core ~loc ~params ~name c in
              let l, _ = add_expr0 ~loc ~name:("_" ^ v) (cv.e_to, cv.e_of) in
              l, cv.e_acc)
              ps l in
          let param_exprs = List.flatten conv in
          let fs = List.map (fun f -> (fun ?obj_to obj ~add:_  ->
              f ?obj_to obj ~add:param_exprs)) fs in
          fs
        | _ -> []
      end
    | _ -> Location.raise_errorf ~loc "Inherit type not handled"

and variant_expr0 ?case ?(remember=true) ?obj_to ~loc ~params ~name (obj :expression) l =
  let l =
    List.fold_left (fun acc rf -> row_expr ?case ~loc ~params ~name rf @ acc) [] l in
  (if remember then
      remember_fields name (l, List.map (param_name ~loc) (Jsoo_type.core_of_param params)));
  let e_to, e_of, acc = conv_map (fun _ e -> e ?obj_to obj ~add:[]) l in
  let rec aux = function
    | [] ->
      eapply ~loc (evar ~loc "failwith") [ estring ~loc "no case matched" ]
    | h :: t -> h (aux t) in
  let e_to = partition_rows e_to in
  let e_of = List.rev e_of in
  mconv ~acc e_to (aux e_of)

and variant_expr ?case ~loc ~params ~name l =
  let obj_str = new_var () in
  let obj = evar ~loc obj_str in
  let cv = variant_expr0 ?case ~loc ~params ~name obj l in
  mconv ~acc:cv.e_acc
    (pexp_fun0 ~loc obj_str @@ Ppx_js.literal_object (pvar ~loc "this") cv.e_to)
    (pexp_fun0 ~loc obj_str cv.e_of)

let field_of_label_declaration ~loc ~params obj pld =
  let name = field_name pld.pld_name.txt in
  debug ~v:2 "\t\tlabel field %s" name;
  let {fa_meth; fa_meth_cb; fa_cb; fa_prop; fa_opt; fa_key; fa_ignore; fa_array;
       fa_number; fa_assoc; fa_case; _} =
    field_attributes ~key:name pld.pld_attributes in
  let name_js = field_name fa_key in
  let obj = evar ~loc obj in
  if not fa_meth then
    match fa_case, pld.pld_type.ptyp_desc with
    | true, Ptyp_variant (l, _, _) ->
      let obj_to = pexp_field ~loc obj (llid ~loc name) in
      let cv = variant_expr0 ~case:true ~remember:false ~obj_to ~loc ~params ~name obj l in
      begin match cv.e_to with
        | [ Ppx_js.Cases _ as c ] ->
          true, mconv ~acc:cv.e_acc c (llid ~loc name, cv.e_of)
        | _ -> Location.raise_errorf ~loc "failed to construct cases from variant"
      end
    | _ ->
      let cv =
        if fa_ignore then mconv (id_fun loc) (id_fun loc)
        else expr_of_core ~loc ~params ~name ?opt:fa_opt ?callback:fa_cb ?meth_callback:fa_meth_cb
            ?array_tup:fa_array ?number:fa_number ?assoc:fa_assoc pld.pld_type in
      let prop = match pld.pld_type.ptyp_desc with
        | Ptyp_constr ({txt; _}, _) when Longident.name txt = "ref" -> Some "prop"
        | _ -> fa_prop in
      false, mconv ~acc:cv.e_acc
        (Ppx_js.Val (mkl ~loc name_js, prop_kind prop, Fresh, eapply ~loc cv.e_to [
             pexp_field ~loc obj (llid ~loc name) ]))
        (mkl ~loc @@ Longident.parse name,
                eapply ~loc cv.e_of [Ppx_js.prop_get ~loc obj name_js])
  else
    let es_to, es_of, vs_to, vs_of, e_to, e_of, acc =
      arrows_to_array ~loc ~params ~name ~meth_start:true pld.pld_type in
    let vs2_of = List.map Ppx_js.Arg.name vs_of in
    let vs2_to = List.map Ppx_js.Arg.name vs_to in
    false, mconv ~acc
      (Ppx_js.Meth (
          mkl ~loc name_js, Public, Fresh,
          pexp_fun_rec ~loc (eapply ~loc e_to [
              eapply ~loc (
                pexp_field ~loc obj (llid ~loc name))
                es_of ]) vs2_of, vs_of))
      (llid ~loc name,
       pexp_fun_rec ~loc (
         eapply ~loc e_of [
           Ppx_js.method_call ~loc ~apply_loc:loc obj (name_js, loc)
             (List.map (fun e_to -> Nolabel, e_to) es_to)]) vs2_to)

let record_expr ~loc ~params ?local l =
  let obj = new_var () in
  let fields_to, fields_of, acc, _allow_overload =
    let l_to, l_of, l_acc, case = List.fold_left (fun (acc_to, acc_of, acc, acc_case) c ->
        let case, cv = field_of_label_declaration ~loc ~params obj c in
        cv.e_to :: acc_to, cv.e_of :: acc_of, cv.e_acc :: acc, case || acc_case)
        ([],[],[], false) l in
    List.rev l_to, List.rev l_of, List.flatten (List.rev l_acc), case in
  let e_to = match local with
    | None -> pexp_fun ~loc (pvar ~loc obj) @@
      Ppx_js.literal_object (pvar ~loc "this") fields_to
    | Some cs -> pexp_function ~loc [
        case ~guard:None
          ~lhs:(ppat_construct ~loc (llid ~loc cs) (Some (pvar ~loc obj)))
          ~rhs:(Ppx_js.literal_object (pvar ~loc "this") fields_to);
        case ~guard:None ~lhs:(ppat_any ~loc)
          ~rhs:(eapply ~loc (evar ~loc "failwith") [estring ~loc "wrong local record construction"])
      ] in
  mconv ~acc
    e_to
    (match local with
     | None -> pexp_fun0 ~loc obj @@ pexp_record ~loc fields_of None
     | Some cs -> pexp_fun0 ~loc obj @@ pexp_construct ~loc (llid ~loc cs)
         (Some (pexp_record ~loc fields_of None)))

let field_of_constructor_declaration ?(case=false) ~loc ~params ~name (obj : expression) pcd =
  (match pcd.pcd_res with
   | Some _ -> Location.raise_errorf ~loc "GADT not handled"
   | None -> ());
  let name_cs = pcd.pcd_name.txt in
  let {fa_meth_cb; fa_cb; fa_prop; fa_key; fa_array; fa_number; fa_assoc; _} =
    field_attributes ~key:name_cs pcd.pcd_attributes in
  let name_js = if case then field_name ~case (name ^ "_" ^ fa_key) else field_name ~case fa_key in
  debug ~v:2 "\t\tconstructor field %s" name;
  let cv, var, local = match pcd.pcd_args with
    | Pcstr_tuple [] -> mconv (id_fun loc) (id_fun loc), None, false
    | Pcstr_tuple [c] ->
      let cv = expr_of_core ~loc ~name ~params ?callback:fa_cb
          ?meth_callback:fa_meth_cb ?number:fa_number ?assoc:fa_assoc c in
      cv, Some (new_var ()), false
    | Pcstr_tuple l ->
      let var = if l = [] then None else Some (new_var ()) in
      if fa_array = Some true then
        let cv = array_tuple_expr ~loc ~params ~name l in
        cv, var, false
      else
        let cv = tuple_expr ~loc ~name ~params ?callback:fa_cb
            ?meth_callback:fa_meth_cb ?number:fa_number ?assoc:fa_assoc l in
        let tuple_name = get_tuple_name name in
        let cv = acc_expr ~loc ~name:tuple_name ~params cv in
        cv, var, false
    | Pcstr_record l ->
      let name = "_" ^ pcd.pcd_name.txt ^ _jsoo in
      debug ~v:2 "\tadd record convs %s" name;
      let cv = record_expr ~loc ~params ~local:pcd.pcd_name.txt l in
      let cv = acc_expr ~loc ~name ~params cv in
      cv, Some (new_var ()), true in
  case_expr
    ~case_:case ~loc
    ~name:name_cs ~name_js
    ~add:[] ?prop:fa_prop ~local
    obj cv var

let constructor_variant_expr ?case ~loc ~params ~name l =
  let obj_str = new_var () in
  let obj = evar ~loc obj_str in
  let rec aux = function
    | [] ->
      eapply ~loc (evar ~loc "failwith") [ estring ~loc "no case matched" ]
    | h :: t -> h (aux t) in
  let fields_to, fields_of, acc = conv_map (fun _ c ->
      field_of_constructor_declaration ?case ~loc ~params ~name obj c) l in
  mconv ~acc
    (pexp_fun0 ~loc obj_str @@ Ppx_js.literal_object (pvar ~loc "this")
       (partition_rows fields_to))
    (pexp_fun0 ~loc obj_str @@ aux (List.rev fields_of))

let declaration_of_manifest ?case ~loc ~params ~name c =
  let {fa_meth_cb; fa_cb; fa_opt; fa_array; fa_number; fa_assoc; _} =
    field_attributes ~key:name c.ptyp_attributes in
  match c.ptyp_desc with
  | Ptyp_tuple l ->
    if fa_array = Some true then
      array_tuple_expr ~loc ~params ~name l
    else
      tuple_expr ~loc ~params ~name ?number:fa_number ?assoc:fa_assoc l
  | Ptyp_variant (l, _, _) -> variant_expr ?case ~loc ~params ~name l
  | _ -> expr_of_core ~loc ~params ~name ?opt:fa_opt ?meth_callback:fa_meth_cb
           ?callback:fa_cb ?number:fa_number ?assoc:fa_assoc c

let declaration_of_type_kind ?case ~loc ~name ~params kind manifest = match kind, manifest with
  | Ptype_abstract, None -> Location.raise_errorf ~loc "abstract type"
  | Ptype_open, _ -> Location.raise_errorf ~loc "open type"
  | Ptype_abstract, Some manifest ->
    debug ~v:2 "\tjsoo_conv from manifest";
    declaration_of_manifest ?case ~loc ~params ~name manifest
  | Ptype_variant l, _ ->
    debug ~v:2 "\tjsoo_conv from variant";
    constructor_variant_expr ?case ~loc ~params ~name l
  | Ptype_record l, _ ->
    debug ~v:2 "\tjsoo_conv from record";
    record_expr ~loc ~params l

let conv_expressions ?case ~loc t =
  let name = t.ptype_name.txt in
  let params = t.ptype_params in
  let cv =
    declaration_of_type_kind ?case ~loc ~name ~params t.ptype_kind t.ptype_manifest in
  mconv ~acc:cv.e_acc (add_params_fun ~loc cv.e_to params) (add_params_fun ~loc cv.e_of params)

let conv_signatures ~loc ~is_class_type t =
  let name = t.ptype_name.txt in
  let name_js = jsoo_name name in
  let params = t.ptype_params in
  let rec aux = function
    | [] -> (fun expr -> expr), []
    | ({ptyp_desc = Ptyp_var x; _}, _) :: t ->
      let t_to = ptyp_arrow ~loc Nolabel (ptyp_var ~loc x) (ptyp_var ~loc (x ^ _jsoo)) in
      let t_of = ptyp_arrow ~loc Nolabel (ptyp_var ~loc (x ^ _jsoo)) (ptyp_var ~loc x) in
      let c, vs = aux t in
      (fun expr -> ptyp_arrow ~loc Nolabel (ptyp_tuple ~loc [t_to; t_of]) (c expr)),
      x :: vs
    | _ :: t -> aux t in
  let c, vars = aux params in
  let ml_vars = List.map (fun v -> ptyp_var ~loc v) vars in
  let js_vars = List.map (fun v -> ptyp_var ~loc (v ^ _jsoo)) vars in
  let jsoo_ct_sig = ptyp_constr ~loc (llid ~loc name_js) js_vars in
  let jsoo_sig =
    if is_class_type then ptyp_constr ~loc (llid ~loc (js_mod "t")) [jsoo_ct_sig]
        else jsoo_ct_sig in
  let ml_sig = ptyp_constr ~loc (mkl ~loc @@ Longident.parse name) ml_vars in
  let to_sig = c @@ ptyp_arrow ~loc Nolabel ml_sig jsoo_sig in
  let of_sig = c @@ ptyp_arrow ~loc Nolabel jsoo_sig ml_sig in
  to_sig, of_sig, ptyp_tuple ~loc [to_sig; of_sig]

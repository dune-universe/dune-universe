open Ppxlib
open Ast_builder.Default
open Utils

let case_expr ~loc ?(is_record=false) ?kind ?(typ=`Cons) ~name enc =
  let pconstruct ~loc p = match typ with
    | `Cons -> ppat_construct ~loc (llid ~loc name) p
    | `Variant -> ppat_variant ~loc name p in
  let econstruct ~loc e = match typ with
    | `Cons -> pexp_construct ~loc (llid ~loc name) e
    | `Variant -> pexp_variant ~loc name e in
  let kind_enc = match kind with
    | None -> None
    | Some kind ->
      Some (enc_apply ~loc "obj1" [
          enc_apply ~loc "req" [
            estring ~loc "kind";
            enc_apply ~loc "constant" [ estring ~loc kind ] ] ]) in
  let v = match enc with None -> None | Some _ -> Some (new_var ()) in
  let enc = match kind_enc, enc with
    | None, None -> enc_var ~loc "empty"
    | None, Some enc | Some enc, None -> enc
    | Some kenc, Some enc -> enc_apply ~loc "merge_objs" [kenc; enc] in
  let rhs_to = match is_record, v, kind with
    | true, Some v, None -> esome ~loc (econstruct (Some (evar ~loc v)))
    | true, Some v, Some _ -> esome ~loc (pexp_tuple [
        eunit ~loc;
        econstruct ~loc (Some (evar ~loc v))])
    | _, None, _ -> esome ~loc eunit
    | _, Some v, None -> esome ~loc (evar v)
    | _, Some v, Some _ -> esome ~loc (pexp_tuple [ eunit ~loc; evar ~loc v ]) in
  let construct = pexp_function ~loc [
      case ~guard:None
        ~lhs:(pconstruct ~loc (Option.map (pvar ~loc) v))
        ~rhs:rhs_to;
      case ~guard:None
        ~lhs:(ppat_any ~loc)
        ~rhs:(enone ~loc)
    ] in
  let destruct = match is_record, v, kind with
    | true, Some v, _ ->
      pexp_function ~loc [
        case ~guard:None
          ~lhs:(match kind with
              | None -> pconstruct ~loc (Some (pvar ~loc v))
              | Some _ -> ppat_tuple ~loc [
                  punit ~loc;
                  pconstruct ~loc (Some (pvar ~loc v)) ])
          ~rhs:(econstruct ~loc (Some (evar ~loc v)));
        case ~guard:None
          ~lhs:(ppat_any ~loc)
          ~rhs:(eapply ~loc (evar ~loc "failwith") [ estring ~loc "wrong local record" ])
      ]
    | _, None, _ ->  pexp_fun ~loc (punit ~loc) (econstruct ~loc None)
    | _, Some v, None -> pexp_fun ~loc (pvar ~loc v) (econstruct ~loc (Some (evar ~loc v)))
    | _, Some v, Some _ ->
      pexp_fun ~loc (ppat_tuple ~loc [ punit ~loc; pvar ~loc v ])
        (econstruct ~loc (Some (evar ~loc v))) in
  enc, construct, destruct

let def_expr ~loc ?title ?description ?schema ~name e =
  let describe e =
    let title = Option.fold ~none:(enone ~loc)
        ~some:(fun e -> esome ~loc (fun ~loc:_ -> e)) title in
    let description = Option.fold ~none:(enone ~loc)
        ~some:(fun e -> esome ~loc (fun ~loc:_ -> e)) description in
    pexp_apply ~loc (evar ~loc (enc_mod "def")) [
      Nolabel, estring ~loc name;
      Optional "title", title;
      Optional "description", description;
      Nolabel, e ] in
  let add_schema e schema =
    pexp_apply ~loc (evar ~loc (enc_mod "conv")) [
      Nolabel, pexp_fun ~loc (pvar ~loc "x") (evar ~loc "x");
      Nolabel, pexp_fun ~loc (pvar ~loc "x") (evar ~loc "x");
      Labelled "schema", schema;
      Nolabel, e ] in
  match title, description, schema with
  | None, None , None -> e
  | None, None, Some schema -> add_schema e schema
  | _, _, None -> describe e
  | _, _, Some schema -> describe (add_schema e schema)

let ignore_expr ~loc ?(ign=false) e =
  if not ign then e
  else
    enc_apply ~loc "conv" [
      pexp_fun ~loc (pvar ~loc "x") (pexp_tuple ~loc [evar ~loc "x"; eunit ~loc]);
      pexp_fun ~loc (ppat_tuple ~loc [pvar ~loc "x"; punit ~loc]) (evar ~loc "x");
      enc_apply ~loc "merge_objs" [e; enc_var ~loc "unit"]
    ]

let mu_expr ~loc ?(mu=false) ~name e =
  if not mu then e
  else pexp_fun ~loc (pvar ~loc (enc_name name)) e

let result_expr ~loc ok err =
  enc_apply ~loc "union" [ elist ~loc [
        enc_apply ~loc "case" [
          ok;
          pexp_function ~loc [
            case ~guard:None ~lhs:(ppat_construct ~loc (llid ~loc "Ok") (Some (pvar ~loc "x")))
              ~rhs:(esome ~loc (evar "x"));
            case ~guard:None ~lhs:(ppat_any ~loc) ~rhs:(enone ~loc) ];
          pexp_fun ~loc (pvar ~loc "x") (pexp_construct ~loc (llid ~loc "Ok") (Some (evar ~loc "x")))
        ];
        enc_apply ~loc "case" [
          enc_apply ~loc "obj1" [ enc_apply ~loc "req" [estring ~loc "error"; err] ];
          pexp_function ~loc [
            case ~guard:None ~lhs:(ppat_construct ~loc (llid ~loc "Error") (Some (pvar ~loc "x")))
              ~rhs:(esome ~loc (evar "x"));
            case ~guard:None ~lhs:(ppat_any ~loc) ~rhs:(enone ~loc) ];
          pexp_fun ~loc (pvar ~loc "x") (pexp_construct ~loc (llid ~loc "Error") (Some (evar ~loc "x")))
        ]
      ]
    ]

let rec core ~loc ?opt ?assoc ?enum ?obj ?enc ?obj1 ?option c =
  let {co_assoc; co_enum; co_obj; co_enc; co_obj1; _} =
    core_attrs ?assoc ?enum ?obj ?enc ?obj1 c.ptyp_attributes in
  match co_enc with
  | Some e -> e
  | None ->
    let e = match c.ptyp_desc with
      | Ptyp_any -> enc_var ~loc "any_ezjson_value"
      | Ptyp_var v -> evar ~loc ("_" ^ enc_name v)
      | Ptyp_constr ({txt; _}, l) ->
        constr ~loc ?opt ?assoc:co_assoc ?option (Longident.name txt) l
      | Ptyp_tuple l -> tuple ~loc ?obj:co_obj ?option l
      | Ptyp_variant (l, _, _) -> variant ~loc ?enum:co_enum ?option l
      | Ptyp_object (l, _) -> object_expr ~loc ?option l
      | _ -> raise_error ~loc "not handled" in
    match co_obj1 with
    | None -> e
    | Some f -> enc_apply ~loc "obj1" [enc_apply ~loc "req" [ estring ~loc f; e ] ]

and core_opt ~loc ?option c =
  let {co_exclude; co_merge; _} = core_attrs c.ptyp_attributes in
  match co_exclude with
  | Some e -> `Exclude e
  | None -> `Include (core ~loc ?option c, co_merge)

and constr ~loc ?(opt=false) ?(assoc=false) ?option s l =
  match s, l with
  | "int", _ | "Int.t", _ -> enc_var ~loc "int"
  | "int32", _ | "Int32.t", _ -> enc_var ~loc "int32"
  | "int64", _ | "Int64.t", _ -> enc_var ~loc "int53"
  | "float", _ | "Float.t", _ -> enc_var ~loc "float"
  | "bool", _ | "Bool.t", _ -> enc_var ~loc "bool"
  | "string", _ | "String.t", _ -> enc_var ~loc "string"
  | "bytes", _ | "Bytes.t", _ -> enc_var ~loc "bytes"
  | "list", [{ptyp_desc=Ptyp_tuple [{ptyp_desc=Ptyp_constr ({txt;_}, []); _}; c]; _}]
    when assoc && Longident.name txt = "string" ->
    let e = core ~loc ?option c in
    enc_apply ~loc "assoc" [e]
  | "list", [c] | "List.t", [c] ->
    let e = core ~loc ?option c in
    enc_apply ~loc "list" [e]
  | "array", [c] | "Array.t", [c] ->
    let e = core ~loc ?option c in
    enc_apply ~loc "array" [e]
  | "option", [c] | "Option.t", [c] ->
    let e = core ~loc ?option c in
    if opt then e else enc_apply ~loc "option" [e]
  | "Json_repr.ezjsonm", _ | "ezjsonm", _ | "Ezjsonm.value", _ ->
    enc_var ~loc "any_ezjson_value"
  | "Json_repr.any", _ -> enc_var ~loc "any_value"
  | "unit", _ -> enc_var ~loc "empty"
  | "result", [ok; err] | "Result.t", [ok; err] ->
    let ok = core ~loc ?option ok in
    let err = core ~loc ?option err in
    result_expr ~loc ok err
  | "Lazy.t", [c] ->
    let e = core ~loc ?option c in
    conv1 ~loc
      (fun e -> eapply ~loc (evar ~loc "Lazy.force") [e])
      (fun e -> eapply ~loc (evar ~loc "Lazy.from_val") [e])
      e
  | "char", _ | "Char.t", _ ->
    conv1 ~loc
      (fun e -> eapply ~loc (evar ~loc "String.make") [eint ~loc 1; e])
      (fun e -> eapply ~loc (evar ~loc "String.get") [e; eint ~loc 0])
      (enc_var ~loc "string")
  | "ref", [c] ->
    let e = core ~loc ?option c in
    conv1 ~loc
      (fun e -> pexp_field ~loc e (llid ~loc "contents"))
      (fun e -> eapply ~loc (evar ~loc "ref") [e])
      e
  | _ ->
    let es = List.map (core ~loc ?option) l in
    eapply ~loc (evar ~loc (enc_name s)) es

and row ~loc ?option prf =
  let {cs_kind; cs_assoc; cs_enum; cs_obj1; _} =
    constructor_attrs prf.prf_attributes in
  match prf.prf_desc with
  | Rtag ({txt; _}, _, []) ->
    case_expr ~loc ?kind:cs_kind ~typ:`Variant ~name:txt None
  | Rtag ({txt; _}, _, h :: _) ->
    let e = core ~loc ?assoc:cs_assoc ?enum:cs_enum ?obj1:cs_obj1 ?option h in
    case_expr ~loc ?kind:cs_kind ~typ:`Variant ~name:txt (Some e)
  | _ -> raise_error ~loc "inherit of variant not handled"

and variant ~loc ?(enum=false) ?option l =
  let aux l =
    enc_apply ~loc "union" [
    elist ~loc
      (List.map (fun (enc, construct, destruct) ->
           enc_apply ~loc "case" [ enc; construct; destruct ]) (List.map (row ~loc ?option) l)) ] in
  if enum then
    match List.fold_left (fun acc r ->
        let {cs_key; _} = constructor_attrs r.prf_attributes in
        match acc, r.prf_desc with
        | Some acc, Rtag ({txt; _}, _, []) ->
          let key = match cs_key with Some k -> k | None -> txt in
          Some ((pexp_tuple ~loc [ estring ~loc key; pexp_variant ~loc txt None ]) :: acc)
        | _ -> None) (Some []) l with
    | None -> aux l
    | Some l ->
      enc_apply ~loc "string_enum" [
        elist ~loc (List.rev l) ]
  else aux l

and tuple ~loc ?(obj=false) ?option l =
  let l =
    if obj then List.mapi (fun i c -> field ~loc ~name:(string_of_int i) ?option c) l
    else List.map (core_opt ~loc ?option) l in
  let esf = List.filter_map (function `Exclude _ -> None | `Include e -> Some e) l in
  if List.for_all (function `Exclude _ -> false | _ -> true) l then
    obj_expr ~loc ~kind:(if obj then "obj" else "tup") esf
  else
    let pat_to = ppat_tuple ~loc (List.mapi (fun i -> function
        | `Exclude _ -> ppat_any ~loc
        | `Include _ -> pvar ~loc ("t" ^ string_of_int i)) l) in
    let _, rev = List.fold_left (fun (i, acc) -> function
        | `Exclude _ -> i+1, acc
        | `Include _ -> i+1, ("t" ^ string_of_int i) :: acc) (0, []) l in
    let s = List.rev rev in
    let exp_of = pexp_tuple ~loc (List.mapi (fun i -> function
        | `Exclude e -> e
        | `Include _ -> evar ~loc ("t" ^ string_of_int i)) l) in
    enc_apply ~loc "conv" [
      pexp_fun ~loc pat_to (pexp_tuple ~loc (List.map (evar ~loc) s));
      pexp_fun ~loc (ppat_tuple ~loc (List.map (pvar ~loc) s)) exp_of;
      obj_expr ~loc ~kind:(if obj then "obj" else "tup") esf;
    ]

and field ~loc ?attrs ~name ?option c =
  let attrs = match attrs with None -> c.ptyp_attributes | Some a -> a in
  let opt = match c.ptyp_desc with
    | Ptyp_constr ({txt; _}, _) when Longident.name txt = "option" || Longident.name txt = "Option.t"
      -> true
    | _ -> false in
  let {fa_field=(field, opt, dft); fa_key; fa_title; fa_description; fa_assoc;
       fa_enum; fa_exclude; fa_obj; fa_enc; fa_obj1; fa_merge; fa_construct_default} =
    field_attrs ~key:name ~opt ?option attrs in
  match fa_exclude with
  | None ->
    let enc = core ~loc ~opt ?assoc:fa_assoc ?enum:fa_enum ?obj:fa_obj
        ?enc:fa_enc ?obj1:fa_obj1 ?option c in
    if fa_merge then `Include (enc, true)
    else
      let title = match fa_title with None -> [] | Some t -> [Labelled "title", t] in
      let description = match fa_description with None -> [] | Some d -> [Labelled "description", d] in
      let construct = if fa_construct_default then [Labelled "construct", ebool ~loc true] else [] in
      let dft = match dft with None -> [] | Some e -> [Nolabel, e] in
      let f = pexp_apply ~loc (evar ~loc (enc_mod field)) (
          construct @ title @ description @
          [ Nolabel, estring ~loc fa_key; Nolabel, enc ] @ dft) in
      `Include (f, false)
  | Some e -> `Exclude e

and object_expr ~loc ?option ?ign l =
  let l = List.filter_map (fun pof ->
      let attrs = pof.pof_attributes in
      match pof.pof_desc with
      | Oinherit _ -> None
      | Otag ({txt; _}, c) -> Some (txt, field ~loc ~attrs ~name:txt ?option c)) l in
  let encs = List.filter_map (fun (n, e) -> match e with `Include e -> Some (n, e) | _ -> None) l in
  let names, encs = List.split encs in
  let construct =
    pexp_fun ~loc (pvar ~loc "x") @@
    pexp_tuple ~loc (List.map (fun txt -> pexp_send ~loc (evar ~loc "x") {txt; loc}) names) in
  let destruct =
    pexp_fun ~loc (ppat_tuple ~loc (List.map (pvar ~loc) names)) @@
    pexp_object ~loc @@
    class_structure ~self:(ppat_any ~loc)
      ~fields:(List.map (fun (txt, e) -> match e with
          | `Include _e -> pcf_method ~loc ({txt;loc}, Public, Cfk_concrete (Fresh, evar ~loc txt))
          | `Exclude e -> pcf_method ~loc ({txt;loc}, Public, Cfk_concrete (Fresh, e))) l) in
  let e = enc_apply ~loc "conv" [ construct; destruct; obj_expr ~loc encs ] in
  ignore_expr ~loc ?ign e

let record_label ~loc ?(rm_prefix=false) ?option pld =
  let name = pld.pld_name.txt in
  let name = if not rm_prefix then name else
      match String.index_opt name '_' with
      | None -> name
      | Some i -> String.sub name (i+1) (String.length name - i -1) in
  let e = field ~loc ~attrs:pld.pld_attributes ~name ?option pld.pld_type in
  (pld.pld_name.txt, e)

let same_prefix l =
  fst @@ List.fold_left (fun (b, prefix) pld ->
      if not b then b, prefix
      else
        let name = pld.pld_name.txt in
        match prefix, String.index_opt name '_' with
        | None, None -> true, Some ""
        | None, Some i -> true, Some (String.sub name 0 i)
        | Some _, None | Some _, Some 0 -> false, Some ""
        | Some prefix, Some i ->
          let pre = String.sub name 0 i in
          if pre = prefix then true, Some prefix else false, Some "") (true, None) l

let record ?local ?ign ?rm_prefix ?option ~loc l =
  let rm_prefix = match rm_prefix with
    | None -> same_prefix l
    | Some b -> b in
  let l = List.map (record_label ~loc ~rm_prefix ?option) l in
  let encs = List.filter_map (fun (_, e) -> match e with `Include e -> Some e | _ -> None) l in
  let lhs_to = ppat_record ~loc (List.map (fun (n, e) ->
      llid ~loc n,
      match e with `Include _ -> pvar ~loc n | `Exclude _ -> ppat_any ~loc) l) Closed in
  let rhs_to = pexp_tuple ~loc (List.filter_map (fun (n, e) -> match e with
      | `Include _ -> Some (evar ~loc n)
      | `Exclude _ -> None) l) in
  let pat_of = ppat_tuple ~loc (List.filter_map (fun (n, e) ->
      match e with `Include _ -> Some (pvar ~loc n) | `Exclude _ -> None) l) in
  let exp_of = pexp_record ~loc (List.map (fun (n, e) ->
      llid ~loc n,
      match e with `Include _ -> evar ~loc n | `Exclude e -> e) l) None in
  let construct, destruct = match local with
    | None ->
      pexp_fun ~loc lhs_to rhs_to,
      pexp_fun ~loc pat_of exp_of
    | Some cname ->
      pexp_function ~loc [
        case ~guard:None
          ~lhs:(ppat_construct ~loc (llid ~loc cname) (Some lhs_to))
          ~rhs:rhs_to;
        case ~guard:None ~lhs:(ppat_any ~loc)
          ~rhs:(eapply ~loc (evar ~loc "failwith") [ estring ~loc "wrong local record" ])
      ],
      pexp_fun ~loc pat_of
        (pexp_construct ~loc (llid ~loc cname) @@ (Some exp_of))
  in
  let e = enc_apply ~loc "conv" [ construct; destruct; obj_expr ~loc encs ] in
  ignore_expr ~loc ?ign e

let constructor_label ~loc ?option pcd =
  let cname = pcd.pcd_name.txt in
  let {cs_kind; cs_assoc; cs_enum; cs_obj; cs_enc; cs_title; cs_description;
       cs_ignore; cs_rm_prefix; cs_obj1; _} = constructor_attrs pcd.pcd_attributes in
  let enc, is_record = match pcd.pcd_args with
    | Pcstr_tuple [] -> None, false
    | Pcstr_tuple [c] ->
      Some (core ~loc ?assoc:cs_assoc
              ?enum:cs_enum ?obj:cs_obj ?enc:cs_enc ?obj1:cs_obj1 ?option c), false
    | Pcstr_tuple l -> Some (core ~loc ?obj:cs_obj ?enc:cs_enc ?option (ptyp_tuple ~loc l)), false
    | Pcstr_record l ->
      Some (record ~local:cname ~loc ~ign:cs_ignore ~rm_prefix:cs_rm_prefix ?option l), true in
  let enc, to_, of_ = case_expr ~loc ~is_record ?kind:cs_kind ~typ:`Cons ~name:cname enc in
  def_expr ~loc ?title:cs_title ?description:cs_description
    ~name:cname enc, to_, of_

let all_uppercase l =
  List.for_all (fun pcd ->
      pcd.pcd_name.txt = String.uppercase_ascii pcd.pcd_name.txt) l

let constructor ~loc ?(enum=false) ?option l =
  let aux l =
    enc_apply ~loc "union" [
      elist ~loc
        (List.map (fun (enc, to_, of_) ->
             enc_apply ~loc "case"
               [ enc; to_; of_ ]) (List.map (constructor_label ~loc ?option) l)) ] in
  if enum then
    let all_uppercase = all_uppercase l in
    match List.fold_left (fun acc pcd ->
        let {cs_key; _} = constructor_attrs pcd.pcd_attributes in
        match acc, pcd.pcd_args with
        | Some acc, Pcstr_tuple [] ->
          let key = match cs_key with
            | Some k -> k
            | None ->
              if all_uppercase then pcd.pcd_name.txt
              else String.uncapitalize_ascii pcd.pcd_name.txt in
          Some ((pexp_tuple ~loc [
              estring ~loc key;
              pexp_construct ~loc (llid ~loc pcd.pcd_name.txt) None ]) :: acc)
        | _ -> None) (Some []) l with
    | None -> aux l
    | Some l ->
      enc_apply ~loc "string_enum" [
        elist ~loc (List.rev l) ]
  else aux l

let expressions ~loc ?enum ?ign ?mu ?rm_prefix ?title ?description ?schema ?option t =
  let name = t.ptype_name.txt in
  let e = match t.ptype_kind, t.ptype_manifest with
    | Ptype_abstract, None -> raise_error ~loc "abstract type"
    | Ptype_open, _ -> raise_error ~loc "open type"
    | Ptype_abstract, Some m ->
      debug ~v:2 "\tfrom manifest";
      core ~loc ?option m
    | Ptype_variant l, _ ->
      debug ~v:2 "\tfrom variant";
      constructor ~loc ?enum ?option l
    | Ptype_record l, _ ->
      debug ~v:2 "\tfrom record";
      record ~loc ?ign ?rm_prefix ?option l in
  let e = def_expr ~loc ?title ?description ?schema ~name:t.ptype_name.txt e in
  mu_expr ~loc ?mu ~name e

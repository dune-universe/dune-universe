open Ppxlib
open Ast_builder.Default
open Common

let jsoo_types ~loc (_rec_flag, l) case =
  let l = List.map (fun t ->
      let name = t.ptype_name.txt in
      let name_js = mkl ~loc @@ jsoo_name name in
      let params = t.ptype_params in
      let ct_or_t, ct_ext = Jsoo_type.declaration_of_type_kind ~case ~loc t in
      match ct_or_t with
      | CT expr ->
        ct_ext @ [class_infos ~loc ~virt:Concrete ~params ~name:name_js ~expr], None, true
      | TT c -> ct_ext, Some (
          type_declaration ~loc ~name:name_js ~params ~cstrs:[]
            ~kind:Ptype_abstract ~manifest:(Some c) ~private_:Public), false
    ) l in
  List.flatten @@ List.map (fun (x, _, _) -> x) l,
  List.filter_map (fun (_, x, _) -> x) l,
  List.map (fun (_, _, x) -> x) l

let str_jsoo_types ~loc ~path:_ (rec_flag, l) case =
  let cts, ts, _ = jsoo_types ~loc (rec_flag, l) case in
  let str = pstr_class_type ~loc cts :: (match ts with [] -> [] | _ -> [ pstr_type ~loc rec_flag ts ]) in
  debug "jsoo_type:\n%s\n" (str_of_structure str);
  str

let sig_jsoo_types ~loc ~path:_ (rec_flag, l) case =
  let cts, ts, _ = jsoo_types ~loc (rec_flag, l) case in
  psig_class_type ~loc cts :: (match ts with [] -> [] | _ -> [ psig_type ~loc rec_flag ts ])

let jsoo_convs ~loc (_rec_flag, l) case =
  List.map (fun t ->
      let cv = Jsoo_conv.conv_expressions ~case ~loc t in
      t, cv) l

let str_jsoo_convs ~loc ~path:_ (rec_flag, l) case =
  let l = jsoo_convs ~loc (rec_flag, l) case in
  let vals, conv = List.split @@ List.map (fun (t, cv) ->
      let name = t.ptype_name.txt in
      let name_to, name_of, name_conv =
        jsoo_name_to name, jsoo_name_of name, jsoo_name_conv name in
      let acc = List.flatten @@ List.map (fun ({Jsoo_conv.e_to; e_of; _}, (name_to, name_of)) ->
          [ value_binding ~loc ~pat:(pvar ~loc name_to) ~expr:e_to;
            value_binding ~loc ~pat:(pvar ~loc name_of) ~expr:e_of ]) cv.Jsoo_conv.e_acc in
      acc @ [ value_binding ~loc ~pat:(pvar ~loc name_to) ~expr:cv.Jsoo_conv.e_to;
              value_binding ~loc ~pat:(pvar ~loc name_of) ~expr:cv.Jsoo_conv.e_of ],
      value_binding ~loc ~pat:(pvar ~loc name_conv)
        ~expr:(pexp_tuple ~loc [evar ~loc name_to; evar ~loc name_of])) l in
  let str = [ pstr_value ~loc rec_flag (List.flatten vals);
              pstr_value ~loc Nonrecursive conv ] in
  debug "jsoo_conv:\n%s\n" (str_of_structure str);
  str

let str_gen ~loc ~path:_ (rec_flag, l) case =
  let cts, ts, are_class_type = jsoo_types ~loc (rec_flag, l) case in
  let convs = jsoo_convs ~loc (rec_flag, l) case in
  let vals, conv = List.split @@ List.map2 (fun (t, cv) is_class_type ->
      let name = t.ptype_name.txt in
      let _name_js, name_to, name_of, name_conv =
        jsoo_name name, jsoo_name_to name, jsoo_name_of name, jsoo_name_conv name in
      let to_sig, of_sig, conv_sig = Jsoo_conv.conv_signatures ~loc ~is_class_type t in
      let acc = List.flatten @@ List.map (fun ({Jsoo_conv.e_to; e_of; _}, (name_to, name_of)) ->
          [ value_binding ~loc ~pat:(pvar ~loc name_to) ~expr:e_to;
            value_binding ~loc ~pat:(pvar ~loc name_of) ~expr:e_of ]) cv.Jsoo_conv.e_acc in
      acc @ [ value_binding ~loc ~pat:(ppat_constraint ~loc (pvar ~loc name_to) to_sig)
                ~expr:cv.Jsoo_conv.e_to;
              value_binding ~loc ~pat:(ppat_constraint ~loc (pvar ~loc name_of) of_sig)
                ~expr:cv.Jsoo_conv.e_of ],
      value_binding ~loc ~pat:(ppat_constraint ~loc (pvar ~loc name_conv) conv_sig)
        ~expr:(pexp_tuple ~loc [evar ~loc name_to; evar ~loc name_of])
    ) convs are_class_type in
  let str = pstr_class_type ~loc cts :: (match ts with [] -> [] | _ -> [ pstr_type ~loc rec_flag ts ])
            @ [ pstr_value ~loc rec_flag (List.flatten vals); pstr_value ~loc Nonrecursive conv ] in
  debug "jsoo:\n%s\n" (str_of_structure str);
  str


let () =
  let args_str = Deriving.Args.(empty +> flag "case") in
  let args_sig = Deriving.Args.(empty +> flag "case") in
  let str_type_decl = Deriving.Generator.make args_str str_gen in
  let sig_type_decl = Deriving.Generator.make args_sig sig_jsoo_types in
  Deriving.ignore @@ Deriving.add jsoo ~str_type_decl ~sig_type_decl;
  let str_type_decl = Deriving.Generator.make args_str str_jsoo_types in
  let sig_type_decl = Deriving.Generator.make args_sig sig_jsoo_types in
  Deriving.ignore @@ Deriving.add "jsoo_type" ~str_type_decl ~sig_type_decl;
  let str_type_decl = Deriving.Generator.make args_str str_jsoo_convs in
  Deriving.ignore @@ Deriving.add "jsoo_conv" ~str_type_decl

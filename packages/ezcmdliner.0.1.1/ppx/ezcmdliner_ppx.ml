open Ppxlib




let label_aka_attribute = Attribute.declare
    "cmdliner.aka"
    Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload __)
    (fun expr -> expr)

let label_doc_attribute = Attribute.declare
    "cmdliner.doc"
    Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload __)
    (fun expr -> expr)

let label_docv_attribute = Attribute.declare
    "cmdliner.docv"
    Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload __)
    (fun expr -> expr)

let label_env_docs_attribute = Attribute.declare
    "cmdliner.env.docs"
    Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload __)
    (fun expr -> expr)

let label_env_doc_attribute = Attribute.declare
    "cmdliner.env.doc"
    Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload __)
    (fun expr -> expr)

let label_env_attribute = Attribute.declare
    "cmdliner.env"
    Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload __)
    (fun expr -> expr)

let label_docs_attribute = Attribute.declare
    "cmdliner.docs"
    Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload __)
    (fun expr -> expr)

let label_default_attribute = Attribute.declare
    "cmdliner.default"
    Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload __)
    (fun expr -> expr)

let label_pos_attribute = Attribute.declare
    "cmdliner.pos"
    Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload __)
    (fun expr -> expr)

let label_enum_attribute = Attribute.declare
    "cmdliner.enum"
    Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload __)
    (fun expr -> expr)

let label_sep_attribute = Attribute.declare
    "cmdliner.sep"
    Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload __)
    (fun expr -> expr)

let type_xrefs_attribute = Attribute.declare
    "cmdliner.xrefs"
    Attribute.Context.type_declaration
    Ast_pattern.(single_expr_payload __)
    (fun expr -> expr)

let type_man_attribute = Attribute.declare
    "cmdliner.man"
    Attribute.Context.type_declaration
    Ast_pattern.(single_expr_payload __)
    (fun expr -> expr)

let type_envs_attribute = Attribute.declare
    "cmdliner.envs"
    Attribute.Context.type_declaration
    Ast_pattern.(single_expr_payload __)
    (fun expr -> expr)

let type_doc_attribute = Attribute.declare
    "cmdliner.doc"
    Attribute.Context.type_declaration
    Ast_pattern.(single_expr_payload __)
    (fun expr -> expr)

let type_version_attribute = Attribute.declare
    "cmdliner.version"
    Attribute.Context.type_declaration
    Ast_pattern.(single_expr_payload __)
    (fun expr -> expr)

let attributes = [
  Attribute.T label_aka_attribute;
  Attribute.T label_doc_attribute;
  Attribute.T label_doc_attribute;
  Attribute.T label_env_docs_attribute;
  Attribute.T label_env_doc_attribute;
  Attribute.T label_env_attribute;
  Attribute.T label_docs_attribute;
  Attribute.T label_default_attribute;
  Attribute.T label_pos_attribute;
  Attribute.T label_enum_attribute;
  Attribute.T label_enum_attribute;
  Attribute.T type_xrefs_attribute;
  Attribute.T type_man_attribute;
  Attribute.T type_envs_attribute;
  Attribute.T type_doc_attribute;
  Attribute.T type_version_attribute;
]

let opt : loc:Location.t -> expression option -> expression = fun ~loc -> function
  | None -> [%expr None]
  | Some [%expr [%e? e]] -> [%expr Some [%e e]]

let rec ocaml_doc : attributes -> (string * Location.t) option = function
  | [] ->
    None
  | {attr_name = {txt = name; _};
     attr_payload =
       PStr [{pstr_desc = Pstr_eval ({
           pexp_desc = Pexp_constant (Pconst_string (doc_str, _)); _}, _); _}];
     attr_loc} :: others ->
    if name = "ocaml.doc" then
      Some (doc_str, attr_loc)
    else ocaml_doc others
  | _ :: others ->
    ocaml_doc others

let label_aka : label_declaration -> expression = fun label ->
  let loc = label.pld_loc in
  match Attribute.get label_aka_attribute label with
  | Some [%expr [%e? names]] ->
    names
  | _ ->
    (* default: use label name and its first character *)
    let name = label.pld_name.txt in
    let first = String.make 1 name.[0] in (* can't fail *)
    let ename = Ast_builder.Default.(
        elist ~loc [estring ~loc first; estring ~loc name])in
    [%expr [%e ename]]

let label_doc : label_declaration -> expression = fun label ->
  let loc = label.pld_loc in
  match Attribute.get label_doc_attribute label with
  | Some [%expr [%e? doc]] ->
    [%expr Some [%e doc]]
  | _ ->
    begin match ocaml_doc label.pld_attributes with
      | None -> [%expr None]
      | Some (doc, loc) ->
       let trim_doc = String.trim doc in
       let edoc = Ast_builder.Default.estring ~loc trim_doc in
       [%expr Some [%e edoc]]
    end

let label_env_docs : label_declaration -> expression = fun label ->
  let loc = label.pld_loc in
  match Attribute.get label_env_docs_attribute label with
  | None ->
    [%expr None]
  | Some [%expr [%e? section]] ->
    [%expr Some [%e section]]

let label_env_doc : label_declaration -> expression = fun label ->
  let loc = label.pld_loc in
  match Attribute.get label_env_doc_attribute label with
  | None ->
    [%expr None]
  | Some [%expr [%e? doc]] ->
    [%expr Some [%e doc]]

let label_env : label_declaration -> expression = fun label ->
  let loc = label.pld_loc in
  match Attribute.get label_env_attribute label with
  | None ->
    [%expr None]
  | Some [%expr [%e? var]] ->
    let docs = label_env_docs label in
    let doc = label_env_doc label in
    [%expr Some (Cmdliner.Arg.env_var ?docs:[%e docs] ?doc:[%e doc] [%e var])]

let label_docs : label_declaration -> expression = fun label ->
  let loc = label.pld_loc in
  match Attribute.get label_docs_attribute label with
  | None ->
    [%expr None]
  | Some [%expr [%e? section]] ->
    [%expr Some [%e section]]

let label_infos : label_declaration -> expression = fun label ->
  let loc = label.pld_loc in
  let aka = label_aka label in
  let docs = label_docs label in
  let doc = label_doc label in
  let docv = opt ~loc:label.pld_loc @@ Attribute.get label_docv_attribute label in
  let env = label_env label in
  [%expr
    let docs = [%e docs] in
    let doc = [%e doc] in
    let docv = [%e docv] in
    let env = [%e env] in
    Cmdliner.Arg.info ?docs ?doc ?docv ?env [%e aka]]

let expr_opt : loc:Location.t -> expression option -> expression = fun ~loc e ->
  match e with
  | None -> [%expr None]
  | Some exp -> [%expr Some [%e exp]]

let rec converter : sep:expression -> core_type -> expression = fun ~sep typ ->
  let loc = typ.ptyp_loc in
  match typ with
  | [%type: bool] -> [%expr bool]
  | [%type: char] -> [%expr char]
  | [%type: int] -> [%expr int]
  | [%type: nativeint] -> [%expr nativeint]
  | [%type: int32] -> [%expr int32]
  | [%type: int64] -> [%expr int64]
  | [%type: float] -> [%expr float]
  | [%type: string] -> [%expr string]
  | [%type: [%t? a] list] -> [%expr list ?sep:[%e sep] [%e converter ~sep a]]
  | [%type: [%t? a] array] -> [%expr array ?sep:[%e sep] [%e converter ~sep a]]
  | [%type: [%t? a] * [%t? b]] ->
    [%expr t2 [%e converter ~sep a] [%e converter ~sep b]]
  | [%type: [%t? a] * [%t? b] * [%t? c]] ->
    [%expr t3 [%e converter ~sep a] [%e converter ~sep b] [%e converter ~sep c]]
  | [%type: [%t? a] * [%t? b] * [%t? c] * [%t? d]] ->
    [%expr t4
        [%e converter ~sep a] [%e converter ~sep b]
        [%e converter ~sep c] [%e converter ~sep d]]
  | _ ->
    Location.raise_errorf ~loc
      "cmdliner: don't know what to do with %a"
      Pprintast.core_type typ

let label_conv : label_declaration -> expression = fun label ->
  let loc = label.pld_loc in
  match Attribute.get label_enum_attribute label with
  | None ->
    let sep = expr_opt ~loc @@ Attribute.get label_sep_attribute label in
    converter ~sep label.pld_type
  | Some [%expr [%e? enum_list]] -> [%expr enum [%e enum_list]]

let label_term : label_declaration -> expression = fun label ->
  let loc = label.pld_loc in
  let infos = label_infos label in
  let default_attr = Attribute.get label_default_attribute label in
  let pos_attr = Attribute.get label_pos_attribute label in
  let conv = label_conv label in
  let term = match default_attr, pos_attr, label.pld_type with
    | None, None, [%type: bool] ->
      (* special case: boolean flag *)
      [%expr value & flag infos]
    | None, None, [%type: [%t? _] list] ->
      (* optional argument: special list case which can be specified in several chunks
         with a empty list as default *)
      [%expr Cmdliner.Term.(const List.concat $ (value & opt_all [%e conv] [] & infos))]
    | None, None, _ ->
      (* required opt *)
      [%expr required & opt (some [%e conv]) None & infos]
    | None, Some [%expr [%e? index]], _ ->
      (* required pos *)
      [%expr required & pos [%e index] (some [%e conv]) None & infos]
    | Some [%expr [%e? default]], None, [%type: [%t? _] list] ->
      (* optional argument: special list case which can be specified in several chunks *)
      [%expr Cmdliner.Term.(const List.concat $ (value & opt_all [%e conv] [%e default] & infos))]
    | Some [%expr [%e? default]], None, _ ->
      (* optional argument *)
      [%expr value & opt [%e conv] [%e default] & infos]
    | Some [%expr [%e? default]], Some [%expr [%e? index]], _ ->
      (* optional pos: don't understand clearly what this
         means but i still forward to cmdliner... *)
      [%expr value & pos [%e index] [%e conv] [%e default] & infos] in
  [%expr
    let infos = [%e infos] in
    Cmdliner.Arg.([%e term])]

let suffix = "cmdliner_t"

let type_env ~loc td =
  match Attribute.get type_envs_attribute td with
  | None -> [%expr None]
  | Some l ->
    let rec envs = function
      | [%expr []] -> [%expr []]
      | [%expr  ([%e? name], [%e? doc], [%e? docs]) :: [%e? t]] ->
        [%expr Term.env_info ?docs:[%e docs] ?doc:[%e doc] [%e name] :: [%e envs t]]
      | e ->
        Location.raise_errorf ~loc
          "cmdliner: don't know what to do with %a"
          Pprintast.expression e in
    [%expr Some [%e envs l]]

let type_doc : type_declaration -> expression = fun td ->
  let loc = td.ptype_loc in
  match Attribute.get type_doc_attribute td with
  | Some [%expr [%e? doc]] ->
    [%expr Some [%e doc]]
  | _ ->
    begin match ocaml_doc td.ptype_attributes with
      | None -> [%expr None]
      | Some (doc, loc) ->
       let trim_doc = String.trim doc in
       let edoc = Ast_builder.Default.estring ~loc trim_doc in
       [%expr Some [%e edoc]]
    end


let expand_str_type_decl : loc:Location.t -> type_declaration -> structure =
  fun ~loc type_decl ->
  match type_decl with
  | {ptype_name = {txt = type_name; loc = type_name_loc};
     ptype_kind = Ptype_record labels;
     ptype_loc; _} ->
    (* record *)
    let prefix = match type_name with
      | "t" -> ""
      | _ -> Fmt.str "%s_" type_name in
    (* generate args *)
    let args = List.map (fun label  ->
        let label_id = Fmt.str "%s%s_%s" prefix label.pld_name.txt suffix in
        let plabel_id = Ast_builder.Default.pvar ~loc label_id in
        label_id, [%stri let [%p plabel_id] = [%e label_term label]]
      ) labels in
    (* generate type's term *)
    let type_id = Fmt.str "%s%s" prefix suffix in
    let ptype_id = Ast_builder.Default.pvar ~loc:type_name_loc type_id in
    let final_record = Ast_builder.Default.pexp_record ~loc
        (List.map (fun label ->
             let lid = {txt = Lident label.pld_name.txt; loc = label.pld_name.loc} in
             let exp = Ast_builder.Default.evar ~loc:label.pld_name.loc label.pld_name.txt in
             lid, exp) labels)  None in
    let mk_exp = List.fold_right (fun label exp ->
        let plabel = Ast_builder.Default.pvar ~loc:label.pld_name.loc label.pld_name.txt in
        Ast_builder.Default.pexp_fun ~loc Nolabel None plabel exp) labels final_record in
    let final_app = List.fold_left (fun exp (lbl_t, _) ->
        let e_lbl_t = Ast_builder.Default.evar ~loc lbl_t in
        [%expr [%e exp] $ [%e e_lbl_t]])
        [%expr const mk] args in
    (* generate cmdliner function *)
    let etype_id = Ast_builder.Default.evar ~loc:type_name_loc type_id in
    let xrefs = opt ~loc:ptype_loc @@ Attribute.get type_xrefs_attribute type_decl in
    let man = opt ~loc:ptype_loc @@ Attribute.get type_man_attribute type_decl in
    let envs = type_env ~loc:ptype_loc type_decl in
    let doc = type_doc type_decl in
    let version = opt ~loc:ptype_loc @@ Attribute.get type_version_attribute type_decl in
    List.map snd args @
    [%str
      let [%p ptype_id] =
        let mk = [%e mk_exp] in
        Cmdliner.Term.([%e final_app])

      let cmdliner f =
        let name = Filename.basename Sys.executable_name in
        let open Cmdliner in
        let info = Term.info name
            ?man_xrefs:[%e xrefs]
            ?man:[%e man]
            ?envs:[%e envs]
            ?doc:[%e doc]
            ?version:[%e version]
            ~exits:Term.default_exits in
        let term_t = Term.(const f $ [%e etype_id]) in
        Term.exit @@ Term.eval (term_t, info)
    ]
  | _ -> []

let expand_str :
  ctxt:Expansion_context.Deriver.t ->
  rec_flag * type_declaration list  ->
  structure =
  fun ~ctxt (_rec_flag, type_decl_list) ->
  (* let omp_config = Expansion_context.Deriver.omp_config ctxt in *)
  let is_ocamldep_pass =
    String.equal "ocamldep" (Expansion_context.Deriver.tool_name ctxt) in
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  if is_ocamldep_pass then [] else
    List.map (expand_str_type_decl ~loc) type_decl_list |> List.concat


let expand_sig ~ctxt _input_ast =
  (* let omp_config = Expansion_context.Deriver.omp_config ctxt in *)
  let is_ocamldep_pass =
    String.equal "ocamldep" (Expansion_context.Deriver.tool_name ctxt) in
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  if is_ocamldep_pass then []
  else [%sig: val foo : int]

let str_type_decl_generator =
  Deriving.Generator.V2.make_noarg
    ~attributes
    expand_str

let sig_type_decl_generator =
  Deriving.Generator.V2.make_noarg
    ~attributes
    expand_sig

let cmdliner_deriver =
  Deriving.add
    ~str_type_decl:str_type_decl_generator
    ~sig_type_decl:sig_type_decl_generator
    "cmdliner"

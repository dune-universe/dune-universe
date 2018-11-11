open Earley_core
open Input
open Earley
open Charset
open Ast_helper
open Asttypes
open Parsetree
open Longident
open Pa_ast
open Pa_lexing
open Helper
include Pa_ocaml_prelude
module Make(Initial:Extension) =
  struct
    include Initial
    let ouident = uident
    let uident = Earley_core.Earley.declare_grammar "uident"
    let _ =
      Earley_core.Earley.set_grammar uident
        (Earley_core.Earley.alternatives
           [Earley_core.Earley.fsequence_ignore
              (Earley_core.Earley.string "$uid:" "$uid:")
              (Earley_core.Earley.fsequence expression
                 (Earley_core.Earley.fsequence_ignore
                    (Earley_core.Earley.no_blank_test ())
                    (Earley_core.Earley.fsequence_ignore
                       (Earley_core.Earley.char '$' '$')
                       (Earley_core.Earley.empty_pos
                          (fun __loc__start__buf ->
                             fun __loc__start__pos ->
                               fun __loc__end__buf ->
                                 fun __loc__end__pos ->
                                   let _loc =
                                     locate __loc__start__buf
                                       __loc__start__pos __loc__end__buf
                                       __loc__end__pos in
                                   fun e -> Quote.string_antiquotation _loc e)))));
           ouident])
    let olident = lident
    let lident = Earley_core.Earley.declare_grammar "lident"
    let _ =
      Earley_core.Earley.set_grammar lident
        (Earley_core.Earley.alternatives
           [Earley_core.Earley.fsequence_ignore
              (Earley_core.Earley.string "$lid:" "$lid:")
              (Earley_core.Earley.fsequence expression
                 (Earley_core.Earley.fsequence_ignore
                    (Earley_core.Earley.no_blank_test ())
                    (Earley_core.Earley.fsequence_ignore
                       (Earley_core.Earley.char '$' '$')
                       (Earley_core.Earley.empty_pos
                          (fun __loc__start__buf ->
                             fun __loc__start__pos ->
                               fun __loc__end__buf ->
                                 fun __loc__end__pos ->
                                   let _loc =
                                     locate __loc__start__buf
                                       __loc__start__pos __loc__end__buf
                                       __loc__end__pos in
                                   fun e -> Quote.string_antiquotation _loc e)))));
           olident])
    let oident = ident
    let ident = Earley_core.Earley.declare_grammar "ident"
    let _ =
      Earley_core.Earley.set_grammar ident
        (Earley_core.Earley.alternatives
           [Earley_core.Earley.fsequence_ignore
              (Earley_core.Earley.string "$ident:" "$ident:")
              (Earley_core.Earley.fsequence expression
                 (Earley_core.Earley.fsequence_ignore
                    (Earley_core.Earley.no_blank_test ())
                    (Earley_core.Earley.fsequence_ignore
                       (Earley_core.Earley.char '$' '$')
                       (Earley_core.Earley.empty_pos
                          (fun __loc__start__buf ->
                             fun __loc__start__pos ->
                               fun __loc__end__buf ->
                                 fun __loc__end__pos ->
                                   let _loc =
                                     locate __loc__start__buf
                                       __loc__start__pos __loc__end__buf
                                       __loc__end__pos in
                                   fun e -> Quote.string_antiquotation _loc e)))));
           oident])
    let mk_unary_op loc name loc_name arg =
      match (name, (arg.pexp_desc)) with
      | ("-", Pexp_constant (Pconst_integer (n, o))) ->
          Exp.constant ~loc (Const.integer ?suffix:o ("-" ^ n))
      | (("-"|"-."), Pexp_constant (Pconst_float (f, o))) ->
          Exp.constant ~loc (Const.float ?suffix:o ("-" ^ f))
      | ("+", Pexp_constant (Pconst_integer _))
        |(("+"|"+."), Pexp_constant (Pconst_float _)) ->
          Exp.mk ~loc arg.pexp_desc
      | (("-"|"-."|"+"|"+."), _) ->
          let lid = id_loc (Lident ("~" ^ name)) loc_name in
          let fn = Exp.ident ~loc:loc_name lid in
          Exp.apply ~loc fn [(nolabel, arg)]
      | _ ->
          let lid = id_loc (Lident name) loc_name in
          let fn = Exp.ident ~loc:loc_name lid in
          Exp.apply ~loc fn [(nolabel, arg)]
    let mk_binary_op loc e' op loc_op e =
      if op = "::"
      then
        let lid = id_loc (Lident "::") loc_op in
        Exp.construct ~loc lid (Some (Exp.tuple ~loc:(ghost loc) [e'; e]))
      else
        (let id = Exp.ident ~loc:loc_op (id_loc (Lident op) loc_op) in
         Exp.apply ~loc id [(nolabel, e'); (nolabel, e)])
    let wrap_type_annotation loc types core_type body =
      let exp = Exp.constraint_ ~loc body core_type in
      let exp =
        List.fold_right (fun ty -> fun exp -> Exp.newtype ~loc ty exp) types
          exp in
      (exp,
        (Typ.poly ~loc:(ghost loc) types
           (Typ.varify_constructors types core_type)))
    type tree =
      | Node of tree * tree 
      | Leaf of string 
    let string_of_tree (t : tree) =
      (let b = Buffer.create 101 in
       let rec fn =
         function
         | Leaf s -> Buffer.add_string b s
         | Node (a, b) -> (fn a; fn b) in
       fn t; Buffer.contents b : string)
    let label_name = lident
    let ty_label = Earley_core.Earley.declare_grammar "ty_label"
    let _ =
      Earley_core.Earley.set_grammar ty_label
        (Earley_core.Earley.fsequence_ignore
           (Earley_core.Earley.char '~' '~')
           (Earley_core.Earley.fsequence_ignore
              (Earley_core.Earley.no_blank_test ())
              (Earley_core.Earley.fsequence lident
                 (Earley_core.Earley.fsequence_ignore
                    (Earley_core.Earley.char ':' ':')
                    (Earley_core.Earley.empty (fun s -> labelled s))))))
    let ty_opt_label = Earley_core.Earley.declare_grammar "ty_opt_label"
    let _ =
      Earley_core.Earley.set_grammar ty_opt_label
        (Earley_core.Earley.fsequence_ignore
           (Earley_core.Earley.char '?' '?')
           (Earley_core.Earley.fsequence_ignore
              (Earley_core.Earley.no_blank_test ())
              (Earley_core.Earley.fsequence lident
                 (Earley_core.Earley.fsequence_ignore
                    (Earley_core.Earley.char ':' ':')
                    (Earley_core.Earley.empty (fun s -> optional s))))))
    let maybe_opt_label =
      Earley_core.Earley.declare_grammar "maybe_opt_label"
    let _ =
      Earley_core.Earley.set_grammar maybe_opt_label
        (Earley_core.Earley.fsequence
           (Earley_core.Earley.option None
              (Earley_core.Earley.apply (fun x -> Some x)
                 (Earley_core.Earley.string "?" "?")))
           (Earley_core.Earley.fsequence label_name
              (Earley_core.Earley.empty
                 (fun ln ->
                    fun o -> if o = None then labelled ln else optional ln))))
    let list_antiquotation _loc e =
      let open Quote in
        let generic_antiquote e =
          function | Quote_loc -> e | _ -> failwith "invalid antiquotation" in
        make_list_antiquotation _loc Quote_loc (generic_antiquote e)
    let operator_name =
      alternatives ((prefix_symbol Prefix) ::
        (List.map infix_symbol infix_prios))
    let value_name = Earley_core.Earley.declare_grammar "value_name"
    let _ =
      Earley_core.Earley.set_grammar value_name
        (Earley_core.Earley.alternatives
           [Earley_core.Earley.fsequence_ignore
              (Earley_core.Earley.char '(' '(')
              (Earley_core.Earley.fsequence operator_name
                 (Earley_core.Earley.fsequence_ignore
                    (Earley_core.Earley.char ')' ')')
                    (Earley_core.Earley.empty (fun op -> op))));
           lident])
    let constr_name = uident
    let tag_name = Earley_core.Earley.declare_grammar "tag_name"
    let _ =
      Earley_core.Earley.set_grammar tag_name
        (Earley_core.Earley.fsequence_ignore
           (Earley_core.Earley.string "`" "`")
           (Earley_core.Earley.fsequence ident
              (Earley_core.Earley.empty (fun c -> c))))
    let typeconstr_name = lident
    let field_name = lident
    let smodule_name = uident
    let module_name = Earley_core.Earley.declare_grammar "module_name"
    let _ =
      Earley_core.Earley.set_grammar module_name
        (Earley_core.Earley.fsequence uident
           (Earley_core.Earley.empty_pos
              (fun __loc__start__buf ->
                 fun __loc__start__pos ->
                   fun __loc__end__buf ->
                     fun __loc__end__pos ->
                       let _loc =
                         locate __loc__start__buf __loc__start__pos
                           __loc__end__buf __loc__end__pos in
                       fun u -> id_loc u _loc)))
    let modtype_name = ident
    let class_name = lident
    let inst_var_name = lident
    let method_name = Earley_core.Earley.declare_grammar "method_name"
    let _ =
      Earley_core.Earley.set_grammar method_name
        (Earley_core.Earley.fsequence lident
           (Earley_core.Earley.empty_pos
              (fun __loc__start__buf ->
                 fun __loc__start__pos ->
                   fun __loc__end__buf ->
                     fun __loc__end__pos ->
                       let _loc =
                         locate __loc__start__buf __loc__start__pos
                           __loc__end__buf __loc__end__pos in
                       fun id -> id_loc id _loc)))
    let (module_path_gen, set_module_path_gen) =
      grammar_family "module_path_gen"
    let (module_path_suit, set_module_path_suit) =
      grammar_family "module_path_suit"
    let (module_path_suit_aux, module_path_suit_aux__set__grammar) =
      Earley_core.Earley.grammar_family "module_path_suit_aux"
    let _ =
      module_path_suit_aux__set__grammar
        (fun allow_app ->
           Earley_core.Earley.alternatives
             ((Earley_core.Earley.fsequence_ignore
                 (Earley_core.Earley.string "." ".")
                 (Earley_core.Earley.fsequence smodule_name
                    (Earley_core.Earley.empty
                       (fun m -> fun acc -> Ldot (acc, m))))) ::
             ((if allow_app
               then
                 [Earley_core.Earley.fsequence_ignore
                    (Earley_core.Earley.string "(" "(")
                    (Earley_core.Earley.fsequence (module_path_gen true)
                       (Earley_core.Earley.fsequence_ignore
                          (Earley_core.Earley.string ")" ")")
                          (Earley_core.Earley.empty
                             (fun m' -> fun a -> Lapply (a, m')))))]
               else []) @ [])))
    let _ =
      set_module_path_suit
        (fun allow_app ->
           Earley_core.Earley.alternatives
             [Earley_core.Earley.fsequence_ignore
                (Earley_core.Earley.empty ())
                (Earley_core.Earley.empty (fun acc -> acc));
             Earley_core.Earley.fsequence (module_path_suit_aux allow_app)
               (Earley_core.Earley.fsequence (module_path_suit allow_app)
                  (Earley_core.Earley.empty
                     (fun g -> fun f -> fun acc -> g (f acc))))])
    let _ =
      set_module_path_gen
        (fun allow_app ->
           Earley_core.Earley.fsequence smodule_name
             (Earley_core.Earley.fsequence (module_path_suit allow_app)
                (Earley_core.Earley.empty (fun s -> fun m -> s (Lident m)))))
    let module_path = module_path_gen false
    let extended_module_path = module_path_gen true
    let _ =
      set_grammar value_path
        (Earley_core.Earley.fsequence
           (Earley_core.Earley.option None
              (Earley_core.Earley.apply (fun x -> Some x)
                 (Earley_core.Earley.fsequence module_path
                    (Earley_core.Earley.fsequence_ignore
                       (Earley_core.Earley.string "." ".")
                       (Earley_core.Earley.empty (fun m -> m))))))
           (Earley_core.Earley.fsequence value_name
              (Earley_core.Earley.empty
                 (fun vn ->
                    fun mp ->
                      match mp with
                      | None -> Lident vn
                      | Some p -> Ldot (p, vn)))))
    let constr = Earley_core.Earley.declare_grammar "constr"
    let _ =
      Earley_core.Earley.set_grammar constr
        (Earley_core.Earley.fsequence
           (Earley_core.Earley.option None
              (Earley_core.Earley.apply (fun x -> Some x)
                 (Earley_core.Earley.fsequence module_path
                    (Earley_core.Earley.fsequence_ignore
                       (Earley_core.Earley.string "." ".")
                       (Earley_core.Earley.empty (fun m -> m))))))
           (Earley_core.Earley.fsequence constr_name
              (Earley_core.Earley.empty
                 (fun cn ->
                    fun mp ->
                      match mp with
                      | None -> Lident cn
                      | Some p -> Ldot (p, cn)))))
    let typeconstr = Earley_core.Earley.declare_grammar "typeconstr"
    let _ =
      Earley_core.Earley.set_grammar typeconstr
        (Earley_core.Earley.fsequence
           (Earley_core.Earley.option None
              (Earley_core.Earley.apply (fun x -> Some x)
                 (Earley_core.Earley.fsequence extended_module_path
                    (Earley_core.Earley.fsequence_ignore
                       (Earley_core.Earley.string "." ".")
                       (Earley_core.Earley.empty (fun m -> m))))))
           (Earley_core.Earley.fsequence typeconstr_name
              (Earley_core.Earley.empty
                 (fun tcn ->
                    fun mp ->
                      match mp with
                      | None -> Lident tcn
                      | Some p -> Ldot (p, tcn)))))
    let field = Earley_core.Earley.declare_grammar "field"
    let _ =
      Earley_core.Earley.set_grammar field
        (Earley_core.Earley.fsequence
           (Earley_core.Earley.option None
              (Earley_core.Earley.apply (fun x -> Some x)
                 (Earley_core.Earley.fsequence module_path
                    (Earley_core.Earley.fsequence_ignore
                       (Earley_core.Earley.string "." ".")
                       (Earley_core.Earley.empty (fun m -> m))))))
           (Earley_core.Earley.fsequence field_name
              (Earley_core.Earley.empty
                 (fun fn ->
                    fun mp ->
                      match mp with
                      | None -> Lident fn
                      | Some p -> Ldot (p, fn)))))
    let class_path = Earley_core.Earley.declare_grammar "class_path"
    let _ =
      Earley_core.Earley.set_grammar class_path
        (Earley_core.Earley.fsequence
           (Earley_core.Earley.option None
              (Earley_core.Earley.apply (fun x -> Some x)
                 (Earley_core.Earley.fsequence module_path
                    (Earley_core.Earley.fsequence_ignore
                       (Earley_core.Earley.string "." ".")
                       (Earley_core.Earley.empty (fun m -> m))))))
           (Earley_core.Earley.fsequence class_name
              (Earley_core.Earley.empty
                 (fun cn ->
                    fun mp ->
                      match mp with
                      | None -> Lident cn
                      | Some p -> Ldot (p, cn)))))
    let modtype_path = Earley_core.Earley.declare_grammar "modtype_path"
    let _ =
      Earley_core.Earley.set_grammar modtype_path
        (Earley_core.Earley.fsequence
           (Earley_core.Earley.option None
              (Earley_core.Earley.apply (fun x -> Some x)
                 (Earley_core.Earley.fsequence extended_module_path
                    (Earley_core.Earley.fsequence_ignore
                       (Earley_core.Earley.string "." ".")
                       (Earley_core.Earley.empty (fun m -> m))))))
           (Earley_core.Earley.fsequence modtype_name
              (Earley_core.Earley.empty
                 (fun mtn ->
                    fun mp ->
                      match mp with
                      | None -> Lident mtn
                      | Some p -> Ldot (p, mtn)))))
    let classtype_path = Earley_core.Earley.declare_grammar "classtype_path"
    let _ =
      Earley_core.Earley.set_grammar classtype_path
        (Earley_core.Earley.fsequence
           (Earley_core.Earley.option None
              (Earley_core.Earley.apply (fun x -> Some x)
                 (Earley_core.Earley.fsequence extended_module_path
                    (Earley_core.Earley.fsequence_ignore
                       (Earley_core.Earley.string "." ".")
                       (Earley_core.Earley.empty (fun m -> m))))))
           (Earley_core.Earley.fsequence class_name
              (Earley_core.Earley.empty
                 (fun cn ->
                    fun mp ->
                      match mp with
                      | None -> Lident cn
                      | Some p -> Ldot (p, cn)))))
    let opt_variance = Earley_core.Earley.declare_grammar "opt_variance"
    let _ =
      Earley_core.Earley.set_grammar opt_variance
        (Earley_core.Earley.fsequence
           (Earley_core.Earley.option None
              (Earley_core.Earley.apply (fun x -> Some x)
                 (Earley_str.regexp "[+-]" (fun groupe -> groupe 0))))
           (Earley_core.Earley.empty
              (fun v ->
                 match v with
                 | None -> Invariant
                 | Some "+" -> Covariant
                 | Some "-" -> Contravariant
                 | _ -> assert false)))
    let override_flag = Earley_core.Earley.declare_grammar "override_flag"
    let _ =
      Earley_core.Earley.set_grammar override_flag
        (Earley_core.Earley.fsequence
           (Earley_core.Earley.option None
              (Earley_core.Earley.apply (fun x -> Some x)
                 (Earley_core.Earley.string "!" "!")))
           (Earley_core.Earley.empty
              (fun o -> if o <> None then Override else Fresh)))
    let attr_id = Earley_core.Earley.declare_grammar "attr_id"
    let _ =
      Earley_core.Earley.set_grammar attr_id
        (Earley_core.Earley.fsequence ident
           (Earley_core.Earley.fsequence
              (Earley_core.Earley.apply (fun f -> f [])
                 (Earley_core.Earley.fixpoint' (fun l -> l)
                    (Earley_core.Earley.fsequence_ignore
                       (Earley_core.Earley.char '.' '.')
                       (Earley_core.Earley.fsequence ident
                          (Earley_core.Earley.empty (fun id -> id))))
                    (fun x -> fun f -> fun l -> f (x :: l))))
              (Earley_core.Earley.empty_pos
                 (fun __loc__start__buf ->
                    fun __loc__start__pos ->
                      fun __loc__end__buf ->
                        fun __loc__end__pos ->
                          let _loc =
                            locate __loc__start__buf __loc__start__pos
                              __loc__end__buf __loc__end__pos in
                          fun l ->
                            fun id ->
                              id_loc (String.concat "." (id :: l)) _loc))))
    let payload = Earley_core.Earley.declare_grammar "payload"
    let _ =
      Earley_core.Earley.set_grammar payload
        (Earley_core.Earley.alternatives
           [Earley_core.Earley.fsequence_ignore
              (Earley_core.Earley.char '?' '?')
              (Earley_core.Earley.fsequence pattern
                 (Earley_core.Earley.fsequence
                    (Earley_core.Earley.option None
                       (Earley_core.Earley.apply (fun x -> Some x)
                          (Earley_core.Earley.fsequence_ignore when_kw
                             (Earley_core.Earley.fsequence expression
                                (Earley_core.Earley.empty (fun e -> e))))))
                    (Earley_core.Earley.empty (fun e -> fun p -> PPat (p, e)))));
           Earley_core.Earley.fsequence structure
             (Earley_core.Earley.empty (fun s -> PStr s));
           Earley_core.Earley.fsequence_ignore
             (Earley_core.Earley.char ':' ':')
             (Earley_core.Earley.fsequence typexpr
                (Earley_core.Earley.empty (fun t -> PTyp t)))])
    let attribute = Earley_core.Earley.declare_grammar "attribute"
    let _ =
      Earley_core.Earley.set_grammar attribute
        (Earley_core.Earley.fsequence_ignore
           (Earley_core.Earley.string "[@" "[@")
           (Earley_core.Earley.fsequence attr_id
              (Earley_core.Earley.fsequence payload
                 (Earley_core.Earley.fsequence_ignore
                    (Earley_core.Earley.char ']' ']')
                    (Earley_core.Earley.empty (fun p -> fun id -> (id, p)))))))
    let attributes = Earley_core.Earley.declare_grammar "attributes"
    let _ =
      Earley_core.Earley.set_grammar attributes
        (Earley_core.Earley.apply (fun f -> f [])
           (Earley_core.Earley.fixpoint' (fun l -> l) attribute
              (fun x -> fun f -> fun l -> f (x :: l))))
    let ext_attributes = Earley_core.Earley.declare_grammar "ext_attributes"
    let _ =
      Earley_core.Earley.set_grammar ext_attributes
        (Earley_core.Earley.fsequence
           (Earley_core.Earley.option None
              (Earley_core.Earley.apply (fun x -> Some x)
                 (Earley_core.Earley.fsequence_ignore
                    (Earley_core.Earley.char '%' '%')
                    (Earley_core.Earley.fsequence attribute
                       (Earley_core.Earley.empty (fun a -> a))))))
           (Earley_core.Earley.fsequence attributes
              (Earley_core.Earley.empty (fun l -> fun a -> (a, l)))))
    let post_item_attributes =
      Earley_core.Earley.declare_grammar "post_item_attributes"
    let _ =
      Earley_core.Earley.set_grammar post_item_attributes
        (Earley_core.Earley.apply (fun f -> f [])
           (Earley_core.Earley.fixpoint' (fun l -> l)
              (Earley_core.Earley.fsequence_ignore
                 (Earley_core.Earley.string "[@@" "[@@")
                 (Earley_core.Earley.fsequence attr_id
                    (Earley_core.Earley.fsequence payload
                       (Earley_core.Earley.fsequence_ignore
                          (Earley_core.Earley.char ']' ']')
                          (Earley_core.Earley.empty
                             (fun p -> fun id -> (id, p)))))))
              (fun x -> fun f -> fun l -> f (x :: l))))
    let floating_attribute =
      Earley_core.Earley.declare_grammar "floating_attribute"
    let _ =
      Earley_core.Earley.set_grammar floating_attribute
        (Earley_core.Earley.fsequence_ignore
           (Earley_core.Earley.string "[@@@" "[@@@")
           (Earley_core.Earley.fsequence attr_id
              (Earley_core.Earley.fsequence payload
                 (Earley_core.Earley.fsequence_ignore
                    (Earley_core.Earley.char ']' ']')
                    (Earley_core.Earley.empty (fun p -> fun id -> (id, p)))))))
    let extension = Earley_core.Earley.declare_grammar "extension"
    let _ =
      Earley_core.Earley.set_grammar extension
        (Earley_core.Earley.fsequence_ignore
           (Earley_core.Earley.string "[%" "[%")
           (Earley_core.Earley.fsequence attr_id
              (Earley_core.Earley.fsequence payload
                 (Earley_core.Earley.fsequence_ignore
                    (Earley_core.Earley.char ']' ']')
                    (Earley_core.Earley.empty (fun p -> fun id -> (id, p)))))))
    let floating_extension =
      Earley_core.Earley.declare_grammar "floating_extension"
    let _ =
      Earley_core.Earley.set_grammar floating_extension
        (Earley_core.Earley.fsequence_ignore
           (Earley_core.Earley.string "[%%" "[%%")
           (Earley_core.Earley.fsequence attr_id
              (Earley_core.Earley.fsequence payload
                 (Earley_core.Earley.fsequence_ignore
                    (Earley_core.Earley.char ']' ']')
                    (Earley_core.Earley.empty (fun p -> fun id -> (id, p)))))))
    let only_poly_typexpr =
      Earley_core.Earley.declare_grammar "only_poly_typexpr"
    let _ =
      Earley_core.Earley.set_grammar only_poly_typexpr
        (Earley_core.Earley.fsequence
           (Earley_core.Earley.apply (fun f -> f [])
              (Earley_core.Earley.fixpoint1' (fun l -> l)
                 (Earley_core.Earley.fsequence_ignore
                    (Earley_core.Earley.char '\'' '\'')
                    (Earley_core.Earley.fsequence ident
                       (Earley_core.Earley.empty_pos
                          (fun __loc__start__buf ->
                             fun __loc__start__pos ->
                               fun __loc__end__buf ->
                                 fun __loc__end__pos ->
                                   let _loc =
                                     locate __loc__start__buf
                                       __loc__start__pos __loc__end__buf
                                       __loc__end__pos in
                                   fun id -> id_loc id _loc))))
                 (fun x -> fun f -> fun l -> f (x :: l))))
           (Earley_core.Earley.fsequence_ignore
              (Earley_core.Earley.char '.' '.')
              (Earley_core.Earley.fsequence typexpr
                 (Earley_core.Earley.empty_pos
                    (fun __loc__start__buf ->
                       fun __loc__start__pos ->
                         fun __loc__end__buf ->
                           fun __loc__end__pos ->
                             let _loc =
                               locate __loc__start__buf __loc__start__pos
                                 __loc__end__buf __loc__end__pos in
                             fun te -> fun ids -> Typ.poly ~loc:_loc ids te)))))
    let poly_typexpr = Earley_core.Earley.declare_grammar "poly_typexpr"
    let _ =
      Earley_core.Earley.set_grammar poly_typexpr
        (Earley_core.Earley.alternatives
           [typexpr;
           Earley_core.Earley.fsequence
             (Earley_core.Earley.apply (fun f -> f [])
                (Earley_core.Earley.fixpoint1' (fun l -> l)
                   (Earley_core.Earley.fsequence_ignore
                      (Earley_core.Earley.char '\'' '\'')
                      (Earley_core.Earley.fsequence ident
                         (Earley_core.Earley.empty_pos
                            (fun __loc__start__buf ->
                               fun __loc__start__pos ->
                                 fun __loc__end__buf ->
                                   fun __loc__end__pos ->
                                     let _loc =
                                       locate __loc__start__buf
                                         __loc__start__pos __loc__end__buf
                                         __loc__end__pos in
                                     fun id -> id_loc id _loc))))
                   (fun x -> fun f -> fun l -> f (x :: l))))
             (Earley_core.Earley.fsequence_ignore
                (Earley_core.Earley.char '.' '.')
                (Earley_core.Earley.fsequence typexpr
                   (Earley_core.Earley.empty_pos
                      (fun __loc__start__buf ->
                         fun __loc__start__pos ->
                           fun __loc__end__buf ->
                             fun __loc__end__pos ->
                               let _loc =
                                 locate __loc__start__buf __loc__start__pos
                                   __loc__end__buf __loc__end__pos in
                               fun te -> fun ids -> Typ.poly ~loc:_loc ids te))))])
    let poly_syntax_typexpr =
      Earley_core.Earley.declare_grammar "poly_syntax_typexpr"
    let _ =
      Earley_core.Earley.set_grammar poly_syntax_typexpr
        (Earley_core.Earley.fsequence type_kw
           (Earley_core.Earley.fsequence
              (Earley_core.Earley.apply (fun f -> f [])
                 (Earley_core.Earley.fixpoint1' (fun l -> l)
                    (Earley_core.Earley.fsequence_position typeconstr_name
                       (Earley_core.Earley.empty
                          (fun str ->
                             fun pos ->
                               fun str' ->
                                 fun pos' ->
                                   fun id ->
                                     let _loc_id = locate str pos str' pos' in
                                     id_loc id _loc_id)))
                    (fun x -> fun f -> fun l -> f (x :: l))))
              (Earley_core.Earley.fsequence_ignore
                 (Earley_core.Earley.char '.' '.')
                 (Earley_core.Earley.fsequence typexpr
                    (Earley_core.Earley.empty
                       (fun te -> fun ids -> fun _default_0 -> (ids, te)))))))
    let method_type = Earley_core.Earley.declare_grammar "method_type"
    let _ =
      Earley_core.Earley.set_grammar method_type
        (Earley_core.Earley.alternatives
           [Earley_core.Earley.fsequence
              (typexpr_lvl (next_type_prio DashType))
              (Earley_core.Earley.empty (fun ty -> Oinherit ty));
           Earley_core.Earley.fsequence method_name
             (Earley_core.Earley.fsequence_ignore
                (Earley_core.Earley.char ':' ':')
                (Earley_core.Earley.fsequence poly_typexpr
                   (Earley_core.Earley.empty
                      (fun pte -> fun mn -> Otag (mn, [], pte)))))])
    let tag_spec = Earley_core.Earley.declare_grammar "tag_spec"
    let _ =
      Earley_core.Earley.set_grammar tag_spec
        (Earley_core.Earley.alternatives
           [Earley_core.Earley.fsequence typexpr
              (Earley_core.Earley.empty (fun te -> Rinherit te));
           Earley_core.Earley.fsequence_position tag_name
             (Earley_core.Earley.fsequence
                (Earley_core.Earley.option None
                   (Earley_core.Earley.apply (fun x -> Some x)
                      (Earley_core.Earley.fsequence_ignore of_kw
                         (Earley_core.Earley.fsequence
                            (Earley_core.Earley.option None
                               (Earley_core.Earley.apply (fun x -> Some x)
                                  (Earley_core.Earley.char '&' '&')))
                            (Earley_core.Earley.fsequence typexpr
                               (Earley_core.Earley.empty
                                  (fun _default_0 ->
                                     fun _default_1 ->
                                       (_default_1, _default_0))))))))
                (Earley_core.Earley.empty
                   (fun te ->
                      fun str ->
                        fun pos ->
                          fun str' ->
                            fun pos' ->
                              fun tn ->
                                let _loc_tn = locate str pos str' pos' in
                                let (amp, t) =
                                  match te with
                                  | None -> (true, [])
                                  | Some (amp, l) -> ((amp <> None), [l]) in
                                let tn = id_loc tn _loc_tn in
                                Rtag (tn, [], amp, t))))])
    let tag_spec_first = Earley_core.Earley.declare_grammar "tag_spec_first"
    let _ =
      Earley_core.Earley.set_grammar tag_spec_first
        (Earley_core.Earley.alternatives
           [Earley_core.Earley.fsequence
              (Earley_core.Earley.option None
                 (Earley_core.Earley.apply (fun x -> Some x) typexpr))
              (Earley_core.Earley.fsequence_ignore
                 (Earley_core.Earley.char '|' '|')
                 (Earley_core.Earley.fsequence tag_spec
                    (Earley_core.Earley.empty
                       (fun ts ->
                          fun te ->
                            match te with
                            | None -> [ts]
                            | Some te -> [Rinherit te; ts]))));
           Earley_core.Earley.fsequence_position tag_name
             (Earley_core.Earley.fsequence
                (Earley_core.Earley.option None
                   (Earley_core.Earley.apply (fun x -> Some x)
                      (Earley_core.Earley.fsequence_ignore of_kw
                         (Earley_core.Earley.fsequence
                            (Earley_core.Earley.option None
                               (Earley_core.Earley.apply (fun x -> Some x)
                                  (Earley_core.Earley.char '&' '&')))
                            (Earley_core.Earley.fsequence typexpr
                               (Earley_core.Earley.empty
                                  (fun _default_0 ->
                                     fun _default_1 ->
                                       (_default_1, _default_0))))))))
                (Earley_core.Earley.empty
                   (fun te ->
                      fun str ->
                        fun pos ->
                          fun str' ->
                            fun pos' ->
                              fun tn ->
                                let _loc_tn = locate str pos str' pos' in
                                let (amp, t) =
                                  match te with
                                  | None -> (true, [])
                                  | Some (amp, l) -> ((amp <> None), [l]) in
                                let tn = id_loc tn _loc_tn in
                                [Rtag (tn, [], amp, t)])))])
    let tag_spec_full = Earley_core.Earley.declare_grammar "tag_spec_full"
    let _ =
      Earley_core.Earley.set_grammar tag_spec_full
        (Earley_core.Earley.alternatives
           [Earley_core.Earley.fsequence typexpr
              (Earley_core.Earley.empty (fun te -> Rinherit te));
           Earley_core.Earley.fsequence_position tag_name
             (Earley_core.Earley.fsequence
                (Earley_core.Earley.option (true, [])
                   (Earley_core.Earley.fsequence of_kw
                      (Earley_core.Earley.fsequence
                         (Earley_core.Earley.option None
                            (Earley_core.Earley.apply (fun x -> Some x)
                               (Earley_core.Earley.char '&' '&')))
                         (Earley_core.Earley.fsequence typexpr
                            (Earley_core.Earley.fsequence
                               (Earley_core.Earley.apply (fun f -> f [])
                                  (Earley_core.Earley.fixpoint' (fun l -> l)
                                     (Earley_core.Earley.fsequence_ignore
                                        (Earley_core.Earley.char '&' '&')
                                        (Earley_core.Earley.fsequence typexpr
                                           (Earley_core.Earley.empty
                                              (fun te -> te))))
                                     (fun x -> fun f -> fun l -> f (x :: l))))
                               (Earley_core.Earley.empty
                                  (fun tes ->
                                     fun te ->
                                       fun amp ->
                                         fun _default_0 ->
                                           ((amp <> None), (te :: tes)))))))))
                (Earley_core.Earley.empty
                   (fun ((amp, tes) as _default_0) ->
                      fun str ->
                        fun pos ->
                          fun str' ->
                            fun pos' ->
                              fun tn ->
                                let _loc_tn = locate str pos str' pos' in
                                let tn = id_loc tn _loc_tn in
                                Rtag (tn, [], amp, tes))))])
    let polymorphic_variant_type =
      Earley_core.Earley.declare_grammar "polymorphic_variant_type"
    let _ =
      Earley_core.Earley.set_grammar polymorphic_variant_type
        (Earley_core.Earley.alternatives
           [Earley_core.Earley.fsequence_ignore
              (Earley_core.Earley.string "[<" "[<")
              (Earley_core.Earley.fsequence
                 (Earley_core.Earley.option None
                    (Earley_core.Earley.apply (fun x -> Some x)
                       (Earley_core.Earley.char '|' '|')))
                 (Earley_core.Earley.fsequence tag_spec_full
                    (Earley_core.Earley.fsequence
                       (Earley_core.Earley.apply (fun f -> f [])
                          (Earley_core.Earley.fixpoint' (fun l -> l)
                             (Earley_core.Earley.fsequence_ignore
                                (Earley_core.Earley.char '|' '|')
                                (Earley_core.Earley.fsequence tag_spec_full
                                   (Earley_core.Earley.empty (fun tsf -> tsf))))
                             (fun x -> fun f -> fun l -> f (x :: l))))
                       (Earley_core.Earley.fsequence
                          (Earley_core.Earley.option []
                             (Earley_core.Earley.fsequence_ignore
                                (Earley_core.Earley.char '>' '>')
                                (Earley_core.Earley.fsequence
                                   (Earley_core.Earley.apply (fun f -> f [])
                                      (Earley_core.Earley.fixpoint1'
                                         (fun l -> l) tag_name
                                         (fun x ->
                                            fun f -> fun l -> f (x :: l))))
                                   (Earley_core.Earley.empty (fun tns -> tns)))))
                          (Earley_core.Earley.fsequence_ignore
                             (Earley_core.Earley.char ']' ']')
                             (Earley_core.Earley.empty_pos
                                (fun __loc__start__buf ->
                                   fun __loc__start__pos ->
                                     fun __loc__end__buf ->
                                       fun __loc__end__pos ->
                                         let _loc =
                                           locate __loc__start__buf
                                             __loc__start__pos
                                             __loc__end__buf __loc__end__pos in
                                         fun tns ->
                                           fun tfss ->
                                             fun tfs ->
                                               fun _default_0 ->
                                                 Typ.variant ~loc:_loc (tfs
                                                   :: tfss) Closed (Some tns))))))));
           Earley_core.Earley.fsequence_ignore
             (Earley_core.Earley.char '[' '[')
             (Earley_core.Earley.fsequence tag_spec_first
                (Earley_core.Earley.fsequence
                   (Earley_core.Earley.apply (fun f -> f [])
                      (Earley_core.Earley.fixpoint' (fun l -> l)
                         (Earley_core.Earley.fsequence_ignore
                            (Earley_core.Earley.char '|' '|')
                            (Earley_core.Earley.fsequence tag_spec
                               (Earley_core.Earley.empty (fun ts -> ts))))
                         (fun x -> fun f -> fun l -> f (x :: l))))
                   (Earley_core.Earley.fsequence_ignore
                      (Earley_core.Earley.char ']' ']')
                      (Earley_core.Earley.empty_pos
                         (fun __loc__start__buf ->
                            fun __loc__start__pos ->
                              fun __loc__end__buf ->
                                fun __loc__end__pos ->
                                  let _loc =
                                    locate __loc__start__buf
                                      __loc__start__pos __loc__end__buf
                                      __loc__end__pos in
                                  fun tss ->
                                    fun tsf ->
                                      Typ.variant ~loc:_loc (tsf @ tss)
                                        Closed None)))));
           Earley_core.Earley.fsequence_ignore
             (Earley_core.Earley.string "[>" "[>")
             (Earley_core.Earley.fsequence
                (Earley_core.Earley.option None
                   (Earley_core.Earley.apply (fun x -> Some x) tag_spec))
                (Earley_core.Earley.fsequence
                   (Earley_core.Earley.apply (fun f -> f [])
                      (Earley_core.Earley.fixpoint' (fun l -> l)
                         (Earley_core.Earley.fsequence_ignore
                            (Earley_core.Earley.char '|' '|')
                            (Earley_core.Earley.fsequence tag_spec
                               (Earley_core.Earley.empty (fun ts -> ts))))
                         (fun x -> fun f -> fun l -> f (x :: l))))
                   (Earley_core.Earley.fsequence_ignore
                      (Earley_core.Earley.char ']' ']')
                      (Earley_core.Earley.empty_pos
                         (fun __loc__start__buf ->
                            fun __loc__start__pos ->
                              fun __loc__end__buf ->
                                fun __loc__end__pos ->
                                  let _loc =
                                    locate __loc__start__buf
                                      __loc__start__pos __loc__end__buf
                                      __loc__end__pos in
                                  fun tss ->
                                    fun ts ->
                                      let tss =
                                        match ts with
                                        | None -> tss
                                        | Some ts -> ts :: tss in
                                      Typ.variant ~loc:_loc tss Open None)))))] : 
        core_type grammar)
    let package_constraint =
      Earley_core.Earley.declare_grammar "package_constraint"
    let _ =
      Earley_core.Earley.set_grammar package_constraint
        (Earley_core.Earley.fsequence type_kw
           (Earley_core.Earley.fsequence_position typeconstr
              (Earley_core.Earley.fsequence_ignore
                 (Earley_core.Earley.char '=' '=')
                 (Earley_core.Earley.fsequence typexpr
                    (Earley_core.Earley.empty
                       (fun te ->
                          fun str ->
                            fun pos ->
                              fun str' ->
                                fun pos' ->
                                  fun tc ->
                                    let _loc_tc = locate str pos str' pos' in
                                    fun _default_0 ->
                                      let tc = id_loc tc _loc_tc in (tc, te)))))))
    let package_type = Earley_core.Earley.declare_grammar "package_type"
    let _ =
      Earley_core.Earley.set_grammar package_type
        (Earley_core.Earley.fsequence_position modtype_path
           (Earley_core.Earley.fsequence
              (Earley_core.Earley.option []
                 (Earley_core.Earley.fsequence with_kw
                    (Earley_core.Earley.fsequence package_constraint
                       (Earley_core.Earley.fsequence
                          (Earley_core.Earley.apply (fun f -> f [])
                             (Earley_core.Earley.fixpoint' (fun l -> l)
                                (Earley_core.Earley.fsequence_ignore and_kw
                                   (Earley_core.Earley.fsequence
                                      package_constraint
                                      (Earley_core.Earley.empty
                                         (fun _default_0 -> _default_0))))
                                (fun x -> fun f -> fun l -> f (x :: l))))
                          (Earley_core.Earley.empty
                             (fun pcs ->
                                fun pc -> fun _default_0 -> pc :: pcs))))))
              (Earley_core.Earley.empty_pos
                 (fun __loc__start__buf ->
                    fun __loc__start__pos ->
                      fun __loc__end__buf ->
                        fun __loc__end__pos ->
                          let _loc =
                            locate __loc__start__buf __loc__start__pos
                              __loc__end__buf __loc__end__pos in
                          fun cs ->
                            fun str ->
                              fun pos ->
                                fun str' ->
                                  fun pos' ->
                                    fun mtp ->
                                      let _loc_mtp = locate str pos str' pos' in
                                      Typ.package ~loc:_loc
                                        (id_loc mtp _loc_mtp) cs))))
    let opt_present = Earley_core.Earley.declare_grammar "opt_present"
    let _ =
      Earley_core.Earley.set_grammar opt_present
        (Earley_core.Earley.alternatives
           [Earley_core.Earley.fsequence_ignore (Earley_core.Earley.empty ())
              (Earley_core.Earley.empty []);
           Earley_core.Earley.fsequence_ignore
             (Earley_core.Earley.string "[>" "[>")
             (Earley_core.Earley.fsequence
                (Earley_core.Earley.apply (fun f -> f [])
                   (Earley_core.Earley.fixpoint1' (fun l -> l) tag_name
                      (fun x -> fun f -> fun l -> f (x :: l))))
                (Earley_core.Earley.fsequence_ignore
                   (Earley_core.Earley.string "]" "]")
                   (Earley_core.Earley.empty (fun l -> l))))])
    let mkoption loc d =
      let loc = ghost loc in
      Typ.constr ~loc (id_loc (Ldot ((Lident "*predef*"), "option")) loc) [d]
    let extra_types_grammar lvl =
      alternatives (List.map (fun g -> g lvl) extra_types)
    let op_cl =
      Earley_core.Earley.alternatives
        [Earley_core.Earley.fsequence_ignore (Earley_core.Earley.empty ())
           (Earley_core.Earley.empty Closed);
        Earley_core.Earley.fsequence (Earley_core.Earley.string ".." "..")
          (Earley_core.Earley.empty (fun d -> Open))]
    let _ =
      set_typexpr_lvl
        ([(((fun _ -> true)),
            (Earley_core.Earley.fsequence_ignore
               (Earley_core.Earley.char '$' '$')
               (Earley_core.Earley.fsequence_ignore
                  (Earley_core.Earley.no_blank_test ())
                  (Earley_core.Earley.fsequence
                     (Earley_core.Earley.option "type"
                        (Earley_core.Earley.fsequence
                           (Earley_str.regexp ~name:"[a-z]+" "[a-z]+"
                              (fun groupe -> groupe 0))
                           (Earley_core.Earley.fsequence_ignore
                              (Earley_core.Earley.no_blank_test ())
                              (Earley_core.Earley.fsequence_ignore
                                 (Earley_core.Earley.char ':' ':')
                                 (Earley_core.Earley.empty
                                    (fun _default_0 -> _default_0))))))
                     (Earley_core.Earley.fsequence expression
                        (Earley_core.Earley.fsequence_ignore
                           (Earley_core.Earley.no_blank_test ())
                           (Earley_core.Earley.fsequence_ignore
                              (Earley_core.Earley.char '$' '$')
                              (Earley_core.Earley.empty_pos
                                 (fun __loc__start__buf ->
                                    fun __loc__start__pos ->
                                      fun __loc__end__buf ->
                                        fun __loc__end__pos ->
                                          let _loc =
                                            locate __loc__start__buf
                                              __loc__start__pos
                                              __loc__end__buf __loc__end__pos in
                                          fun e ->
                                            fun aq ->
                                              let open Quote in
                                                let e_loc =
                                                  exp_ident _loc "_loc" in
                                                let generic_antiquote e =
                                                  function
                                                  | Quote_ptyp -> e
                                                  | _ ->
                                                      failwith
                                                        "invalid antiquotation" in
                                                let f =
                                                  match aq with
                                                  | "type" ->
                                                      generic_antiquote e
                                                  | "tuple" ->
                                                      generic_antiquote
                                                        (quote_apply e_loc
                                                           _loc
                                                           (pa_ast
                                                              "typ_tuple")
                                                           [quote_location_t
                                                              e_loc _loc _loc;
                                                           e])
                                                  | _ -> give_up () in
                                                Quote.ptyp_antiquotation _loc
                                                  f)))))))));
         (((fun _ -> true)),
           (Earley_core.Earley.fsequence_ignore
              (Earley_core.Earley.string "'" "'")
              (Earley_core.Earley.fsequence ident
                 (Earley_core.Earley.empty_pos
                    (fun __loc__start__buf ->
                       fun __loc__start__pos ->
                         fun __loc__end__buf ->
                           fun __loc__end__pos ->
                             let _loc =
                               locate __loc__start__buf __loc__start__pos
                                 __loc__end__buf __loc__end__pos in
                             fun id -> Typ.var ~loc:_loc id)))));
         (((fun _ -> true)),
           (Earley_core.Earley.fsequence joker_kw
              (Earley_core.Earley.empty_pos
                 (fun __loc__start__buf ->
                    fun __loc__start__pos ->
                      fun __loc__end__buf ->
                        fun __loc__end__pos ->
                          let _loc =
                            locate __loc__start__buf __loc__start__pos
                              __loc__end__buf __loc__end__pos in
                          fun _default_0 -> Typ.any ~loc:_loc ()))));
         (((fun _ -> true)),
           (Earley_core.Earley.fsequence_ignore
              (Earley_core.Earley.char '(' '(')
              (Earley_core.Earley.fsequence module_kw
                 (Earley_core.Earley.fsequence package_type
                    (Earley_core.Earley.fsequence_ignore
                       (Earley_core.Earley.char ')' ')')
                       (Earley_core.Earley.empty_pos
                          (fun __loc__start__buf ->
                             fun __loc__start__pos ->
                               fun __loc__end__buf ->
                                 fun __loc__end__pos ->
                                   let _loc =
                                     locate __loc__start__buf
                                       __loc__start__pos __loc__end__buf
                                       __loc__end__pos in
                                   fun pt ->
                                     fun _default_0 ->
                                       loc_typ _loc pt.ptyp_desc)))))));
         (((fun (allow_par, lvl) -> allow_par)),
           (Earley_core.Earley.fsequence_ignore
              (Earley_core.Earley.char '(' '(')
              (Earley_core.Earley.fsequence typexpr
                 (Earley_core.Earley.fsequence
                    (Earley_core.Earley.apply (fun f -> f [])
                       (Earley_core.Earley.fixpoint' (fun l -> l) attribute
                          (fun x -> fun f -> fun l -> f (x :: l))))
                    (Earley_core.Earley.fsequence_ignore
                       (Earley_core.Earley.char ')' ')')
                       (Earley_core.Earley.empty
                          (fun at ->
                             fun te -> { te with ptyp_attributes = at })))))));
         (((fun (allow_par, lvl) -> lvl <= Arr)),
           (Earley_core.Earley.fsequence ty_opt_label
              (Earley_core.Earley.fsequence (typexpr_lvl ProdType)
                 (Earley_core.Earley.fsequence arrow_re
                    (Earley_core.Earley.fsequence (typexpr_lvl Arr)
                       (Earley_core.Earley.empty_pos
                          (fun __loc__start__buf ->
                             fun __loc__start__pos ->
                               fun __loc__end__buf ->
                                 fun __loc__end__pos ->
                                   let _loc =
                                     locate __loc__start__buf
                                       __loc__start__pos __loc__end__buf
                                       __loc__end__pos in
                                   fun te' ->
                                     fun _default_0 ->
                                       fun te ->
                                         fun ln ->
                                           Typ.arrow ~loc:_loc ln te te')))))));
         (((fun (allow_par, lvl) -> lvl <= Arr)),
           (Earley_core.Earley.fsequence label_name
              (Earley_core.Earley.fsequence_ignore
                 (Earley_core.Earley.char ':' ':')
                 (Earley_core.Earley.fsequence (typexpr_lvl ProdType)
                    (Earley_core.Earley.fsequence arrow_re
                       (Earley_core.Earley.fsequence (typexpr_lvl Arr)
                          (Earley_core.Earley.empty_pos
                             (fun __loc__start__buf ->
                                fun __loc__start__pos ->
                                  fun __loc__end__buf ->
                                    fun __loc__end__pos ->
                                      let _loc =
                                        locate __loc__start__buf
                                          __loc__start__pos __loc__end__buf
                                          __loc__end__pos in
                                      fun te' ->
                                        fun _default_0 ->
                                          fun te ->
                                            fun ln ->
                                              Typ.arrow ~loc:_loc
                                                (labelled ln) te te'))))))));
         (((fun (allow_par, lvl) -> lvl <= Arr)),
           (Earley_core.Earley.fsequence (typexpr_lvl ProdType)
              (Earley_core.Earley.fsequence arrow_re
                 (Earley_core.Earley.fsequence (typexpr_lvl Arr)
                    (Earley_core.Earley.empty_pos
                       (fun __loc__start__buf ->
                          fun __loc__start__pos ->
                            fun __loc__end__buf ->
                              fun __loc__end__pos ->
                                let _loc =
                                  locate __loc__start__buf __loc__start__pos
                                    __loc__end__buf __loc__end__pos in
                                fun te' ->
                                  fun _default_0 ->
                                    fun te ->
                                      Typ.arrow ~loc:_loc nolabel te te'))))));
         (((fun _ -> true)),
           (Earley_core.Earley.fsequence_position typeconstr
              (Earley_core.Earley.empty_pos
                 (fun __loc__start__buf ->
                    fun __loc__start__pos ->
                      fun __loc__end__buf ->
                        fun __loc__end__pos ->
                          let _loc =
                            locate __loc__start__buf __loc__start__pos
                              __loc__end__buf __loc__end__pos in
                          fun str ->
                            fun pos ->
                              fun str' ->
                                fun pos' ->
                                  fun tc ->
                                    let _loc_tc = locate str pos str' pos' in
                                    Typ.constr ~loc:_loc (id_loc tc _loc_tc)
                                      []))));
         (((fun (allow_par, lvl) -> lvl <= AppType)),
           (Earley_core.Earley.fsequence_ignore
              (Earley_core.Earley.char '(' '(')
              (Earley_core.Earley.fsequence typexpr
                 (Earley_core.Earley.fsequence
                    (Earley_core.Earley.apply (fun f -> f [])
                       (Earley_core.Earley.fixpoint1' (fun l -> l)
                          (Earley_core.Earley.fsequence_ignore
                             (Earley_core.Earley.char ',' ',')
                             (Earley_core.Earley.fsequence typexpr
                                (Earley_core.Earley.empty (fun te -> te))))
                          (fun x -> fun f -> fun l -> f (x :: l))))
                    (Earley_core.Earley.fsequence_ignore
                       (Earley_core.Earley.char ')' ')')
                       (Earley_core.Earley.fsequence_position typeconstr
                          (Earley_core.Earley.empty_pos
                             (fun __loc__start__buf ->
                                fun __loc__start__pos ->
                                  fun __loc__end__buf ->
                                    fun __loc__end__pos ->
                                      let _loc =
                                        locate __loc__start__buf
                                          __loc__start__pos __loc__end__buf
                                          __loc__end__pos in
                                      fun str ->
                                        fun pos ->
                                          fun str' ->
                                            fun pos' ->
                                              fun tc ->
                                                let _loc_tc =
                                                  locate str pos str' pos' in
                                                fun tes ->
                                                  fun te ->
                                                    Typ.constr ~loc:_loc
                                                      (id_loc tc _loc_tc) (te
                                                      :: tes)))))))));
         (((fun (allow_par, lvl) -> lvl <= AppType)),
           (Earley_core.Earley.fsequence (typexpr_lvl AppType)
              (Earley_core.Earley.fsequence_position typeconstr
                 (Earley_core.Earley.empty_pos
                    (fun __loc__start__buf ->
                       fun __loc__start__pos ->
                         fun __loc__end__buf ->
                           fun __loc__end__pos ->
                             let _loc =
                               locate __loc__start__buf __loc__start__pos
                                 __loc__end__buf __loc__end__pos in
                             fun str ->
                               fun pos ->
                                 fun str' ->
                                   fun pos' ->
                                     fun tc ->
                                       let _loc_tc = locate str pos str' pos' in
                                       fun t ->
                                         Typ.constr ~loc:_loc
                                           (id_loc tc _loc_tc) [t])))));
         (((fun _ -> true)), polymorphic_variant_type);
         (((fun _ -> true)),
           (Earley_core.Earley.fsequence_ignore
              (Earley_core.Earley.char '<' '<')
              (Earley_core.Earley.fsequence op_cl
                 (Earley_core.Earley.fsequence_ignore
                    (Earley_core.Earley.char '>' '>')
                    (Earley_core.Earley.empty_pos
                       (fun __loc__start__buf ->
                          fun __loc__start__pos ->
                            fun __loc__end__buf ->
                              fun __loc__end__pos ->
                                let _loc =
                                  locate __loc__start__buf __loc__start__pos
                                    __loc__end__buf __loc__end__pos in
                                fun rv -> Typ.object_ ~loc:_loc [] rv))))));
         (((fun _ -> true)),
           (Earley_core.Earley.fsequence_ignore
              (Earley_core.Earley.char '<' '<')
              (Earley_core.Earley.fsequence
                 (Earley.list1 method_type semi_col)
                 (Earley_core.Earley.fsequence
                    (Earley_core.Earley.option Closed
                       (Earley_core.Earley.fsequence_ignore semi_col
                          (Earley_core.Earley.fsequence op_cl
                             (Earley_core.Earley.empty
                                (fun _default_0 -> _default_0)))))
                    (Earley_core.Earley.fsequence_ignore
                       (Earley_core.Earley.char '>' '>')
                       (Earley_core.Earley.empty_pos
                          (fun __loc__start__buf ->
                             fun __loc__start__pos ->
                               fun __loc__end__buf ->
                                 fun __loc__end__pos ->
                                   let _loc =
                                     locate __loc__start__buf
                                       __loc__start__pos __loc__end__buf
                                       __loc__end__pos in
                                   fun rv ->
                                     fun mts -> Typ.object_ ~loc:_loc mts rv)))))));
         (((fun _ -> true)),
           (Earley_core.Earley.fsequence_ignore
              (Earley_core.Earley.char '#' '#')
              (Earley_core.Earley.fsequence_position class_path
                 (Earley_core.Earley.empty_pos
                    (fun __loc__start__buf ->
                       fun __loc__start__pos ->
                         fun __loc__end__buf ->
                           fun __loc__end__pos ->
                             let _loc =
                               locate __loc__start__buf __loc__start__pos
                                 __loc__end__buf __loc__end__pos in
                             fun str ->
                               fun pos ->
                                 fun str' ->
                                   fun pos' ->
                                     fun cp ->
                                       let _loc_cp = locate str pos str' pos' in
                                       Typ.class_ ~loc:_loc
                                         (id_loc cp _loc_cp) [])))));
         (((fun (allow_par, lvl) -> lvl <= DashType)),
           (Earley_core.Earley.fsequence (typexpr_lvl DashType)
              (Earley_core.Earley.fsequence_ignore
                 (Earley_core.Earley.char '#' '#')
                 (Earley_core.Earley.fsequence_position class_path
                    (Earley_core.Earley.empty_pos
                       (fun __loc__start__buf ->
                          fun __loc__start__pos ->
                            fun __loc__end__buf ->
                              fun __loc__end__pos ->
                                let _loc =
                                  locate __loc__start__buf __loc__start__pos
                                    __loc__end__buf __loc__end__pos in
                                fun str ->
                                  fun pos ->
                                    fun str' ->
                                      fun pos' ->
                                        fun cp ->
                                          let _loc_cp =
                                            locate str pos str' pos' in
                                          fun te ->
                                            Typ.class_ ~loc:_loc
                                              (id_loc cp _loc_cp) [te]))))));
         (((fun _ -> true)),
           (Earley_core.Earley.fsequence_ignore
              (Earley_core.Earley.char '(' '(')
              (Earley_core.Earley.fsequence typexpr
                 (Earley_core.Earley.fsequence
                    (Earley_core.Earley.apply (fun f -> f [])
                       (Earley_core.Earley.fixpoint' (fun l -> l)
                          (Earley_core.Earley.fsequence_ignore
                             (Earley_core.Earley.char ',' ',')
                             (Earley_core.Earley.fsequence typexpr
                                (Earley_core.Earley.empty (fun te -> te))))
                          (fun x -> fun f -> fun l -> f (x :: l))))
                    (Earley_core.Earley.fsequence_ignore
                       (Earley_core.Earley.char ')' ')')
                       (Earley_core.Earley.fsequence_ignore
                          (Earley_core.Earley.char '#' '#')
                          (Earley_core.Earley.fsequence_position class_path
                             (Earley_core.Earley.empty_pos
                                (fun __loc__start__buf ->
                                   fun __loc__start__pos ->
                                     fun __loc__end__buf ->
                                       fun __loc__end__pos ->
                                         let _loc =
                                           locate __loc__start__buf
                                             __loc__start__pos
                                             __loc__end__buf __loc__end__pos in
                                         fun str ->
                                           fun pos ->
                                             fun str' ->
                                               fun pos' ->
                                                 fun cp ->
                                                   let _loc_cp =
                                                     locate str pos str' pos' in
                                                   fun tes ->
                                                     fun te ->
                                                       Typ.class_ ~loc:_loc
                                                         (id_loc cp _loc_cp)
                                                         (te :: tes))))))))));
         (((fun (allow_par, lvl) -> lvl <= ProdType)),
           (Earley_core.Earley.fsequence
              (Earley.list2 (typexpr_lvl DashType)
                 (Earley_core.Earley.alternatives
                    [Earley_core.Earley.fsequence_ignore
                       (Earley_core.Earley.string "\195\151" "\195\151")
                       (Earley_core.Earley.empty ());
                    Earley_core.Earley.fsequence_ignore
                      (Earley_core.Earley.char '*' '*')
                      (Earley_core.Earley.empty ())]))
              (Earley_core.Earley.empty_pos
                 (fun __loc__start__buf ->
                    fun __loc__start__pos ->
                      fun __loc__end__buf ->
                        fun __loc__end__pos ->
                          let _loc =
                            locate __loc__start__buf __loc__start__pos
                              __loc__end__buf __loc__end__pos in
                          fun tes -> Typ.tuple ~loc:_loc tes))));
         (((fun (allow_par, lvl) -> lvl <= As)),
           (Earley_core.Earley.fsequence (typexpr_lvl As)
              (Earley_core.Earley.fsequence as_kw
                 (Earley_core.Earley.fsequence_ignore
                    (Earley_core.Earley.char '\'' '\'')
                    (Earley_core.Earley.fsequence ident
                       (Earley_core.Earley.empty_pos
                          (fun __loc__start__buf ->
                             fun __loc__start__pos ->
                               fun __loc__end__buf ->
                                 fun __loc__end__pos ->
                                   let _loc =
                                     locate __loc__start__buf
                                       __loc__start__pos __loc__end__buf
                                       __loc__end__pos in
                                   fun id ->
                                     fun _default_0 ->
                                       fun te -> Typ.alias ~loc:_loc te id)))))))],
          (fun (allow_par, lvl) -> [extra_types_grammar lvl]))
    let type_param = Earley_core.Earley.declare_grammar "type_param"
    let _ =
      Earley_core.Earley.set_grammar type_param
        (Earley_core.Earley.alternatives
           [Earley_core.Earley.fsequence opt_variance
              (Earley_core.Earley.fsequence_position
                 (Earley_core.Earley.char '_' '_')
                 (Earley_core.Earley.empty
                    (fun str ->
                       fun pos ->
                         fun str' ->
                           fun pos' ->
                             fun j ->
                               let _loc_j = locate str pos str' pos' in
                               fun var -> ((Joker _loc_j), var))));
           Earley_core.Earley.fsequence opt_variance
             (Earley_core.Earley.fsequence_position
                (Earley_core.Earley.fsequence_ignore
                   (Earley_core.Earley.char '\'' '\'')
                   (Earley_core.Earley.fsequence ident
                      (Earley_core.Earley.empty
                         (fun _default_0 -> _default_0))))
                (Earley_core.Earley.empty
                   (fun str ->
                      fun pos ->
                        fun str' ->
                          fun pos' ->
                            fun id ->
                              let _loc_id = locate str pos str' pos' in
                              fun var -> ((Name (id_loc id _loc_id)), var))))])
    let type_params = Earley_core.Earley.declare_grammar "type_params"
    let _ =
      Earley_core.Earley.set_grammar type_params
        (Earley_core.Earley.alternatives
           [Earley_core.Earley.fsequence_ignore
              (Earley_core.Earley.char '(' '(')
              (Earley_core.Earley.fsequence type_param
                 (Earley_core.Earley.fsequence
                    (Earley_core.Earley.apply (fun f -> f [])
                       (Earley_core.Earley.fixpoint' (fun l -> l)
                          (Earley_core.Earley.fsequence_ignore
                             (Earley_core.Earley.char ',' ',')
                             (Earley_core.Earley.fsequence type_param
                                (Earley_core.Earley.empty (fun tp -> tp))))
                          (fun x -> fun f -> fun l -> f (x :: l))))
                    (Earley_core.Earley.fsequence_ignore
                       (Earley_core.Earley.char ')' ')')
                       (Earley_core.Earley.empty
                          (fun tps -> fun tp -> tp :: tps)))));
           Earley_core.Earley.fsequence_ignore (Earley_core.Earley.empty ())
             (Earley_core.Earley.empty []);
           Earley_core.Earley.fsequence type_param
             (Earley_core.Earley.empty (fun tp -> [tp]))])
    let type_equation = Earley_core.Earley.declare_grammar "type_equation"
    let _ =
      Earley_core.Earley.set_grammar type_equation
        (Earley_core.Earley.fsequence_ignore
           (Earley_core.Earley.char '=' '=')
           (Earley_core.Earley.fsequence private_flag
              (Earley_core.Earley.fsequence typexpr
                 (Earley_core.Earley.empty (fun te -> fun p -> (p, te))))))
    let type_constraint =
      Earley_core.Earley.declare_grammar "type_constraint"
    let _ =
      Earley_core.Earley.set_grammar type_constraint
        (Earley_core.Earley.fsequence constraint_kw
           (Earley_core.Earley.fsequence_position
              (Earley_core.Earley.fsequence_ignore
                 (Earley_core.Earley.char '\'' '\'')
                 (Earley_core.Earley.fsequence ident
                    (Earley_core.Earley.empty (fun _default_0 -> _default_0))))
              (Earley_core.Earley.fsequence_ignore
                 (Earley_core.Earley.char '=' '=')
                 (Earley_core.Earley.fsequence typexpr
                    (Earley_core.Earley.empty_pos
                       (fun __loc__start__buf ->
                          fun __loc__start__pos ->
                            fun __loc__end__buf ->
                              fun __loc__end__pos ->
                                let _loc =
                                  locate __loc__start__buf __loc__start__pos
                                    __loc__end__buf __loc__end__pos in
                                fun te ->
                                  fun str ->
                                    fun pos ->
                                      fun str' ->
                                        fun pos' ->
                                          fun id ->
                                            let _loc_id =
                                              locate str pos str' pos' in
                                            fun _default_0 ->
                                              ((Typ.var ~loc:_loc_id id), te,
                                                (merge2 _loc_id _loc))))))))
    let constr_name2 = Earley_core.Earley.declare_grammar "constr_name2"
    let _ =
      Earley_core.Earley.set_grammar constr_name2
        (Earley_core.Earley.alternatives
           [Earley_core.Earley.fsequence_ignore
              (Earley_core.Earley.char '(' '(')
              (Earley_core.Earley.fsequence_ignore
                 (Earley_core.Earley.char ')' ')')
                 (Earley_core.Earley.empty "()"));
           constr_name])
    let (bar, bar__set__grammar) = Earley_core.Earley.grammar_family "bar"
    let _ =
      bar__set__grammar
        (fun with_bar ->
           Earley_core.Earley.alternatives
             ((Earley_core.Earley.fsequence_ignore
                 (Earley_core.Earley.char '|' '|')
                 (Earley_core.Earley.empty ())) ::
             ((if not with_bar
               then
                 [Earley_core.Earley.fsequence_ignore
                    (Earley_core.Earley.empty ())
                    (Earley_core.Earley.empty ())]
               else []) @ [])))
    let (constr_decl, constr_decl__set__grammar) =
      Earley_core.Earley.grammar_family "constr_decl"
    let _ =
      constr_decl__set__grammar
        (fun with_bar ->
           Earley_core.Earley.fsequence (bar with_bar)
             (Earley_core.Earley.fsequence_position constr_name2
                (Earley_core.Earley.fsequence
                   (Earley_core.Earley.alternatives
                      [Earley_core.Earley.fsequence_ignore
                         (Earley_core.Earley.char ':' ':')
                         (Earley_core.Earley.fsequence_ignore
                            (Earley_core.Earley.char '{' '{')
                            (Earley_core.Earley.fsequence field_decl_list
                               (Earley_core.Earley.fsequence_ignore
                                  (Earley_core.Earley.char '}' '}')
                                  (Earley_core.Earley.fsequence arrow_re
                                     (Earley_core.Earley.fsequence
                                        (typexpr_lvl (next_type_prio Arr))
                                        (Earley_core.Earley.empty
                                           (fun te ->
                                              fun _default_0 ->
                                                fun fds ->
                                                  ((Pcstr_record fds),
                                                    (Some te)))))))));
                      Earley_core.Earley.fsequence
                        (Earley_core.Earley.option None
                           (Earley_core.Earley.apply (fun x -> Some x)
                              (Earley_core.Earley.fsequence_ignore of_kw
                                 (Earley_core.Earley.fsequence
                                    (Earley_core.Earley.alternatives
                                       [Earley_core.Earley.fsequence
                                          typexpr_nopar
                                          (Earley_core.Earley.empty
                                             (fun te -> (te, false)));
                                       Earley_core.Earley.fsequence_ignore
                                         (Earley_core.Earley.char '(' '(')
                                         (Earley_core.Earley.fsequence
                                            typexpr
                                            (Earley_core.Earley.fsequence_ignore
                                               (Earley_core.Earley.char ')'
                                                  ')')
                                               (Earley_core.Earley.empty
                                                  (fun te -> (te, true)))))])
                                    (Earley_core.Earley.empty
                                       (fun _default_0 -> _default_0))))))
                        (Earley_core.Earley.empty
                           (fun te ->
                              let tes =
                                match te with
                                | None -> []
                                | Some
                                    ({ ptyp_desc = Ptyp_tuple tes }, false)
                                    -> tes
                                | Some (t, _) -> [t] in
                              ((Pcstr_tuple tes), None)));
                      Earley_core.Earley.fsequence of_kw
                        (Earley_core.Earley.fsequence_ignore
                           (Earley_core.Earley.char '{' '{')
                           (Earley_core.Earley.fsequence field_decl_list
                              (Earley_core.Earley.fsequence_ignore
                                 (Earley_core.Earley.char '}' '}')
                                 (Earley_core.Earley.empty
                                    (fun fds ->
                                       fun _default_0 ->
                                         ((Pcstr_record fds), None))))));
                      Earley_core.Earley.fsequence_ignore
                        (Earley_core.Earley.char ':' ':')
                        (Earley_core.Earley.fsequence
                           (Earley_core.Earley.option []
                              (Earley_core.Earley.fsequence
                                 (typexpr_lvl (next_type_prio ProdType))
                                 (Earley_core.Earley.fsequence
                                    (Earley_core.Earley.apply (fun f -> f [])
                                       (Earley_core.Earley.fixpoint'
                                          (fun l -> l)
                                          (Earley_core.Earley.fsequence_ignore
                                             (Earley_core.Earley.char '*' '*')
                                             (Earley_core.Earley.fsequence
                                                (typexpr_lvl
                                                   (next_type_prio ProdType))
                                                (Earley_core.Earley.empty
                                                   (fun _default_0 ->
                                                      _default_0))))
                                          (fun x ->
                                             fun f -> fun l -> f (x :: l))))
                                    (Earley_core.Earley.fsequence arrow_re
                                       (Earley_core.Earley.empty
                                          (fun _default_0 ->
                                             fun tes -> fun te -> te :: tes))))))
                           (Earley_core.Earley.fsequence
                              (typexpr_lvl (next_type_prio Arr))
                              (Earley_core.Earley.empty
                                 (fun te ->
                                    fun tes -> ((Pcstr_tuple tes), (Some te))))))])
                   (Earley_core.Earley.fsequence post_item_attributes
                      (Earley_core.Earley.empty
                         (fun a ->
                            fun ((args, res) as _default_0) ->
                              fun str ->
                                fun pos ->
                                  fun str' ->
                                    fun pos' ->
                                      fun cn ->
                                        let _loc_cn =
                                          locate str pos str' pos' in
                                        fun _default_1 ->
                                          let name = id_loc cn _loc_cn in
                                          (name, args, res, a)))))))
    [@@@ocaml.text " FIXME OCAML: the bar is included in position "]
    let (type_constr_decl, type_constr_decl__set__grammar) =
      Earley_core.Earley.grammar_family "type_constr_decl"
    let _ =
      type_constr_decl__set__grammar
        (fun with_bar ->
           Earley_core.Earley.fsequence (constr_decl with_bar)
             (Earley_core.Earley.empty_pos
                (fun __loc__start__buf ->
                   fun __loc__start__pos ->
                     fun __loc__end__buf ->
                       fun __loc__end__pos ->
                         let _loc =
                           locate __loc__start__buf __loc__start__pos
                             __loc__end__buf __loc__end__pos in
                         fun ((name, args, res, a) as _default_0) ->
                           Type.constructor ~attrs:(attach_attrib _loc a)
                             ~loc:_loc ~args ?res name)))
    let (type_constr_extn, type_constr_extn__set__grammar) =
      Earley_core.Earley.grammar_family "type_constr_extn"
    let _ =
      type_constr_extn__set__grammar
        (fun with_bar ->
           Earley_core.Earley.alternatives
             [Earley_core.Earley.fsequence_position lident
                (Earley_core.Earley.fsequence_ignore
                   (Earley_core.Earley.char '=' '=')
                   (Earley_core.Earley.fsequence_position constr
                      (Earley_core.Earley.fsequence post_item_attributes
                         (Earley_core.Earley.empty_pos
                            (fun __loc__start__buf ->
                               fun __loc__start__pos ->
                                 fun __loc__end__buf ->
                                   fun __loc__end__pos ->
                                     let _loc =
                                       locate __loc__start__buf
                                         __loc__start__pos __loc__end__buf
                                         __loc__end__pos in
                                     fun a ->
                                       fun str ->
                                         fun pos ->
                                           fun str' ->
                                             fun pos' ->
                                               fun cn ->
                                                 let _loc_cn =
                                                   locate str pos str' pos' in
                                                 fun str ->
                                                   fun pos ->
                                                     fun str' ->
                                                       fun pos' ->
                                                         fun li ->
                                                           let _loc_li =
                                                             locate str pos
                                                               str' pos' in
                                                           Te.rebind
                                                             ~attrs:(
                                                             attach_attrib
                                                               _loc a)
                                                             ~loc:_loc
                                                             (id_loc li
                                                                _loc_li)
                                                             (id_loc cn
                                                                _loc_cn))))));
             Earley_core.Earley.fsequence (constr_decl with_bar)
               (Earley_core.Earley.empty_pos
                  (fun __loc__start__buf ->
                     fun __loc__start__pos ->
                       fun __loc__end__buf ->
                         fun __loc__end__pos ->
                           let _loc =
                             locate __loc__start__buf __loc__start__pos
                               __loc__end__buf __loc__end__pos in
                           fun ((name, args, res, a) as _default_0) ->
                             Te.decl ~attrs:(attach_attrib _loc a) ~loc:_loc
                               ~args ?res name))])
    let _ =
      set_grammar constr_decl_list
        (Earley_core.Earley.alternatives
           [Earley_core.Earley.fsequence_ignore (Earley_core.Earley.empty ())
              (Earley_core.Earley.empty []);
           Earley_core.Earley.fsequence (type_constr_decl false)
             (Earley_core.Earley.fsequence
                (Earley_core.Earley.apply (fun f -> f [])
                   (Earley_core.Earley.fixpoint' (fun l -> l)
                      (type_constr_decl true)
                      (fun x -> fun f -> fun l -> f (x :: l))))
                (Earley_core.Earley.empty (fun cds -> fun cd -> cd :: cds)));
           Earley_core.Earley.fsequence (Earley_core.Earley.char '$' '$')
             (Earley_core.Earley.fsequence_ignore
                (Earley_core.Earley.no_blank_test ())
                (Earley_core.Earley.fsequence (expression_lvl (NoMatch, App))
                   (Earley_core.Earley.fsequence_ignore
                      (Earley_core.Earley.no_blank_test ())
                      (Earley_core.Earley.fsequence_ignore
                         (Earley_core.Earley.char '$' '$')
                         (Earley_core.Earley.fsequence constr_decl_list
                            (Earley_core.Earley.empty_pos
                               (fun __loc__start__buf ->
                                  fun __loc__start__pos ->
                                    fun __loc__end__buf ->
                                      fun __loc__end__pos ->
                                        let _loc =
                                          locate __loc__start__buf
                                            __loc__start__pos __loc__end__buf
                                            __loc__end__pos in
                                        fun cds ->
                                          fun e ->
                                            fun dol ->
                                              (list_antiquotation _loc e) @
                                                cds)))))))])
    let constr_extn_list =
      Earley_core.Earley.declare_grammar "constr_extn_list"
    let _ =
      Earley_core.Earley.set_grammar constr_extn_list
        (Earley_core.Earley.alternatives
           [Earley_core.Earley.fsequence_ignore (Earley_core.Earley.empty ())
              (Earley_core.Earley.empty []);
           Earley_core.Earley.fsequence (type_constr_extn false)
             (Earley_core.Earley.fsequence
                (Earley_core.Earley.apply (fun f -> f [])
                   (Earley_core.Earley.fixpoint' (fun l -> l)
                      (type_constr_extn true)
                      (fun x -> fun f -> fun l -> f (x :: l))))
                (Earley_core.Earley.empty (fun cds -> fun cd -> cd :: cds)));
           Earley_core.Earley.fsequence (Earley_core.Earley.char '$' '$')
             (Earley_core.Earley.fsequence_ignore
                (Earley_core.Earley.no_blank_test ())
                (Earley_core.Earley.fsequence (expression_lvl (NoMatch, App))
                   (Earley_core.Earley.fsequence_ignore
                      (Earley_core.Earley.no_blank_test ())
                      (Earley_core.Earley.fsequence_ignore
                         (Earley_core.Earley.char '$' '$')
                         (Earley_core.Earley.fsequence constr_extn_list
                            (Earley_core.Earley.empty_pos
                               (fun __loc__start__buf ->
                                  fun __loc__start__pos ->
                                    fun __loc__end__buf ->
                                      fun __loc__end__pos ->
                                        let _loc =
                                          locate __loc__start__buf
                                            __loc__start__pos __loc__end__buf
                                            __loc__end__pos in
                                        fun cds ->
                                          fun e ->
                                            fun dol ->
                                              (list_antiquotation _loc e) @
                                                cds)))))))])
    let field_decl_semi =
      Earley_core.Earley.declare_grammar "field_decl_semi"
    let _ =
      Earley_core.Earley.set_grammar field_decl_semi
        (Earley_core.Earley.fsequence mutable_flag
           (Earley_core.Earley.fsequence_position field_name
              (Earley_core.Earley.fsequence_ignore
                 (Earley_core.Earley.string ":" ":")
                 (Earley_core.Earley.fsequence poly_typexpr
                    (Earley_core.Earley.fsequence semi_col
                       (Earley_core.Earley.empty_pos
                          (fun __loc__start__buf ->
                             fun __loc__start__pos ->
                               fun __loc__end__buf ->
                                 fun __loc__end__pos ->
                                   let _loc =
                                     locate __loc__start__buf
                                       __loc__start__pos __loc__end__buf
                                       __loc__end__pos in
                                   fun _default_0 ->
                                     fun pte ->
                                       fun str ->
                                         fun pos ->
                                           fun str' ->
                                             fun pos' ->
                                               fun fn ->
                                                 let _loc_fn =
                                                   locate str pos str' pos' in
                                                 fun m ->
                                                   label_declaration
                                                     ~attributes:(attach_attrib
                                                                    _loc [])
                                                     _loc (id_loc fn _loc_fn)
                                                     m pte)))))))
    let field_decl = Earley_core.Earley.declare_grammar "field_decl"
    let _ =
      Earley_core.Earley.set_grammar field_decl
        (Earley_core.Earley.fsequence mutable_flag
           (Earley_core.Earley.fsequence_position field_name
              (Earley_core.Earley.fsequence_ignore
                 (Earley_core.Earley.string ":" ":")
                 (Earley_core.Earley.fsequence poly_typexpr
                    (Earley_core.Earley.empty_pos
                       (fun __loc__start__buf ->
                          fun __loc__start__pos ->
                            fun __loc__end__buf ->
                              fun __loc__end__pos ->
                                let _loc =
                                  locate __loc__start__buf __loc__start__pos
                                    __loc__end__buf __loc__end__pos in
                                fun pte ->
                                  fun str ->
                                    fun pos ->
                                      fun str' ->
                                        fun pos' ->
                                          fun fn ->
                                            let _loc_fn =
                                              locate str pos str' pos' in
                                            fun m ->
                                              label_declaration
                                                ~attributes:(attach_attrib
                                                               _loc []) _loc
                                                (id_loc fn _loc_fn) m pte))))))
    let field_decl_aux = Earley_core.Earley.declare_grammar "field_decl_aux"
    let _ =
      Earley_core.Earley.set_grammar field_decl_aux
        (Earley_core.Earley.alternatives
           [Earley_core.Earley.fsequence (Earley_core.Earley.char '$' '$')
              (Earley_core.Earley.fsequence_ignore
                 (Earley_core.Earley.no_blank_test ())
                 (Earley_core.Earley.fsequence
                    (expression_lvl (NoMatch, App))
                    (Earley_core.Earley.fsequence_ignore
                       (Earley_core.Earley.no_blank_test ())
                       (Earley_core.Earley.fsequence_ignore
                          (Earley_core.Earley.char '$' '$')
                          (Earley_core.Earley.fsequence
                             (Earley_core.Earley.option None
                                (Earley_core.Earley.apply (fun x -> Some x)
                                   (Earley_core.Earley.char ';' ';')))
                             (Earley_core.Earley.fsequence field_decl_list
                                (Earley_core.Earley.empty_pos
                                   (fun __loc__start__buf ->
                                      fun __loc__start__pos ->
                                        fun __loc__end__buf ->
                                          fun __loc__end__pos ->
                                            let _loc =
                                              locate __loc__start__buf
                                                __loc__start__pos
                                                __loc__end__buf
                                                __loc__end__pos in
                                            fun ls ->
                                              fun _default_0 ->
                                                fun e ->
                                                  fun dol ->
                                                    list_antiquotation _loc e))))))));
           Earley_core.Earley.fsequence_ignore (Earley_core.Earley.empty ())
             (Earley_core.Earley.empty []);
           Earley_core.Earley.fsequence field_decl_aux
             (Earley_core.Earley.fsequence field_decl_semi
                (Earley_core.Earley.empty (fun fd -> fun fs -> fd :: fs)))])
    let _ =
      set_grammar field_decl_list
        (Earley_core.Earley.alternatives
           [Earley_core.Earley.fsequence field_decl_aux
              (Earley_core.Earley.fsequence field_decl
                 (Earley_core.Earley.empty
                    (fun fd -> fun fs -> List.rev (fd :: fs))));
           Earley_core.Earley.fsequence field_decl_aux
             (Earley_core.Earley.empty (fun fs -> List.rev fs))])
    let type_representation =
      Earley_core.Earley.declare_grammar "type_representation"
    let _ =
      Earley_core.Earley.set_grammar type_representation
        (Earley_core.Earley.alternatives
           [Earley_core.Earley.fsequence_ignore
              (Earley_core.Earley.string ".." "..")
              (Earley_core.Earley.empty Ptype_open);
           Earley_core.Earley.fsequence_ignore
             (Earley_core.Earley.string "{" "{")
             (Earley_core.Earley.fsequence field_decl_list
                (Earley_core.Earley.fsequence_ignore
                   (Earley_core.Earley.string "}" "}")
                   (Earley_core.Earley.empty (fun fds -> Ptype_record fds))));
           Earley_core.Earley.fsequence constr_decl_list
             (Earley_core.Earley.empty
                (fun cds -> if cds = [] then give_up (); Ptype_variant cds))])
    let type_information =
      Earley_core.Earley.declare_grammar "type_information"
    let _ =
      Earley_core.Earley.set_grammar type_information
        (Earley_core.Earley.fsequence
           (Earley_core.Earley.option None
              (Earley_core.Earley.apply (fun x -> Some x) type_equation))
           (Earley_core.Earley.fsequence
              (Earley_core.Earley.option None
                 (Earley_core.Earley.apply (fun x -> Some x)
                    (Earley_core.Earley.fsequence_ignore
                       (Earley_core.Earley.char '=' '=')
                       (Earley_core.Earley.fsequence private_flag
                          (Earley_core.Earley.fsequence type_representation
                             (Earley_core.Earley.empty
                                (fun tr -> fun pri -> (pri, tr))))))))
              (Earley_core.Earley.fsequence
                 (Earley_core.Earley.apply (fun f -> f [])
                    (Earley_core.Earley.fixpoint' (fun l -> l)
                       type_constraint
                       (fun x -> fun f -> fun l -> f (x :: l))))
                 (Earley_core.Earley.empty
                    (fun cstrs ->
                       fun ptr ->
                         fun te ->
                           let (pri, tkind) =
                             match ptr with
                             | None -> (Public, Ptype_abstract)
                             | Some c -> c in
                           (pri, te, tkind, cstrs))))))
    let typedef_gen att constr filter =
      Earley_core.Earley.fsequence type_params
        (Earley_core.Earley.fsequence_position constr
           (Earley_core.Earley.fsequence type_information
              (Earley_core.Earley.fsequence
                 (Earley_core.Earley.alternatives
                    ((if not att
                      then
                        [Earley_core.Earley.fsequence_ignore
                           (Earley_core.Earley.empty ())
                           (Earley_core.Earley.empty [])]
                      else []) @
                       ((if att then [post_item_attributes] else []) @ [])))
                 (Earley_core.Earley.empty_pos
                    (fun __loc__start__buf ->
                       fun __loc__start__pos ->
                         fun __loc__end__buf ->
                           fun __loc__end__pos ->
                             let _loc =
                               locate __loc__start__buf __loc__start__pos
                                 __loc__end__buf __loc__end__pos in
                             fun a ->
                               fun ti ->
                                 fun str ->
                                   fun pos ->
                                     fun str' ->
                                       fun pos' ->
                                         fun tcn ->
                                           let _loc_tcn =
                                             locate str pos str' pos' in
                                           fun tps ->
                                             fun prev_loc ->
                                               let _loc =
                                                 match (prev_loc : Location.t
                                                                    option)
                                                 with
                                                 | None -> _loc
                                                 | Some l -> merge2 l _loc in
                                               let (pri, te, tkind, cstrs) =
                                                 ti in
                                               let (pri, te) =
                                                 match te with
                                                 | None -> (pri, None)
                                                 | Some (Private, te) ->
                                                     (if pri = Private
                                                      then give_up ();
                                                      (Private, (Some te)))
                                                 | Some (_, te) ->
                                                     (pri, (Some te)) in
                                               ((id_loc tcn _loc_tcn),
                                                 (type_declaration
                                                    ~attributes:(if att
                                                                 then
                                                                   attach_attrib
                                                                    _loc a
                                                                 else [])
                                                    _loc
                                                    (id_loc (filter tcn)
                                                       _loc_tcn) tps cstrs
                                                    tkind pri te)))))))
    let type_extension = Earley_core.Earley.declare_grammar "type_extension"
    let _ =
      Earley_core.Earley.set_grammar type_extension
        (Earley_core.Earley.fsequence type_kw
           (Earley_core.Earley.fsequence type_params
              (Earley_core.Earley.fsequence_position typeconstr
                 (Earley_core.Earley.fsequence_ignore
                    (Earley_core.Earley.string "+=" "+=")
                    (Earley_core.Earley.fsequence private_flag
                       (Earley_core.Earley.fsequence constr_extn_list
                          (Earley_core.Earley.fsequence post_item_attributes
                             (Earley_core.Earley.empty
                                (fun attrs ->
                                   fun cds ->
                                     fun priv ->
                                       fun str ->
                                         fun pos ->
                                           fun str' ->
                                             fun pos' ->
                                               fun tcn ->
                                                 let _loc_tcn =
                                                   locate str pos str' pos' in
                                                 fun params ->
                                                   fun _default_0 ->
                                                     let tcn =
                                                       id_loc tcn _loc_tcn in
                                                     let params =
                                                       params_map params in
                                                     Te.mk ~attrs ~params
                                                       ~priv tcn cds)))))))))
    let typedef = Earley_core.Earley.declare_grammar "typedef"
    let _ =
      Earley_core.Earley.set_grammar typedef
        (typedef_gen true typeconstr_name (fun x -> x))
    let typedef_in_constraint =
      Earley_core.Earley.declare_grammar "typedef_in_constraint"
    let _ =
      Earley_core.Earley.set_grammar typedef_in_constraint
        (typedef_gen false typeconstr Longident.last)
    let type_definition =
      Earley_core.Earley.declare_grammar "type_definition"
    let _ =
      Earley_core.Earley.set_grammar type_definition
        (Earley_core.Earley.fsequence_position type_kw
           (Earley_core.Earley.fsequence typedef
              (Earley_core.Earley.fsequence
                 (Earley_core.Earley.apply (fun f -> f [])
                    (Earley_core.Earley.fixpoint' (fun l -> l)
                       (Earley_core.Earley.fsequence_position and_kw
                          (Earley_core.Earley.fsequence typedef
                             (Earley_core.Earley.empty
                                (fun td ->
                                   fun str ->
                                     fun pos ->
                                       fun str' ->
                                         fun pos' ->
                                           fun l ->
                                             let _loc_l =
                                               locate str pos str' pos' in
                                             snd (td (Some _loc_l))))))
                       (fun x -> fun f -> fun l -> f (x :: l))))
                 (Earley_core.Earley.empty
                    (fun tds ->
                       fun td ->
                         fun str ->
                           fun pos ->
                             fun str' ->
                               fun pos' ->
                                 fun l ->
                                   let _loc_l = locate str pos str' pos' in
                                   (snd (td (Some _loc_l))) :: tds)))))
    let exception_definition =
      Earley_core.Earley.declare_grammar "exception_definition"
    let _ =
      Earley_core.Earley.set_grammar exception_definition
        (Earley_core.Earley.alternatives
           [Earley_core.Earley.fsequence exception_kw
              (Earley_core.Earley.fsequence (constr_decl false)
                 (Earley_core.Earley.empty_pos
                    (fun __loc__start__buf ->
                       fun __loc__start__pos ->
                         fun __loc__end__buf ->
                           fun __loc__end__pos ->
                             let _loc =
                               locate __loc__start__buf __loc__start__pos
                                 __loc__end__buf __loc__end__pos in
                             fun ((name, args, res, a) as _default_0) ->
                               fun _default_1 ->
                                 let cd =
                                   Te.decl ~attrs:(attach_attrib _loc a)
                                     ~loc:_loc ~args ?res name in
                                 (Str.exception_ ~loc:_loc cd).pstr_desc)));
           Earley_core.Earley.fsequence exception_kw
             (Earley_core.Earley.fsequence_position constr_name
                (Earley_core.Earley.fsequence_ignore
                   (Earley_core.Earley.char '=' '=')
                   (Earley_core.Earley.fsequence_position constr
                      (Earley_core.Earley.empty_pos
                         (fun __loc__start__buf ->
                            fun __loc__start__pos ->
                              fun __loc__end__buf ->
                                fun __loc__end__pos ->
                                  let _loc =
                                    locate __loc__start__buf
                                      __loc__start__pos __loc__end__buf
                                      __loc__end__pos in
                                  fun str ->
                                    fun pos ->
                                      fun str' ->
                                        fun pos' ->
                                          fun c ->
                                            let _loc_c =
                                              locate str pos str' pos' in
                                            fun str ->
                                              fun pos ->
                                                fun str' ->
                                                  fun pos' ->
                                                    fun cn ->
                                                      let _loc_cn =
                                                        locate str pos str'
                                                          pos' in
                                                      fun _default_0 ->
                                                        (let name =
                                                           id_loc cn _loc_cn in
                                                         let ex =
                                                           id_loc c _loc_c in
                                                         Str.exception_
                                                           ~loc:_loc
                                                           (Te.rebind
                                                              ~loc:_loc name
                                                              ex)).pstr_desc)))))])
    let class_field_spec = declare_grammar "class_field_spec"
    let class_body_type = declare_grammar "class_body_type"
    let virt_mut = Earley_core.Earley.declare_grammar "virt_mut"
    let _ =
      Earley_core.Earley.set_grammar virt_mut
        (Earley_core.Earley.alternatives
           [Earley_core.Earley.fsequence mutable_kw
              (Earley_core.Earley.fsequence virtual_kw
                 (Earley_core.Earley.empty
                    (fun _default_0 -> fun _default_1 -> (Virtual, Mutable))));
           Earley_core.Earley.fsequence virtual_flag
             (Earley_core.Earley.fsequence mutable_flag
                (Earley_core.Earley.empty (fun m -> fun v -> (v, m))))])
    let virt_priv = Earley_core.Earley.declare_grammar "virt_priv"
    let _ =
      Earley_core.Earley.set_grammar virt_priv
        (Earley_core.Earley.alternatives
           [Earley_core.Earley.fsequence private_kw
              (Earley_core.Earley.fsequence virtual_kw
                 (Earley_core.Earley.empty
                    (fun _default_0 -> fun _default_1 -> (Virtual, Private))));
           Earley_core.Earley.fsequence virtual_flag
             (Earley_core.Earley.fsequence private_flag
                (Earley_core.Earley.empty (fun p -> fun v -> (v, p))))])
    let _ =
      set_grammar class_field_spec
        (Earley_core.Earley.alternatives
           [Earley_core.Earley.fsequence floating_extension
              (Earley_core.Earley.empty_pos
                 (fun __loc__start__buf ->
                    fun __loc__start__pos ->
                      fun __loc__end__buf ->
                        fun __loc__end__pos ->
                          let _loc =
                            locate __loc__start__buf __loc__start__pos
                              __loc__end__buf __loc__end__pos in
                          fun ((s, l) as _default_0) ->
                            pctf_loc _loc (Pctf_extension (s, l))));
           Earley_core.Earley.fsequence inherit_kw
             (Earley_core.Earley.fsequence class_body_type
                (Earley_core.Earley.empty_pos
                   (fun __loc__start__buf ->
                      fun __loc__start__pos ->
                        fun __loc__end__buf ->
                          fun __loc__end__pos ->
                            let _loc =
                              locate __loc__start__buf __loc__start__pos
                                __loc__end__buf __loc__end__pos in
                            fun cbt ->
                              fun _default_0 ->
                                pctf_loc _loc (Pctf_inherit cbt))));
           Earley_core.Earley.fsequence val_kw
             (Earley_core.Earley.fsequence virt_mut
                (Earley_core.Earley.fsequence_position inst_var_name
                   (Earley_core.Earley.fsequence_ignore
                      (Earley_core.Earley.string ":" ":")
                      (Earley_core.Earley.fsequence typexpr
                         (Earley_core.Earley.empty_pos
                            (fun __loc__start__buf ->
                               fun __loc__start__pos ->
                                 fun __loc__end__buf ->
                                   fun __loc__end__pos ->
                                     let _loc =
                                       locate __loc__start__buf
                                         __loc__start__pos __loc__end__buf
                                         __loc__end__pos in
                                     fun te ->
                                       fun str ->
                                         fun pos ->
                                           fun str' ->
                                             fun pos' ->
                                               fun ivn ->
                                                 let _loc_ivn =
                                                   locate str pos str' pos' in
                                                 fun
                                                   ((vir, mut) as _default_0)
                                                   ->
                                                   fun _default_1 ->
                                                     let ivn =
                                                       id_loc ivn _loc_ivn in
                                                     pctf_loc _loc
                                                       (Pctf_val
                                                          (ivn, mut, vir, te))))))));
           Earley_core.Earley.fsequence method_kw
             (Earley_core.Earley.fsequence virt_priv
                (Earley_core.Earley.fsequence method_name
                   (Earley_core.Earley.fsequence_ignore
                      (Earley_core.Earley.string ":" ":")
                      (Earley_core.Earley.fsequence poly_typexpr
                         (Earley_core.Earley.empty_pos
                            (fun __loc__start__buf ->
                               fun __loc__start__pos ->
                                 fun __loc__end__buf ->
                                   fun __loc__end__pos ->
                                     let _loc =
                                       locate __loc__start__buf
                                         __loc__start__pos __loc__end__buf
                                         __loc__end__pos in
                                     fun te ->
                                       fun mn ->
                                         fun ((v, pri) as _default_0) ->
                                           fun _default_1 ->
                                             Ctf.method_ ~loc:_loc mn pri v
                                               te))))));
           Earley_core.Earley.fsequence constraint_kw
             (Earley_core.Earley.fsequence typexpr
                (Earley_core.Earley.fsequence_ignore
                   (Earley_core.Earley.char '=' '=')
                   (Earley_core.Earley.fsequence typexpr
                      (Earley_core.Earley.empty_pos
                         (fun __loc__start__buf ->
                            fun __loc__start__pos ->
                              fun __loc__end__buf ->
                                fun __loc__end__pos ->
                                  let _loc =
                                    locate __loc__start__buf
                                      __loc__start__pos __loc__end__buf
                                      __loc__end__pos in
                                  fun te' ->
                                    fun te ->
                                      fun _default_0 ->
                                        pctf_loc _loc
                                          (Pctf_constraint (te, te')))))));
           Earley_core.Earley.fsequence floating_attribute
             (Earley_core.Earley.empty_pos
                (fun __loc__start__buf ->
                   fun __loc__start__pos ->
                     fun __loc__end__buf ->
                       fun __loc__end__pos ->
                         let _loc =
                           locate __loc__start__buf __loc__start__pos
                             __loc__end__buf __loc__end__pos in
                         fun ((s, l) as _default_0) ->
                           pctf_loc _loc (Pctf_attribute (s, l))))])
    let _ =
      set_grammar class_body_type
        (Earley_core.Earley.alternatives
           [Earley_core.Earley.fsequence
              (Earley_core.Earley.option []
                 (Earley_core.Earley.fsequence_ignore
                    (Earley_core.Earley.string "[" "[")
                    (Earley_core.Earley.fsequence typexpr
                       (Earley_core.Earley.fsequence
                          (Earley_core.Earley.apply (fun f -> f [])
                             (Earley_core.Earley.fixpoint' (fun l -> l)
                                (Earley_core.Earley.fsequence_ignore
                                   (Earley_core.Earley.string "," ",")
                                   (Earley_core.Earley.fsequence typexpr
                                      (Earley_core.Earley.empty
                                         (fun te -> te))))
                                (fun x -> fun f -> fun l -> f (x :: l))))
                          (Earley_core.Earley.fsequence_ignore
                             (Earley_core.Earley.string "]" "]")
                             (Earley_core.Earley.empty
                                (fun tes -> fun te -> te :: tes)))))))
              (Earley_core.Earley.fsequence_position classtype_path
                 (Earley_core.Earley.empty_pos
                    (fun __loc__start__buf ->
                       fun __loc__start__pos ->
                         fun __loc__end__buf ->
                           fun __loc__end__pos ->
                             let _loc =
                               locate __loc__start__buf __loc__start__pos
                                 __loc__end__buf __loc__end__pos in
                             fun str ->
                               fun pos ->
                                 fun str' ->
                                   fun pos' ->
                                     fun ctp ->
                                       let _loc_ctp =
                                         locate str pos str' pos' in
                                       fun tes ->
                                         let ctp = id_loc ctp _loc_ctp in
                                         pcty_loc _loc
                                           (Pcty_constr (ctp, tes)))));
           Earley_core.Earley.fsequence object_kw
             (Earley_core.Earley.fsequence_position
                (Earley_core.Earley.option None
                   (Earley_core.Earley.apply (fun x -> Some x)
                      (Earley_core.Earley.fsequence_ignore
                         (Earley_core.Earley.string "(" "(")
                         (Earley_core.Earley.fsequence typexpr
                            (Earley_core.Earley.fsequence_ignore
                               (Earley_core.Earley.string ")" ")")
                               (Earley_core.Earley.empty (fun te -> te)))))))
                (Earley_core.Earley.fsequence
                   (Earley_core.Earley.apply (fun f -> f [])
                      (Earley_core.Earley.fixpoint' (fun l -> l)
                         class_field_spec
                         (fun x -> fun f -> fun l -> f (x :: l))))
                   (Earley_core.Earley.fsequence end_kw
                      (Earley_core.Earley.empty_pos
                         (fun __loc__start__buf ->
                            fun __loc__start__pos ->
                              fun __loc__end__buf ->
                                fun __loc__end__pos ->
                                  let _loc =
                                    locate __loc__start__buf
                                      __loc__start__pos __loc__end__buf
                                      __loc__end__pos in
                                  fun _default_0 ->
                                    fun cfs ->
                                      fun str ->
                                        fun pos ->
                                          fun str' ->
                                            fun pos' ->
                                              fun te ->
                                                let _loc_te =
                                                  locate str pos str' pos' in
                                                fun _default_1 ->
                                                  let self =
                                                    match te with
                                                    | None ->
                                                        loc_typ _loc_te
                                                          Ptyp_any
                                                    | Some t -> t in
                                                  let sign =
                                                    {
                                                      pcsig_self = self;
                                                      pcsig_fields = cfs
                                                    } in
                                                  pcty_loc _loc
                                                    (Pcty_signature sign))))))])
    let class_type = Earley_core.Earley.declare_grammar "class_type"
    let _ =
      Earley_core.Earley.set_grammar class_type
        (Earley_core.Earley.fsequence
           (Earley_core.Earley.apply (fun f -> f [])
              (Earley_core.Earley.fixpoint' (fun l -> l)
                 (Earley_core.Earley.fsequence
                    (Earley_core.Earley.option None
                       (Earley_core.Earley.apply (fun x -> Some x)
                          maybe_opt_label))
                    (Earley_core.Earley.fsequence_ignore
                       (Earley_core.Earley.string ":" ":")
                       (Earley_core.Earley.fsequence typexpr
                          (Earley_core.Earley.empty
                             (fun te -> fun l -> (l, te))))))
                 (fun x -> fun f -> fun l -> f (x :: l))))
           (Earley_core.Earley.fsequence class_body_type
              (Earley_core.Earley.empty_pos
                 (fun __loc__start__buf ->
                    fun __loc__start__pos ->
                      fun __loc__end__buf ->
                        fun __loc__end__pos ->
                          let _loc =
                            locate __loc__start__buf __loc__start__pos
                              __loc__end__buf __loc__end__pos in
                          fun cbt ->
                            fun tes ->
                              let app acc (lab, te) =
                                match lab with
                                | None ->
                                    pcty_loc _loc
                                      (Pcty_arrow (nolabel, te, acc))
                                | Some l ->
                                    pcty_loc _loc
                                      (Pcty_arrow
                                         (l,
                                           (match l with
                                            | Optional _ -> te
                                            | _ -> te), acc)) in
                              List.fold_left app cbt (List.rev tes)))))
    let type_parameters =
      Earley_core.Earley.declare_grammar "type_parameters"
    let _ =
      Earley_core.Earley.set_grammar type_parameters
        (Earley_core.Earley.fsequence type_param
           (Earley_core.Earley.fsequence
              (Earley_core.Earley.apply (fun f -> f [])
                 (Earley_core.Earley.fixpoint' (fun l -> l)
                    (Earley_core.Earley.fsequence_ignore
                       (Earley_core.Earley.string "," ",")
                       (Earley_core.Earley.fsequence type_param
                          (Earley_core.Earley.empty (fun i2 -> i2))))
                    (fun x -> fun f -> fun l -> f (x :: l))))
              (Earley_core.Earley.empty (fun l -> fun i1 -> i1 :: l))))
    let class_spec = Earley_core.Earley.declare_grammar "class_spec"
    let _ =
      Earley_core.Earley.set_grammar class_spec
        (Earley_core.Earley.fsequence virtual_flag
           (Earley_core.Earley.fsequence
              (Earley_core.Earley.option []
                 (Earley_core.Earley.fsequence_ignore
                    (Earley_core.Earley.string "[" "[")
                    (Earley_core.Earley.fsequence type_parameters
                       (Earley_core.Earley.fsequence_ignore
                          (Earley_core.Earley.string "]" "]")
                          (Earley_core.Earley.empty (fun params -> params))))))
              (Earley_core.Earley.fsequence_position class_name
                 (Earley_core.Earley.fsequence_ignore
                    (Earley_core.Earley.string ":" ":")
                    (Earley_core.Earley.fsequence class_type
                       (Earley_core.Earley.empty_pos
                          (fun __loc__start__buf ->
                             fun __loc__start__pos ->
                               fun __loc__end__buf ->
                                 fun __loc__end__pos ->
                                   let _loc =
                                     locate __loc__start__buf
                                       __loc__start__pos __loc__end__buf
                                       __loc__end__pos in
                                   fun ct ->
                                     fun str ->
                                       fun pos ->
                                         fun str' ->
                                           fun pos' ->
                                             fun cn ->
                                               let _loc_cn =
                                                 locate str pos str' pos' in
                                               fun params ->
                                                 fun v ->
                                                   class_type_declaration
                                                     ~attributes:(attach_attrib
                                                                    _loc [])
                                                     _loc (id_loc cn _loc_cn)
                                                     params v ct)))))))
    let class_specification =
      Earley_core.Earley.declare_grammar "class_specification"
    let _ =
      Earley_core.Earley.set_grammar class_specification
        (Earley_core.Earley.fsequence class_kw
           (Earley_core.Earley.fsequence class_spec
              (Earley_core.Earley.fsequence
                 (Earley_core.Earley.apply (fun f -> f [])
                    (Earley_core.Earley.fixpoint' (fun l -> l)
                       (Earley_core.Earley.fsequence_ignore and_kw
                          (Earley_core.Earley.fsequence class_spec
                             (Earley_core.Earley.empty
                                (fun _default_0 -> _default_0))))
                       (fun x -> fun f -> fun l -> f (x :: l))))
                 (Earley_core.Earley.empty
                    (fun css -> fun cs -> fun _default_0 -> cs :: css)))))
    let classtype_def = Earley_core.Earley.declare_grammar "classtype_def"
    let _ =
      Earley_core.Earley.set_grammar classtype_def
        (Earley_core.Earley.fsequence virtual_flag
           (Earley_core.Earley.fsequence
              (Earley_core.Earley.option []
                 (Earley_core.Earley.fsequence_ignore
                    (Earley_core.Earley.string "[" "[")
                    (Earley_core.Earley.fsequence type_parameters
                       (Earley_core.Earley.fsequence_ignore
                          (Earley_core.Earley.string "]" "]")
                          (Earley_core.Earley.empty (fun tp -> tp))))))
              (Earley_core.Earley.fsequence_position class_name
                 (Earley_core.Earley.fsequence_ignore
                    (Earley_core.Earley.char '=' '=')
                    (Earley_core.Earley.fsequence class_body_type
                       (Earley_core.Earley.empty
                          (fun cbt ->
                             fun str ->
                               fun pos ->
                                 fun str' ->
                                   fun pos' ->
                                     fun cn ->
                                       let _loc_cn = locate str pos str' pos' in
                                       fun params ->
                                         fun v ->
                                           fun _l ->
                                             class_type_declaration
                                               ~attributes:(attach_attrib _l
                                                              []) _l
                                               (id_loc cn _loc_cn) params v
                                               cbt)))))))
    let classtype_definition =
      Earley_core.Earley.declare_grammar "classtype_definition"
    let _ =
      Earley_core.Earley.set_grammar classtype_definition
        (Earley_core.Earley.fsequence_position class_kw
           (Earley_core.Earley.fsequence type_kw
              (Earley_core.Earley.fsequence_position classtype_def
                 (Earley_core.Earley.fsequence
                    (Earley_core.Earley.apply (fun f -> f [])
                       (Earley_core.Earley.fixpoint' (fun l -> l)
                          (Earley_core.Earley.fsequence_ignore and_kw
                             (Earley_core.Earley.fsequence classtype_def
                                (Earley_core.Earley.empty_pos
                                   (fun __loc__start__buf ->
                                      fun __loc__start__pos ->
                                        fun __loc__end__buf ->
                                          fun __loc__end__pos ->
                                            let _loc =
                                              locate __loc__start__buf
                                                __loc__start__pos
                                                __loc__end__buf
                                                __loc__end__pos in
                                            fun cd -> cd _loc))))
                          (fun x -> fun f -> fun l -> f (x :: l))))
                    (Earley_core.Earley.empty
                       (fun cds ->
                          fun str ->
                            fun pos ->
                              fun str' ->
                                fun pos' ->
                                  fun cd ->
                                    let _loc_cd = locate str pos str' pos' in
                                    fun _default_0 ->
                                      fun str ->
                                        fun pos ->
                                          fun str' ->
                                            fun pos' ->
                                              fun k ->
                                                let _loc_k =
                                                  locate str pos str' pos' in
                                                (cd (merge2 _loc_k _loc_cd))
                                                  :: cds))))))
    let constant = Earley_core.Earley.declare_grammar "constant"
    let _ =
      Earley_core.Earley.set_grammar constant
        (Earley_core.Earley.alternatives
           [Earley_core.Earley.fsequence int_litteral
              (Earley_core.Earley.empty
                 (fun ((i, suffix) as _default_0) -> Const.integer ?suffix i));
           Earley_core.Earley.fsequence float_litteral
             (Earley_core.Earley.empty
                (fun ((f, suffix) as _default_0) -> Const.float ?suffix f));
           Earley_core.Earley.fsequence char_litteral
             (Earley_core.Earley.empty (fun c -> Const.char c));
           Earley_core.Earley.fsequence string_litteral
             (Earley_core.Earley.empty
                (fun ((s, delim) as _default_0) ->
                   Const.string ?quotation_delimiter:delim s));
           Earley_core.Earley.fsequence regexp_litteral
             (Earley_core.Earley.empty (fun s -> const_string s));
           Earley_core.Earley.fsequence new_regexp_litteral
             (Earley_core.Earley.empty (fun s -> const_string s))])
    let neg_constant = Earley_core.Earley.declare_grammar "neg_constant"
    let _ =
      Earley_core.Earley.set_grammar neg_constant
        (Earley_core.Earley.alternatives
           [Earley_core.Earley.fsequence_ignore
              (Earley_core.Earley.char '-' '-')
              (Earley_core.Earley.fsequence int_litteral
                 (Earley_core.Earley.empty
                    (fun ((i, suffix) as _default_0) ->
                       Const.integer ?suffix ("-" ^ i))));
           Earley_core.Earley.fsequence_ignore
             (Earley_core.Earley.char '-' '-')
             (Earley_core.Earley.fsequence_ignore
                (Earley_core.Earley.no_blank_test ())
                (Earley_core.Earley.fsequence
                   (Earley_core.Earley.option None
                      (Earley_core.Earley.apply (fun x -> Some x)
                         (Earley_core.Earley.char '.' '.')))
                   (Earley_core.Earley.fsequence float_litteral
                      (Earley_core.Earley.empty
                         (fun ((f, suffix) as _default_0) ->
                            fun _default_1 -> Const.float ?suffix ("-" ^ f))))))])
    let (extra_patterns_grammar, extra_patterns_grammar__set__grammar) =
      Earley_core.Earley.grammar_family "extra_patterns_grammar"
    let _ =
      extra_patterns_grammar__set__grammar
        (fun lvl -> alternatives (List.map (fun g -> g lvl) extra_patterns))
    let _ =
      set_pattern_lvl
        ([(((fun (as_ok, lvl) -> lvl <= AtomPat)),
            (Earley_core.Earley.fsequence_ignore
               (Earley_core.Earley.char '$' '$')
               (Earley_core.Earley.fsequence_ignore
                  (Earley_core.Earley.no_blank_test ())
                  (Earley_core.Earley.fsequence
                     (Earley_core.Earley.option "pat"
                        (Earley_core.Earley.fsequence
                           (Earley_str.regexp ~name:"[a-z]+" "[a-z]+"
                              (fun groupe -> groupe 0))
                           (Earley_core.Earley.fsequence_ignore
                              (Earley_core.Earley.no_blank_test ())
                              (Earley_core.Earley.fsequence_ignore
                                 (Earley_core.Earley.char ':' ':')
                                 (Earley_core.Earley.empty
                                    (fun _default_0 -> _default_0))))))
                     (Earley_core.Earley.fsequence expression
                        (Earley_core.Earley.fsequence_ignore
                           (Earley_core.Earley.no_blank_test ())
                           (Earley_core.Earley.fsequence_ignore
                              (Earley_core.Earley.char '$' '$')
                              (Earley_core.Earley.empty_pos
                                 (fun __loc__start__buf ->
                                    fun __loc__start__pos ->
                                      fun __loc__end__buf ->
                                        fun __loc__end__pos ->
                                          let _loc =
                                            locate __loc__start__buf
                                              __loc__start__pos
                                              __loc__end__buf __loc__end__pos in
                                          fun e ->
                                            fun aq ->
                                              let open Quote in
                                                let e_loc =
                                                  exp_ident _loc "_loc" in
                                                let locate _loc e =
                                                  quote_record e_loc _loc
                                                    [((parsetree "ppat_desc"),
                                                       e);
                                                    ((parsetree "ppat_loc"),
                                                      (quote_location_t e_loc
                                                         _loc _loc));
                                                    ((parsetree
                                                        "ppat_attributes"),
                                                      (quote_attributes e_loc
                                                         _loc []))] in
                                                let generic_antiquote e =
                                                  function
                                                  | Quote_ppat -> e
                                                  | _ ->
                                                      failwith
                                                        ("invalid antiquotation type ppat expected at "
                                                           ^
                                                           (string_location
                                                              _loc)) in
                                                let f =
                                                  match aq with
                                                  | "pat" ->
                                                      generic_antiquote e
                                                  | "bool" ->
                                                      let e =
                                                        quote_const e_loc
                                                          _loc
                                                          (parsetree
                                                             "Ppat_constant")
                                                          [quote_apply e_loc
                                                             _loc
                                                             (pa_ast
                                                                "const_bool")
                                                             [e]] in
                                                      generic_antiquote
                                                        (locate _loc e)
                                                  | "int" ->
                                                      let e =
                                                        quote_const e_loc
                                                          _loc
                                                          (parsetree
                                                             "Ppat_constant")
                                                          [quote_apply e_loc
                                                             _loc
                                                             (pa_ast
                                                                "const_int")
                                                             [e]] in
                                                      generic_antiquote
                                                        (locate _loc e)
                                                  | "string" ->
                                                      let e =
                                                        quote_const e_loc
                                                          _loc
                                                          (parsetree
                                                             "Ppat_constant")
                                                          [quote_apply e_loc
                                                             _loc
                                                             (pa_ast
                                                                "const_string")
                                                             [e]] in
                                                      generic_antiquote
                                                        (locate _loc e)
                                                  | "list" ->
                                                      generic_antiquote
                                                        (quote_apply e_loc
                                                           _loc
                                                           (pa_ast "pat_list")
                                                           [quote_location_t
                                                              e_loc _loc _loc;
                                                           quote_location_t
                                                             e_loc _loc _loc;
                                                           e])
                                                  | "tuple" ->
                                                      generic_antiquote
                                                        (quote_apply e_loc
                                                           _loc
                                                           (pa_ast
                                                              "pat_tuple")
                                                           [quote_location_t
                                                              e_loc _loc _loc;
                                                           e])
                                                  | "array" ->
                                                      generic_antiquote
                                                        (quote_apply e_loc
                                                           _loc
                                                           (pa_ast
                                                              "pat_array")
                                                           [quote_location_t
                                                              e_loc _loc _loc;
                                                           e])
                                                  | _ -> give_up () in
                                                Quote.ppat_antiquotation _loc
                                                  f)))))))));
         (((fun _ -> true)),
           (Earley_core.Earley.fsequence_position value_name
              (Earley_core.Earley.empty_pos
                 (fun __loc__start__buf ->
                    fun __loc__start__pos ->
                      fun __loc__end__buf ->
                        fun __loc__end__pos ->
                          let _loc =
                            locate __loc__start__buf __loc__start__pos
                              __loc__end__buf __loc__end__pos in
                          fun str ->
                            fun pos ->
                              fun str' ->
                                fun pos' ->
                                  fun vn ->
                                    let _loc_vn = locate str pos str' pos' in
                                    Pat.var ~loc:_loc (id_loc vn _loc_vn)))));
         (((fun _ -> true)),
           (Earley_core.Earley.fsequence joker_kw
              (Earley_core.Earley.empty_pos
                 (fun __loc__start__buf ->
                    fun __loc__start__pos ->
                      fun __loc__end__buf ->
                        fun __loc__end__pos ->
                          let _loc =
                            locate __loc__start__buf __loc__start__pos
                              __loc__end__buf __loc__end__pos in
                          fun _default_0 -> Pat.any ~loc:_loc ()))));
         (((fun _ -> true)),
           (Earley_core.Earley.fsequence char_litteral
              (Earley_core.Earley.fsequence_ignore
                 (Earley_core.Earley.string ".." "..")
                 (Earley_core.Earley.fsequence char_litteral
                    (Earley_core.Earley.empty_pos
                       (fun __loc__start__buf ->
                          fun __loc__start__pos ->
                            fun __loc__end__buf ->
                              fun __loc__end__pos ->
                                let _loc =
                                  locate __loc__start__buf __loc__start__pos
                                    __loc__end__buf __loc__end__pos in
                                fun c2 ->
                                  fun c1 ->
                                    Pat.interval ~loc:_loc (Const.char c1)
                                      (Const.char c2)))))));
         (((fun (as_ok, lvl) -> lvl <= AtomPat)),
           (Earley_core.Earley.fsequence
              (Earley_core.Earley.alternatives [neg_constant; constant])
              (Earley_core.Earley.empty_pos
                 (fun __loc__start__buf ->
                    fun __loc__start__pos ->
                      fun __loc__end__buf ->
                        fun __loc__end__pos ->
                          let _loc =
                            locate __loc__start__buf __loc__start__pos
                              __loc__end__buf __loc__end__pos in
                          fun c -> Pat.constant ~loc:_loc c))));
         (((fun _ -> true)),
           (Earley_core.Earley.fsequence_ignore
              (Earley_core.Earley.char '(' '(')
              (Earley_core.Earley.fsequence pattern
                 (Earley_core.Earley.fsequence
                    (Earley_core.Earley.option None
                       (Earley_core.Earley.apply (fun x -> Some x)
                          (Earley_core.Earley.fsequence_ignore
                             (Earley_core.Earley.char ':' ':')
                             (Earley_core.Earley.fsequence typexpr
                                (Earley_core.Earley.empty
                                   (fun _default_0 -> _default_0))))))
                    (Earley_core.Earley.fsequence_ignore
                       (Earley_core.Earley.char ')' ')')
                       (Earley_core.Earley.empty_pos
                          (fun __loc__start__buf ->
                             fun __loc__start__pos ->
                               fun __loc__end__buf ->
                                 fun __loc__end__pos ->
                                   let _loc =
                                     locate __loc__start__buf
                                       __loc__start__pos __loc__end__buf
                                       __loc__end__pos in
                                   fun ty ->
                                     fun p ->
                                       let p =
                                         match ty with
                                         | None -> loc_pat _loc p.ppat_desc
                                         | Some ty ->
                                             Pat.constraint_ ~loc:_loc p ty in
                                       p)))))));
         (((fun (as_ok, lvl) -> lvl <= ConstrPat)),
           (Earley_core.Earley.fsequence lazy_kw
              (Earley_core.Earley.fsequence (pattern_lvl (false, ConstrPat))
                 (Earley_core.Earley.empty_pos
                    (fun __loc__start__buf ->
                       fun __loc__start__pos ->
                         fun __loc__end__buf ->
                           fun __loc__end__pos ->
                             let _loc =
                               locate __loc__start__buf __loc__start__pos
                                 __loc__end__buf __loc__end__pos in
                             fun p -> fun _default_0 -> Pat.lazy_ ~loc:_loc p)))));
         (((fun (as_ok, lvl) -> lvl <= ConstrPat)),
           (Earley_core.Earley.fsequence exception_kw
              (Earley_core.Earley.fsequence (pattern_lvl (false, ConstrPat))
                 (Earley_core.Earley.empty_pos
                    (fun __loc__start__buf ->
                       fun __loc__start__pos ->
                         fun __loc__end__buf ->
                           fun __loc__end__pos ->
                             let _loc =
                               locate __loc__start__buf __loc__start__pos
                                 __loc__end__buf __loc__end__pos in
                             fun p ->
                               fun _default_0 -> Pat.exception_ ~loc:_loc p)))));
         (((fun (as_ok, lvl) -> lvl <= ConstrPat)),
           (Earley_core.Earley.fsequence_position constr
              (Earley_core.Earley.fsequence (pattern_lvl (false, ConstrPat))
                 (Earley_core.Earley.empty_pos
                    (fun __loc__start__buf ->
                       fun __loc__start__pos ->
                         fun __loc__end__buf ->
                           fun __loc__end__pos ->
                             let _loc =
                               locate __loc__start__buf __loc__start__pos
                                 __loc__end__buf __loc__end__pos in
                             fun p ->
                               fun str ->
                                 fun pos ->
                                   fun str' ->
                                     fun pos' ->
                                       fun c ->
                                         let _loc_c =
                                           locate str pos str' pos' in
                                         Pat.construct ~loc:_loc
                                           (id_loc c _loc_c) (Some p))))));
         (((fun _ -> true)),
           (Earley_core.Earley.fsequence_position constr
              (Earley_core.Earley.empty_pos
                 (fun __loc__start__buf ->
                    fun __loc__start__pos ->
                      fun __loc__end__buf ->
                        fun __loc__end__pos ->
                          let _loc =
                            locate __loc__start__buf __loc__start__pos
                              __loc__end__buf __loc__end__pos in
                          fun str ->
                            fun pos ->
                              fun str' ->
                                fun pos' ->
                                  fun c ->
                                    let _loc_c = locate str pos str' pos' in
                                    Pat.construct ~loc:_loc (id_loc c _loc_c)
                                      None))));
         (((fun _ -> true)),
           (Earley_core.Earley.fsequence bool_lit
              (Earley_core.Earley.empty_pos
                 (fun __loc__start__buf ->
                    fun __loc__start__pos ->
                      fun __loc__end__buf ->
                        fun __loc__end__pos ->
                          let _loc =
                            locate __loc__start__buf __loc__start__pos
                              __loc__end__buf __loc__end__pos in
                          fun b ->
                            Pat.construct ~loc:_loc (id_loc (Lident b) _loc)
                              None))));
         (((fun (as_ok, lvl) -> lvl <= ConstrPat)),
           (Earley_core.Earley.fsequence tag_name
              (Earley_core.Earley.fsequence (pattern_lvl (false, ConstrPat))
                 (Earley_core.Earley.empty_pos
                    (fun __loc__start__buf ->
                       fun __loc__start__pos ->
                         fun __loc__end__buf ->
                           fun __loc__end__pos ->
                             let _loc =
                               locate __loc__start__buf __loc__start__pos
                                 __loc__end__buf __loc__end__pos in
                             fun p ->
                               fun c -> Pat.variant ~loc:_loc c (Some p))))));
         (((fun _ -> true)),
           (Earley_core.Earley.fsequence tag_name
              (Earley_core.Earley.empty_pos
                 (fun __loc__start__buf ->
                    fun __loc__start__pos ->
                      fun __loc__end__buf ->
                        fun __loc__end__pos ->
                          let _loc =
                            locate __loc__start__buf __loc__start__pos
                              __loc__end__buf __loc__end__pos in
                          fun c -> Pat.variant ~loc:_loc c None))));
         (((fun _ -> true)),
           (Earley_core.Earley.fsequence (Earley_core.Earley.char '#' '#')
              (Earley_core.Earley.fsequence_position typeconstr
                 (Earley_core.Earley.empty_pos
                    (fun __loc__start__buf ->
                       fun __loc__start__pos ->
                         fun __loc__end__buf ->
                           fun __loc__end__pos ->
                             let _loc =
                               locate __loc__start__buf __loc__start__pos
                                 __loc__end__buf __loc__end__pos in
                             fun str ->
                               fun pos ->
                                 fun str' ->
                                   fun pos' ->
                                     fun t ->
                                       let _loc_t = locate str pos str' pos' in
                                       fun s ->
                                         Pat.type_ ~loc:_loc
                                           (id_loc t _loc_t))))));
         (((fun _ -> true)),
           (Earley_core.Earley.fsequence (Earley_core.Earley.char '{' '{')
              (Earley_core.Earley.fsequence_position field
                 (Earley_core.Earley.fsequence
                    (Earley_core.Earley.option None
                       (Earley_core.Earley.apply (fun x -> Some x)
                          (Earley_core.Earley.fsequence_ignore
                             (Earley_core.Earley.char '=' '=')
                             (Earley_core.Earley.fsequence pattern
                                (Earley_core.Earley.empty (fun p -> p))))))
                    (Earley_core.Earley.fsequence
                       (Earley_core.Earley.apply (fun f -> f [])
                          (Earley_core.Earley.fixpoint' (fun l -> l)
                             (Earley_core.Earley.fsequence semi_col
                                (Earley_core.Earley.fsequence_position field
                                   (Earley_core.Earley.fsequence
                                      (Earley_core.Earley.option None
                                         (Earley_core.Earley.apply
                                            (fun x -> Some x)
                                            (Earley_core.Earley.fsequence_ignore
                                               (Earley_core.Earley.char '='
                                                  '=')
                                               (Earley_core.Earley.fsequence
                                                  pattern
                                                  (Earley_core.Earley.empty
                                                     (fun p -> p))))))
                                      (Earley_core.Earley.empty
                                         (fun p ->
                                            fun str ->
                                              fun pos ->
                                                fun str' ->
                                                  fun pos' ->
                                                    fun f ->
                                                      let _loc_f =
                                                        locate str pos str'
                                                          pos' in
                                                      fun _default_0 ->
                                                        ((id_loc f _loc_f),
                                                          p))))))
                             (fun x -> fun f -> fun l -> f (x :: l))))
                       (Earley_core.Earley.fsequence
                          (Earley_core.Earley.option None
                             (Earley_core.Earley.apply (fun x -> Some x)
                                (Earley_core.Earley.fsequence semi_col
                                   (Earley_core.Earley.fsequence joker_kw
                                      (Earley_core.Earley.empty
                                         (fun _default_0 ->
                                            fun _default_1 -> ()))))))
                          (Earley_core.Earley.fsequence
                             (Earley_core.Earley.option None
                                (Earley_core.Earley.apply (fun x -> Some x)
                                   semi_col))
                             (Earley_core.Earley.fsequence_ignore
                                (Earley_core.Earley.char '}' '}')
                                (Earley_core.Earley.empty_pos
                                   (fun __loc__start__buf ->
                                      fun __loc__start__pos ->
                                        fun __loc__end__buf ->
                                          fun __loc__end__pos ->
                                            let _loc =
                                              locate __loc__start__buf
                                                __loc__start__pos
                                                __loc__end__buf
                                                __loc__end__pos in
                                            fun _default_0 ->
                                              fun clsd ->
                                                fun fps ->
                                                  fun p ->
                                                    fun str ->
                                                      fun pos ->
                                                        fun str' ->
                                                          fun pos' ->
                                                            fun f ->
                                                              let _loc_f =
                                                                locate str
                                                                  pos str'
                                                                  pos' in
                                                              fun s ->
                                                                let all =
                                                                  ((id_loc f
                                                                    _loc_f),
                                                                    p)
                                                                  :: fps in
                                                                let f
                                                                  (lab, pat)
                                                                  =
                                                                  match pat
                                                                  with
                                                                  | Some p ->
                                                                    (lab, p)
                                                                  | None ->
                                                                    let slab
                                                                    =
                                                                    match 
                                                                    lab.txt
                                                                    with
                                                                    | 
                                                                    Lident s
                                                                    ->
                                                                    id_loc s
                                                                    lab.loc
                                                                    | 
                                                                    _ ->
                                                                    give_up
                                                                    () in
                                                                    (lab,
                                                                    (loc_pat
                                                                    lab.loc
                                                                    (Ppat_var
                                                                    slab))) in
                                                                let all =
                                                                  List.map f
                                                                    all in
                                                                let cl =
                                                                  match clsd
                                                                  with
                                                                  | None ->
                                                                    Closed
                                                                  | Some _ ->
                                                                    Open in
                                                                Pat.record
                                                                  ~loc:_loc
                                                                  all cl))))))))));
         (((fun _ -> true)),
           (Earley_core.Earley.fsequence_ignore
              (Earley_core.Earley.char '[' '[')
              (Earley_core.Earley.fsequence (list1 pattern semi_col)
                 (Earley_core.Earley.fsequence
                    (Earley_core.Earley.option None
                       (Earley_core.Earley.apply (fun x -> Some x) semi_col))
                    (Earley_core.Earley.fsequence_position
                       (Earley_core.Earley.char ']' ']')
                       (Earley_core.Earley.empty_pos
                          (fun __loc__start__buf ->
                             fun __loc__start__pos ->
                               fun __loc__end__buf ->
                                 fun __loc__end__pos ->
                                   let _loc =
                                     locate __loc__start__buf
                                       __loc__start__pos __loc__end__buf
                                       __loc__end__pos in
                                   fun str ->
                                     fun pos ->
                                       fun str' ->
                                         fun pos' ->
                                           fun c ->
                                             let _loc_c =
                                               locate str pos str' pos' in
                                             fun _default_0 ->
                                               fun ps ->
                                                 pat_list _loc _loc_c ps)))))));
         (((fun _ -> true)),
           (Earley_core.Earley.fsequence_ignore
              (Earley_core.Earley.char '[' '[')
              (Earley_core.Earley.fsequence_ignore
                 (Earley_core.Earley.char ']' ']')
                 (Earley_core.Earley.empty_pos
                    (fun __loc__start__buf ->
                       fun __loc__start__pos ->
                         fun __loc__end__buf ->
                           fun __loc__end__pos ->
                             let _loc =
                               locate __loc__start__buf __loc__start__pos
                                 __loc__end__buf __loc__end__pos in
                             Pat.construct ~loc:_loc
                               (id_loc (Lident "[]") _loc) None)))));
         (((fun _ -> true)),
           (Earley_core.Earley.fsequence_ignore
              (Earley_core.Earley.string "[|" "[|")
              (Earley_core.Earley.fsequence (list0 pattern semi_col)
                 (Earley_core.Earley.fsequence
                    (Earley_core.Earley.option None
                       (Earley_core.Earley.apply (fun x -> Some x) semi_col))
                    (Earley_core.Earley.fsequence_ignore
                       (Earley_core.Earley.string "|]" "|]")
                       (Earley_core.Earley.empty_pos
                          (fun __loc__start__buf ->
                             fun __loc__start__pos ->
                               fun __loc__end__buf ->
                                 fun __loc__end__pos ->
                                   let _loc =
                                     locate __loc__start__buf
                                       __loc__start__pos __loc__end__buf
                                       __loc__end__pos in
                                   fun _default_0 ->
                                     fun ps -> Pat.array ~loc:_loc ps)))))));
         (((fun _ -> true)),
           (Earley_core.Earley.fsequence_ignore
              (Earley_core.Earley.char '(' '(')
              (Earley_core.Earley.fsequence_ignore
                 (Earley_core.Earley.char ')' ')')
                 (Earley_core.Earley.empty_pos
                    (fun __loc__start__buf ->
                       fun __loc__start__pos ->
                         fun __loc__end__buf ->
                           fun __loc__end__pos ->
                             let _loc =
                               locate __loc__start__buf __loc__start__pos
                                 __loc__end__buf __loc__end__pos in
                             Pat.construct ~loc:_loc
                               (id_loc (Lident "()") _loc) None)))));
         (((fun _ -> true)),
           (Earley_core.Earley.fsequence begin_kw
              (Earley_core.Earley.fsequence end_kw
                 (Earley_core.Earley.empty_pos
                    (fun __loc__start__buf ->
                       fun __loc__start__pos ->
                         fun __loc__end__buf ->
                           fun __loc__end__pos ->
                             let _loc =
                               locate __loc__start__buf __loc__start__pos
                                 __loc__end__buf __loc__end__pos in
                             fun _default_0 ->
                               fun _default_1 ->
                                 Pat.construct ~loc:_loc
                                   (id_loc (Lident "()") _loc) None)))));
         (((fun (as_ok, lvl) -> lvl <= AtomPat)),
           (Earley_core.Earley.fsequence_ignore
              (Earley_core.Earley.char '(' '(')
              (Earley_core.Earley.fsequence module_kw
                 (Earley_core.Earley.fsequence module_name
                    (Earley_core.Earley.fsequence
                       (Earley_core.Earley.option None
                          (Earley_core.Earley.apply (fun x -> Some x)
                             (Earley_core.Earley.fsequence_ignore
                                (Earley_core.Earley.string ":" ":")
                                (Earley_core.Earley.fsequence package_type
                                   (Earley_core.Earley.empty (fun pt -> pt))))))
                       (Earley_core.Earley.fsequence_ignore
                          (Earley_core.Earley.char ')' ')')
                          (Earley_core.Earley.empty_pos
                             (fun __loc__start__buf ->
                                fun __loc__start__pos ->
                                  fun __loc__end__buf ->
                                    fun __loc__end__pos ->
                                      let _loc =
                                        locate __loc__start__buf
                                          __loc__start__pos __loc__end__buf
                                          __loc__end__pos in
                                      fun pt ->
                                        fun mn ->
                                          fun _default_0 ->
                                            let unpack = Ppat_unpack mn in
                                            let pat =
                                              match pt with
                                              | None -> unpack
                                              | Some pt ->
                                                  let pt =
                                                    loc_typ (ghost _loc)
                                                      pt.ptyp_desc in
                                                  Ppat_constraint
                                                    ((loc_pat _loc unpack),
                                                      pt) in
                                            loc_pat _loc pat))))))));
         (((fun (as_ok, lvl) -> lvl <= AltPat)),
           (Earley_core.Earley.fsequence (pattern_lvl (true, AltPat))
              (Earley_core.Earley.fsequence_ignore
                 (Earley_core.Earley.char '|' '|')
                 (Earley_core.Earley.fsequence
                    (pattern_lvl (false, (next_pat_prio AltPat)))
                    (Earley_core.Earley.empty_pos
                       (fun __loc__start__buf ->
                          fun __loc__start__pos ->
                            fun __loc__end__buf ->
                              fun __loc__end__pos ->
                                let _loc =
                                  locate __loc__start__buf __loc__start__pos
                                    __loc__end__buf __loc__end__pos in
                                fun p' -> fun p -> Pat.or_ ~loc:_loc p p'))))));
         (((fun (as_ok, lvl) -> lvl <= TupPat)),
           (Earley_core.Earley.fsequence
              (Earley_core.Earley.apply (fun f -> f [])
                 (Earley_core.Earley.fixpoint1' (fun l -> l)
                    (Earley_core.Earley.fsequence
                       (pattern_lvl (true, (next_pat_prio TupPat)))
                       (Earley_core.Earley.fsequence_ignore
                          (Earley_core.Earley.char ',' ',')
                          (Earley_core.Earley.empty
                             (fun _default_0 -> _default_0))))
                    (fun x -> fun f -> fun l -> f (x :: l))))
              (Earley_core.Earley.fsequence
                 (pattern_lvl (false, (next_pat_prio TupPat)))
                 (Earley_core.Earley.empty_pos
                    (fun __loc__start__buf ->
                       fun __loc__start__pos ->
                         fun __loc__end__buf ->
                           fun __loc__end__pos ->
                             let _loc =
                               locate __loc__start__buf __loc__start__pos
                                 __loc__end__buf __loc__end__pos in
                             fun p ->
                               fun ps -> Pat.tuple ~loc:_loc (ps @ [p]))))));
         (((fun (as_ok, lvl) -> lvl <= ConsPat)),
           (Earley_core.Earley.fsequence
              (pattern_lvl (true, (next_pat_prio ConsPat)))
              (Earley_core.Earley.fsequence_position
                 (Earley_core.Earley.string "::" "::")
                 (Earley_core.Earley.fsequence (pattern_lvl (false, ConsPat))
                    (Earley_core.Earley.empty_pos
                       (fun __loc__start__buf ->
                          fun __loc__start__pos ->
                            fun __loc__end__buf ->
                              fun __loc__end__pos ->
                                let _loc =
                                  locate __loc__start__buf __loc__start__pos
                                    __loc__end__buf __loc__end__pos in
                                fun p' ->
                                  fun str ->
                                    fun pos ->
                                      fun str' ->
                                        fun pos' ->
                                          fun c ->
                                            let _loc_c =
                                              locate str pos str' pos' in
                                            fun p ->
                                              let cons =
                                                id_loc (Lident "::") _loc_c in
                                              let args =
                                                loc_pat (ghost _loc)
                                                  (Ppat_tuple [p; p']) in
                                              Pat.construct ~loc:_loc cons
                                                (Some args)))))));
         (((fun (as_ok, lvl) -> lvl <= AtomPat)),
           (Earley_core.Earley.fsequence_ignore
              (Earley_core.Earley.char '$' '$')
              (Earley_core.Earley.fsequence_ignore
                 (Earley_core.Earley.no_blank_test ())
                 (Earley_core.Earley.fsequence uident
                    (Earley_core.Earley.empty
                       (fun c ->
                          try
                            let str = Sys.getenv c in
                            parse_string ~filename:("ENV:" ^ c) pattern
                              ocaml_blank str
                          with | Not_found -> give_up ()))))))],
          (fun (as_ok, lvl) -> (extra_patterns_grammar (as_ok, lvl)) ::
             ((if as_ok
               then
                 [Earley_core.Earley.fsequence (pattern_lvl (as_ok, lvl))
                    (Earley_core.Earley.fsequence as_kw
                       (Earley_core.Earley.fsequence_position value_name
                          (Earley_core.Earley.empty_pos
                             (fun __loc__start__buf ->
                                fun __loc__start__pos ->
                                  fun __loc__end__buf ->
                                    fun __loc__end__pos ->
                                      let _loc =
                                        locate __loc__start__buf
                                          __loc__start__pos __loc__end__buf
                                          __loc__end__pos in
                                      fun str ->
                                        fun pos ->
                                          fun str' ->
                                            fun pos' ->
                                              fun vn ->
                                                let _loc_vn =
                                                  locate str pos str' pos' in
                                                fun _default_0 ->
                                                  fun p ->
                                                    Pat.alias ~loc:_loc p
                                                      (id_loc vn _loc_vn)))))]
               else []) @ [])))
    let let_re = "\\(let\\)\\|\\(val\\)\\b"
    type assoc =
      | NoAssoc 
      | Left 
      | Right 
    let assoc =
      function
      | Prefix|Dot|Dash|Opp -> NoAssoc
      | Prod|Sum|Eq -> Left
      | _ -> Right
    let infix_prio s =
      match s.[0] with
      | '*' ->
          if ((String.length s) > 1) && ((s.[1]) = '*') then Pow else Prod
      | '/'|'%' -> Prod
      | '+'|'-' -> Sum
      | ':' ->
          if ((String.length s) > 1) && ((s.[1]) = '=') then Aff else Cons
      | '<' -> if ((String.length s) > 1) && ((s.[1]) = '-') then Aff else Eq
      | '@'|'^' -> Append
      | '&' ->
          if
            ((String.length s) = 1) ||
              (((String.length s) = 2) && ((s.[1]) = '&'))
          then Conj
          else Eq
      | '|' ->
          if ((String.length s) = 2) && ((s.[1]) = '|') then Disj else Eq
      | '='|'>'|'$'|'!' -> Eq
      | 'o' -> Disj
      | 'm' -> Prod
      | 'a' -> Pow
      | 'l' -> (match s.[1] with | 's' -> Pow | _ -> Prod)
      | _ -> (Printf.printf "%s\n%!" s; assert false)
    let prefix_prio s =
      if (s = "-") || ((s = "-.") || ((s = "+") || (s = "+.")))
      then Opp
      else Prefix
    let array_function loc str name =
      let name = if !fast then "unsafe_" ^ name else name in
      loc_expr loc (Pexp_ident (id_loc (Ldot ((Lident str), name)) loc))
    let bigarray_function loc str name =
      let name = if !fast then "unsafe_" ^ name else name in
      let lid = Ldot ((Ldot ((Lident "Bigarray"), str)), name) in
      loc_expr loc (Pexp_ident (id_loc lid loc))
    let untuplify exp =
      match exp.pexp_desc with | Pexp_tuple es -> es | _ -> [exp]
    let bigarray_get _loc arr arg =
      let get = if !fast then "unsafe_get" else "get" in
      match untuplify arg with
      | c1::[] ->
          {
            Parsetree.pexp_desc =
              (Parsetree.Pexp_apply
                 ({
                    Parsetree.pexp_desc =
                      (Parsetree.Pexp_ident
                         {
                           Asttypes.txt =
                             (Longident.Ldot
                                ((Longident.Ldot
                                    ((Longident.Lident "Bigarray"), "Array1")),
                                  get));
                           Asttypes.loc = _loc
                         });
                    Parsetree.pexp_loc = _loc;
                    Parsetree.pexp_attributes = []
                  }, [(Asttypes.Nolabel, arr); (Asttypes.Nolabel, c1)]));
            Parsetree.pexp_loc = _loc;
            Parsetree.pexp_attributes = []
          }
      | c1::c2::[] ->
          {
            Parsetree.pexp_desc =
              (Parsetree.Pexp_apply
                 ({
                    Parsetree.pexp_desc =
                      (Parsetree.Pexp_ident
                         {
                           Asttypes.txt =
                             (Longident.Ldot
                                ((Longident.Ldot
                                    ((Longident.Lident "Bigarray"), "Array2")),
                                  get));
                           Asttypes.loc = _loc
                         });
                    Parsetree.pexp_loc = _loc;
                    Parsetree.pexp_attributes = []
                  },
                   [(Asttypes.Nolabel, arr);
                   (Asttypes.Nolabel, c1);
                   (Asttypes.Nolabel, c2)]));
            Parsetree.pexp_loc = _loc;
            Parsetree.pexp_attributes = []
          }
      | c1::c2::c3::[] ->
          {
            Parsetree.pexp_desc =
              (Parsetree.Pexp_apply
                 ({
                    Parsetree.pexp_desc =
                      (Parsetree.Pexp_ident
                         {
                           Asttypes.txt =
                             (Longident.Ldot
                                ((Longident.Ldot
                                    ((Longident.Lident "Bigarray"), "Array3")),
                                  get));
                           Asttypes.loc = _loc
                         });
                    Parsetree.pexp_loc = _loc;
                    Parsetree.pexp_attributes = []
                  },
                   [(Asttypes.Nolabel, arr);
                   (Asttypes.Nolabel, c1);
                   (Asttypes.Nolabel, c2);
                   (Asttypes.Nolabel, c3)]));
            Parsetree.pexp_loc = _loc;
            Parsetree.pexp_attributes = []
          }
      | coords ->
          {
            Parsetree.pexp_desc =
              (Parsetree.Pexp_apply
                 ({
                    Parsetree.pexp_desc =
                      (Parsetree.Pexp_ident
                         {
                           Asttypes.txt =
                             (Longident.Ldot
                                ((Longident.Ldot
                                    ((Longident.Lident "Bigarray"),
                                      "Genarray")), get));
                           Asttypes.loc = _loc
                         });
                    Parsetree.pexp_loc = _loc;
                    Parsetree.pexp_attributes = []
                  },
                   [(Asttypes.Nolabel, arr);
                   (Asttypes.Nolabel, (Pa_ast.exp_array _loc coords))]));
            Parsetree.pexp_loc = _loc;
            Parsetree.pexp_attributes = []
          }
    let bigarray_set _loc arr arg newval =
      let set = if !fast then "unsafe_set" else "set" in
      match untuplify arg with
      | c1::[] ->
          {
            Parsetree.pexp_desc =
              (Parsetree.Pexp_apply
                 ({
                    Parsetree.pexp_desc =
                      (Parsetree.Pexp_ident
                         {
                           Asttypes.txt =
                             (Longident.Ldot
                                ((Longident.Ldot
                                    ((Longident.Lident "Bigarray"), "Array1")),
                                  set));
                           Asttypes.loc = _loc
                         });
                    Parsetree.pexp_loc = _loc;
                    Parsetree.pexp_attributes = []
                  },
                   [(Asttypes.Nolabel, arr);
                   (Asttypes.Nolabel, c1);
                   (Asttypes.Nolabel, newval)]));
            Parsetree.pexp_loc = _loc;
            Parsetree.pexp_attributes = []
          }
      | c1::c2::[] ->
          {
            Parsetree.pexp_desc =
              (Parsetree.Pexp_apply
                 ({
                    Parsetree.pexp_desc =
                      (Parsetree.Pexp_ident
                         {
                           Asttypes.txt =
                             (Longident.Ldot
                                ((Longident.Ldot
                                    ((Longident.Lident "Bigarray"), "Array2")),
                                  set));
                           Asttypes.loc = _loc
                         });
                    Parsetree.pexp_loc = _loc;
                    Parsetree.pexp_attributes = []
                  },
                   [(Asttypes.Nolabel, arr);
                   (Asttypes.Nolabel, c1);
                   (Asttypes.Nolabel, c2);
                   (Asttypes.Nolabel, newval)]));
            Parsetree.pexp_loc = _loc;
            Parsetree.pexp_attributes = []
          }
      | c1::c2::c3::[] ->
          {
            Parsetree.pexp_desc =
              (Parsetree.Pexp_apply
                 ({
                    Parsetree.pexp_desc =
                      (Parsetree.Pexp_ident
                         {
                           Asttypes.txt =
                             (Longident.Ldot
                                ((Longident.Ldot
                                    ((Longident.Lident "Bigarray"), "Array3")),
                                  set));
                           Asttypes.loc = _loc
                         });
                    Parsetree.pexp_loc = _loc;
                    Parsetree.pexp_attributes = []
                  },
                   [(Asttypes.Nolabel, arr);
                   (Asttypes.Nolabel, c1);
                   (Asttypes.Nolabel, c2);
                   (Asttypes.Nolabel, c3);
                   (Asttypes.Nolabel, newval)]));
            Parsetree.pexp_loc = _loc;
            Parsetree.pexp_attributes = []
          }
      | coords ->
          {
            Parsetree.pexp_desc =
              (Parsetree.Pexp_apply
                 ({
                    Parsetree.pexp_desc =
                      (Parsetree.Pexp_ident
                         {
                           Asttypes.txt =
                             (Longident.Ldot
                                ((Longident.Ldot
                                    ((Longident.Lident "Bigarray"),
                                      "Genarray")), set));
                           Asttypes.loc = _loc
                         });
                    Parsetree.pexp_loc = _loc;
                    Parsetree.pexp_attributes = []
                  },
                   [(Asttypes.Nolabel, arr);
                   (Asttypes.Nolabel, (Pa_ast.exp_array _loc coords));
                   (Asttypes.Nolabel, newval)]));
            Parsetree.pexp_loc = _loc;
            Parsetree.pexp_attributes = []
          }
    let constructor = Earley_core.Earley.declare_grammar "constructor"
    let _ =
      Earley_core.Earley.set_grammar constructor
        (Earley_core.Earley.fsequence
           (Earley_core.Earley.option None
              (Earley_core.Earley.apply (fun x -> Some x)
                 (Earley_core.Earley.fsequence module_path
                    (Earley_core.Earley.fsequence_ignore
                       (Earley_core.Earley.string "." ".")
                       (Earley_core.Earley.empty (fun m -> m))))))
           (Earley_core.Earley.fsequence
              (Earley_core.Earley.alternatives [bool_lit; uident])
              (Earley_core.Earley.empty
                 (fun id ->
                    fun m ->
                      match m with
                      | None -> Lident id
                      | Some m -> Ldot (m, id)))))
    let argument = Earley_core.Earley.declare_grammar "argument"
    let _ =
      Earley_core.Earley.set_grammar argument
        (Earley_core.Earley.alternatives
           [Earley_core.Earley.fsequence
              (expression_lvl (NoMatch, (next_exp App)))
              (Earley_core.Earley.empty (fun e -> (nolabel, e)));
           Earley_core.Earley.fsequence_ignore
             (Earley_core.Earley.char '~' '~')
             (Earley_core.Earley.fsequence_position lident
                (Earley_core.Earley.fsequence no_colon
                   (Earley_core.Earley.empty
                      (fun _default_0 ->
                         fun str ->
                           fun pos ->
                             fun str' ->
                               fun pos' ->
                                 fun id ->
                                   let _loc_id = locate str pos str' pos' in
                                   ((labelled id),
                                     (loc_expr _loc_id
                                        (Pexp_ident
                                           (id_loc (Lident id) _loc_id))))))));
           Earley_core.Earley.fsequence ty_label
             (Earley_core.Earley.fsequence
                (expression_lvl (NoMatch, (next_exp App)))
                (Earley_core.Earley.empty (fun e -> fun id -> (id, e))));
           Earley_core.Earley.fsequence_ignore
             (Earley_core.Earley.char '?' '?')
             (Earley_core.Earley.fsequence_position lident
                (Earley_core.Earley.empty
                   (fun str ->
                      fun pos ->
                        fun str' ->
                          fun pos' ->
                            fun id ->
                              let _loc_id = locate str pos str' pos' in
                              ((optional id),
                                (Exp.ident ~loc:_loc_id
                                   (id_loc (Lident id) _loc_id))))));
           Earley_core.Earley.fsequence ty_opt_label
             (Earley_core.Earley.fsequence
                (expression_lvl (NoMatch, (next_exp App)))
                (Earley_core.Earley.empty (fun e -> fun id -> (id, e))))])
    let _ =
      set_parameter
        (fun allow_new_type ->
           Earley_core.Earley.alternatives
             ((if allow_new_type
               then
                 [Earley_core.Earley.fsequence_ignore
                    (Earley_core.Earley.char '(' '(')
                    (Earley_core.Earley.fsequence type_kw
                       (Earley_core.Earley.fsequence_position typeconstr_name
                          (Earley_core.Earley.fsequence_ignore
                             (Earley_core.Earley.char ')' ')')
                             (Earley_core.Earley.empty
                                (fun str ->
                                   fun pos ->
                                     fun str' ->
                                       fun pos' ->
                                         fun name ->
                                           let _loc_name =
                                             locate str pos str' pos' in
                                           fun _default_0 ->
                                             let name = id_loc name _loc_name in
                                             `Type name)))))]
               else []) @
                [Earley_core.Earley.fsequence (pattern_lvl (false, AtomPat))
                   (Earley_core.Earley.empty
                      (fun pat -> `Arg (nolabel, None, pat)));
                Earley_core.Earley.fsequence_ignore
                  (Earley_core.Earley.char '~' '~')
                  (Earley_core.Earley.fsequence_ignore
                     (Earley_core.Earley.char '(' '(')
                     (Earley_core.Earley.fsequence_position lident
                        (Earley_core.Earley.fsequence
                           (Earley_core.Earley.option None
                              (Earley_core.Earley.apply (fun x -> Some x)
                                 (Earley_core.Earley.fsequence_ignore
                                    (Earley_core.Earley.string ":" ":")
                                    (Earley_core.Earley.fsequence typexpr
                                       (Earley_core.Earley.empty (fun t -> t))))))
                           (Earley_core.Earley.fsequence_ignore
                              (Earley_core.Earley.string ")" ")")
                              (Earley_core.Earley.empty_pos
                                 (fun __loc__start__buf ->
                                    fun __loc__start__pos ->
                                      fun __loc__end__buf ->
                                        fun __loc__end__pos ->
                                          let _loc =
                                            locate __loc__start__buf
                                              __loc__start__pos
                                              __loc__end__buf __loc__end__pos in
                                          fun t ->
                                            fun str ->
                                              fun pos ->
                                                fun str' ->
                                                  fun pos' ->
                                                    fun id ->
                                                      let _loc_id =
                                                        locate str pos str'
                                                          pos' in
                                                      let pat =
                                                        loc_pat _loc_id
                                                          (Ppat_var
                                                             (id_loc id
                                                                _loc_id)) in
                                                      let pat =
                                                        match t with
                                                        | None -> pat
                                                        | Some t ->
                                                            loc_pat _loc
                                                              (Ppat_constraint
                                                                 (pat, t)) in
                                                      `Arg
                                                        ((labelled id), None,
                                                          pat)))))));
                Earley_core.Earley.fsequence ty_label
                  (Earley_core.Earley.fsequence pattern
                     (Earley_core.Earley.empty
                        (fun pat -> fun id -> `Arg (id, None, pat))));
                Earley_core.Earley.fsequence_ignore
                  (Earley_core.Earley.char '~' '~')
                  (Earley_core.Earley.fsequence_position lident
                     (Earley_core.Earley.fsequence no_colon
                        (Earley_core.Earley.empty
                           (fun _default_0 ->
                              fun str ->
                                fun pos ->
                                  fun str' ->
                                    fun pos' ->
                                      fun id ->
                                        let _loc_id =
                                          locate str pos str' pos' in
                                        `Arg
                                          ((labelled id), None,
                                            (loc_pat _loc_id
                                               (Ppat_var (id_loc id _loc_id))))))));
                Earley_core.Earley.fsequence_ignore
                  (Earley_core.Earley.char '?' '?')
                  (Earley_core.Earley.fsequence_ignore
                     (Earley_core.Earley.char '(' '(')
                     (Earley_core.Earley.fsequence_position lident
                        (Earley_core.Earley.fsequence_position
                           (Earley_core.Earley.option None
                              (Earley_core.Earley.apply (fun x -> Some x)
                                 (Earley_core.Earley.fsequence_ignore
                                    (Earley_core.Earley.char ':' ':')
                                    (Earley_core.Earley.fsequence typexpr
                                       (Earley_core.Earley.empty (fun t -> t))))))
                           (Earley_core.Earley.fsequence
                              (Earley_core.Earley.option None
                                 (Earley_core.Earley.apply (fun x -> Some x)
                                    (Earley_core.Earley.fsequence_ignore
                                       (Earley_core.Earley.char '=' '=')
                                       (Earley_core.Earley.fsequence
                                          expression
                                          (Earley_core.Earley.empty
                                             (fun e -> e))))))
                              (Earley_core.Earley.fsequence_ignore
                                 (Earley_core.Earley.char ')' ')')
                                 (Earley_core.Earley.empty
                                    (fun e ->
                                       fun str ->
                                         fun pos ->
                                           fun str' ->
                                             fun pos' ->
                                               fun t ->
                                                 let _loc_t =
                                                   locate str pos str' pos' in
                                                 fun str ->
                                                   fun pos ->
                                                     fun str' ->
                                                       fun pos' ->
                                                         fun id ->
                                                           let _loc_id =
                                                             locate str pos
                                                               str' pos' in
                                                           let pat =
                                                             loc_pat _loc_id
                                                               (Ppat_var
                                                                  (id_loc id
                                                                    _loc_id)) in
                                                           let pat =
                                                             match t with
                                                             | None -> pat
                                                             | Some t ->
                                                                 loc_pat
                                                                   (merge2
                                                                    _loc_id
                                                                    _loc_t)
                                                                   (Ppat_constraint
                                                                    (pat, t)) in
                                                           `Arg
                                                             ((optional id),
                                                               e, pat))))))));
                Earley_core.Earley.fsequence ty_opt_label
                  (Earley_core.Earley.fsequence_ignore
                     (Earley_core.Earley.string "(" "(")
                     (Earley_core.Earley.fsequence_position pattern
                        (Earley_core.Earley.fsequence_position
                           (Earley_core.Earley.option None
                              (Earley_core.Earley.apply (fun x -> Some x)
                                 (Earley_core.Earley.fsequence_ignore
                                    (Earley_core.Earley.char ':' ':')
                                    (Earley_core.Earley.fsequence typexpr
                                       (Earley_core.Earley.empty (fun t -> t))))))
                           (Earley_core.Earley.fsequence
                              (Earley_core.Earley.option None
                                 (Earley_core.Earley.apply (fun x -> Some x)
                                    (Earley_core.Earley.fsequence_ignore
                                       (Earley_core.Earley.char '=' '=')
                                       (Earley_core.Earley.fsequence
                                          expression
                                          (Earley_core.Earley.empty
                                             (fun e -> e))))))
                              (Earley_core.Earley.fsequence_ignore
                                 (Earley_core.Earley.char ')' ')')
                                 (Earley_core.Earley.empty
                                    (fun e ->
                                       fun str ->
                                         fun pos ->
                                           fun str' ->
                                             fun pos' ->
                                               fun t ->
                                                 let _loc_t =
                                                   locate str pos str' pos' in
                                                 fun str ->
                                                   fun pos ->
                                                     fun str' ->
                                                       fun pos' ->
                                                         fun pat ->
                                                           let _loc_pat =
                                                             locate str pos
                                                               str' pos' in
                                                           fun id ->
                                                             let pat =
                                                               match t with
                                                               | None -> pat
                                                               | Some t ->
                                                                   loc_pat
                                                                    (merge2
                                                                    _loc_pat
                                                                    _loc_t)
                                                                    (Ppat_constraint
                                                                    (pat, t)) in
                                                             `Arg
                                                               (id, e, pat))))))));
                Earley_core.Earley.fsequence ty_opt_label
                  (Earley_core.Earley.fsequence pattern
                     (Earley_core.Earley.empty
                        (fun pat -> fun id -> `Arg (id, None, pat))));
                Earley_core.Earley.fsequence_ignore
                  (Earley_core.Earley.char '?' '?')
                  (Earley_core.Earley.fsequence_position lident
                     (Earley_core.Earley.empty
                        (fun str ->
                           fun pos ->
                             fun str' ->
                               fun pos' ->
                                 fun id ->
                                   let _loc_id = locate str pos str' pos' in
                                   `Arg
                                     ((optional id), None,
                                       (loc_pat _loc_id
                                          (Ppat_var (id_loc id _loc_id)))))))]))
    let apply_params ?(gh= false)  _loc params e =
      let f acc =
        function
        | (`Arg (lbl, opt, pat), _loc') ->
            loc_expr (ghost (merge2 _loc' _loc))
              (pexp_fun (lbl, opt, pat, acc))
        | (`Type name, _loc') ->
            Exp.newtype ~loc:(merge2 _loc' _loc) name acc in
      let e = List.fold_left f e (List.rev params) in
      if gh then e else de_ghost e
    [@@@ocaml.text
      " FIXME OCAML: should be ghost, or above should not be ghost "]
    let apply_params_cls ?(gh= false)  _loc params e =
      let ghost _loc' = if gh then merge2 _loc' _loc else _loc in
      let f acc =
        function
        | (`Arg (lbl, opt, pat), _loc') ->
            loc_pcl (ghost _loc') (Pcl_fun (lbl, opt, pat, acc))
        | (`Type name, _) -> assert false in
      List.fold_left f e (List.rev params)
    [@@@ocaml.text " FIXME OCAML: shoud be ghost as above ? "]
    let right_member = Earley_core.Earley.declare_grammar "right_member"
    let _ =
      Earley_core.Earley.set_grammar right_member
        (Earley_core.Earley.fsequence
           (Earley_core.Earley.apply (fun f -> f [])
              (Earley_core.Earley.fixpoint1' (fun l -> l)
                 (Earley_core.Earley.fsequence_position (parameter true)
                    (Earley_core.Earley.empty
                       (fun str ->
                          fun pos ->
                            fun str' ->
                              fun pos' ->
                                fun lb ->
                                  let _loc_lb = locate str pos str' pos' in
                                  (lb, _loc_lb))))
                 (fun x -> fun f -> fun l -> f (x :: l))))
           (Earley_core.Earley.fsequence_position
              (Earley_core.Earley.option None
                 (Earley_core.Earley.apply (fun x -> Some x)
                    (Earley_core.Earley.fsequence_ignore
                       (Earley_core.Earley.char ':' ':')
                       (Earley_core.Earley.fsequence typexpr
                          (Earley_core.Earley.empty (fun t -> t))))))
              (Earley_core.Earley.fsequence_ignore
                 (Earley_core.Earley.char '=' '=')
                 (Earley_core.Earley.fsequence_position expression
                    (Earley_core.Earley.empty_pos
                       (fun __loc__start__buf ->
                          fun __loc__start__pos ->
                            fun __loc__end__buf ->
                              fun __loc__end__pos ->
                                let _loc =
                                  locate __loc__start__buf __loc__start__pos
                                    __loc__end__buf __loc__end__pos in
                                fun str ->
                                  fun pos ->
                                    fun str' ->
                                      fun pos' ->
                                        fun e ->
                                          let _loc_e =
                                            locate str pos str' pos' in
                                          fun str ->
                                            fun pos ->
                                              fun str' ->
                                                fun pos' ->
                                                  fun ty ->
                                                    let _loc_ty =
                                                      locate str pos str'
                                                        pos' in
                                                    fun l ->
                                                      let e =
                                                        match ty with
                                                        | None -> e
                                                        | Some ty ->
                                                            loc_expr
                                                              (ghost
                                                                 (merge2
                                                                    _loc_ty
                                                                    _loc))
                                                              (pexp_constraint
                                                                 (e, ty)) in
                                                      apply_params ~gh:true
                                                        _loc_e l e))))))
    let eright_member = Earley_core.Earley.declare_grammar "eright_member"
    let _ =
      Earley_core.Earley.set_grammar eright_member
        (Earley_core.Earley.fsequence
           (Earley_core.Earley.option None
              (Earley_core.Earley.apply (fun x -> Some x)
                 (Earley_core.Earley.fsequence_ignore
                    (Earley_core.Earley.char ':' ':')
                    (Earley_core.Earley.fsequence typexpr
                       (Earley_core.Earley.empty (fun t -> t))))))
           (Earley_core.Earley.fsequence_ignore
              (Earley_core.Earley.char '=' '=')
              (Earley_core.Earley.fsequence expression
                 (Earley_core.Earley.empty (fun e -> fun ty -> (ty, e))))))
    let _ =
      set_grammar let_binding
        (Earley_core.Earley.alternatives
           [Earley_core.Earley.fsequence (Earley_core.Earley.char '$' '$')
              (Earley_core.Earley.fsequence_ignore
                 (Earley_core.Earley.no_blank_test ())
                 (Earley_core.Earley.fsequence
                    (Earley_core.Earley.option "bindings"
                       (Earley_core.Earley.fsequence
                          (Earley_core.Earley.string "bindings" "bindings")
                          (Earley_core.Earley.fsequence_ignore
                             (Earley_core.Earley.string ":" ":")
                             (Earley_core.Earley.empty (fun c -> c)))))
                    (Earley_core.Earley.fsequence
                       (expression_lvl (NoMatch, App))
                       (Earley_core.Earley.fsequence_ignore
                          (Earley_core.Earley.no_blank_test ())
                          (Earley_core.Earley.fsequence_ignore
                             (Earley_core.Earley.char '$' '$')
                             (Earley_core.Earley.fsequence
                                (Earley_core.Earley.option []
                                   (Earley_core.Earley.fsequence_ignore
                                      and_kw
                                      (Earley_core.Earley.fsequence
                                         let_binding
                                         (Earley_core.Earley.empty
                                            (fun _default_0 -> _default_0)))))
                                (Earley_core.Earley.empty_pos
                                   (fun __loc__start__buf ->
                                      fun __loc__start__pos ->
                                        fun __loc__end__buf ->
                                          fun __loc__end__pos ->
                                            let _loc =
                                              locate __loc__start__buf
                                                __loc__start__pos
                                                __loc__end__buf
                                                __loc__end__pos in
                                            fun l ->
                                              fun e ->
                                                fun aq ->
                                                  fun dol ->
                                                    let open Quote in
                                                      let generic_antiquote e
                                                        =
                                                        function
                                                        | Quote_loc -> e
                                                        | _ ->
                                                            failwith
                                                              "invalid antiquotation" in
                                                      let f =
                                                        match aq with
                                                        | "bindings" ->
                                                            generic_antiquote
                                                              e
                                                        | _ -> give_up () in
                                                      make_list_antiquotation
                                                        _loc Quote_loc f))))))));
           Earley_core.Earley.fsequence_position pattern
             (Earley_core.Earley.fsequence_position eright_member
                (Earley_core.Earley.fsequence post_item_attributes
                   (Earley_core.Earley.fsequence
                      (Earley_core.Earley.option []
                         (Earley_core.Earley.fsequence_ignore and_kw
                            (Earley_core.Earley.fsequence let_binding
                               (Earley_core.Earley.empty
                                  (fun _default_0 -> _default_0)))))
                      (Earley_core.Earley.empty_pos
                         (fun __loc__start__buf ->
                            fun __loc__start__pos ->
                              fun __loc__end__buf ->
                                fun __loc__end__pos ->
                                  let _loc =
                                    locate __loc__start__buf
                                      __loc__start__pos __loc__end__buf
                                      __loc__end__pos in
                                  fun l ->
                                    fun a ->
                                      fun str ->
                                        fun pos ->
                                          fun str' ->
                                            fun pos' ->
                                              fun erm ->
                                                let _loc_erm =
                                                  locate str pos str' pos' in
                                                fun str ->
                                                  fun pos ->
                                                    fun str' ->
                                                      fun pos' ->
                                                        fun pat ->
                                                          let _loc_pat =
                                                            locate str pos
                                                              str' pos' in
                                                          let (_ty, e) = erm in
                                                          let loc =
                                                            merge2 _loc_pat
                                                              _loc_erm in
                                                          let (pat, e) =
                                                            match _ty with
                                                            | None ->
                                                                (pat, e)
                                                            | Some ty ->
                                                                let loc =
                                                                  ghost _loc in
                                                                let poly_ty =
                                                                  Typ.poly
                                                                    ~loc:(
                                                                    ghost
                                                                    _loc) []
                                                                    ty in
                                                                ((Pat.constraint_
                                                                    ~loc pat
                                                                    poly_ty),
                                                                  (Exp.constraint_
                                                                    ~loc e ty)) in
                                                          (value_binding
                                                             ~attributes:(
                                                             attach_attrib
                                                               loc a) loc pat
                                                             e)
                                                            :: l)))));
           Earley_core.Earley.fsequence_position value_name
             (Earley_core.Earley.fsequence_position right_member
                (Earley_core.Earley.fsequence post_item_attributes
                   (Earley_core.Earley.fsequence
                      (Earley_core.Earley.option []
                         (Earley_core.Earley.fsequence_ignore and_kw
                            (Earley_core.Earley.fsequence let_binding
                               (Earley_core.Earley.empty
                                  (fun _default_0 -> _default_0)))))
                      (Earley_core.Earley.empty
                         (fun l ->
                            fun a ->
                              fun str ->
                                fun pos ->
                                  fun str' ->
                                    fun pos' ->
                                      fun e ->
                                        let _loc_e = locate str pos str' pos' in
                                        fun str ->
                                          fun pos ->
                                            fun str' ->
                                              fun pos' ->
                                                fun vn ->
                                                  let _loc_vn =
                                                    locate str pos str' pos' in
                                                  let loc =
                                                    merge2 _loc_vn _loc_e in
                                                  let pat =
                                                    pat_ident _loc_vn vn in
                                                  (value_binding
                                                     ~attributes:(attach_attrib
                                                                    loc a)
                                                     loc pat e)
                                                    :: l)))));
           Earley_core.Earley.fsequence_position value_name
             (Earley_core.Earley.fsequence_ignore
                (Earley_core.Earley.char ':' ':')
                (Earley_core.Earley.fsequence only_poly_typexpr
                   (Earley_core.Earley.fsequence_ignore
                      (Earley_core.Earley.char '=' '=')
                      (Earley_core.Earley.fsequence_position expression
                         (Earley_core.Earley.fsequence post_item_attributes
                            (Earley_core.Earley.fsequence
                               (Earley_core.Earley.option []
                                  (Earley_core.Earley.fsequence_ignore and_kw
                                     (Earley_core.Earley.fsequence
                                        let_binding
                                        (Earley_core.Earley.empty
                                           (fun _default_0 -> _default_0)))))
                               (Earley_core.Earley.empty_pos
                                  (fun __loc__start__buf ->
                                     fun __loc__start__pos ->
                                       fun __loc__end__buf ->
                                         fun __loc__end__pos ->
                                           let _loc =
                                             locate __loc__start__buf
                                               __loc__start__pos
                                               __loc__end__buf
                                               __loc__end__pos in
                                           fun l ->
                                             fun a ->
                                               fun str ->
                                                 fun pos ->
                                                   fun str' ->
                                                     fun pos' ->
                                                       fun e ->
                                                         let _loc_e =
                                                           locate str pos
                                                             str' pos' in
                                                         fun ty ->
                                                           fun str ->
                                                             fun pos ->
                                                               fun str' ->
                                                                 fun pos' ->
                                                                   fun vn ->
                                                                    let _loc_vn
                                                                    =
                                                                    locate
                                                                    str pos
                                                                    str' pos' in
                                                                    let pat =
                                                                    loc_pat
                                                                    (ghost
                                                                    _loc)
                                                                    (Ppat_constraint
                                                                    ((loc_pat
                                                                    _loc_vn
                                                                    (Ppat_var
                                                                    (id_loc
                                                                    vn
                                                                    _loc_vn))),
                                                                    (loc_typ
                                                                    (ghost
                                                                    _loc)
                                                                    ty.ptyp_desc))) in
                                                                    let loc =
                                                                    merge2
                                                                    _loc_vn
                                                                    _loc_e in
                                                                    (value_binding
                                                                    ~attributes:(
                                                                    attach_attrib
                                                                    loc a)
                                                                    loc pat e)
                                                                    :: l))))))));
           Earley_core.Earley.fsequence_position value_name
             (Earley_core.Earley.fsequence_ignore
                (Earley_core.Earley.char ':' ':')
                (Earley_core.Earley.fsequence poly_syntax_typexpr
                   (Earley_core.Earley.fsequence_ignore
                      (Earley_core.Earley.char '=' '=')
                      (Earley_core.Earley.fsequence_position expression
                         (Earley_core.Earley.fsequence post_item_attributes
                            (Earley_core.Earley.fsequence
                               (Earley_core.Earley.option []
                                  (Earley_core.Earley.fsequence_ignore and_kw
                                     (Earley_core.Earley.fsequence
                                        let_binding
                                        (Earley_core.Earley.empty
                                           (fun _default_0 -> _default_0)))))
                               (Earley_core.Earley.empty
                                  (fun l ->
                                     fun a ->
                                       fun str ->
                                         fun pos ->
                                           fun str' ->
                                             fun pos' ->
                                               fun e ->
                                                 let _loc_e =
                                                   locate str pos str' pos' in
                                                 fun
                                                   ((ids, ty) as _default_0)
                                                   ->
                                                   fun str ->
                                                     fun pos ->
                                                       fun str' ->
                                                         fun pos' ->
                                                           fun vn ->
                                                             let _loc_vn =
                                                               locate str pos
                                                                 str' pos' in
                                                             let loc =
                                                               merge2 _loc_vn
                                                                 _loc_e in
                                                             let (e, ty) =
                                                               wrap_type_annotation
                                                                 loc ids ty e in
                                                             let pat =
                                                               loc_pat
                                                                 (ghost loc)
                                                                 (Ppat_constraint
                                                                    ((loc_pat
                                                                    _loc_vn
                                                                    (Ppat_var
                                                                    (id_loc
                                                                    vn
                                                                    _loc_vn))),
                                                                    ty)) in
                                                             (value_binding
                                                                ~attributes:(
                                                                attach_attrib
                                                                  loc a) loc
                                                                pat e)
                                                               :: l))))))))])
    [@@@ocaml.text " FIXME OCAML: shoud not change the position below "]
    let (match_case, match_case__set__grammar) =
      Earley_core.Earley.grammar_family "match_case"
    let match_case __curry__varx0 __curry__varx1 =
      match_case (__curry__varx0, __curry__varx1)
    let _ =
      match_case__set__grammar
        (fun (alm, lvl) ->
           Earley_core.Earley.fsequence pattern
             (Earley_core.Earley.fsequence
                (Earley_core.Earley.option None
                   (Earley_core.Earley.apply (fun x -> Some x)
                      (Earley_core.Earley.fsequence_ignore when_kw
                         (Earley_core.Earley.fsequence expression
                            (Earley_core.Earley.empty
                               (fun _default_0 -> _default_0))))))
                (Earley_core.Earley.fsequence arrow_re
                   (Earley_core.Earley.fsequence
                      (Earley_core.Earley.alternatives
                         [Earley_core.Earley.fsequence_ignore
                            (Earley_core.Earley.string "." ".")
                            (Earley_core.Earley.empty_pos
                               (fun __loc__start__buf ->
                                  fun __loc__start__pos ->
                                    fun __loc__end__buf ->
                                      fun __loc__end__pos ->
                                        let _loc =
                                          locate __loc__start__buf
                                            __loc__start__pos __loc__end__buf
                                            __loc__end__pos in
                                        Exp.unreachable ~loc:_loc ()));
                         expression_lvl (alm, lvl)])
                      (Earley_core.Earley.empty
                         (fun e ->
                            fun _default_0 ->
                              fun w -> fun pat -> make_case pat e w))))))
    let _ =
      set_grammar match_cases
        (Earley_core.Earley.alternatives
           [Earley_core.Earley.fsequence_ignore
              (Earley_core.Earley.char '$' '$')
              (Earley_core.Earley.fsequence_ignore
                 (Earley_core.Earley.no_blank_test ())
                 (Earley_core.Earley.fsequence
                    (Earley_core.Earley.option None
                       (Earley_core.Earley.apply (fun x -> Some x)
                          (Earley_core.Earley.fsequence_ignore
                             (Earley_core.Earley.string "cases" "cases")
                             (Earley_core.Earley.fsequence_ignore
                                (Earley_core.Earley.string ":" ":")
                                (Earley_core.Earley.empty ())))))
                    (Earley_core.Earley.fsequence expression
                       (Earley_core.Earley.fsequence_ignore
                          (Earley_core.Earley.no_blank_test ())
                          (Earley_core.Earley.fsequence_ignore
                             (Earley_core.Earley.char '$' '$')
                             (Earley_core.Earley.empty_pos
                                (fun __loc__start__buf ->
                                   fun __loc__start__pos ->
                                     fun __loc__end__buf ->
                                       fun __loc__end__pos ->
                                         let _loc =
                                           locate __loc__start__buf
                                             __loc__start__pos
                                             __loc__end__buf __loc__end__pos in
                                         fun e ->
                                           fun _default_0 ->
                                             list_antiquotation _loc e)))))));
           Earley_core.Earley.fsequence
             (Earley_core.Earley.option None
                (Earley_core.Earley.apply (fun x -> Some x)
                   (Earley_core.Earley.char '|' '|')))
             (Earley_core.Earley.fsequence
                (Earley_core.Earley.apply (fun f -> f [])
                   (Earley_core.Earley.fixpoint' (fun l -> l)
                      (Earley_core.Earley.fsequence (match_case Let Seq)
                         (Earley_core.Earley.fsequence_ignore
                            (Earley_core.Earley.char '|' '|')
                            (Earley_core.Earley.empty
                               (fun _default_0 -> _default_0))))
                      (fun x -> fun f -> fun l -> f (x :: l))))
                (Earley_core.Earley.fsequence (match_case Match Seq)
                   (Earley_core.Earley.fsequence no_semi
                      (Earley_core.Earley.empty
                         (fun _default_0 ->
                            fun x -> fun l -> fun _default_1 -> l @ [x])))));
           Earley_core.Earley.fsequence_ignore (Earley_core.Earley.empty ())
             (Earley_core.Earley.empty [])])
    let type_coercion = Earley_core.Earley.declare_grammar "type_coercion"
    let _ =
      Earley_core.Earley.set_grammar type_coercion
        (Earley_core.Earley.alternatives
           [Earley_core.Earley.fsequence_ignore
              (Earley_core.Earley.string ":>" ":>")
              (Earley_core.Earley.fsequence typexpr
                 (Earley_core.Earley.empty (fun t' -> (None, (Some t')))));
           Earley_core.Earley.fsequence_ignore
             (Earley_core.Earley.string ":" ":")
             (Earley_core.Earley.fsequence typexpr
                (Earley_core.Earley.fsequence
                   (Earley_core.Earley.option None
                      (Earley_core.Earley.apply (fun x -> Some x)
                         (Earley_core.Earley.fsequence_ignore
                            (Earley_core.Earley.string ":>" ":>")
                            (Earley_core.Earley.fsequence typexpr
                               (Earley_core.Earley.empty (fun t' -> t'))))))
                   (Earley_core.Earley.empty
                      (fun t' -> fun t -> ((Some t), t')))))])
    let expression_list =
      Earley_core.Earley.declare_grammar "expression_list"
    let _ =
      Earley_core.Earley.set_grammar expression_list
        (Earley_core.Earley.alternatives
           [Earley_core.Earley.fsequence_ignore (Earley_core.Earley.empty ())
              (Earley_core.Earley.empty []);
           Earley_core.Earley.fsequence
             (Earley_core.Earley.apply (fun f -> f [])
                (Earley_core.Earley.fixpoint' (fun l -> l)
                   (Earley_core.Earley.fsequence_position
                      (expression_lvl (NoMatch, (next_exp Seq)))
                      (Earley_core.Earley.fsequence_ignore semi_col
                         (Earley_core.Earley.empty
                            (fun str ->
                               fun pos ->
                                 fun str' ->
                                   fun pos' ->
                                     fun e ->
                                       let _loc_e = locate str pos str' pos' in
                                       (e, _loc_e)))))
                   (fun x -> fun f -> fun l -> f (x :: l))))
             (Earley_core.Earley.fsequence_position
                (expression_lvl (Match, (next_exp Seq)))
                (Earley_core.Earley.fsequence
                   (Earley_core.Earley.option None
                      (Earley_core.Earley.apply (fun x -> Some x) semi_col))
                   (Earley_core.Earley.empty
                      (fun _default_0 ->
                         fun str ->
                           fun pos ->
                             fun str' ->
                               fun pos' ->
                                 fun e ->
                                   let _loc_e = locate str pos str' pos' in
                                   fun l -> l @ [(e, _loc_e)]))))])
    let record_item = Earley_core.Earley.declare_grammar "record_item"
    let _ =
      Earley_core.Earley.set_grammar record_item
        (Earley_core.Earley.alternatives
           [Earley_core.Earley.fsequence_position lident
              (Earley_core.Earley.empty
                 (fun str ->
                    fun pos ->
                      fun str' ->
                        fun pos' ->
                          fun f ->
                            let _loc_f = locate str pos str' pos' in
                            let id = id_loc (Lident f) _loc_f in
                            (id, (loc_expr _loc_f (Pexp_ident id)))));
           Earley_core.Earley.fsequence_position field
             (Earley_core.Earley.fsequence_ignore
                (Earley_core.Earley.char '=' '=')
                (Earley_core.Earley.fsequence
                   (expression_lvl (NoMatch, (next_exp Seq)))
                   (Earley_core.Earley.empty
                      (fun e ->
                         fun str ->
                           fun pos ->
                             fun str' ->
                               fun pos' ->
                                 fun f ->
                                   let _loc_f = locate str pos str' pos' in
                                   ((id_loc f _loc_f), e)))))])
    let last_record_item =
      Earley_core.Earley.declare_grammar "last_record_item"
    let _ =
      Earley_core.Earley.set_grammar last_record_item
        (Earley_core.Earley.alternatives
           [Earley_core.Earley.fsequence_position lident
              (Earley_core.Earley.empty
                 (fun str ->
                    fun pos ->
                      fun str' ->
                        fun pos' ->
                          fun f ->
                            let _loc_f = locate str pos str' pos' in
                            let id = id_loc (Lident f) _loc_f in
                            (id, (loc_expr _loc_f (Pexp_ident id)))));
           Earley_core.Earley.fsequence_position field
             (Earley_core.Earley.fsequence_ignore
                (Earley_core.Earley.char '=' '=')
                (Earley_core.Earley.fsequence
                   (expression_lvl (Match, (next_exp Seq)))
                   (Earley_core.Earley.empty
                      (fun e ->
                         fun str ->
                           fun pos ->
                             fun str' ->
                               fun pos' ->
                                 fun f ->
                                   let _loc_f = locate str pos str' pos' in
                                   ((id_loc f _loc_f), e)))))])
    let _ =
      set_grammar record_list
        (Earley_core.Earley.alternatives
           [Earley_core.Earley.fsequence_ignore (Earley_core.Earley.empty ())
              (Earley_core.Earley.empty []);
           Earley_core.Earley.fsequence
             (Earley_core.Earley.apply (fun f -> f [])
                (Earley_core.Earley.fixpoint' (fun l -> l)
                   (Earley_core.Earley.fsequence record_item
                      (Earley_core.Earley.fsequence_ignore semi_col
                         (Earley_core.Earley.empty
                            (fun _default_0 -> _default_0))))
                   (fun x -> fun f -> fun l -> f (x :: l))))
             (Earley_core.Earley.fsequence last_record_item
                (Earley_core.Earley.fsequence
                   (Earley_core.Earley.option None
                      (Earley_core.Earley.apply (fun x -> Some x) semi_col))
                   (Earley_core.Earley.empty
                      (fun _default_0 -> fun it -> fun l -> l @ [it]))))])
    let obj_item = Earley_core.Earley.declare_grammar "obj_item"
    let _ =
      Earley_core.Earley.set_grammar obj_item
        (Earley_core.Earley.fsequence_position inst_var_name
           (Earley_core.Earley.fsequence_ignore
              (Earley_core.Earley.char '=' '=')
              (Earley_core.Earley.fsequence
                 (expression_lvl (Match, (next_exp Seq)))
                 (Earley_core.Earley.empty
                    (fun e ->
                       fun str ->
                         fun pos ->
                           fun str' ->
                             fun pos' ->
                               fun v ->
                                 let _loc_v = locate str pos str' pos' in
                                 ((id_loc v _loc_v), e))))))
    let class_expr_base =
      Earley_core.Earley.declare_grammar "class_expr_base"
    let _ =
      Earley_core.Earley.set_grammar class_expr_base
        (Earley_core.Earley.alternatives
           [Earley_core.Earley.fsequence object_kw
              (Earley_core.Earley.fsequence class_body
                 (Earley_core.Earley.fsequence end_kw
                    (Earley_core.Earley.empty_pos
                       (fun __loc__start__buf ->
                          fun __loc__start__pos ->
                            fun __loc__end__buf ->
                              fun __loc__end__pos ->
                                let _loc =
                                  locate __loc__start__buf __loc__start__pos
                                    __loc__end__buf __loc__end__pos in
                                fun _default_0 ->
                                  fun cb ->
                                    fun _default_1 ->
                                      loc_pcl _loc (Pcl_structure cb)))));
           Earley_core.Earley.fsequence_position class_path
             (Earley_core.Earley.empty_pos
                (fun __loc__start__buf ->
                   fun __loc__start__pos ->
                     fun __loc__end__buf ->
                       fun __loc__end__pos ->
                         let _loc =
                           locate __loc__start__buf __loc__start__pos
                             __loc__end__buf __loc__end__pos in
                         fun str ->
                           fun pos ->
                             fun str' ->
                               fun pos' ->
                                 fun cp ->
                                   let _loc_cp = locate str pos str' pos' in
                                   let cp = id_loc cp _loc_cp in
                                   loc_pcl _loc (Pcl_constr (cp, []))));
           Earley_core.Earley.fsequence_ignore
             (Earley_core.Earley.char '[' '[')
             (Earley_core.Earley.fsequence typexpr
                (Earley_core.Earley.fsequence
                   (Earley_core.Earley.apply (fun f -> f [])
                      (Earley_core.Earley.fixpoint' (fun l -> l)
                         (Earley_core.Earley.fsequence_ignore
                            (Earley_core.Earley.char ',' ',')
                            (Earley_core.Earley.fsequence typexpr
                               (Earley_core.Earley.empty (fun te -> te))))
                         (fun x -> fun f -> fun l -> f (x :: l))))
                   (Earley_core.Earley.fsequence_ignore
                      (Earley_core.Earley.char ']' ']')
                      (Earley_core.Earley.fsequence_position class_path
                         (Earley_core.Earley.empty_pos
                            (fun __loc__start__buf ->
                               fun __loc__start__pos ->
                                 fun __loc__end__buf ->
                                   fun __loc__end__pos ->
                                     let _loc =
                                       locate __loc__start__buf
                                         __loc__start__pos __loc__end__buf
                                         __loc__end__pos in
                                     fun str ->
                                       fun pos ->
                                         fun str' ->
                                           fun pos' ->
                                             fun cp ->
                                               let _loc_cp =
                                                 locate str pos str' pos' in
                                               fun tes ->
                                                 fun te ->
                                                   let cp = id_loc cp _loc_cp in
                                                   loc_pcl _loc
                                                     (Pcl_constr
                                                        (cp, (te :: tes)))))))));
           Earley_core.Earley.fsequence_ignore
             (Earley_core.Earley.string "(" "(")
             (Earley_core.Earley.fsequence class_expr
                (Earley_core.Earley.fsequence_ignore
                   (Earley_core.Earley.string ")" ")")
                   (Earley_core.Earley.empty_pos
                      (fun __loc__start__buf ->
                         fun __loc__start__pos ->
                           fun __loc__end__buf ->
                             fun __loc__end__pos ->
                               let _loc =
                                 locate __loc__start__buf __loc__start__pos
                                   __loc__end__buf __loc__end__pos in
                               fun ce -> loc_pcl _loc ce.pcl_desc))));
           Earley_core.Earley.fsequence_ignore
             (Earley_core.Earley.string "(" "(")
             (Earley_core.Earley.fsequence class_expr
                (Earley_core.Earley.fsequence_ignore
                   (Earley_core.Earley.string ":" ":")
                   (Earley_core.Earley.fsequence class_type
                      (Earley_core.Earley.fsequence_ignore
                         (Earley_core.Earley.string ")" ")")
                         (Earley_core.Earley.empty_pos
                            (fun __loc__start__buf ->
                               fun __loc__start__pos ->
                                 fun __loc__end__buf ->
                                   fun __loc__end__pos ->
                                     let _loc =
                                       locate __loc__start__buf
                                         __loc__start__pos __loc__end__buf
                                         __loc__end__pos in
                                     fun ct ->
                                       fun ce ->
                                         loc_pcl _loc
                                           (Pcl_constraint (ce, ct))))))));
           Earley_core.Earley.fsequence fun_kw
             (Earley_core.Earley.fsequence
                (Earley_core.Earley.apply (fun f -> f [])
                   (Earley_core.Earley.fixpoint1' (fun l -> l)
                      (Earley_core.Earley.fsequence (parameter false)
                         (Earley_core.Earley.empty_pos
                            (fun __loc__start__buf ->
                               fun __loc__start__pos ->
                                 fun __loc__end__buf ->
                                   fun __loc__end__pos ->
                                     let _loc =
                                       locate __loc__start__buf
                                         __loc__start__pos __loc__end__buf
                                         __loc__end__pos in
                                     fun p -> (p, _loc))))
                      (fun x -> fun f -> fun l -> f (x :: l))))
                (Earley_core.Earley.fsequence arrow_re
                   (Earley_core.Earley.fsequence class_expr
                      (Earley_core.Earley.empty_pos
                         (fun __loc__start__buf ->
                            fun __loc__start__pos ->
                              fun __loc__end__buf ->
                                fun __loc__end__pos ->
                                  let _loc =
                                    locate __loc__start__buf
                                      __loc__start__pos __loc__end__buf
                                      __loc__end__pos in
                                  fun ce ->
                                    fun _default_0 ->
                                      fun ps ->
                                        fun _default_1 ->
                                          apply_params_cls _loc ps ce)))));
           Earley_core.Earley.fsequence let_kw
             (Earley_core.Earley.fsequence rec_flag
                (Earley_core.Earley.fsequence let_binding
                   (Earley_core.Earley.fsequence in_kw
                      (Earley_core.Earley.fsequence class_expr
                         (Earley_core.Earley.empty_pos
                            (fun __loc__start__buf ->
                               fun __loc__start__pos ->
                                 fun __loc__end__buf ->
                                   fun __loc__end__pos ->
                                     let _loc =
                                       locate __loc__start__buf
                                         __loc__start__pos __loc__end__buf
                                         __loc__end__pos in
                                     fun ce ->
                                       fun _default_0 ->
                                         fun lbs ->
                                           fun r ->
                                             fun _default_1 ->
                                               loc_pcl _loc
                                                 (Pcl_let (r, lbs, ce))))))))])
    let _ =
      set_grammar class_expr
        (Earley_core.Earley.fsequence class_expr_base
           (Earley_core.Earley.fsequence
              (Earley_core.Earley.option None
                 (Earley_core.Earley.apply (fun x -> Some x)
                    (Earley_core.Earley.apply (fun f -> f [])
                       (Earley_core.Earley.fixpoint1' (fun l -> l) argument
                          (fun x -> fun f -> fun l -> f (x :: l))))))
              (Earley_core.Earley.empty_pos
                 (fun __loc__start__buf ->
                    fun __loc__start__pos ->
                      fun __loc__end__buf ->
                        fun __loc__end__pos ->
                          let _loc =
                            locate __loc__start__buf __loc__start__pos
                              __loc__end__buf __loc__end__pos in
                          fun args ->
                            fun ce ->
                              match args with
                              | None -> ce
                              | Some l -> loc_pcl _loc (Pcl_apply (ce, l))))))
    let class_field = Earley_core.Earley.declare_grammar "class_field"
    let _ =
      Earley_core.Earley.set_grammar class_field
        (Earley_core.Earley.alternatives
           [Earley_core.Earley.fsequence floating_extension
              (Earley_core.Earley.empty_pos
                 (fun __loc__start__buf ->
                    fun __loc__start__pos ->
                      fun __loc__end__buf ->
                        fun __loc__end__pos ->
                          let _loc =
                            locate __loc__start__buf __loc__start__pos
                              __loc__end__buf __loc__end__pos in
                          fun ((s, l) as _default_0) ->
                            loc_pcf _loc (Pcf_extension (s, l))));
           Earley_core.Earley.fsequence inherit_kw
             (Earley_core.Earley.fsequence override_flag
                (Earley_core.Earley.fsequence class_expr
                   (Earley_core.Earley.fsequence
                      (Earley_core.Earley.option None
                         (Earley_core.Earley.apply (fun x -> Some x)
                            (Earley_core.Earley.fsequence_ignore as_kw
                               (Earley_core.Earley.fsequence_position lident
                                  (Earley_core.Earley.empty
                                     (fun str ->
                                        fun pos ->
                                          fun str' ->
                                            fun pos' ->
                                              fun id ->
                                                let _loc_id =
                                                  locate str pos str' pos' in
                                                id_loc id _loc_id))))))
                      (Earley_core.Earley.empty_pos
                         (fun __loc__start__buf ->
                            fun __loc__start__pos ->
                              fun __loc__end__buf ->
                                fun __loc__end__pos ->
                                  let _loc =
                                    locate __loc__start__buf
                                      __loc__start__pos __loc__end__buf
                                      __loc__end__pos in
                                  fun id ->
                                    fun ce ->
                                      fun o ->
                                        fun _default_0 ->
                                          loc_pcf _loc
                                            (Pcf_inherit (o, ce, id)))))));
           Earley_core.Earley.fsequence val_kw
             (Earley_core.Earley.fsequence override_flag
                (Earley_core.Earley.fsequence mutable_flag
                   (Earley_core.Earley.fsequence_position inst_var_name
                      (Earley_core.Earley.fsequence
                         (Earley_core.Earley.option None
                            (Earley_core.Earley.apply (fun x -> Some x)
                               (Earley_core.Earley.fsequence_ignore
                                  (Earley_core.Earley.char ':' ':')
                                  (Earley_core.Earley.fsequence typexpr
                                     (Earley_core.Earley.empty (fun t -> t))))))
                         (Earley_core.Earley.fsequence_ignore
                            (Earley_core.Earley.char '=' '=')
                            (Earley_core.Earley.fsequence expression
                               (Earley_core.Earley.empty_pos
                                  (fun __loc__start__buf ->
                                     fun __loc__start__pos ->
                                       fun __loc__end__buf ->
                                         fun __loc__end__pos ->
                                           let _loc =
                                             locate __loc__start__buf
                                               __loc__start__pos
                                               __loc__end__buf
                                               __loc__end__pos in
                                           fun e ->
                                             fun te ->
                                               fun str ->
                                                 fun pos ->
                                                   fun str' ->
                                                     fun pos' ->
                                                       fun ivn ->
                                                         let _loc_ivn =
                                                           locate str pos
                                                             str' pos' in
                                                         fun m ->
                                                           fun o ->
                                                             fun _default_0
                                                               ->
                                                               let ivn =
                                                                 id_loc ivn
                                                                   _loc_ivn in
                                                               let ex =
                                                                 match te
                                                                 with
                                                                 | None -> e
                                                                 | Some t ->
                                                                    loc_expr
                                                                    (ghost
                                                                    (merge2
                                                                    _loc_ivn
                                                                    _loc))
                                                                    (pexp_constraint
                                                                    (e, t)) in
                                                               loc_pcf _loc
                                                                 (Pcf_val
                                                                    (ivn, m,
                                                                    (Cfk_concrete
                                                                    (o, ex))))))))))));
           Earley_core.Earley.fsequence val_kw
             (Earley_core.Earley.fsequence mutable_flag
                (Earley_core.Earley.fsequence virtual_kw
                   (Earley_core.Earley.fsequence_position inst_var_name
                      (Earley_core.Earley.fsequence_ignore
                         (Earley_core.Earley.string ":" ":")
                         (Earley_core.Earley.fsequence typexpr
                            (Earley_core.Earley.empty_pos
                               (fun __loc__start__buf ->
                                  fun __loc__start__pos ->
                                    fun __loc__end__buf ->
                                      fun __loc__end__pos ->
                                        let _loc =
                                          locate __loc__start__buf
                                            __loc__start__pos __loc__end__buf
                                            __loc__end__pos in
                                        fun te ->
                                          fun str ->
                                            fun pos ->
                                              fun str' ->
                                                fun pos' ->
                                                  fun ivn ->
                                                    let _loc_ivn =
                                                      locate str pos str'
                                                        pos' in
                                                    fun _default_0 ->
                                                      fun m ->
                                                        fun _default_1 ->
                                                          let ivn =
                                                            id_loc ivn
                                                              _loc_ivn in
                                                          loc_pcf _loc
                                                            (Pcf_val
                                                               (ivn, m,
                                                                 (Cfk_virtual
                                                                    te))))))))));
           Earley_core.Earley.fsequence val_kw
             (Earley_core.Earley.fsequence virtual_kw
                (Earley_core.Earley.fsequence mutable_kw
                   (Earley_core.Earley.fsequence_position inst_var_name
                      (Earley_core.Earley.fsequence_ignore
                         (Earley_core.Earley.string ":" ":")
                         (Earley_core.Earley.fsequence typexpr
                            (Earley_core.Earley.empty_pos
                               (fun __loc__start__buf ->
                                  fun __loc__start__pos ->
                                    fun __loc__end__buf ->
                                      fun __loc__end__pos ->
                                        let _loc =
                                          locate __loc__start__buf
                                            __loc__start__pos __loc__end__buf
                                            __loc__end__pos in
                                        fun te ->
                                          fun str ->
                                            fun pos ->
                                              fun str' ->
                                                fun pos' ->
                                                  fun ivn ->
                                                    let _loc_ivn =
                                                      locate str pos str'
                                                        pos' in
                                                    fun _default_0 ->
                                                      fun _default_1 ->
                                                        fun _default_2 ->
                                                          let ivn =
                                                            id_loc ivn
                                                              _loc_ivn in
                                                          loc_pcf _loc
                                                            (Pcf_val
                                                               (ivn, Mutable,
                                                                 (Cfk_virtual
                                                                    te))))))))));
           Earley_core.Earley.fsequence method_kw
             (Earley_core.Earley.fsequence_position
                (Earley_core.Earley.fsequence override_flag
                   (Earley_core.Earley.fsequence private_flag
                      (Earley_core.Earley.fsequence method_name
                         (Earley_core.Earley.empty
                            (fun _default_0 ->
                               fun _default_1 ->
                                 fun _default_2 ->
                                   (_default_2, _default_1, _default_0))))))
                (Earley_core.Earley.fsequence_ignore
                   (Earley_core.Earley.string ":" ":")
                   (Earley_core.Earley.fsequence poly_typexpr
                      (Earley_core.Earley.fsequence_ignore
                         (Earley_core.Earley.char '=' '=')
                         (Earley_core.Earley.fsequence expression
                            (Earley_core.Earley.empty_pos
                               (fun __loc__start__buf ->
                                  fun __loc__start__pos ->
                                    fun __loc__end__buf ->
                                      fun __loc__end__pos ->
                                        let _loc =
                                          locate __loc__start__buf
                                            __loc__start__pos __loc__end__buf
                                            __loc__end__pos in
                                        fun e ->
                                          fun te ->
                                            fun str ->
                                              fun pos ->
                                                fun str' ->
                                                  fun pos' ->
                                                    fun t ->
                                                      let _loc_t =
                                                        locate str pos str'
                                                          pos' in
                                                      fun _default_0 ->
                                                        let (o, p, mn) = t in
                                                        let e =
                                                          loc_expr
                                                            (ghost
                                                               (merge2 _loc_t
                                                                  _loc))
                                                            (Pexp_poly
                                                               (e, (Some te))) in
                                                        loc_pcf _loc
                                                          (Pcf_method
                                                             (mn, p,
                                                               (Cfk_concrete
                                                                  (o, e)))))))))));
           Earley_core.Earley.fsequence method_kw
             (Earley_core.Earley.fsequence_position
                (Earley_core.Earley.fsequence override_flag
                   (Earley_core.Earley.fsequence private_flag
                      (Earley_core.Earley.fsequence method_name
                         (Earley_core.Earley.empty
                            (fun _default_0 ->
                               fun _default_1 ->
                                 fun _default_2 ->
                                   (_default_2, _default_1, _default_0))))))
                (Earley_core.Earley.fsequence_ignore
                   (Earley_core.Earley.string ":" ":")
                   (Earley_core.Earley.fsequence poly_syntax_typexpr
                      (Earley_core.Earley.fsequence_ignore
                         (Earley_core.Earley.char '=' '=')
                         (Earley_core.Earley.fsequence_position expression
                            (Earley_core.Earley.empty_pos
                               (fun __loc__start__buf ->
                                  fun __loc__start__pos ->
                                    fun __loc__end__buf ->
                                      fun __loc__end__pos ->
                                        let _loc =
                                          locate __loc__start__buf
                                            __loc__start__pos __loc__end__buf
                                            __loc__end__pos in
                                        fun str ->
                                          fun pos ->
                                            fun str' ->
                                              fun pos' ->
                                                fun e ->
                                                  let _loc_e =
                                                    locate str pos str' pos' in
                                                  fun
                                                    ((ids, te) as _default_0)
                                                    ->
                                                    fun str ->
                                                      fun pos ->
                                                        fun str' ->
                                                          fun pos' ->
                                                            fun t ->
                                                              let _loc_t =
                                                                locate str
                                                                  pos str'
                                                                  pos' in
                                                              fun _default_1
                                                                ->
                                                                let (o, p,
                                                                    mn)
                                                                  = t in
                                                                let _loc_e =
                                                                  merge2
                                                                    _loc_t
                                                                    _loc in
                                                                let (e, poly)
                                                                  =
                                                                  wrap_type_annotation
                                                                    _loc_e
                                                                    ids te e in
                                                                let e =
                                                                  loc_expr
                                                                    (
                                                                    ghost
                                                                    _loc_e)
                                                                    (
                                                                    Pexp_poly
                                                                    (e,
                                                                    (Some
                                                                    poly))) in
                                                                loc_pcf _loc
                                                                  (Pcf_method
                                                                    (mn, p,
                                                                    (Cfk_concrete
                                                                    (o, e)))))))))));
           Earley_core.Earley.fsequence method_kw
             (Earley_core.Earley.fsequence_position
                (Earley_core.Earley.fsequence override_flag
                   (Earley_core.Earley.fsequence private_flag
                      (Earley_core.Earley.fsequence method_name
                         (Earley_core.Earley.empty
                            (fun _default_0 ->
                               fun _default_1 ->
                                 fun _default_2 ->
                                   (_default_2, _default_1, _default_0))))))
                (Earley_core.Earley.fsequence
                   (Earley_core.Earley.apply (fun f -> f [])
                      (Earley_core.Earley.fixpoint' (fun l -> l)
                         (Earley_core.Earley.fsequence_position
                            (parameter true)
                            (Earley_core.Earley.empty
                               (fun str ->
                                  fun pos ->
                                    fun str' ->
                                      fun pos' ->
                                        fun p ->
                                          let _loc_p =
                                            locate str pos str' pos' in
                                          (p, _loc_p))))
                         (fun x -> fun f -> fun l -> f (x :: l))))
                   (Earley_core.Earley.fsequence_position
                      (Earley_core.Earley.option None
                         (Earley_core.Earley.apply (fun x -> Some x)
                            (Earley_core.Earley.fsequence_ignore
                               (Earley_core.Earley.string ":" ":")
                               (Earley_core.Earley.fsequence typexpr
                                  (Earley_core.Earley.empty (fun te -> te))))))
                      (Earley_core.Earley.fsequence_ignore
                         (Earley_core.Earley.char '=' '=')
                         (Earley_core.Earley.fsequence_position expression
                            (Earley_core.Earley.empty_pos
                               (fun __loc__start__buf ->
                                  fun __loc__start__pos ->
                                    fun __loc__end__buf ->
                                      fun __loc__end__pos ->
                                        let _loc =
                                          locate __loc__start__buf
                                            __loc__start__pos __loc__end__buf
                                            __loc__end__pos in
                                        fun str ->
                                          fun pos ->
                                            fun str' ->
                                              fun pos' ->
                                                fun e ->
                                                  let _loc_e =
                                                    locate str pos str' pos' in
                                                  fun str ->
                                                    fun pos ->
                                                      fun str' ->
                                                        fun pos' ->
                                                          fun te ->
                                                            let _loc_te =
                                                              locate str pos
                                                                str' pos' in
                                                            fun ps ->
                                                              fun str ->
                                                                fun pos ->
                                                                  fun str' ->
                                                                    fun pos'
                                                                    ->
                                                                    fun t ->
                                                                    let _loc_t
                                                                    =
                                                                    locate
                                                                    str pos
                                                                    str' pos' in
                                                                    fun
                                                                    _default_0
                                                                    ->
                                                                    let 
                                                                    (o, p,
                                                                    mn) = t in
                                                                    if
                                                                    (ps = [])
                                                                    &&
                                                                    (te <>
                                                                    None)
                                                                    then
                                                                    give_up
                                                                    ();
                                                                    (
                                                                    let e =
                                                                    match te
                                                                    with
                                                                    | 
                                                                    None -> e
                                                                    | 
                                                                    Some te
                                                                    ->
                                                                    loc_expr
                                                                    (ghost
                                                                    (merge2
                                                                    _loc_te
                                                                    _loc_e))
                                                                    (pexp_constraint
                                                                    (e, te)) in
                                                                    let e
                                                                    : expression
                                                                    =
                                                                    apply_params
                                                                    ~gh:true
                                                                    _loc_e ps
                                                                    e in
                                                                    let e =
                                                                    loc_expr
                                                                    (ghost
                                                                    (merge2
                                                                    _loc_t
                                                                    _loc_e))
                                                                    (Pexp_poly
                                                                    (e, None)) in
                                                                    loc_pcf
                                                                    _loc
                                                                    (Pcf_method
                                                                    (mn, p,
                                                                    (Cfk_concrete
                                                                    (o, e))))))))))));
           Earley_core.Earley.fsequence method_kw
             (Earley_core.Earley.fsequence private_flag
                (Earley_core.Earley.fsequence virtual_kw
                   (Earley_core.Earley.fsequence method_name
                      (Earley_core.Earley.fsequence_ignore
                         (Earley_core.Earley.string ":" ":")
                         (Earley_core.Earley.fsequence poly_typexpr
                            (Earley_core.Earley.empty_pos
                               (fun __loc__start__buf ->
                                  fun __loc__start__pos ->
                                    fun __loc__end__buf ->
                                      fun __loc__end__pos ->
                                        let _loc =
                                          locate __loc__start__buf
                                            __loc__start__pos __loc__end__buf
                                            __loc__end__pos in
                                        fun pte ->
                                          fun mn ->
                                            fun _default_0 ->
                                              fun p ->
                                                fun _default_1 ->
                                                  loc_pcf _loc
                                                    (Pcf_method
                                                       (mn, p,
                                                         (Cfk_virtual pte))))))))));
           Earley_core.Earley.fsequence method_kw
             (Earley_core.Earley.fsequence virtual_kw
                (Earley_core.Earley.fsequence private_kw
                   (Earley_core.Earley.fsequence method_name
                      (Earley_core.Earley.fsequence_ignore
                         (Earley_core.Earley.string ":" ":")
                         (Earley_core.Earley.fsequence poly_typexpr
                            (Earley_core.Earley.empty_pos
                               (fun __loc__start__buf ->
                                  fun __loc__start__pos ->
                                    fun __loc__end__buf ->
                                      fun __loc__end__pos ->
                                        let _loc =
                                          locate __loc__start__buf
                                            __loc__start__pos __loc__end__buf
                                            __loc__end__pos in
                                        fun pte ->
                                          fun mn ->
                                            fun _default_0 ->
                                              fun _default_1 ->
                                                fun _default_2 ->
                                                  loc_pcf _loc
                                                    (Pcf_method
                                                       (mn, Private,
                                                         (Cfk_virtual pte))))))))));
           Earley_core.Earley.fsequence constraint_kw
             (Earley_core.Earley.fsequence typexpr
                (Earley_core.Earley.fsequence_ignore
                   (Earley_core.Earley.char '=' '=')
                   (Earley_core.Earley.fsequence typexpr
                      (Earley_core.Earley.empty_pos
                         (fun __loc__start__buf ->
                            fun __loc__start__pos ->
                              fun __loc__end__buf ->
                                fun __loc__end__pos ->
                                  let _loc =
                                    locate __loc__start__buf
                                      __loc__start__pos __loc__end__buf
                                      __loc__end__pos in
                                  fun te' ->
                                    fun te ->
                                      fun _default_0 ->
                                        loc_pcf _loc
                                          (Pcf_constraint (te, te')))))));
           Earley_core.Earley.fsequence initializer_kw
             (Earley_core.Earley.fsequence expression
                (Earley_core.Earley.empty_pos
                   (fun __loc__start__buf ->
                      fun __loc__start__pos ->
                        fun __loc__end__buf ->
                          fun __loc__end__pos ->
                            let _loc =
                              locate __loc__start__buf __loc__start__pos
                                __loc__end__buf __loc__end__pos in
                            fun e ->
                              fun _default_0 ->
                                loc_pcf _loc (Pcf_initializer e))));
           Earley_core.Earley.fsequence floating_attribute
             (Earley_core.Earley.empty_pos
                (fun __loc__start__buf ->
                   fun __loc__start__pos ->
                     fun __loc__end__buf ->
                       fun __loc__end__pos ->
                         let _loc =
                           locate __loc__start__buf __loc__start__pos
                             __loc__end__buf __loc__end__pos in
                         fun ((s, l) as _default_0) ->
                           loc_pcf _loc (Pcf_attribute (s, l))))])
    let _ =
      set_grammar class_body
        (Earley_core.Earley.fsequence_position
           (Earley_core.Earley.option None
              (Earley_core.Earley.apply (fun x -> Some x) pattern))
           (Earley_core.Earley.fsequence
              (Earley_core.Earley.apply (fun f -> f [])
                 (Earley_core.Earley.fixpoint' (fun l -> l) class_field
                    (fun x -> fun f -> fun l -> f (x :: l))))
              (Earley_core.Earley.empty
                 (fun f ->
                    fun str ->
                      fun pos ->
                        fun str' ->
                          fun pos' ->
                            fun p ->
                              let _loc_p = locate str pos str' pos' in
                              let p =
                                match p with
                                | None -> loc_pat (ghost _loc_p) Ppat_any
                                | Some p -> p in
                              { pcstr_self = p; pcstr_fields = f }))))
    let class_binding = Earley_core.Earley.declare_grammar "class_binding"
    let _ =
      Earley_core.Earley.set_grammar class_binding
        (Earley_core.Earley.fsequence virtual_flag
           (Earley_core.Earley.fsequence
              (Earley_core.Earley.option []
                 (Earley_core.Earley.fsequence_ignore
                    (Earley_core.Earley.string "[" "[")
                    (Earley_core.Earley.fsequence type_parameters
                       (Earley_core.Earley.fsequence_ignore
                          (Earley_core.Earley.string "]" "]")
                          (Earley_core.Earley.empty (fun params -> params))))))
              (Earley_core.Earley.fsequence_position class_name
                 (Earley_core.Earley.fsequence_position
                    (Earley_core.Earley.apply (fun f -> f [])
                       (Earley_core.Earley.fixpoint' (fun l -> l)
                          (Earley_core.Earley.fsequence (parameter false)
                             (Earley_core.Earley.empty_pos
                                (fun __loc__start__buf ->
                                   fun __loc__start__pos ->
                                     fun __loc__end__buf ->
                                       fun __loc__end__pos ->
                                         let _loc =
                                           locate __loc__start__buf
                                             __loc__start__pos
                                             __loc__end__buf __loc__end__pos in
                                         fun p -> (p, _loc))))
                          (fun x -> fun f -> fun l -> f (x :: l))))
                    (Earley_core.Earley.fsequence
                       (Earley_core.Earley.option None
                          (Earley_core.Earley.apply (fun x -> Some x)
                             (Earley_core.Earley.fsequence_ignore
                                (Earley_core.Earley.string ":" ":")
                                (Earley_core.Earley.fsequence class_type
                                   (Earley_core.Earley.empty (fun ct -> ct))))))
                       (Earley_core.Earley.fsequence_ignore
                          (Earley_core.Earley.char '=' '=')
                          (Earley_core.Earley.fsequence class_expr
                             (Earley_core.Earley.empty_pos
                                (fun __loc__start__buf ->
                                   fun __loc__start__pos ->
                                     fun __loc__end__buf ->
                                       fun __loc__end__pos ->
                                         let _loc =
                                           locate __loc__start__buf
                                             __loc__start__pos
                                             __loc__end__buf __loc__end__pos in
                                         fun ce ->
                                           fun ct ->
                                             fun str ->
                                               fun pos ->
                                                 fun str' ->
                                                   fun pos' ->
                                                     fun ps ->
                                                       let _loc_ps =
                                                         locate str pos str'
                                                           pos' in
                                                       fun str ->
                                                         fun pos ->
                                                           fun str' ->
                                                             fun pos' ->
                                                               fun cn ->
                                                                 let _loc_cn
                                                                   =
                                                                   locate str
                                                                    pos str'
                                                                    pos' in
                                                                 fun params
                                                                   ->
                                                                   fun v ->
                                                                    let ce =
                                                                    apply_params_cls
                                                                    ~gh:true
                                                                    (merge2
                                                                    _loc_ps
                                                                    _loc) ps
                                                                    ce in
                                                                    let ce =
                                                                    match ct
                                                                    with
                                                                    | 
                                                                    None ->
                                                                    ce
                                                                    | 
                                                                    Some ct
                                                                    ->
                                                                    loc_pcl
                                                                    _loc
                                                                    (Pcl_constraint
                                                                    (ce, ct)) in
                                                                    fun _loc
                                                                    ->
                                                                    class_type_declaration
                                                                    ~attributes:(
                                                                    attach_attrib
                                                                    _loc [])
                                                                    _loc
                                                                    (id_loc
                                                                    cn
                                                                    _loc_cn)
                                                                    params v
                                                                    ce)))))))))
    let class_definition =
      Earley_core.Earley.declare_grammar "class_definition"
    let _ =
      Earley_core.Earley.set_grammar class_definition
        (Earley_core.Earley.fsequence_position class_kw
           (Earley_core.Earley.fsequence_position class_binding
              (Earley_core.Earley.fsequence
                 (Earley_core.Earley.apply (fun f -> f [])
                    (Earley_core.Earley.fixpoint' (fun l -> l)
                       (Earley_core.Earley.fsequence_ignore and_kw
                          (Earley_core.Earley.fsequence class_binding
                             (Earley_core.Earley.empty_pos
                                (fun __loc__start__buf ->
                                   fun __loc__start__pos ->
                                     fun __loc__end__buf ->
                                       fun __loc__end__pos ->
                                         let _loc =
                                           locate __loc__start__buf
                                             __loc__start__pos
                                             __loc__end__buf __loc__end__pos in
                                         fun cb -> cb _loc))))
                       (fun x -> fun f -> fun l -> f (x :: l))))
                 (Earley_core.Earley.empty
                    (fun cbs ->
                       fun str ->
                         fun pos ->
                           fun str' ->
                             fun pos' ->
                               fun cb ->
                                 let _loc_cb = locate str pos str' pos' in
                                 fun str ->
                                   fun pos ->
                                     fun str' ->
                                       fun pos' ->
                                         fun k ->
                                           let _loc_k =
                                             locate str pos str' pos' in
                                           (cb (merge2 _loc_k _loc_cb)) ::
                                             cbs)))))
    let pexp_list _loc ?loc_cl  l =
      if l = []
      then loc_expr _loc (pexp_construct ((id_loc (Lident "[]") _loc), None))
      else
        (let loc_cl =
           ghost (match loc_cl with | None -> _loc | Some pos -> pos) in
         List.fold_right
           (fun (x, pos) ->
              fun acc ->
                let _loc = ghost (merge2 pos loc_cl) in
                loc_expr _loc
                  (pexp_construct
                     ((id_loc (Lident "::") (ghost _loc)),
                       (Some (loc_expr _loc (Pexp_tuple [x; acc])))))) l
           (loc_expr loc_cl
              (pexp_construct ((id_loc (Lident "[]") loc_cl), None))))
    let apply_lbl _loc (lbl, e) =
      let e =
        match e with
        | None -> loc_expr _loc (Pexp_ident (id_loc (Lident lbl) _loc))
        | Some e -> e in
      (lbl, e)
    let rec mk_seq loc_c final =
      function
      | [] -> final
      | x::l ->
          let res = mk_seq loc_c final l in
          loc_expr (merge2 x.pexp_loc loc_c) (Pexp_sequence (x, res))
    let (extra_expressions_grammar, extra_expressions_grammar__set__grammar)
      = Earley_core.Earley.grammar_family "extra_expressions_grammar"
    let _ =
      extra_expressions_grammar__set__grammar
        (fun c -> alternatives (List.map (fun g -> g c) extra_expressions))
    let structure_item_simple = declare_grammar "structure_item_simple"
    let (left_expr, left_expr__set__grammar) =
      Earley_core.Earley.grammar_prio "left_expr"
    let (prefix_expr, prefix_expr__set__grammar) =
      Earley_core.Earley.grammar_family "prefix_expr"
    let rec infix_expr lvl =
      if (assoc lvl) = Left
      then
        Earley_core.Earley.fsequence (expression_lvl (NoMatch, lvl))
          (Earley_core.Earley.fsequence_position (infix_symbol lvl)
             (Earley_core.Earley.empty
                (fun str ->
                   fun pos ->
                     fun str' ->
                       fun pos' ->
                         fun op ->
                           let _loc_op = locate str pos str' pos' in
                           fun e' ->
                             ((next_exp lvl), false,
                               (fun e ->
                                  fun (_l, _) ->
                                    mk_binary_op _l e' op _loc_op e)))))
      else
        if (assoc lvl) = NoAssoc
        then
          Earley_core.Earley.fsequence
            (expression_lvl (NoMatch, (next_exp lvl)))
            (Earley_core.Earley.fsequence_position (infix_symbol lvl)
               (Earley_core.Earley.empty
                  (fun str ->
                     fun pos ->
                       fun str' ->
                         fun pos' ->
                           fun op ->
                             let _loc_op = locate str pos str' pos' in
                             fun e' ->
                               ((next_exp lvl), false,
                                 (fun e ->
                                    fun (_l, _) ->
                                      mk_binary_op _l e' op _loc_op e)))))
        else
          Earley_core.Earley.fsequence
            (Earley_core.Earley.apply (fun f -> f [])
               (Earley_core.Earley.fixpoint1' (fun l -> l)
                  (Earley_core.Earley.fsequence
                     (expression_lvl (NoMatch, (next_exp lvl)))
                     (Earley_core.Earley.fsequence_position
                        (infix_symbol lvl)
                        (Earley_core.Earley.empty_pos
                           (fun __loc__start__buf ->
                              fun __loc__start__pos ->
                                fun __loc__end__buf ->
                                  fun __loc__end__pos ->
                                    let _loc =
                                      locate __loc__start__buf
                                        __loc__start__pos __loc__end__buf
                                        __loc__end__pos in
                                    fun str ->
                                      fun pos ->
                                        fun str' ->
                                          fun pos' ->
                                            fun op ->
                                              let _loc_op =
                                                locate str pos str' pos' in
                                              fun e' ->
                                                (_loc, e', op, _loc_op)))))
                  (fun x -> fun f -> fun l -> f (x :: l))))
            (Earley_core.Earley.empty
               (fun ls ->
                  ((next_exp lvl), false,
                    (fun e ->
                       fun (_l, _) ->
                         List.fold_right
                           (fun (_loc_e, e', op, _loc_op) ->
                              fun acc ->
                                mk_binary_op (merge2 _loc_e _l) e' op _loc_op
                                  acc) ls e))))
    let _ =
      left_expr__set__grammar
        ([(((fun (alm, lvl) -> lvl <= Pow)), (infix_expr Pow));
         (((fun (alm, lvl) -> (allow_let alm) && (lvl < App))),
           (Earley_core.Earley.fsequence fun_kw
              (Earley_core.Earley.fsequence
                 (Earley_core.Earley.apply (fun f -> f [])
                    (Earley_core.Earley.fixpoint' (fun l -> l)
                       (Earley_core.Earley.fsequence_position
                          (parameter true)
                          (Earley_core.Earley.empty
                             (fun str ->
                                fun pos ->
                                  fun str' ->
                                    fun pos' ->
                                      fun lbl ->
                                        let _loc_lbl =
                                          locate str pos str' pos' in
                                        (lbl, _loc_lbl))))
                       (fun x -> fun f -> fun l -> f (x :: l))))
                 (Earley_core.Earley.fsequence arrow_re
                    (Earley_core.Earley.empty_pos
                       (fun __loc__start__buf ->
                          fun __loc__start__pos ->
                            fun __loc__end__buf ->
                              fun __loc__end__pos ->
                                let _loc =
                                  locate __loc__start__buf __loc__start__pos
                                    __loc__end__buf __loc__end__pos in
                                fun _default_0 ->
                                  fun l ->
                                    fun _default_1 ->
                                      (Seq, false,
                                        (fun e ->
                                           fun (_loc, _) ->
                                             loc_expr _loc
                                               (apply_params _loc l e).pexp_desc))))))));
         (((fun (alm, lvl) -> (allow_let alm) && (lvl < App))),
           (Earley_core.Earley.fsequence_ignore let_kw
              (Earley_core.Earley.fsequence
                 (Earley_core.Earley.alternatives
                    [Earley_core.Earley.fsequence open_kw
                       (Earley_core.Earley.fsequence override_flag
                          (Earley_core.Earley.fsequence_position module_path
                             (Earley_core.Earley.empty
                                (fun str ->
                                   fun pos ->
                                     fun str' ->
                                       fun pos' ->
                                         fun mp ->
                                           let _loc_mp =
                                             locate str pos str' pos' in
                                           fun o ->
                                             fun _default_0 ->
                                               fun e ->
                                                 fun (_l, _) ->
                                                   let mp = id_loc mp _loc_mp in
                                                   loc_expr _l
                                                     (Pexp_open (o, mp, e))))));
                    Earley_core.Earley.fsequence rec_flag
                      (Earley_core.Earley.fsequence let_binding
                         (Earley_core.Earley.empty
                            (fun l ->
                               fun r ->
                                 fun e ->
                                   fun (_l, _) ->
                                     loc_expr _l (Pexp_let (r, l, e)))));
                    Earley_core.Earley.fsequence module_kw
                      (Earley_core.Earley.fsequence module_name
                         (Earley_core.Earley.fsequence
                            (Earley_core.Earley.apply (fun f -> f [])
                               (Earley_core.Earley.fixpoint' (fun l -> l)
                                  (Earley_core.Earley.fsequence_ignore
                                     (Earley_core.Earley.char '(' '(')
                                     (Earley_core.Earley.fsequence
                                        module_name
                                        (Earley_core.Earley.fsequence
                                           (Earley_core.Earley.option None
                                              (Earley_core.Earley.apply
                                                 (fun x -> Some x)
                                                 (Earley_core.Earley.fsequence_ignore
                                                    (Earley_core.Earley.char
                                                       ':' ':')
                                                    (Earley_core.Earley.fsequence
                                                       module_type
                                                       (Earley_core.Earley.empty
                                                          (fun _default_0 ->
                                                             _default_0))))))
                                           (Earley_core.Earley.fsequence_ignore
                                              (Earley_core.Earley.char ')'
                                                 ')')
                                              (Earley_core.Earley.empty_pos
                                                 (fun __loc__start__buf ->
                                                    fun __loc__start__pos ->
                                                      fun __loc__end__buf ->
                                                        fun __loc__end__pos
                                                          ->
                                                          let _loc =
                                                            locate
                                                              __loc__start__buf
                                                              __loc__start__pos
                                                              __loc__end__buf
                                                              __loc__end__pos in
                                                          fun mt ->
                                                            fun mn ->
                                                              (mn, mt, _loc)))))))
                                  (fun x -> fun f -> fun l -> f (x :: l))))
                            (Earley_core.Earley.fsequence_position
                               (Earley_core.Earley.option None
                                  (Earley_core.Earley.apply (fun x -> Some x)
                                     (Earley_core.Earley.fsequence_ignore
                                        (Earley_core.Earley.string ":" ":")
                                        (Earley_core.Earley.fsequence
                                           module_type
                                           (Earley_core.Earley.empty
                                              (fun mt -> mt))))))
                               (Earley_core.Earley.fsequence_ignore
                                  (Earley_core.Earley.string "=" "=")
                                  (Earley_core.Earley.fsequence_position
                                     module_expr
                                     (Earley_core.Earley.empty
                                        (fun str ->
                                           fun pos ->
                                             fun str' ->
                                               fun pos' ->
                                                 fun me ->
                                                   let _loc_me =
                                                     locate str pos str' pos' in
                                                   fun str ->
                                                     fun pos ->
                                                       fun str' ->
                                                         fun pos' ->
                                                           fun mt ->
                                                             let _loc_mt =
                                                               locate str pos
                                                                 str' pos' in
                                                             fun l ->
                                                               fun mn ->
                                                                 fun
                                                                   _default_0
                                                                   ->
                                                                   fun e ->
                                                                    fun
                                                                    (_l, _)
                                                                    ->
                                                                    let me =
                                                                    match mt
                                                                    with
                                                                    | 
                                                                    None ->
                                                                    me
                                                                    | 
                                                                    Some mt
                                                                    ->
                                                                    mexpr_loc
                                                                    (merge2
                                                                    _loc_mt
                                                                    _loc_me)
                                                                    (Pmod_constraint
                                                                    (me, mt)) in
                                                                    let me =
                                                                    List.fold_left
                                                                    (fun acc
                                                                    ->
                                                                    fun
                                                                    (mn, mt,
                                                                    _l) ->
                                                                    mexpr_loc
                                                                    (merge2
                                                                    _l
                                                                    _loc_me)
                                                                    (Pmod_functor
                                                                    (mn, mt,
                                                                    acc))) me
                                                                    (List.rev
                                                                    l) in
                                                                    loc_expr
                                                                    _l
                                                                    (Pexp_letmodule
                                                                    (mn, me,
                                                                    e)))))))))])
                 (Earley_core.Earley.fsequence_ignore in_kw
                    (Earley_core.Earley.empty (fun f -> (Seq, false, f)))))));
         (((fun (alm, lvl) -> ((allow_let alm) || (lvl = If)) && (lvl < App))),
           (Earley_core.Earley.fsequence if_kw
              (Earley_core.Earley.fsequence expression
                 (Earley_core.Earley.fsequence then_kw
                    (Earley_core.Earley.fsequence
                       (expression_lvl (Match, (next_exp Seq)))
                       (Earley_core.Earley.fsequence else_kw
                          (Earley_core.Earley.empty_pos
                             (fun __loc__start__buf ->
                                fun __loc__start__pos ->
                                  fun __loc__end__buf ->
                                    fun __loc__end__pos ->
                                      let _loc =
                                        locate __loc__start__buf
                                          __loc__start__pos __loc__end__buf
                                          __loc__end__pos in
                                      fun _default_0 ->
                                        fun e ->
                                          fun _default_1 ->
                                            fun c ->
                                              fun _default_2 ->
                                                ((next_exp Seq), false,
                                                  (fun e' ->
                                                     fun (_loc, _) ->
                                                       {
                                                         Parsetree.pexp_desc
                                                           =
                                                           (Parsetree.Pexp_ifthenelse
                                                              (c, e,
                                                                (Some e')));
                                                         Parsetree.pexp_loc =
                                                           _loc;
                                                         Parsetree.pexp_attributes
                                                           = []
                                                       }))))))))));
         (((fun (alm, lvl) -> ((allow_let alm) || (lvl = If)) && (lvl < App))),
           (Earley_core.Earley.fsequence if_kw
              (Earley_core.Earley.fsequence expression
                 (Earley_core.Earley.fsequence then_kw
                    (Earley_core.Earley.empty_pos
                       (fun __loc__start__buf ->
                          fun __loc__start__pos ->
                            fun __loc__end__buf ->
                              fun __loc__end__pos ->
                                let _loc =
                                  locate __loc__start__buf __loc__start__pos
                                    __loc__end__buf __loc__end__pos in
                                fun _default_0 ->
                                  fun c ->
                                    fun _default_1 ->
                                      ((next_exp Seq), true,
                                        (fun e ->
                                           fun (_loc, _) ->
                                             {
                                               Parsetree.pexp_desc =
                                                 (Parsetree.Pexp_ifthenelse
                                                    (c, e, None));
                                               Parsetree.pexp_loc = _loc;
                                               Parsetree.pexp_attributes = []
                                             }))))))));
         (((fun (alm, lvl) -> lvl <= Seq)),
           (Earley_core.Earley.fsequence
              (Earley_core.Earley.apply (fun f -> f [])
                 (Earley_core.Earley.fixpoint1' (fun l -> l)
                    (Earley_core.Earley.fsequence
                       (expression_lvl (NoMatch, (next_exp Seq)))
                       (Earley_core.Earley.fsequence_ignore semi_col
                          (Earley_core.Earley.empty
                             (fun _default_0 -> _default_0))))
                    (fun x -> fun f -> fun l -> f (x :: l))))
              (Earley_core.Earley.empty
                 (fun ls ->
                    ((next_exp Seq), false,
                      (fun e' -> fun (_, _l) -> mk_seq _l e' ls))))));
         (((fun (alm, lvl) -> lvl <= Aff)),
           (Earley_core.Earley.fsequence_position inst_var_name
              (Earley_core.Earley.fsequence_ignore
                 (Earley_core.Earley.string "<-" "<-")
                 (Earley_core.Earley.empty
                    (fun str ->
                       fun pos ->
                         fun str' ->
                           fun pos' ->
                             fun v ->
                               let _loc_v = locate str pos str' pos' in
                               ((next_exp Aff), false,
                                 (fun e ->
                                    fun (_l, _) ->
                                      loc_expr _l
                                        (Pexp_setinstvar
                                           ((id_loc v _loc_v), e)))))))));
         (((fun (alm, lvl) -> lvl <= Aff)),
           (Earley_core.Earley.fsequence (expression_lvl (NoMatch, Dot))
              (Earley_core.Earley.fsequence_ignore
                 (Earley_core.Earley.char '.' '.')
                 (Earley_core.Earley.fsequence
                    (Earley_core.Earley.alternatives
                       [Earley_core.Earley.fsequence_position field
                          (Earley_core.Earley.empty
                             (fun str ->
                                fun pos ->
                                  fun str' ->
                                    fun pos' ->
                                      fun f ->
                                        let _loc_f = locate str pos str' pos' in
                                        fun e' ->
                                          fun e ->
                                            fun (_l, _) ->
                                              let f = id_loc f _loc_f in
                                              loc_expr _l
                                                (Pexp_setfield (e', f, e))));
                       Earley_core.Earley.fsequence_ignore
                         (Earley_core.Earley.string "(" "(")
                         (Earley_core.Earley.fsequence expression
                            (Earley_core.Earley.fsequence_ignore
                               (Earley_core.Earley.string ")" ")")
                               (Earley_core.Earley.empty
                                  (fun f ->
                                     fun e' ->
                                       fun e ->
                                         fun (_l, _) ->
                                           exp_apply _l
                                             (array_function
                                                (ghost
                                                   (merge2 e'.pexp_loc _l))
                                                "Array" "set") [e'; f; e]))));
                       Earley_core.Earley.fsequence_ignore
                         (Earley_core.Earley.string "[" "[")
                         (Earley_core.Earley.fsequence expression
                            (Earley_core.Earley.fsequence_ignore
                               (Earley_core.Earley.string "]" "]")
                               (Earley_core.Earley.empty
                                  (fun f ->
                                     fun e' ->
                                       fun e ->
                                         fun (_l, _) ->
                                           exp_apply _l
                                             (array_function
                                                (ghost
                                                   (merge2 e'.pexp_loc _l))
                                                "String" "set") [e'; f; e]))));
                       Earley_core.Earley.fsequence_ignore
                         (Earley_core.Earley.string "{" "{")
                         (Earley_core.Earley.fsequence expression
                            (Earley_core.Earley.fsequence_ignore
                               (Earley_core.Earley.string "}" "}")
                               (Earley_core.Earley.empty
                                  (fun f ->
                                     fun e' ->
                                       fun e ->
                                         fun (_l, _) ->
                                           de_ghost
                                             (bigarray_set
                                                (ghost
                                                   (merge2 e'.pexp_loc _l))
                                                e' f e)))))])
                    (Earley_core.Earley.fsequence_ignore
                       (Earley_core.Earley.string "<-" "<-")
                       (Earley_core.Earley.empty
                          (fun f -> fun e' -> ((next_exp Aff), false, (f e')))))))));
         (((fun (alm, lvl) -> lvl <= Tupl)),
           (Earley_core.Earley.fsequence
              (Earley_core.Earley.apply (fun f -> f [])
                 (Earley_core.Earley.fixpoint1' (fun l -> l)
                    (Earley_core.Earley.fsequence
                       (expression_lvl (NoMatch, (next_exp Tupl)))
                       (Earley_core.Earley.fsequence_ignore
                          (Earley_core.Earley.char ',' ',')
                          (Earley_core.Earley.empty
                             (fun _default_0 -> _default_0))))
                    (fun x -> fun f -> fun l -> f (x :: l))))
              (Earley_core.Earley.empty
                 (fun l ->
                    ((next_exp Tupl), false,
                      (fun e' -> fun (_l, _) -> Exp.tuple ~loc:_l (l @ [e'])))))));
         (((fun (alm, lvl) -> lvl <= App)),
           (Earley_core.Earley.fsequence assert_kw
              (Earley_core.Earley.empty
                 (fun _default_0 ->
                    ((next_exp App), false,
                      (fun e -> fun (_l, _) -> loc_expr _l (Pexp_assert e)))))));
         (((fun (alm, lvl) -> lvl <= App)),
           (Earley_core.Earley.fsequence lazy_kw
              (Earley_core.Earley.empty
                 (fun _default_0 ->
                    ((next_exp App), false,
                      (fun e -> fun (_l, _) -> loc_expr _l (Pexp_lazy e)))))));
         (((fun (alm, lvl) -> lvl <= Opp)), (prefix_expr Opp));
         (((fun (alm, lvl) -> lvl <= Prefix)), (prefix_expr Prefix));
         (((fun (alm, lvl) -> lvl <= Prod)), (infix_expr Prod));
         (((fun (alm, lvl) -> lvl <= Sum)), (infix_expr Sum));
         (((fun (alm, lvl) -> lvl <= Append)), (infix_expr Append));
         (((fun (alm, lvl) -> lvl <= Cons)), (infix_expr Cons));
         (((fun (alm, lvl) -> lvl <= Aff)), (infix_expr Aff));
         (((fun (alm, lvl) -> lvl <= Eq)), (infix_expr Eq));
         (((fun (alm, lvl) -> lvl <= Conj)), (infix_expr Conj));
         (((fun (alm, lvl) -> lvl <= Disj)), (infix_expr Disj))],
          (fun (alm, lvl) -> []))
    let _ =
      prefix_expr__set__grammar
        (fun lvl ->
           Earley_core.Earley.fsequence_position (prefix_symbol lvl)
             (Earley_core.Earley.empty
                (fun str ->
                   fun pos ->
                     fun str' ->
                       fun pos' ->
                         fun p ->
                           let _loc_p = locate str pos str' pos' in
                           (lvl, false,
                             (fun e ->
                                fun (_l, _) -> mk_unary_op _l p _loc_p e)))))
    let prefix_expression =
      Earley_core.Earley.declare_grammar "prefix_expression"
    let _ =
      Earley_core.Earley.set_grammar prefix_expression
        (Earley_core.Earley.alternatives
           [alternatives extra_prefix_expressions;
           Earley_core.Earley.fsequence function_kw
             (Earley_core.Earley.fsequence match_cases
                (Earley_core.Earley.empty_pos
                   (fun __loc__start__buf ->
                      fun __loc__start__pos ->
                        fun __loc__end__buf ->
                          fun __loc__end__pos ->
                            let _loc =
                              locate __loc__start__buf __loc__start__pos
                                __loc__end__buf __loc__end__pos in
                            fun l ->
                              fun _default_0 ->
                                {
                                  Parsetree.pexp_desc =
                                    (Parsetree.Pexp_function l);
                                  Parsetree.pexp_loc = _loc;
                                  Parsetree.pexp_attributes = []
                                })));
           Earley_core.Earley.fsequence match_kw
             (Earley_core.Earley.fsequence expression
                (Earley_core.Earley.fsequence with_kw
                   (Earley_core.Earley.fsequence match_cases
                      (Earley_core.Earley.empty_pos
                         (fun __loc__start__buf ->
                            fun __loc__start__pos ->
                              fun __loc__end__buf ->
                                fun __loc__end__pos ->
                                  let _loc =
                                    locate __loc__start__buf
                                      __loc__start__pos __loc__end__buf
                                      __loc__end__pos in
                                  fun l ->
                                    fun _default_0 ->
                                      fun e ->
                                        fun _default_1 ->
                                          {
                                            Parsetree.pexp_desc =
                                              (Parsetree.Pexp_match (e, l));
                                            Parsetree.pexp_loc = _loc;
                                            Parsetree.pexp_attributes = []
                                          })))));
           Earley_core.Earley.fsequence try_kw
             (Earley_core.Earley.fsequence expression
                (Earley_core.Earley.fsequence with_kw
                   (Earley_core.Earley.fsequence match_cases
                      (Earley_core.Earley.empty_pos
                         (fun __loc__start__buf ->
                            fun __loc__start__pos ->
                              fun __loc__end__buf ->
                                fun __loc__end__pos ->
                                  let _loc =
                                    locate __loc__start__buf
                                      __loc__start__pos __loc__end__buf
                                      __loc__end__pos in
                                  fun l ->
                                    fun _default_0 ->
                                      fun e ->
                                        fun _default_1 ->
                                          {
                                            Parsetree.pexp_desc =
                                              (Parsetree.Pexp_try (e, l));
                                            Parsetree.pexp_loc = _loc;
                                            Parsetree.pexp_attributes = []
                                          })))))])
    let (right_expression, right_expression__set__grammar) =
      Earley_core.Earley.grammar_prio "right_expression"
    let _ =
      right_expression__set__grammar
        ([(((fun lvl -> lvl <= Atom)),
            (Earley_core.Earley.fsequence_ignore
               (Earley_core.Earley.char '$' '$')
               (Earley_core.Earley.fsequence_ignore
                  (Earley_core.Earley.no_blank_test ())
                  (Earley_core.Earley.fsequence
                     (Earley_core.Earley.option "expr"
                        (Earley_core.Earley.fsequence
                           (Earley_str.regexp ~name:"[a-z]+" "[a-z]+"
                              (fun groupe -> groupe 0))
                           (Earley_core.Earley.fsequence_ignore
                              (Earley_core.Earley.no_blank_test ())
                              (Earley_core.Earley.fsequence_ignore
                                 (Earley_core.Earley.char ':' ':')
                                 (Earley_core.Earley.empty
                                    (fun _default_0 -> _default_0))))))
                     (Earley_core.Earley.fsequence expression
                        (Earley_core.Earley.fsequence_ignore
                           (Earley_core.Earley.no_blank_test ())
                           (Earley_core.Earley.fsequence_ignore
                              (Earley_core.Earley.char '$' '$')
                              (Earley_core.Earley.empty_pos
                                 (fun __loc__start__buf ->
                                    fun __loc__start__pos ->
                                      fun __loc__end__buf ->
                                        fun __loc__end__pos ->
                                          let _loc =
                                            locate __loc__start__buf
                                              __loc__start__pos
                                              __loc__end__buf __loc__end__pos in
                                          fun e ->
                                            fun aq ->
                                              let f =
                                                let open Quote in
                                                  let e_loc =
                                                    exp_ident _loc "_loc" in
                                                  let locate _loc e =
                                                    quote_record e_loc _loc
                                                      [((parsetree
                                                           "pexp_desc"), e);
                                                      ((parsetree "pexp_loc"),
                                                        (quote_location_t
                                                           e_loc _loc _loc));
                                                      ((parsetree
                                                          "pexp_attributes"),
                                                        (quote_attributes
                                                           e_loc _loc []))] in
                                                  let generic_antiquote e =
                                                    function
                                                    | Quote_pexp -> e
                                                    | _ ->
                                                        failwith
                                                          "Bad antiquotation..." in
                                                  let quote_loc _loc e =
                                                    quote_record e_loc _loc
                                                      [((Ldot
                                                           ((Lident
                                                               "Asttypes"),
                                                             "txt")), e);
                                                      ((Ldot
                                                          ((Lident "Asttypes"),
                                                            "loc")),
                                                        (quote_location_t
                                                           e_loc _loc _loc))] in
                                                  match aq with
                                                  | "expr" ->
                                                      generic_antiquote e
                                                  | "longident" ->
                                                      let e =
                                                        quote_const e_loc
                                                          _loc
                                                          (parsetree
                                                             "Pexp_ident")
                                                          [quote_loc _loc e] in
                                                      generic_antiquote
                                                        (locate _loc e)
                                                  | "bool" ->
                                                      generic_antiquote
                                                        (quote_apply e_loc
                                                           _loc
                                                           (pa_ast "exp_bool")
                                                           [quote_location_t
                                                              e_loc _loc _loc;
                                                           e])
                                                  | "int" ->
                                                      generic_antiquote
                                                        (quote_apply e_loc
                                                           _loc
                                                           (pa_ast "exp_int")
                                                           [quote_location_t
                                                              e_loc _loc _loc;
                                                           e])
                                                  | "float" ->
                                                      generic_antiquote
                                                        (quote_apply e_loc
                                                           _loc
                                                           (pa_ast
                                                              "exp_float")
                                                           [quote_location_t
                                                              e_loc _loc _loc;
                                                           e])
                                                  | "string" ->
                                                      generic_antiquote
                                                        (quote_apply e_loc
                                                           _loc
                                                           (pa_ast
                                                              "exp_string")
                                                           [quote_location_t
                                                              e_loc _loc _loc;
                                                           e])
                                                  | "char" ->
                                                      generic_antiquote
                                                        (quote_apply e_loc
                                                           _loc
                                                           (pa_ast "exp_char")
                                                           [quote_location_t
                                                              e_loc _loc _loc;
                                                           e])
                                                  | "list" ->
                                                      generic_antiquote
                                                        (quote_apply e_loc
                                                           _loc
                                                           (pa_ast "exp_list")
                                                           [quote_location_t
                                                              e_loc _loc _loc;
                                                           e])
                                                  | "tuple" ->
                                                      generic_antiquote
                                                        (quote_apply e_loc
                                                           _loc
                                                           (pa_ast
                                                              "exp_tuple")
                                                           [quote_location_t
                                                              e_loc _loc _loc;
                                                           e])
                                                  | "array" ->
                                                      generic_antiquote
                                                        (quote_apply e_loc
                                                           _loc
                                                           (pa_ast
                                                              "exp_array")
                                                           [quote_location_t
                                                              e_loc _loc _loc;
                                                           e])
                                                  | _ -> give_up () in
                                              Quote.pexp_antiquotation _loc f)))))))));
         (((fun lvl -> lvl <= Atom)),
           (Earley_core.Earley.fsequence_position value_path
              (Earley_core.Earley.empty_pos
                 (fun __loc__start__buf ->
                    fun __loc__start__pos ->
                      fun __loc__end__buf ->
                        fun __loc__end__pos ->
                          let _loc =
                            locate __loc__start__buf __loc__start__pos
                              __loc__end__buf __loc__end__pos in
                          fun str ->
                            fun pos ->
                              fun str' ->
                                fun pos' ->
                                  fun id ->
                                    let _loc_id = locate str pos str' pos' in
                                    loc_expr _loc
                                      (Pexp_ident (id_loc id _loc_id))))));
         (((fun lvl -> lvl <= Atom)),
           (Earley_core.Earley.fsequence constant
              (Earley_core.Earley.empty_pos
                 (fun __loc__start__buf ->
                    fun __loc__start__pos ->
                      fun __loc__end__buf ->
                        fun __loc__end__pos ->
                          let _loc =
                            locate __loc__start__buf __loc__start__pos
                              __loc__end__buf __loc__end__pos in
                          fun c -> loc_expr _loc (Pexp_constant c)))));
         (((fun lvl -> lvl <= Atom)),
           (Earley_core.Earley.fsequence_position module_path
              (Earley_core.Earley.fsequence_ignore
                 (Earley_core.Earley.string "." ".")
                 (Earley_core.Earley.fsequence_ignore
                    (Earley_core.Earley.string "(" "(")
                    (Earley_core.Earley.fsequence expression
                       (Earley_core.Earley.fsequence_ignore
                          (Earley_core.Earley.string ")" ")")
                          (Earley_core.Earley.empty_pos
                             (fun __loc__start__buf ->
                                fun __loc__start__pos ->
                                  fun __loc__end__buf ->
                                    fun __loc__end__pos ->
                                      let _loc =
                                        locate __loc__start__buf
                                          __loc__start__pos __loc__end__buf
                                          __loc__end__pos in
                                      fun e ->
                                        fun str ->
                                          fun pos ->
                                            fun str' ->
                                              fun pos' ->
                                                fun mp ->
                                                  let _loc_mp =
                                                    locate str pos str' pos' in
                                                  let mp = id_loc mp _loc_mp in
                                                  loc_expr _loc
                                                    (Pexp_open (Fresh, mp, e))))))))));
         (((fun lvl -> lvl <= Atom)),
           (Earley_core.Earley.fsequence_position module_path
              (Earley_core.Earley.fsequence_ignore
                 (Earley_core.Earley.char '.' '.')
                 (Earley_core.Earley.fsequence_ignore
                    (Earley_core.Earley.char '[' '[')
                    (Earley_core.Earley.fsequence expression_list
                       (Earley_core.Earley.fsequence_position
                          (Earley_core.Earley.char ']' ']')
                          (Earley_core.Earley.empty_pos
                             (fun __loc__start__buf ->
                                fun __loc__start__pos ->
                                  fun __loc__end__buf ->
                                    fun __loc__end__pos ->
                                      let _loc =
                                        locate __loc__start__buf
                                          __loc__start__pos __loc__end__buf
                                          __loc__end__pos in
                                      fun str ->
                                        fun pos ->
                                          fun str' ->
                                            fun pos' ->
                                              fun cl ->
                                                let _loc_cl =
                                                  locate str pos str' pos' in
                                                fun l ->
                                                  fun str ->
                                                    fun pos ->
                                                      fun str' ->
                                                        fun pos' ->
                                                          fun mp ->
                                                            let _loc_mp =
                                                              locate str pos
                                                                str' pos' in
                                                            let mp =
                                                              id_loc mp
                                                                _loc_mp in
                                                            loc_expr _loc
                                                              (Pexp_open
                                                                 (Fresh, mp,
                                                                   (loc_expr
                                                                    _loc
                                                                    (pexp_list
                                                                    _loc
                                                                    ~loc_cl:_loc_cl
                                                                    l).pexp_desc)))))))))));
         (((fun lvl -> lvl <= Atom)),
           (Earley_core.Earley.fsequence_position module_path
              (Earley_core.Earley.fsequence_ignore
                 (Earley_core.Earley.char '.' '.')
                 (Earley_core.Earley.fsequence_ignore
                    (Earley_core.Earley.char '{' '{')
                    (Earley_core.Earley.fsequence
                       (Earley_core.Earley.option None
                          (Earley_core.Earley.apply (fun x -> Some x)
                             (Earley_core.Earley.fsequence expression
                                (Earley_core.Earley.fsequence_ignore with_kw
                                   (Earley_core.Earley.empty
                                      (fun _default_0 -> _default_0))))))
                       (Earley_core.Earley.fsequence record_list
                          (Earley_core.Earley.fsequence_ignore
                             (Earley_core.Earley.char '}' '}')
                             (Earley_core.Earley.empty_pos
                                (fun __loc__start__buf ->
                                   fun __loc__start__pos ->
                                     fun __loc__end__buf ->
                                       fun __loc__end__pos ->
                                         let _loc =
                                           locate __loc__start__buf
                                             __loc__start__pos
                                             __loc__end__buf __loc__end__pos in
                                         fun l ->
                                           fun e ->
                                             fun str ->
                                               fun pos ->
                                                 fun str' ->
                                                   fun pos' ->
                                                     fun mp ->
                                                       let _loc_mp =
                                                         locate str pos str'
                                                           pos' in
                                                       let mp =
                                                         id_loc mp _loc_mp in
                                                       loc_expr _loc
                                                         (Pexp_open
                                                            (Fresh, mp,
                                                              (loc_expr _loc
                                                                 (Pexp_record
                                                                    (l, e))))))))))))));
         (((fun lvl -> lvl <= Atom)),
           (Earley_core.Earley.fsequence_ignore
              (Earley_core.Earley.char '(' '(')
              (Earley_core.Earley.fsequence
                 (Earley_core.Earley.option None
                    (Earley_core.Earley.apply (fun x -> Some x) expression))
                 (Earley_core.Earley.fsequence_ignore
                    (Earley_core.Earley.char ')' ')')
                    (Earley_core.Earley.empty_pos
                       (fun __loc__start__buf ->
                          fun __loc__start__pos ->
                            fun __loc__end__buf ->
                              fun __loc__end__pos ->
                                let _loc =
                                  locate __loc__start__buf __loc__start__pos
                                    __loc__end__buf __loc__end__pos in
                                fun e ->
                                  match e with
                                  | Some e ->
                                      if Quote.is_antiquotation e.pexp_loc
                                      then e
                                      else loc_expr _loc e.pexp_desc
                                  | None ->
                                      let cunit = id_loc (Lident "()") _loc in
                                      loc_expr _loc
                                        (pexp_construct (cunit, None))))))));
         (((fun lvl -> lvl <= Atom)),
           (Earley_core.Earley.fsequence_ignore
              (Earley_core.Earley.char '(' '(')
              (Earley_core.Earley.fsequence no_parser
                 (Earley_core.Earley.fsequence expression
                    (Earley_core.Earley.fsequence type_coercion
                       (Earley_core.Earley.fsequence_ignore
                          (Earley_core.Earley.char ')' ')')
                          (Earley_core.Earley.empty_pos
                             (fun __loc__start__buf ->
                                fun __loc__start__pos ->
                                  fun __loc__end__buf ->
                                    fun __loc__end__pos ->
                                      let _loc =
                                        locate __loc__start__buf
                                          __loc__start__pos __loc__end__buf
                                          __loc__end__pos in
                                      fun t ->
                                        fun e ->
                                          fun _default_0 ->
                                            match t with
                                            | (Some t1, None) ->
                                                loc_expr (ghost _loc)
                                                  (pexp_constraint (e, t1))
                                            | (t1, Some t2) ->
                                                loc_expr (ghost _loc)
                                                  (pexp_coerce (e, t1, t2))
                                            | (None, None) -> assert false))))))));
         (((fun lvl -> lvl <= Atom)),
           (Earley_core.Earley.fsequence begin_kw
              (Earley_core.Earley.fsequence
                 (Earley_core.Earley.option None
                    (Earley_core.Earley.apply (fun x -> Some x) expression))
                 (Earley_core.Earley.fsequence end_kw
                    (Earley_core.Earley.empty_pos
                       (fun __loc__start__buf ->
                          fun __loc__start__pos ->
                            fun __loc__end__buf ->
                              fun __loc__end__pos ->
                                let _loc =
                                  locate __loc__start__buf __loc__start__pos
                                    __loc__end__buf __loc__end__pos in
                                fun _default_0 ->
                                  fun e ->
                                    fun _default_1 ->
                                      match e with
                                      | Some e ->
                                          if
                                            Quote.is_antiquotation e.pexp_loc
                                          then e
                                          else loc_expr _loc e.pexp_desc
                                      | None ->
                                          let cunit =
                                            id_loc (Lident "()") _loc in
                                          loc_expr _loc
                                            (pexp_construct (cunit, None))))))));
         (((fun lvl -> lvl <= App)),
           (Earley_core.Earley.fsequence
              (expression_lvl (NoMatch, (next_exp App)))
              (Earley_core.Earley.fsequence
                 (Earley_core.Earley.apply (fun f -> f [])
                    (Earley_core.Earley.fixpoint1' (fun l -> l) argument
                       (fun x -> fun f -> fun l -> f (x :: l))))
                 (Earley_core.Earley.empty_pos
                    (fun __loc__start__buf ->
                       fun __loc__start__pos ->
                         fun __loc__end__buf ->
                           fun __loc__end__pos ->
                             let _loc =
                               locate __loc__start__buf __loc__start__pos
                                 __loc__end__buf __loc__end__pos in
                             fun l ->
                               fun f ->
                                 loc_expr _loc
                                   (match ((f.pexp_desc), l) with
                                    | (Pexp_construct (c, None),
                                       (Nolabel, a)::[]) ->
                                        Pexp_construct (c, (Some a))
                                    | (Pexp_variant (c, None),
                                       (Nolabel, a)::[]) ->
                                        Pexp_variant (c, (Some a))
                                    | _ -> Pexp_apply (f, l)))))));
         (((fun lvl -> lvl <= Atom)),
           (Earley_core.Earley.fsequence_position constructor
              (Earley_core.Earley.fsequence no_dot
                 (Earley_core.Earley.empty_pos
                    (fun __loc__start__buf ->
                       fun __loc__start__pos ->
                         fun __loc__end__buf ->
                           fun __loc__end__pos ->
                             let _loc =
                               locate __loc__start__buf __loc__start__pos
                                 __loc__end__buf __loc__end__pos in
                             fun _default_0 ->
                               fun str ->
                                 fun pos ->
                                   fun str' ->
                                     fun pos' ->
                                       fun c ->
                                         let _loc_c =
                                           locate str pos str' pos' in
                                         loc_expr _loc
                                           (pexp_construct
                                              ((id_loc c _loc_c), None)))))));
         (((fun lvl -> lvl <= Atom)),
           (Earley_core.Earley.fsequence tag_name
              (Earley_core.Earley.empty_pos
                 (fun __loc__start__buf ->
                    fun __loc__start__pos ->
                      fun __loc__end__buf ->
                        fun __loc__end__pos ->
                          let _loc =
                            locate __loc__start__buf __loc__start__pos
                              __loc__end__buf __loc__end__pos in
                          fun l -> loc_expr _loc (Pexp_variant (l, None))))));
         (((fun lvl -> lvl <= Atom)),
           (Earley_core.Earley.fsequence_ignore
              (Earley_core.Earley.string "[|" "[|")
              (Earley_core.Earley.fsequence expression_list
                 (Earley_core.Earley.fsequence_ignore
                    (Earley_core.Earley.string "|]" "|]")
                    (Earley_core.Earley.empty_pos
                       (fun __loc__start__buf ->
                          fun __loc__start__pos ->
                            fun __loc__end__buf ->
                              fun __loc__end__pos ->
                                let _loc =
                                  locate __loc__start__buf __loc__start__pos
                                    __loc__end__buf __loc__end__pos in
                                fun l ->
                                  loc_expr _loc (Pexp_array (List.map fst l))))))));
         (((fun lvl -> lvl <= Atom)),
           (Earley_core.Earley.fsequence_ignore
              (Earley_core.Earley.char '[' '[')
              (Earley_core.Earley.fsequence expression_list
                 (Earley_core.Earley.fsequence_position
                    (Earley_core.Earley.char ']' ']')
                    (Earley_core.Earley.empty_pos
                       (fun __loc__start__buf ->
                          fun __loc__start__pos ->
                            fun __loc__end__buf ->
                              fun __loc__end__pos ->
                                let _loc =
                                  locate __loc__start__buf __loc__start__pos
                                    __loc__end__buf __loc__end__pos in
                                fun str ->
                                  fun pos ->
                                    fun str' ->
                                      fun pos' ->
                                        fun cl ->
                                          let _loc_cl =
                                            locate str pos str' pos' in
                                          fun l ->
                                            loc_expr _loc
                                              (pexp_list _loc ~loc_cl:_loc_cl
                                                 l).pexp_desc))))));
         (((fun lvl -> lvl <= Atom)),
           (Earley_core.Earley.fsequence_ignore
              (Earley_core.Earley.string "{" "{")
              (Earley_core.Earley.fsequence
                 (Earley_core.Earley.option None
                    (Earley_core.Earley.apply (fun x -> Some x)
                       (Earley_core.Earley.fsequence expression
                          (Earley_core.Earley.fsequence_ignore with_kw
                             (Earley_core.Earley.empty
                                (fun _default_0 -> _default_0))))))
                 (Earley_core.Earley.fsequence record_list
                    (Earley_core.Earley.fsequence_ignore
                       (Earley_core.Earley.string "}" "}")
                       (Earley_core.Earley.empty_pos
                          (fun __loc__start__buf ->
                             fun __loc__start__pos ->
                               fun __loc__end__buf ->
                                 fun __loc__end__pos ->
                                   let _loc =
                                     locate __loc__start__buf
                                       __loc__start__pos __loc__end__buf
                                       __loc__end__pos in
                                   fun l ->
                                     fun e ->
                                       loc_expr _loc (Pexp_record (l, e)))))))));
         (((fun lvl -> lvl <= Atom)),
           (Earley_core.Earley.fsequence while_kw
              (Earley_core.Earley.fsequence expression
                 (Earley_core.Earley.fsequence do_kw
                    (Earley_core.Earley.fsequence expression
                       (Earley_core.Earley.fsequence done_kw
                          (Earley_core.Earley.empty_pos
                             (fun __loc__start__buf ->
                                fun __loc__start__pos ->
                                  fun __loc__end__buf ->
                                    fun __loc__end__pos ->
                                      let _loc =
                                        locate __loc__start__buf
                                          __loc__start__pos __loc__end__buf
                                          __loc__end__pos in
                                      fun _default_0 ->
                                        fun e' ->
                                          fun _default_1 ->
                                            fun e ->
                                              fun _default_2 ->
                                                loc_expr _loc
                                                  (Pexp_while (e, e'))))))))));
         (((fun lvl -> lvl <= Atom)),
           (Earley_core.Earley.fsequence for_kw
              (Earley_core.Earley.fsequence pattern
                 (Earley_core.Earley.fsequence_ignore
                    (Earley_core.Earley.char '=' '=')
                    (Earley_core.Earley.fsequence expression
                       (Earley_core.Earley.fsequence downto_flag
                          (Earley_core.Earley.fsequence expression
                             (Earley_core.Earley.fsequence do_kw
                                (Earley_core.Earley.fsequence expression
                                   (Earley_core.Earley.fsequence done_kw
                                      (Earley_core.Earley.empty_pos
                                         (fun __loc__start__buf ->
                                            fun __loc__start__pos ->
                                              fun __loc__end__buf ->
                                                fun __loc__end__pos ->
                                                  let _loc =
                                                    locate __loc__start__buf
                                                      __loc__start__pos
                                                      __loc__end__buf
                                                      __loc__end__pos in
                                                  fun _default_0 ->
                                                    fun e'' ->
                                                      fun _default_1 ->
                                                        fun e' ->
                                                          fun d ->
                                                            fun e ->
                                                              fun id ->
                                                                fun
                                                                  _default_2
                                                                  ->
                                                                  loc_expr
                                                                    _loc
                                                                    (
                                                                    Pexp_for
                                                                    (id, e,
                                                                    e', d,
                                                                    e''))))))))))))));
         (((fun lvl -> lvl <= Atom)),
           (Earley_core.Earley.fsequence new_kw
              (Earley_core.Earley.fsequence_position class_path
                 (Earley_core.Earley.empty_pos
                    (fun __loc__start__buf ->
                       fun __loc__start__pos ->
                         fun __loc__end__buf ->
                           fun __loc__end__pos ->
                             let _loc =
                               locate __loc__start__buf __loc__start__pos
                                 __loc__end__buf __loc__end__pos in
                             fun str ->
                               fun pos ->
                                 fun str' ->
                                   fun pos' ->
                                     fun p ->
                                       let _loc_p = locate str pos str' pos' in
                                       fun _default_0 ->
                                         loc_expr _loc
                                           (Pexp_new (id_loc p _loc_p)))))));
         (((fun lvl -> lvl <= Atom)),
           (Earley_core.Earley.fsequence object_kw
              (Earley_core.Earley.fsequence class_body
                 (Earley_core.Earley.fsequence end_kw
                    (Earley_core.Earley.empty_pos
                       (fun __loc__start__buf ->
                          fun __loc__start__pos ->
                            fun __loc__end__buf ->
                              fun __loc__end__pos ->
                                let _loc =
                                  locate __loc__start__buf __loc__start__pos
                                    __loc__end__buf __loc__end__pos in
                                fun _default_0 ->
                                  fun o ->
                                    fun _default_1 ->
                                      loc_expr _loc (Pexp_object o)))))));
         (((fun lvl -> lvl <= Atom)),
           (Earley_core.Earley.fsequence_ignore
              (Earley_core.Earley.string "{<" "{<")
              (Earley_core.Earley.fsequence
                 (Earley_core.Earley.option []
                    (Earley_core.Earley.fsequence obj_item
                       (Earley_core.Earley.fsequence
                          (Earley_core.Earley.apply (fun f -> f [])
                             (Earley_core.Earley.fixpoint' (fun l -> l)
                                (Earley_core.Earley.fsequence_ignore semi_col
                                   (Earley_core.Earley.fsequence obj_item
                                      (Earley_core.Earley.empty (fun o -> o))))
                                (fun x -> fun f -> fun l -> f (x :: l))))
                          (Earley_core.Earley.fsequence_ignore
                             (Earley_core.Earley.option None
                                (Earley_core.Earley.apply (fun x -> Some x)
                                   semi_col))
                             (Earley_core.Earley.empty
                                (fun l -> fun o -> o :: l))))))
                 (Earley_core.Earley.fsequence_ignore
                    (Earley_core.Earley.string ">}" ">}")
                    (Earley_core.Earley.empty_pos
                       (fun __loc__start__buf ->
                          fun __loc__start__pos ->
                            fun __loc__end__buf ->
                              fun __loc__end__pos ->
                                let _loc =
                                  locate __loc__start__buf __loc__start__pos
                                    __loc__end__buf __loc__end__pos in
                                fun l -> loc_expr _loc (Pexp_override l)))))));
         (((fun lvl -> lvl <= Atom)),
           (Earley_core.Earley.fsequence_ignore
              (Earley_core.Earley.char '(' '(')
              (Earley_core.Earley.fsequence module_kw
                 (Earley_core.Earley.fsequence module_expr
                    (Earley_core.Earley.fsequence
                       (Earley_core.Earley.option None
                          (Earley_core.Earley.apply (fun x -> Some x)
                             (Earley_core.Earley.fsequence_ignore
                                (Earley_core.Earley.string ":" ":")
                                (Earley_core.Earley.fsequence package_type
                                   (Earley_core.Earley.empty (fun pt -> pt))))))
                       (Earley_core.Earley.fsequence_ignore
                          (Earley_core.Earley.char ')' ')')
                          (Earley_core.Earley.empty_pos
                             (fun __loc__start__buf ->
                                fun __loc__start__pos ->
                                  fun __loc__end__buf ->
                                    fun __loc__end__pos ->
                                      let _loc =
                                        locate __loc__start__buf
                                          __loc__start__pos __loc__end__buf
                                          __loc__end__pos in
                                      fun pt ->
                                        fun me ->
                                          fun _default_0 ->
                                            let desc =
                                              match pt with
                                              | None -> Pexp_pack me
                                              | Some pt ->
                                                  let me =
                                                    loc_expr (ghost _loc)
                                                      (Pexp_pack me) in
                                                  let pt =
                                                    loc_typ (ghost _loc)
                                                      pt.ptyp_desc in
                                                  pexp_constraint (me, pt) in
                                            loc_expr _loc desc))))))));
         (((fun lvl -> lvl <= Dot)),
           (Earley_core.Earley.fsequence (expression_lvl (NoMatch, Dot))
              (Earley_core.Earley.fsequence_ignore
                 (Earley_core.Earley.char '.' '.')
                 (Earley_core.Earley.fsequence
                    (Earley_core.Earley.alternatives
                       [Earley_core.Earley.fsequence_position field
                          (Earley_core.Earley.empty
                             (fun str ->
                                fun pos ->
                                  fun str' ->
                                    fun pos' ->
                                      fun f ->
                                        let _loc_f = locate str pos str' pos' in
                                        fun e' ->
                                          fun _l ->
                                            let f = id_loc f _loc_f in
                                            loc_expr _l (Pexp_field (e', f))));
                       Earley_core.Earley.fsequence_ignore
                         (Earley_core.Earley.string "(" "(")
                         (Earley_core.Earley.fsequence expression
                            (Earley_core.Earley.fsequence_ignore
                               (Earley_core.Earley.string ")" ")")
                               (Earley_core.Earley.empty
                                  (fun f ->
                                     fun e' ->
                                       fun _l ->
                                         exp_apply _l
                                           (array_function
                                              (ghost (merge2 e'.pexp_loc _l))
                                              "Array" "get") [e'; f]))));
                       Earley_core.Earley.fsequence_ignore
                         (Earley_core.Earley.string "[" "[")
                         (Earley_core.Earley.fsequence expression
                            (Earley_core.Earley.fsequence_ignore
                               (Earley_core.Earley.string "]" "]")
                               (Earley_core.Earley.empty
                                  (fun f ->
                                     fun e' ->
                                       fun _l ->
                                         exp_apply _l
                                           (array_function
                                              (ghost (merge2 e'.pexp_loc _l))
                                              "String" "get") [e'; f]))));
                       Earley_core.Earley.fsequence_ignore
                         (Earley_core.Earley.string "{" "{")
                         (Earley_core.Earley.fsequence expression
                            (Earley_core.Earley.fsequence_ignore
                               (Earley_core.Earley.string "}" "}")
                               (Earley_core.Earley.empty
                                  (fun f ->
                                     fun e' ->
                                       fun _l ->
                                         bigarray_get
                                           (ghost (merge2 e'.pexp_loc _l)) e'
                                           f))))])
                    (Earley_core.Earley.empty_pos
                       (fun __loc__start__buf ->
                          fun __loc__start__pos ->
                            fun __loc__end__buf ->
                              fun __loc__end__pos ->
                                let _loc =
                                  locate __loc__start__buf __loc__start__pos
                                    __loc__end__buf __loc__end__pos in
                                fun r -> fun e' -> r e' _loc))))));
         (((fun lvl -> lvl <= Dash)),
           (Earley_core.Earley.fsequence (expression_lvl (NoMatch, Dash))
              (Earley_core.Earley.fsequence_ignore
                 (Earley_core.Earley.char '#' '#')
                 (Earley_core.Earley.fsequence method_name
                    (Earley_core.Earley.empty_pos
                       (fun __loc__start__buf ->
                          fun __loc__start__pos ->
                            fun __loc__end__buf ->
                              fun __loc__end__pos ->
                                let _loc =
                                  locate __loc__start__buf __loc__start__pos
                                    __loc__end__buf __loc__end__pos in
                                fun f -> fun e' -> Exp.send ~loc:_loc e' f))))));
         (((fun lvl -> lvl <= Atom)),
           (Earley_core.Earley.fsequence_ignore
              (Earley_core.Earley.char '$' '$')
              (Earley_core.Earley.fsequence_ignore
                 (Earley_core.Earley.no_blank_test ())
                 (Earley_core.Earley.fsequence uident
                    (Earley_core.Earley.empty_pos
                       (fun __loc__start__buf ->
                          fun __loc__start__pos ->
                            fun __loc__end__buf ->
                              fun __loc__end__pos ->
                                let _loc =
                                  locate __loc__start__buf __loc__start__pos
                                    __loc__end__buf __loc__end__pos in
                                fun c ->
                                  match c with
                                  | "FILE" ->
                                      exp_string _loc
                                        (start_pos _loc).Lexing.pos_fname
                                  | "LINE" ->
                                      exp_int _loc
                                        (start_pos _loc).Lexing.pos_lnum
                                  | _ ->
                                      (try
                                         let str = Sys.getenv c in
                                         parse_string ~filename:("ENV:" ^ c)
                                           expression ocaml_blank str
                                       with | Not_found -> give_up ())))))));
         (((fun lvl -> lvl <= Atom)),
           (Earley_core.Earley.fsequence_ignore
              (Earley_core.Earley.string "<:" "<:")
              (Earley_core.Earley.fsequence
                 (Earley_core.Earley.alternatives
                    [Earley_core.Earley.fsequence_ignore
                       (Earley_core.Earley.string "record" "record")
                       (Earley_core.Earley.fsequence_ignore
                          (Earley_core.Earley.char '<' '<')
                          (Earley_core.Earley.fsequence_position record_list
                             (Earley_core.Earley.fsequence_ignore
                                (Earley_core.Earley.string ">>" ">>")
                                (Earley_core.Earley.empty_pos
                                   (fun __loc__start__buf ->
                                      fun __loc__start__pos ->
                                        fun __loc__end__buf ->
                                          fun __loc__end__pos ->
                                            let _loc =
                                              locate __loc__start__buf
                                                __loc__start__pos
                                                __loc__end__buf
                                                __loc__end__pos in
                                            fun str ->
                                              fun pos ->
                                                fun str' ->
                                                  fun pos' ->
                                                    fun e ->
                                                      let _loc_e =
                                                        locate str pos str'
                                                          pos' in
                                                      let e_loc =
                                                        exp_ident _loc "_loc" in
                                                      let quote_fields =
                                                        let open Quote in
                                                          quote_list
                                                            (fun e_loc ->
                                                               fun _loc ->
                                                                 fun 
                                                                   (x1, x2)
                                                                   ->
                                                                   quote_tuple
                                                                    e_loc
                                                                    _loc
                                                                    [
                                                                    (quote_loc
                                                                    quote_longident)
                                                                    e_loc
                                                                    _loc x1;
                                                                    quote_expression
                                                                    e_loc
                                                                    _loc x2]) in
                                                      quote_fields e_loc
                                                        _loc_e e)))));
                    Earley_core.Earley.fsequence_ignore
                      (Earley_core.Earley.string "expr" "expr")
                      (Earley_core.Earley.fsequence_ignore
                         (Earley_core.Earley.char '<' '<')
                         (Earley_core.Earley.fsequence_position expression
                            (Earley_core.Earley.fsequence_ignore
                               (Earley_core.Earley.string ">>" ">>")
                               (Earley_core.Earley.empty_pos
                                  (fun __loc__start__buf ->
                                     fun __loc__start__pos ->
                                       fun __loc__end__buf ->
                                         fun __loc__end__pos ->
                                           let _loc =
                                             locate __loc__start__buf
                                               __loc__start__pos
                                               __loc__end__buf
                                               __loc__end__pos in
                                           fun str ->
                                             fun pos ->
                                               fun str' ->
                                                 fun pos' ->
                                                   fun e ->
                                                     let _loc_e =
                                                       locate str pos str'
                                                         pos' in
                                                     let e_loc =
                                                       exp_ident _loc "_loc" in
                                                     Quote.quote_expression
                                                       e_loc _loc_e e)))));
                    Earley_core.Earley.fsequence_ignore
                      (Earley_core.Earley.string "type" "type")
                      (Earley_core.Earley.fsequence_ignore
                         (Earley_core.Earley.char '<' '<')
                         (Earley_core.Earley.fsequence_position typexpr
                            (Earley_core.Earley.fsequence_ignore
                               (Earley_core.Earley.string ">>" ">>")
                               (Earley_core.Earley.empty_pos
                                  (fun __loc__start__buf ->
                                     fun __loc__start__pos ->
                                       fun __loc__end__buf ->
                                         fun __loc__end__pos ->
                                           let _loc =
                                             locate __loc__start__buf
                                               __loc__start__pos
                                               __loc__end__buf
                                               __loc__end__pos in
                                           fun str ->
                                             fun pos ->
                                               fun str' ->
                                                 fun pos' ->
                                                   fun e ->
                                                     let _loc_e =
                                                       locate str pos str'
                                                         pos' in
                                                     let e_loc =
                                                       exp_ident _loc "_loc" in
                                                     Quote.quote_core_type
                                                       e_loc _loc_e e)))));
                    Earley_core.Earley.fsequence_ignore
                      (Earley_core.Earley.string "pat" "pat")
                      (Earley_core.Earley.fsequence_ignore
                         (Earley_core.Earley.char '<' '<')
                         (Earley_core.Earley.fsequence_position pattern
                            (Earley_core.Earley.fsequence_ignore
                               (Earley_core.Earley.string ">>" ">>")
                               (Earley_core.Earley.empty_pos
                                  (fun __loc__start__buf ->
                                     fun __loc__start__pos ->
                                       fun __loc__end__buf ->
                                         fun __loc__end__pos ->
                                           let _loc =
                                             locate __loc__start__buf
                                               __loc__start__pos
                                               __loc__end__buf
                                               __loc__end__pos in
                                           fun str ->
                                             fun pos ->
                                               fun str' ->
                                                 fun pos' ->
                                                   fun e ->
                                                     let _loc_e =
                                                       locate str pos str'
                                                         pos' in
                                                     let e_loc =
                                                       exp_ident _loc "_loc" in
                                                     Quote.quote_pattern
                                                       e_loc _loc_e e)))));
                    Earley_core.Earley.fsequence_ignore
                      (Earley_core.Earley.string "struct" "struct")
                      (Earley_core.Earley.fsequence_ignore
                         (Earley_core.Earley.char '<' '<')
                         (Earley_core.Earley.fsequence_position
                            structure_item_simple
                            (Earley_core.Earley.fsequence_ignore
                               (Earley_core.Earley.string ">>" ">>")
                               (Earley_core.Earley.empty_pos
                                  (fun __loc__start__buf ->
                                     fun __loc__start__pos ->
                                       fun __loc__end__buf ->
                                         fun __loc__end__pos ->
                                           let _loc =
                                             locate __loc__start__buf
                                               __loc__start__pos
                                               __loc__end__buf
                                               __loc__end__pos in
                                           fun str ->
                                             fun pos ->
                                               fun str' ->
                                                 fun pos' ->
                                                   fun e ->
                                                     let _loc_e =
                                                       locate str pos str'
                                                         pos' in
                                                     let e_loc =
                                                       exp_ident _loc "_loc" in
                                                     Quote.quote_structure
                                                       e_loc _loc_e e)))));
                    Earley_core.Earley.fsequence_ignore
                      (Earley_core.Earley.string "sig" "sig")
                      (Earley_core.Earley.fsequence_ignore
                         (Earley_core.Earley.char '<' '<')
                         (Earley_core.Earley.fsequence_position
                            signature_item
                            (Earley_core.Earley.fsequence_ignore
                               (Earley_core.Earley.string ">>" ">>")
                               (Earley_core.Earley.empty_pos
                                  (fun __loc__start__buf ->
                                     fun __loc__start__pos ->
                                       fun __loc__end__buf ->
                                         fun __loc__end__pos ->
                                           let _loc =
                                             locate __loc__start__buf
                                               __loc__start__pos
                                               __loc__end__buf
                                               __loc__end__pos in
                                           fun str ->
                                             fun pos ->
                                               fun str' ->
                                                 fun pos' ->
                                                   fun e ->
                                                     let _loc_e =
                                                       locate str pos str'
                                                         pos' in
                                                     let e_loc =
                                                       exp_ident _loc "_loc" in
                                                     Quote.quote_signature
                                                       e_loc _loc_e e)))));
                    Earley_core.Earley.fsequence_ignore
                      (Earley_core.Earley.string "constructors"
                         "constructors")
                      (Earley_core.Earley.fsequence_ignore
                         (Earley_core.Earley.char '<' '<')
                         (Earley_core.Earley.fsequence_position
                            constr_decl_list
                            (Earley_core.Earley.fsequence_ignore
                               (Earley_core.Earley.string ">>" ">>")
                               (Earley_core.Earley.empty_pos
                                  (fun __loc__start__buf ->
                                     fun __loc__start__pos ->
                                       fun __loc__end__buf ->
                                         fun __loc__end__pos ->
                                           let _loc =
                                             locate __loc__start__buf
                                               __loc__start__pos
                                               __loc__end__buf
                                               __loc__end__pos in
                                           fun str ->
                                             fun pos ->
                                               fun str' ->
                                                 fun pos' ->
                                                   fun e ->
                                                     let _loc_e =
                                                       locate str pos str'
                                                         pos' in
                                                     let e_loc =
                                                       exp_ident _loc "_loc" in
                                                     let open Quote in
                                                       quote_list
                                                         quote_constructor_declaration
                                                         e_loc _loc_e e)))));
                    Earley_core.Earley.fsequence_ignore
                      (Earley_core.Earley.string "fields" "fields")
                      (Earley_core.Earley.fsequence_ignore
                         (Earley_core.Earley.char '<' '<')
                         (Earley_core.Earley.fsequence_position
                            field_decl_list
                            (Earley_core.Earley.fsequence_ignore
                               (Earley_core.Earley.string ">>" ">>")
                               (Earley_core.Earley.empty_pos
                                  (fun __loc__start__buf ->
                                     fun __loc__start__pos ->
                                       fun __loc__end__buf ->
                                         fun __loc__end__pos ->
                                           let _loc =
                                             locate __loc__start__buf
                                               __loc__start__pos
                                               __loc__end__buf
                                               __loc__end__pos in
                                           fun str ->
                                             fun pos ->
                                               fun str' ->
                                                 fun pos' ->
                                                   fun e ->
                                                     let _loc_e =
                                                       locate str pos str'
                                                         pos' in
                                                     let e_loc =
                                                       exp_ident _loc "_loc" in
                                                     let open Quote in
                                                       quote_list
                                                         quote_label_declaration
                                                         e_loc _loc_e e)))));
                    Earley_core.Earley.fsequence_ignore
                      (Earley_core.Earley.string "bindings" "bindings")
                      (Earley_core.Earley.fsequence_ignore
                         (Earley_core.Earley.char '<' '<')
                         (Earley_core.Earley.fsequence_position let_binding
                            (Earley_core.Earley.fsequence_ignore
                               (Earley_core.Earley.string ">>" ">>")
                               (Earley_core.Earley.empty_pos
                                  (fun __loc__start__buf ->
                                     fun __loc__start__pos ->
                                       fun __loc__end__buf ->
                                         fun __loc__end__pos ->
                                           let _loc =
                                             locate __loc__start__buf
                                               __loc__start__pos
                                               __loc__end__buf
                                               __loc__end__pos in
                                           fun str ->
                                             fun pos ->
                                               fun str' ->
                                                 fun pos' ->
                                                   fun e ->
                                                     let _loc_e =
                                                       locate str pos str'
                                                         pos' in
                                                     let e_loc =
                                                       exp_ident _loc "_loc" in
                                                     let open Quote in
                                                       quote_list
                                                         quote_value_binding
                                                         e_loc _loc_e e)))));
                    Earley_core.Earley.fsequence_ignore
                      (Earley_core.Earley.string "cases" "cases")
                      (Earley_core.Earley.fsequence_ignore
                         (Earley_core.Earley.char '<' '<')
                         (Earley_core.Earley.fsequence_position match_cases
                            (Earley_core.Earley.fsequence_ignore
                               (Earley_core.Earley.string ">>" ">>")
                               (Earley_core.Earley.empty_pos
                                  (fun __loc__start__buf ->
                                     fun __loc__start__pos ->
                                       fun __loc__end__buf ->
                                         fun __loc__end__pos ->
                                           let _loc =
                                             locate __loc__start__buf
                                               __loc__start__pos
                                               __loc__end__buf
                                               __loc__end__pos in
                                           fun str ->
                                             fun pos ->
                                               fun str' ->
                                                 fun pos' ->
                                                   fun e ->
                                                     let _loc_e =
                                                       locate str pos str'
                                                         pos' in
                                                     let e_loc =
                                                       exp_ident _loc "_loc" in
                                                     let open Quote in
                                                       quote_list quote_case
                                                         e_loc _loc_e e)))));
                    Earley_core.Earley.fsequence_ignore
                      (Earley_core.Earley.string "module" "module")
                      (Earley_core.Earley.fsequence_ignore
                         (Earley_core.Earley.char '<' '<')
                         (Earley_core.Earley.fsequence_position module_expr
                            (Earley_core.Earley.fsequence_ignore
                               (Earley_core.Earley.string ">>" ">>")
                               (Earley_core.Earley.empty_pos
                                  (fun __loc__start__buf ->
                                     fun __loc__start__pos ->
                                       fun __loc__end__buf ->
                                         fun __loc__end__pos ->
                                           let _loc =
                                             locate __loc__start__buf
                                               __loc__start__pos
                                               __loc__end__buf
                                               __loc__end__pos in
                                           fun str ->
                                             fun pos ->
                                               fun str' ->
                                                 fun pos' ->
                                                   fun e ->
                                                     let _loc_e =
                                                       locate str pos str'
                                                         pos' in
                                                     let e_loc =
                                                       exp_ident _loc "_loc" in
                                                     Quote.quote_module_expr
                                                       e_loc _loc_e e)))));
                    Earley_core.Earley.fsequence_ignore
                      (Earley_core.Earley.string "module" "module")
                      (Earley_core.Earley.fsequence_ignore
                         (Earley_core.Earley.string "type" "type")
                         (Earley_core.Earley.fsequence_ignore
                            (Earley_core.Earley.char '<' '<')
                            (Earley_core.Earley.fsequence_position
                               module_type
                               (Earley_core.Earley.fsequence_ignore
                                  (Earley_core.Earley.string ">>" ">>")
                                  (Earley_core.Earley.empty_pos
                                     (fun __loc__start__buf ->
                                        fun __loc__start__pos ->
                                          fun __loc__end__buf ->
                                            fun __loc__end__pos ->
                                              let _loc =
                                                locate __loc__start__buf
                                                  __loc__start__pos
                                                  __loc__end__buf
                                                  __loc__end__pos in
                                              fun str ->
                                                fun pos ->
                                                  fun str' ->
                                                    fun pos' ->
                                                      fun e ->
                                                        let _loc_e =
                                                          locate str pos str'
                                                            pos' in
                                                        let e_loc =
                                                          exp_ident _loc
                                                            "_loc" in
                                                        Quote.quote_module_type
                                                          e_loc _loc_e e))))))])
                 (Earley_core.Earley.empty (fun r -> r)))))],
          (fun lvl -> []))
    let (semicol, semicol__set__grammar) =
      Earley_core.Earley.grammar_prio "semicol"
    let _ =
      semicol__set__grammar
        ([(((fun (alm, lvl) -> lvl > Seq)),
            (Earley_core.Earley.fsequence_ignore
               (Earley_core.Earley.empty ()) (Earley_core.Earley.empty false)));
         (((fun (alm, lvl) -> lvl = Seq)),
           (Earley_core.Earley.fsequence semi_col
              (Earley_core.Earley.empty (fun _default_0 -> true))));
         (((fun (alm, lvl) -> lvl = Seq)),
           (Earley_core.Earley.fsequence no_semi
              (Earley_core.Earley.empty (fun _default_0 -> false))))],
          (fun (alm, lvl) -> []))
    let (noelse, noelse__set__grammar) =
      Earley_core.Earley.grammar_prio "noelse"
    let _ =
      noelse__set__grammar
        ([(((fun b -> not b)),
            (Earley_core.Earley.fsequence_ignore
               (Earley_core.Earley.empty ()) (Earley_core.Earley.empty ())));
         (((fun b -> b)), no_else)], (fun b -> []))
    let (debut, debut__set__grammar) =
      Earley_core.Earley.grammar_family "debut"
    let debut __curry__varx0 __curry__varx1 =
      debut (__curry__varx0, __curry__varx1)
    let _ =
      debut__set__grammar
        (fun (lvl, alm) ->
           Earley_core.Earley.fsequence_position (left_expr (alm, lvl))
             (Earley_core.Earley.empty
                (fun str ->
                   fun pos ->
                     fun str' ->
                       fun pos' ->
                         fun s ->
                           let _loc_s = locate str pos str' pos' in
                           let (lvl0, no_else, f) = s in
                           ((lvl0, no_else), (f, _loc_s)))))
    let (suit, suit__set__grammar) = Earley_core.Earley.grammar_family "suit"
    let suit __curry__varx0 __curry__varx1 __curry__varx2 =
      suit (__curry__varx0, __curry__varx1, __curry__varx2)
    let _ =
      suit__set__grammar
        (fun (lvl, alm, (lvl0, no_else)) ->
           Earley_core.Earley.fsequence_position (expression_lvl (alm, lvl0))
             (Earley_core.Earley.fsequence_position (semicol (alm, lvl))
                (Earley_core.Earley.fsequence (noelse no_else)
                   (Earley_core.Earley.empty
                      (fun _default_0 ->
                         fun str ->
                           fun pos ->
                             fun str' ->
                               fun pos' ->
                                 fun c ->
                                   let _loc_c = locate str pos str' pos' in
                                   fun str ->
                                     fun pos ->
                                       fun str' ->
                                         fun pos' ->
                                           fun e ->
                                             let _loc_e =
                                               locate str pos str' pos' in
                                             fun (f, _loc_s) ->
                                               let _l = merge2 _loc_s _loc_e in
                                               let _loc_c =
                                                 if c then _loc_c else _loc_e in
                                               f e (_l, _loc_c))))))
    let _ =
      set_expression_lvl
        (fun ((alm, lvl) as c) ->
           Earley_core.Earley.alternatives
             ((if allow_match alm
               then
                 [Earley_core.Earley.fsequence prefix_expression
                    (Earley_core.Earley.fsequence (semicol (alm, lvl))
                       (Earley_core.Earley.empty
                          (fun _default_0 -> fun r -> r)))]
               else []) @
                [Earley_core.Earley.fsequence (extra_expressions_grammar c)
                   (Earley_core.Earley.fsequence (semicol (alm, lvl))
                      (Earley_core.Earley.empty
                         (fun _default_0 -> fun e -> e)));
                Earley.dependent_sequence (debut lvl alm) (suit lvl alm);
                Earley_core.Earley.fsequence (right_expression lvl)
                  (Earley_core.Earley.fsequence (semicol (alm, lvl))
                     (Earley_core.Earley.empty (fun _default_0 -> fun r -> r)))]))
    let module_expr_base =
      Earley_core.Earley.declare_grammar "module_expr_base"
    let _ =
      Earley_core.Earley.set_grammar module_expr_base
        (Earley_core.Earley.alternatives
           [Earley_core.Earley.fsequence_ignore
              (Earley_core.Earley.char '(' '(')
              (Earley_core.Earley.fsequence val_kw
                 (Earley_core.Earley.fsequence expression
                    (Earley_core.Earley.fsequence
                       (Earley_core.Earley.option None
                          (Earley_core.Earley.apply (fun x -> Some x)
                             (Earley_core.Earley.fsequence_ignore
                                (Earley_core.Earley.string ":" ":")
                                (Earley_core.Earley.fsequence package_type
                                   (Earley_core.Earley.empty (fun pt -> pt))))))
                       (Earley_core.Earley.fsequence_ignore
                          (Earley_core.Earley.char ')' ')')
                          (Earley_core.Earley.empty_pos
                             (fun __loc__start__buf ->
                                fun __loc__start__pos ->
                                  fun __loc__end__buf ->
                                    fun __loc__end__pos ->
                                      let _loc =
                                        locate __loc__start__buf
                                          __loc__start__pos __loc__end__buf
                                          __loc__end__pos in
                                      fun pt ->
                                        fun e ->
                                          fun _default_0 ->
                                            let e =
                                              match pt with
                                              | None -> Pmod_unpack e
                                              | Some pt ->
                                                  Pmod_unpack
                                                    (loc_expr (ghost _loc)
                                                       (pexp_constraint
                                                          (e, pt))) in
                                            mexpr_loc _loc e))))));
           Earley_core.Earley.fsequence module_path
             (Earley_core.Earley.empty_pos
                (fun __loc__start__buf ->
                   fun __loc__start__pos ->
                     fun __loc__end__buf ->
                       fun __loc__end__pos ->
                         let _loc =
                           locate __loc__start__buf __loc__start__pos
                             __loc__end__buf __loc__end__pos in
                         fun mp ->
                           let mid = id_loc mp _loc in
                           mexpr_loc _loc (Pmod_ident mid)));
           Earley_core.Earley.fsequence
             (Earley_core.Earley.fsequence struct_kw
                (Earley_core.Earley.empty
                   (fun _default_0 -> push_comments ())))
             (Earley_core.Earley.fsequence
                (Earley_core.Earley.fsequence structure
                   (Earley_core.Earley.empty_pos
                      (fun __loc__start__buf ->
                         fun __loc__start__pos ->
                           fun __loc__end__buf ->
                             fun __loc__end__pos ->
                               let _loc =
                                 locate __loc__start__buf __loc__start__pos
                                   __loc__end__buf __loc__end__pos in
                               fun ms -> ms @ (attach_str _loc))))
                (Earley_core.Earley.fsequence
                   (Earley_core.Earley.fsequence end_kw
                      (Earley_core.Earley.empty
                         (fun _default_0 -> pop_comments ())))
                   (Earley_core.Earley.empty_pos
                      (fun __loc__start__buf ->
                         fun __loc__start__pos ->
                           fun __loc__end__buf ->
                             fun __loc__end__pos ->
                               let _loc =
                                 locate __loc__start__buf __loc__start__pos
                                   __loc__end__buf __loc__end__pos in
                               fun _default_0 ->
                                 fun ms ->
                                   fun _default_1 ->
                                     mexpr_loc _loc (Pmod_structure ms)))));
           Earley_core.Earley.fsequence functor_kw
             (Earley_core.Earley.fsequence_ignore
                (Earley_core.Earley.char '(' '(')
                (Earley_core.Earley.fsequence module_name
                   (Earley_core.Earley.fsequence
                      (Earley_core.Earley.option None
                         (Earley_core.Earley.apply (fun x -> Some x)
                            (Earley_core.Earley.fsequence_ignore
                               (Earley_core.Earley.char ':' ':')
                               (Earley_core.Earley.fsequence module_type
                                  (Earley_core.Earley.empty (fun mt -> mt))))))
                      (Earley_core.Earley.fsequence_ignore
                         (Earley_core.Earley.char ')' ')')
                         (Earley_core.Earley.fsequence arrow_re
                            (Earley_core.Earley.fsequence module_expr
                               (Earley_core.Earley.empty_pos
                                  (fun __loc__start__buf ->
                                     fun __loc__start__pos ->
                                       fun __loc__end__buf ->
                                         fun __loc__end__pos ->
                                           let _loc =
                                             locate __loc__start__buf
                                               __loc__start__pos
                                               __loc__end__buf
                                               __loc__end__pos in
                                           fun me ->
                                             fun _default_0 ->
                                               fun mt ->
                                                 fun mn ->
                                                   fun _default_1 ->
                                                     mexpr_loc _loc
                                                       (Pmod_functor
                                                          (mn, mt, me))))))))));
           Earley_core.Earley.fsequence_ignore
             (Earley_core.Earley.char '(' '(')
             (Earley_core.Earley.fsequence module_expr
                (Earley_core.Earley.fsequence
                   (Earley_core.Earley.option None
                      (Earley_core.Earley.apply (fun x -> Some x)
                         (Earley_core.Earley.fsequence_ignore
                            (Earley_core.Earley.char ':' ':')
                            (Earley_core.Earley.fsequence module_type
                               (Earley_core.Earley.empty (fun mt -> mt))))))
                   (Earley_core.Earley.fsequence_ignore
                      (Earley_core.Earley.char ')' ')')
                      (Earley_core.Earley.empty_pos
                         (fun __loc__start__buf ->
                            fun __loc__start__pos ->
                              fun __loc__end__buf ->
                                fun __loc__end__pos ->
                                  let _loc =
                                    locate __loc__start__buf
                                      __loc__start__pos __loc__end__buf
                                      __loc__end__pos in
                                  fun mt ->
                                    fun me ->
                                      match mt with
                                      | None -> me
                                      | Some mt ->
                                          mexpr_loc _loc
                                            (Pmod_constraint (me, mt)))))))])
    let _ =
      set_grammar module_expr
        (Earley_core.Earley.fsequence_position module_expr_base
           (Earley_core.Earley.fsequence
              (Earley_core.Earley.apply (fun f -> f [])
                 (Earley_core.Earley.fixpoint' (fun l -> l)
                    (Earley_core.Earley.fsequence_ignore
                       (Earley_core.Earley.string "(" "(")
                       (Earley_core.Earley.fsequence module_expr
                          (Earley_core.Earley.fsequence_ignore
                             (Earley_core.Earley.string ")" ")")
                             (Earley_core.Earley.empty_pos
                                (fun __loc__start__buf ->
                                   fun __loc__start__pos ->
                                     fun __loc__end__buf ->
                                       fun __loc__end__pos ->
                                         let _loc =
                                           locate __loc__start__buf
                                             __loc__start__pos
                                             __loc__end__buf __loc__end__pos in
                                         fun m -> (_loc, m))))))
                    (fun x -> fun f -> fun l -> f (x :: l))))
              (Earley_core.Earley.empty
                 (fun l ->
                    fun str ->
                      fun pos ->
                        fun str' ->
                          fun pos' ->
                            fun m ->
                              let _loc_m = locate str pos str' pos' in
                              List.fold_left
                                (fun acc ->
                                   fun (_loc_n, n) ->
                                     mexpr_loc (merge2 _loc_m _loc_n)
                                       (Pmod_apply (acc, n))) m l))))
    let module_type_base =
      Earley_core.Earley.declare_grammar "module_type_base"
    let _ =
      Earley_core.Earley.set_grammar module_type_base
        (Earley_core.Earley.alternatives
           [Earley_core.Earley.fsequence module_kw
              (Earley_core.Earley.fsequence type_kw
                 (Earley_core.Earley.fsequence of_kw
                    (Earley_core.Earley.fsequence module_expr
                       (Earley_core.Earley.empty_pos
                          (fun __loc__start__buf ->
                             fun __loc__start__pos ->
                               fun __loc__end__buf ->
                                 fun __loc__end__pos ->
                                   let _loc =
                                     locate __loc__start__buf
                                       __loc__start__pos __loc__end__buf
                                       __loc__end__pos in
                                   fun me ->
                                     fun _default_0 ->
                                       fun _default_1 ->
                                         fun _default_2 ->
                                           mtyp_loc _loc (Pmty_typeof me))))));
           Earley_core.Earley.fsequence modtype_path
             (Earley_core.Earley.empty_pos
                (fun __loc__start__buf ->
                   fun __loc__start__pos ->
                     fun __loc__end__buf ->
                       fun __loc__end__pos ->
                         let _loc =
                           locate __loc__start__buf __loc__start__pos
                             __loc__end__buf __loc__end__pos in
                         fun mp ->
                           let mid = id_loc mp _loc in
                           mtyp_loc _loc (Pmty_ident mid)));
           Earley_core.Earley.fsequence
             (Earley_core.Earley.fsequence sig_kw
                (Earley_core.Earley.empty
                   (fun _default_0 -> push_comments ())))
             (Earley_core.Earley.fsequence
                (Earley_core.Earley.fsequence signature
                   (Earley_core.Earley.empty_pos
                      (fun __loc__start__buf ->
                         fun __loc__start__pos ->
                           fun __loc__end__buf ->
                             fun __loc__end__pos ->
                               let _loc =
                                 locate __loc__start__buf __loc__start__pos
                                   __loc__end__buf __loc__end__pos in
                               fun ms -> ms @ (attach_sig _loc))))
                (Earley_core.Earley.fsequence
                   (Earley_core.Earley.fsequence end_kw
                      (Earley_core.Earley.empty
                         (fun _default_0 -> pop_comments ())))
                   (Earley_core.Earley.empty_pos
                      (fun __loc__start__buf ->
                         fun __loc__start__pos ->
                           fun __loc__end__buf ->
                             fun __loc__end__pos ->
                               let _loc =
                                 locate __loc__start__buf __loc__start__pos
                                   __loc__end__buf __loc__end__pos in
                               fun _default_0 ->
                                 fun ms ->
                                   fun _default_1 ->
                                     mtyp_loc _loc (Pmty_signature ms)))));
           Earley_core.Earley.fsequence functor_kw
             (Earley_core.Earley.fsequence_ignore
                (Earley_core.Earley.char '(' '(')
                (Earley_core.Earley.fsequence module_name
                   (Earley_core.Earley.fsequence
                      (Earley_core.Earley.option None
                         (Earley_core.Earley.apply (fun x -> Some x)
                            (Earley_core.Earley.fsequence_ignore
                               (Earley_core.Earley.char ':' ':')
                               (Earley_core.Earley.fsequence module_type
                                  (Earley_core.Earley.empty (fun mt -> mt))))))
                      (Earley_core.Earley.fsequence_ignore
                         (Earley_core.Earley.char ')' ')')
                         (Earley_core.Earley.fsequence arrow_re
                            (Earley_core.Earley.fsequence module_type
                               (Earley_core.Earley.fsequence no_with
                                  (Earley_core.Earley.empty_pos
                                     (fun __loc__start__buf ->
                                        fun __loc__start__pos ->
                                          fun __loc__end__buf ->
                                            fun __loc__end__pos ->
                                              let _loc =
                                                locate __loc__start__buf
                                                  __loc__start__pos
                                                  __loc__end__buf
                                                  __loc__end__pos in
                                              fun _default_0 ->
                                                fun me ->
                                                  fun _default_1 ->
                                                    fun mt ->
                                                      fun mn ->
                                                        fun _default_2 ->
                                                          mtyp_loc _loc
                                                            (Pmty_functor
                                                               (mn, mt, me)))))))))));
           Earley_core.Earley.fsequence_ignore
             (Earley_core.Earley.string "(" "(")
             (Earley_core.Earley.fsequence module_type
                (Earley_core.Earley.fsequence_ignore
                   (Earley_core.Earley.string ")" ")")
                   (Earley_core.Earley.empty (fun mt -> mt))))])
    let mod_constraint = Earley_core.Earley.declare_grammar "mod_constraint"
    let _ =
      Earley_core.Earley.set_grammar mod_constraint
        (Earley_core.Earley.alternatives
           [Earley_core.Earley.fsequence module_kw
              (Earley_core.Earley.fsequence_position module_path
                 (Earley_core.Earley.fsequence_ignore
                    (Earley_core.Earley.string ":=" ":=")
                    (Earley_core.Earley.fsequence_position
                       extended_module_path
                       (Earley_core.Earley.empty
                          (fun str ->
                             fun pos ->
                               fun str' ->
                                 fun pos' ->
                                   fun emp ->
                                     let _loc_emp = locate str pos str' pos' in
                                     fun str ->
                                       fun pos ->
                                         fun str' ->
                                           fun pos' ->
                                             fun mn ->
                                               let _loc_mn =
                                                 locate str pos str' pos' in
                                               fun _default_0 ->
                                                 let mn = id_loc mn _loc_mn in
                                                 Pwith_modsubst
                                                   (mn,
                                                     (id_loc emp _loc_emp)))))));
           Earley_core.Earley.fsequence_position type_kw
             (Earley_core.Earley.fsequence typedef_in_constraint
                (Earley_core.Earley.empty
                   (fun tf ->
                      fun str ->
                        fun pos ->
                          fun str' ->
                            fun pos' ->
                              fun t ->
                                let _loc_t = locate str pos str' pos' in
                                let (tn, ty) = tf (Some _loc_t) in
                                Pwith_type (tn, ty))));
           Earley_core.Earley.fsequence module_kw
             (Earley_core.Earley.fsequence_position module_path
                (Earley_core.Earley.fsequence_ignore
                   (Earley_core.Earley.char '=' '=')
                   (Earley_core.Earley.fsequence_position
                      extended_module_path
                      (Earley_core.Earley.empty
                         (fun str ->
                            fun pos ->
                              fun str' ->
                                fun pos' ->
                                  fun m2 ->
                                    let _loc_m2 = locate str pos str' pos' in
                                    fun str ->
                                      fun pos ->
                                        fun str' ->
                                          fun pos' ->
                                            fun m1 ->
                                              let _loc_m1 =
                                                locate str pos str' pos' in
                                              fun _default_0 ->
                                                let name = id_loc m1 _loc_m1 in
                                                Pwith_module
                                                  (name, (id_loc m2 _loc_m2)))))));
           Earley_core.Earley.fsequence type_kw
             (Earley_core.Earley.fsequence type_params
                (Earley_core.Earley.fsequence_position typeconstr
                   (Earley_core.Earley.fsequence_ignore
                      (Earley_core.Earley.string ":=" ":=")
                      (Earley_core.Earley.fsequence typexpr
                         (Earley_core.Earley.empty_pos
                            (fun __loc__start__buf ->
                               fun __loc__start__pos ->
                                 fun __loc__end__buf ->
                                   fun __loc__end__pos ->
                                     let _loc =
                                       locate __loc__start__buf
                                         __loc__start__pos __loc__end__buf
                                         __loc__end__pos in
                                     fun te ->
                                       fun str ->
                                         fun pos ->
                                           fun str' ->
                                             fun pos' ->
                                               fun tcn ->
                                                 let _loc_tcn =
                                                   locate str pos str' pos' in
                                                 fun tps ->
                                                   fun _default_0 ->
                                                     let tcn0 =
                                                       id_loc
                                                         (Longident.last tcn)
                                                         _loc_tcn in
                                                     let _tcn =
                                                       id_loc tcn _loc_tcn in
                                                     let td =
                                                       type_declaration _loc
                                                         tcn0 tps []
                                                         Ptype_abstract
                                                         Public (Some te) in
                                                     Pwith_typesubst
                                                       (_tcn, td)))))))])
    let _ =
      set_grammar module_type
        (Earley_core.Earley.fsequence module_type_base
           (Earley_core.Earley.fsequence
              (Earley_core.Earley.option None
                 (Earley_core.Earley.apply (fun x -> Some x)
                    (Earley_core.Earley.fsequence_ignore with_kw
                       (Earley_core.Earley.fsequence mod_constraint
                          (Earley_core.Earley.fsequence
                             (Earley_core.Earley.apply (fun f -> f [])
                                (Earley_core.Earley.fixpoint' (fun l -> l)
                                   (Earley_core.Earley.fsequence_ignore
                                      and_kw
                                      (Earley_core.Earley.fsequence
                                         mod_constraint
                                         (Earley_core.Earley.empty
                                            (fun _default_0 -> _default_0))))
                                   (fun x -> fun f -> fun l -> f (x :: l))))
                             (Earley_core.Earley.empty
                                (fun l -> fun m -> m :: l)))))))
              (Earley_core.Earley.empty_pos
                 (fun __loc__start__buf ->
                    fun __loc__start__pos ->
                      fun __loc__end__buf ->
                        fun __loc__end__pos ->
                          let _loc =
                            locate __loc__start__buf __loc__start__pos
                              __loc__end__buf __loc__end__pos in
                          fun l ->
                            fun m ->
                              match l with
                              | None -> m
                              | Some l -> mtyp_loc _loc (Pmty_with (m, l))))))
    let structure_item_base =
      Earley_core.Earley.declare_grammar "structure_item_base"
    let _ =
      Earley_core.Earley.set_grammar structure_item_base
        (Earley_core.Earley.alternatives
           [Earley_core.Earley.fsequence_ignore
              (Earley_core.Earley.string "$struct:" "$struct:")
              (Earley_core.Earley.fsequence expression
                 (Earley_core.Earley.fsequence_ignore
                    (Earley_core.Earley.no_blank_test ())
                    (Earley_core.Earley.fsequence_ignore
                       (Earley_core.Earley.char '$' '$')
                       (Earley_core.Earley.empty_pos
                          (fun __loc__start__buf ->
                             fun __loc__start__pos ->
                               fun __loc__end__buf ->
                                 fun __loc__end__pos ->
                                   let _loc =
                                     locate __loc__start__buf
                                       __loc__start__pos __loc__end__buf
                                       __loc__end__pos in
                                   fun e ->
                                     let open Quote in
                                       pstr_antiquotation _loc
                                         (function
                                          | Quote_pstr ->
                                              let e_loc =
                                                exp_ident _loc "_loc" in
                                              quote_apply e_loc _loc
                                                (pa_ast "loc_str")
                                                [quote_location_t e_loc _loc
                                                   _loc;
                                                quote_const e_loc _loc
                                                  (parsetree "Pstr_include")
                                                  [quote_record e_loc _loc
                                                     [((parsetree "pincl_loc"),
                                                        (quote_location_t
                                                           e_loc _loc _loc));
                                                     ((parsetree
                                                         "pincl_attributes"),
                                                       (quote_list
                                                          quote_attribute
                                                          e_loc _loc []));
                                                     ((parsetree "pincl_mod"),
                                                       (quote_apply e_loc
                                                          _loc
                                                          (pa_ast "mexpr_loc")
                                                          [quote_location_t
                                                             e_loc _loc _loc;
                                                          quote_const e_loc
                                                            _loc
                                                            (parsetree
                                                               "Pmod_structure")
                                                            [e]]))]]]
                                          | _ ->
                                              failwith "Bad antiquotation..."))))));
           Earley_core.Earley.fsequence
             (Earley_str.regexp ~name:"let" let_re (fun groupe -> groupe 0))
             (Earley_core.Earley.fsequence rec_flag
                (Earley_core.Earley.fsequence let_binding
                   (Earley_core.Earley.empty_pos
                      (fun __loc__start__buf ->
                         fun __loc__start__pos ->
                           fun __loc__end__buf ->
                             fun __loc__end__pos ->
                               let _loc =
                                 locate __loc__start__buf __loc__start__pos
                                   __loc__end__buf __loc__end__pos in
                               fun l ->
                                 fun r ->
                                   fun _default_0 ->
                                     loc_str _loc
                                       (match l with | _ -> Pstr_value (r, l))))));
           Earley_core.Earley.fsequence external_kw
             (Earley_core.Earley.fsequence_position value_name
                (Earley_core.Earley.fsequence_ignore
                   (Earley_core.Earley.string ":" ":")
                   (Earley_core.Earley.fsequence typexpr
                      (Earley_core.Earley.fsequence_ignore
                         (Earley_core.Earley.string "=" "=")
                         (Earley_core.Earley.fsequence
                            (Earley_core.Earley.apply (fun f -> f [])
                               (Earley_core.Earley.fixpoint' (fun l -> l)
                                  string_litteral
                                  (fun x -> fun f -> fun l -> f (x :: l))))
                            (Earley_core.Earley.fsequence
                               post_item_attributes
                               (Earley_core.Earley.empty_pos
                                  (fun __loc__start__buf ->
                                     fun __loc__start__pos ->
                                       fun __loc__end__buf ->
                                         fun __loc__end__pos ->
                                           let _loc =
                                             locate __loc__start__buf
                                               __loc__start__pos
                                               __loc__end__buf
                                               __loc__end__pos in
                                           fun a ->
                                             fun ls ->
                                               fun ty ->
                                                 fun str ->
                                                   fun pos ->
                                                     fun str' ->
                                                       fun pos' ->
                                                         fun n ->
                                                           let _loc_n =
                                                             locate str pos
                                                               str' pos' in
                                                           fun _default_0 ->
                                                             let ls =
                                                               List.map fst
                                                                 ls in
                                                             let l =
                                                               List.length ls in
                                                             if
                                                               (l < 1) ||
                                                                 (l > 3)
                                                             then give_up ();
                                                             loc_str _loc
                                                               (Pstr_primitive
                                                                  {
                                                                    pval_name
                                                                    =
                                                                    (id_loc n
                                                                    _loc_n);
                                                                    pval_type
                                                                    = ty;
                                                                    pval_prim
                                                                    = ls;
                                                                    pval_loc
                                                                    = _loc;
                                                                    pval_attributes
                                                                    =
                                                                    (attach_attrib
                                                                    _loc a)
                                                                  })))))))));
           Earley_core.Earley.fsequence type_definition
             (Earley_core.Earley.empty_pos
                (fun __loc__start__buf ->
                   fun __loc__start__pos ->
                     fun __loc__end__buf ->
                       fun __loc__end__pos ->
                         let _loc =
                           locate __loc__start__buf __loc__start__pos
                             __loc__end__buf __loc__end__pos in
                         fun td -> Str.type_ ~loc:_loc Recursive td));
           Earley_core.Earley.fsequence type_extension
             (Earley_core.Earley.empty_pos
                (fun __loc__start__buf ->
                   fun __loc__start__pos ->
                     fun __loc__end__buf ->
                       fun __loc__end__pos ->
                         let _loc =
                           locate __loc__start__buf __loc__start__pos
                             __loc__end__buf __loc__end__pos in
                         fun te -> Str.type_extension ~loc:_loc te));
           Earley_core.Earley.fsequence exception_definition
             (Earley_core.Earley.empty_pos
                (fun __loc__start__buf ->
                   fun __loc__start__pos ->
                     fun __loc__end__buf ->
                       fun __loc__end__pos ->
                         let _loc =
                           locate __loc__start__buf __loc__start__pos
                             __loc__end__buf __loc__end__pos in
                         fun ex -> loc_str _loc ex));
           Earley_core.Earley.fsequence module_kw
             (Earley_core.Earley.fsequence
                (Earley_core.Earley.alternatives
                   [Earley_core.Earley.fsequence type_kw
                      (Earley_core.Earley.fsequence_position modtype_name
                         (Earley_core.Earley.fsequence
                            (Earley_core.Earley.option None
                               (Earley_core.Earley.apply (fun x -> Some x)
                                  (Earley_core.Earley.fsequence_ignore
                                     (Earley_core.Earley.string "=" "=")
                                     (Earley_core.Earley.fsequence
                                        module_type
                                        (Earley_core.Earley.empty
                                           (fun mt -> mt))))))
                            (Earley_core.Earley.fsequence
                               post_item_attributes
                               (Earley_core.Earley.empty_pos
                                  (fun __loc__start__buf ->
                                     fun __loc__start__pos ->
                                       fun __loc__end__buf ->
                                         fun __loc__end__pos ->
                                           let _loc =
                                             locate __loc__start__buf
                                               __loc__start__pos
                                               __loc__end__buf
                                               __loc__end__pos in
                                           fun a ->
                                             fun mt ->
                                               fun str ->
                                                 fun pos ->
                                                   fun str' ->
                                                     fun pos' ->
                                                       fun mn ->
                                                         let _loc_mn =
                                                           locate str pos
                                                             str' pos' in
                                                         fun _default_0 ->
                                                           Pstr_modtype
                                                             {
                                                               pmtd_name =
                                                                 (id_loc mn
                                                                    _loc_mn);
                                                               pmtd_type = mt;
                                                               pmtd_attributes
                                                                 =
                                                                 (attach_attrib
                                                                    _loc a);
                                                               pmtd_loc =
                                                                 _loc
                                                             })))));
                   Earley_core.Earley.fsequence rec_kw
                     (Earley_core.Earley.fsequence module_name
                        (Earley_core.Earley.fsequence_position
                           (Earley_core.Earley.option None
                              (Earley_core.Earley.apply (fun x -> Some x)
                                 (Earley_core.Earley.fsequence_ignore
                                    (Earley_core.Earley.string ":" ":")
                                    (Earley_core.Earley.fsequence module_type
                                       (Earley_core.Earley.empty
                                          (fun mt -> mt))))))
                           (Earley_core.Earley.fsequence_ignore
                              (Earley_core.Earley.char '=' '=')
                              (Earley_core.Earley.fsequence_position
                                 module_expr
                                 (Earley_core.Earley.fsequence
                                    (Earley_core.Earley.apply (fun f -> f [])
                                       (Earley_core.Earley.fixpoint'
                                          (fun l -> l)
                                          (Earley_core.Earley.fsequence
                                             and_kw
                                             (Earley_core.Earley.fsequence
                                                module_name
                                                (Earley_core.Earley.fsequence_position
                                                   (Earley_core.Earley.option
                                                      None
                                                      (Earley_core.Earley.apply
                                                         (fun x -> Some x)
                                                         (Earley_core.Earley.fsequence_ignore
                                                            (Earley_core.Earley.string
                                                               ":" ":")
                                                            (Earley_core.Earley.fsequence
                                                               module_type
                                                               (Earley_core.Earley.empty
                                                                  (fun mt ->
                                                                    mt))))))
                                                   (Earley_core.Earley.fsequence_ignore
                                                      (Earley_core.Earley.char
                                                         '=' '=')
                                                      (Earley_core.Earley.fsequence_position
                                                         module_expr
                                                         (Earley_core.Earley.empty
                                                            (fun str ->
                                                               fun pos ->
                                                                 fun str' ->
                                                                   fun pos'
                                                                    ->
                                                                    fun me ->
                                                                    let _loc_me
                                                                    =
                                                                    locate
                                                                    str pos
                                                                    str' pos' in
                                                                    fun str
                                                                    ->
                                                                    fun pos
                                                                    ->
                                                                    fun str'
                                                                    ->
                                                                    fun pos'
                                                                    ->
                                                                    fun mt ->
                                                                    let _loc_mt
                                                                    =
                                                                    locate
                                                                    str pos
                                                                    str' pos' in
                                                                    fun mn ->
                                                                    fun
                                                                    _default_0
                                                                    ->
                                                                    module_binding
                                                                    (merge2
                                                                    _loc_mt
                                                                    _loc_me)
                                                                    mn mt me)))))))
                                          (fun x ->
                                             fun f -> fun l -> f (x :: l))))
                                    (Earley_core.Earley.empty
                                       (fun ms ->
                                          fun str ->
                                            fun pos ->
                                              fun str' ->
                                                fun pos' ->
                                                  fun me ->
                                                    let _loc_me =
                                                      locate str pos str'
                                                        pos' in
                                                    fun str ->
                                                      fun pos ->
                                                        fun str' ->
                                                          fun pos' ->
                                                            fun mt ->
                                                              let _loc_mt =
                                                                locate str
                                                                  pos str'
                                                                  pos' in
                                                              fun mn ->
                                                                fun
                                                                  _default_0
                                                                  ->
                                                                  let m =
                                                                    module_binding
                                                                    (merge2
                                                                    _loc_mt
                                                                    _loc_me)
                                                                    mn mt me in
                                                                  Pstr_recmodule
                                                                    (m :: ms))))))));
                   Earley_core.Earley.fsequence module_name
                     (Earley_core.Earley.fsequence
                        (Earley_core.Earley.apply (fun f -> f [])
                           (Earley_core.Earley.fixpoint' (fun l -> l)
                              (Earley_core.Earley.fsequence_ignore
                                 (Earley_core.Earley.string "(" "(")
                                 (Earley_core.Earley.fsequence module_name
                                    (Earley_core.Earley.fsequence
                                       (Earley_core.Earley.option None
                                          (Earley_core.Earley.apply
                                             (fun x -> Some x)
                                             (Earley_core.Earley.fsequence_ignore
                                                (Earley_core.Earley.string
                                                   ":" ":")
                                                (Earley_core.Earley.fsequence
                                                   module_type
                                                   (Earley_core.Earley.empty
                                                      (fun mt -> mt))))))
                                       (Earley_core.Earley.fsequence_ignore
                                          (Earley_core.Earley.string ")" ")")
                                          (Earley_core.Earley.empty_pos
                                             (fun __loc__start__buf ->
                                                fun __loc__start__pos ->
                                                  fun __loc__end__buf ->
                                                    fun __loc__end__pos ->
                                                      let _loc =
                                                        locate
                                                          __loc__start__buf
                                                          __loc__start__pos
                                                          __loc__end__buf
                                                          __loc__end__pos in
                                                      fun mt ->
                                                        fun mn ->
                                                          (mn, mt, _loc)))))))
                              (fun x -> fun f -> fun l -> f (x :: l))))
                        (Earley_core.Earley.fsequence_position
                           (Earley_core.Earley.option None
                              (Earley_core.Earley.apply (fun x -> Some x)
                                 (Earley_core.Earley.fsequence_ignore
                                    (Earley_core.Earley.string ":" ":")
                                    (Earley_core.Earley.fsequence module_type
                                       (Earley_core.Earley.empty
                                          (fun mt -> mt))))))
                           (Earley_core.Earley.fsequence_ignore
                              (Earley_core.Earley.string "=" "=")
                              (Earley_core.Earley.fsequence_position
                                 module_expr
                                 (Earley_core.Earley.empty_pos
                                    (fun __loc__start__buf ->
                                       fun __loc__start__pos ->
                                         fun __loc__end__buf ->
                                           fun __loc__end__pos ->
                                             let _loc =
                                               locate __loc__start__buf
                                                 __loc__start__pos
                                                 __loc__end__buf
                                                 __loc__end__pos in
                                             fun str ->
                                               fun pos ->
                                                 fun str' ->
                                                   fun pos' ->
                                                     fun me ->
                                                       let _loc_me =
                                                         locate str pos str'
                                                           pos' in
                                                       fun str ->
                                                         fun pos ->
                                                           fun str' ->
                                                             fun pos' ->
                                                               fun mt ->
                                                                 let _loc_mt
                                                                   =
                                                                   locate str
                                                                    pos str'
                                                                    pos' in
                                                                 fun l ->
                                                                   fun mn ->
                                                                    let me =
                                                                    match mt
                                                                    with
                                                                    | 
                                                                    None ->
                                                                    me
                                                                    | 
                                                                    Some mt
                                                                    ->
                                                                    mexpr_loc
                                                                    (merge2
                                                                    _loc_mt
                                                                    _loc_me)
                                                                    (Pmod_constraint
                                                                    (me, mt)) in
                                                                    let me =
                                                                    List.fold_left
                                                                    (fun acc
                                                                    ->
                                                                    fun
                                                                    (mn, mt,
                                                                    _loc) ->
                                                                    mexpr_loc
                                                                    (merge2
                                                                    _loc
                                                                    _loc_me)
                                                                    (Pmod_functor
                                                                    (mn, mt,
                                                                    acc))) me
                                                                    (List.rev
                                                                    l) in
                                                                    Pstr_module
                                                                    (module_binding
                                                                    _loc mn
                                                                    None me)))))))])
                (Earley_core.Earley.empty_pos
                   (fun __loc__start__buf ->
                      fun __loc__start__pos ->
                        fun __loc__end__buf ->
                          fun __loc__end__pos ->
                            let _loc =
                              locate __loc__start__buf __loc__start__pos
                                __loc__end__buf __loc__end__pos in
                            fun r -> fun _default_0 -> loc_str _loc r)));
           Earley_core.Earley.fsequence open_kw
             (Earley_core.Earley.fsequence override_flag
                (Earley_core.Earley.fsequence_position module_path
                   (Earley_core.Earley.fsequence post_item_attributes
                      (Earley_core.Earley.empty_pos
                         (fun __loc__start__buf ->
                            fun __loc__start__pos ->
                              fun __loc__end__buf ->
                                fun __loc__end__pos ->
                                  let _loc =
                                    locate __loc__start__buf
                                      __loc__start__pos __loc__end__buf
                                      __loc__end__pos in
                                  fun a ->
                                    fun str ->
                                      fun pos ->
                                        fun str' ->
                                          fun pos' ->
                                            fun m ->
                                              let _loc_m =
                                                locate str pos str' pos' in
                                              fun o ->
                                                fun _default_0 ->
                                                  loc_str _loc
                                                    (Pstr_open
                                                       {
                                                         popen_lid =
                                                           (id_loc m _loc_m);
                                                         popen_override = o;
                                                         popen_loc = _loc;
                                                         popen_attributes =
                                                           (attach_attrib
                                                              _loc a)
                                                       }))))));
           Earley_core.Earley.fsequence include_kw
             (Earley_core.Earley.fsequence module_expr
                (Earley_core.Earley.fsequence post_item_attributes
                   (Earley_core.Earley.empty_pos
                      (fun __loc__start__buf ->
                         fun __loc__start__pos ->
                           fun __loc__end__buf ->
                             fun __loc__end__pos ->
                               let _loc =
                                 locate __loc__start__buf __loc__start__pos
                                   __loc__end__buf __loc__end__pos in
                               fun a ->
                                 fun me ->
                                   fun _default_0 ->
                                     loc_str _loc
                                       (Pstr_include
                                          {
                                            pincl_mod = me;
                                            pincl_loc = _loc;
                                            pincl_attributes =
                                              (attach_attrib _loc a)
                                          })))));
           Earley_core.Earley.fsequence classtype_definition
             (Earley_core.Earley.empty_pos
                (fun __loc__start__buf ->
                   fun __loc__start__pos ->
                     fun __loc__end__buf ->
                       fun __loc__end__pos ->
                         let _loc =
                           locate __loc__start__buf __loc__start__pos
                             __loc__end__buf __loc__end__pos in
                         fun ctd -> loc_str _loc (Pstr_class_type ctd)));
           Earley_core.Earley.fsequence class_definition
             (Earley_core.Earley.empty_pos
                (fun __loc__start__buf ->
                   fun __loc__start__pos ->
                     fun __loc__end__buf ->
                       fun __loc__end__pos ->
                         let _loc =
                           locate __loc__start__buf __loc__start__pos
                             __loc__end__buf __loc__end__pos in
                         fun cds -> loc_str _loc (Pstr_class cds)));
           Earley_core.Earley.fsequence floating_attribute
             (Earley_core.Earley.empty_pos
                (fun __loc__start__buf ->
                   fun __loc__start__pos ->
                     fun __loc__end__buf ->
                       fun __loc__end__pos ->
                         let _loc =
                           locate __loc__start__buf __loc__start__pos
                             __loc__end__buf __loc__end__pos in
                         fun ((s, l) as _default_0) ->
                           loc_str _loc (Pstr_attribute (s, l))));
           Earley_core.Earley.fsequence floating_extension
             (Earley_core.Earley.empty_pos
                (fun __loc__start__buf ->
                   fun __loc__start__pos ->
                     fun __loc__end__buf ->
                       fun __loc__end__pos ->
                         let _loc =
                           locate __loc__start__buf __loc__start__pos
                             __loc__end__buf __loc__end__pos in
                         fun ((s, l) as _default_0) ->
                           loc_str _loc (Pstr_extension ((s, l), []))))])
    let structure_item_aux =
      Earley_core.Earley.declare_grammar "structure_item_aux"
    let _ =
      Earley_core.Earley.set_grammar structure_item_aux
        (Earley_core.Earley.alternatives
           [Earley_core.Earley.fsequence structure_item_aux
              (Earley_core.Earley.fsequence double_semi_col
                 (Earley_core.Earley.fsequence_ignore ext_attributes
                    (Earley_core.Earley.fsequence_position expression
                       (Earley_core.Earley.empty
                          (fun str ->
                             fun pos ->
                               fun str' ->
                                 fun pos' ->
                                   fun e ->
                                     let _loc_e = locate str pos str' pos' in
                                     fun _default_0 ->
                                       fun s1 ->
                                         (loc_str _loc_e (pstr_eval e)) ::
                                         (List.rev_append (attach_str _loc_e)
                                            s1))))));
           Earley_core.Earley.fsequence_ignore ext_attributes
             (Earley_core.Earley.empty []);
           Earley_core.Earley.fsequence_ignore ext_attributes
             (Earley_core.Earley.fsequence_position expression
                (Earley_core.Earley.empty_pos
                   (fun __loc__start__buf ->
                      fun __loc__start__pos ->
                        fun __loc__end__buf ->
                          fun __loc__end__pos ->
                            let _loc =
                              locate __loc__start__buf __loc__start__pos
                                __loc__end__buf __loc__end__pos in
                            fun str ->
                              fun pos ->
                                fun str' ->
                                  fun pos' ->
                                    fun e ->
                                      let _loc_e = locate str pos str' pos' in
                                      (attach_str _loc) @
                                        [loc_str _loc_e (pstr_eval e)])));
           Earley_core.Earley.fsequence structure_item_aux
             (Earley_core.Earley.fsequence
                (Earley_core.Earley.option () double_semi_col)
                (Earley_core.Earley.fsequence_ignore ext_attributes
                   (Earley_core.Earley.fsequence
                      (Earley_core.Earley.alternatives
                         [Earley_core.Earley.fsequence_position
                            structure_item_base
                            (Earley_core.Earley.empty
                               (fun str ->
                                  fun pos ->
                                    fun str' ->
                                      fun pos' ->
                                        fun s2 ->
                                          let _loc_s2 =
                                            locate str pos str' pos' in
                                          fun s1 -> s2 ::
                                            (List.rev_append
                                               (attach_str _loc_s2) s1)));
                         Earley_core.Earley.fsequence_position
                           (alternatives extra_structure)
                           (Earley_core.Earley.empty
                              (fun str ->
                                 fun pos ->
                                   fun str' ->
                                     fun pos' ->
                                       fun e ->
                                         let _loc_e =
                                           locate str pos str' pos' in
                                         fun s1 ->
                                           List.rev_append e
                                             (List.rev_append
                                                (attach_str _loc_e) s1)))])
                      (Earley_core.Earley.empty
                         (fun f -> fun _default_0 -> fun s1 -> f s1)))))])
    let _ =
      set_grammar structure_item
        (Earley_core.Earley.fsequence structure_item_aux
           (Earley_core.Earley.fsequence
              (Earley_core.Earley.option () double_semi_col)
              (Earley_core.Earley.empty
                 (fun _default_0 -> fun l -> List.rev l))))
    let _ =
      set_grammar structure_item_simple
        (Earley_core.Earley.apply (fun f -> f [])
           (Earley_core.Earley.fixpoint' (fun l -> l) structure_item_base
              (fun x -> fun f -> fun l -> f (x :: l))))
    let signature_item_base =
      Earley_core.Earley.declare_grammar "signature_item_base"
    let _ =
      Earley_core.Earley.set_grammar signature_item_base
        (Earley_core.Earley.alternatives
           [Earley_core.Earley.fsequence (Earley_core.Earley.char '$' '$')
              (Earley_core.Earley.fsequence_ignore
                 (Earley_core.Earley.no_blank_test ())
                 (Earley_core.Earley.fsequence expression
                    (Earley_core.Earley.fsequence_ignore
                       (Earley_core.Earley.no_blank_test ())
                       (Earley_core.Earley.fsequence_ignore
                          (Earley_core.Earley.char '$' '$')
                          (Earley_core.Earley.empty_pos
                             (fun __loc__start__buf ->
                                fun __loc__start__pos ->
                                  fun __loc__end__buf ->
                                    fun __loc__end__pos ->
                                      let _loc =
                                        locate __loc__start__buf
                                          __loc__start__pos __loc__end__buf
                                          __loc__end__pos in
                                      fun e ->
                                        fun dol ->
                                          let open Quote in
                                            psig_antiquotation _loc
                                              (function
                                               | Quote_psig -> e
                                               | _ ->
                                                   failwith
                                                     "Bad antiquotation...")))))));
           Earley_core.Earley.fsequence val_kw
             (Earley_core.Earley.fsequence_position value_name
                (Earley_core.Earley.fsequence_ignore
                   (Earley_core.Earley.char ':' ':')
                   (Earley_core.Earley.fsequence typexpr
                      (Earley_core.Earley.fsequence post_item_attributes
                         (Earley_core.Earley.empty_pos
                            (fun __loc__start__buf ->
                               fun __loc__start__pos ->
                                 fun __loc__end__buf ->
                                   fun __loc__end__pos ->
                                     let _loc =
                                       locate __loc__start__buf
                                         __loc__start__pos __loc__end__buf
                                         __loc__end__pos in
                                     fun a ->
                                       fun ty ->
                                         fun str ->
                                           fun pos ->
                                             fun str' ->
                                               fun pos' ->
                                                 fun n ->
                                                   let _loc_n =
                                                     locate str pos str' pos' in
                                                   fun _default_0 ->
                                                     loc_sig _loc
                                                       (psig_value
                                                          ~attributes:(
                                                          attach_attrib _loc
                                                            a) _loc
                                                          (id_loc n _loc_n)
                                                          ty [])))))));
           Earley_core.Earley.fsequence external_kw
             (Earley_core.Earley.fsequence_position value_name
                (Earley_core.Earley.fsequence_ignore
                   (Earley_core.Earley.string ":" ":")
                   (Earley_core.Earley.fsequence typexpr
                      (Earley_core.Earley.fsequence_ignore
                         (Earley_core.Earley.string "=" "=")
                         (Earley_core.Earley.fsequence
                            (Earley_core.Earley.apply (fun f -> f [])
                               (Earley_core.Earley.fixpoint' (fun l -> l)
                                  string_litteral
                                  (fun x -> fun f -> fun l -> f (x :: l))))
                            (Earley_core.Earley.fsequence
                               post_item_attributes
                               (Earley_core.Earley.empty_pos
                                  (fun __loc__start__buf ->
                                     fun __loc__start__pos ->
                                       fun __loc__end__buf ->
                                         fun __loc__end__pos ->
                                           let _loc =
                                             locate __loc__start__buf
                                               __loc__start__pos
                                               __loc__end__buf
                                               __loc__end__pos in
                                           fun a ->
                                             fun ls ->
                                               fun ty ->
                                                 fun str ->
                                                   fun pos ->
                                                     fun str' ->
                                                       fun pos' ->
                                                         fun n ->
                                                           let _loc_n =
                                                             locate str pos
                                                               str' pos' in
                                                           fun _default_0 ->
                                                             let ls =
                                                               List.map fst
                                                                 ls in
                                                             let l =
                                                               List.length ls in
                                                             if
                                                               (l < 1) ||
                                                                 (l > 3)
                                                             then give_up ();
                                                             loc_sig _loc
                                                               (psig_value
                                                                  ~attributes:(
                                                                  attach_attrib
                                                                    _loc a)
                                                                  _loc
                                                                  (id_loc n
                                                                    _loc_n)
                                                                  ty ls)))))))));
           Earley_core.Earley.fsequence type_definition
             (Earley_core.Earley.empty_pos
                (fun __loc__start__buf ->
                   fun __loc__start__pos ->
                     fun __loc__end__buf ->
                       fun __loc__end__pos ->
                         let _loc =
                           locate __loc__start__buf __loc__start__pos
                             __loc__end__buf __loc__end__pos in
                         fun td -> Sig.type_ ~loc:_loc Recursive td));
           Earley_core.Earley.fsequence type_extension
             (Earley_core.Earley.empty_pos
                (fun __loc__start__buf ->
                   fun __loc__start__pos ->
                     fun __loc__end__buf ->
                       fun __loc__end__pos ->
                         let _loc =
                           locate __loc__start__buf __loc__start__pos
                             __loc__end__buf __loc__end__pos in
                         fun te -> Sig.type_extension ~loc:_loc te));
           Earley_core.Earley.fsequence exception_kw
             (Earley_core.Earley.fsequence (constr_decl false)
                (Earley_core.Earley.empty_pos
                   (fun __loc__start__buf ->
                      fun __loc__start__pos ->
                        fun __loc__end__buf ->
                          fun __loc__end__pos ->
                            let _loc =
                              locate __loc__start__buf __loc__start__pos
                                __loc__end__buf __loc__end__pos in
                            fun ((name, args, res, a) as _default_0) ->
                              fun _default_1 ->
                                let cd =
                                  Te.decl ~attrs:(attach_attrib _loc a)
                                    ~loc:_loc ~args ?res name in
                                loc_sig _loc (Psig_exception cd))));
           Earley_core.Earley.fsequence module_kw
             (Earley_core.Earley.fsequence rec_kw
                (Earley_core.Earley.fsequence_position module_name
                   (Earley_core.Earley.fsequence_ignore
                      (Earley_core.Earley.string ":" ":")
                      (Earley_core.Earley.fsequence module_type
                         (Earley_core.Earley.fsequence_position
                            post_item_attributes
                            (Earley_core.Earley.fsequence
                               (Earley_core.Earley.apply (fun f -> f [])
                                  (Earley_core.Earley.fixpoint' (fun l -> l)
                                     (Earley_core.Earley.fsequence and_kw
                                        (Earley_core.Earley.fsequence
                                           module_name
                                           (Earley_core.Earley.fsequence_ignore
                                              (Earley_core.Earley.string ":"
                                                 ":")
                                              (Earley_core.Earley.fsequence
                                                 module_type
                                                 (Earley_core.Earley.fsequence
                                                    post_item_attributes
                                                    (Earley_core.Earley.empty_pos
                                                       (fun __loc__start__buf
                                                          ->
                                                          fun
                                                            __loc__start__pos
                                                            ->
                                                            fun
                                                              __loc__end__buf
                                                              ->
                                                              fun
                                                                __loc__end__pos
                                                                ->
                                                                let _loc =
                                                                  locate
                                                                    __loc__start__buf
                                                                    __loc__start__pos
                                                                    __loc__end__buf
                                                                    __loc__end__pos in
                                                                fun a ->
                                                                  fun mt ->
                                                                    fun mn ->
                                                                    fun
                                                                    _default_0
                                                                    ->
                                                                    module_declaration
                                                                    ~attributes:(
                                                                    attach_attrib
                                                                    _loc a)
                                                                    _loc mn
                                                                    mt)))))))
                                     (fun x -> fun f -> fun l -> f (x :: l))))
                               (Earley_core.Earley.empty_pos
                                  (fun __loc__start__buf ->
                                     fun __loc__start__pos ->
                                       fun __loc__end__buf ->
                                         fun __loc__end__pos ->
                                           let _loc =
                                             locate __loc__start__buf
                                               __loc__start__pos
                                               __loc__end__buf
                                               __loc__end__pos in
                                           fun ms ->
                                             fun str ->
                                               fun pos ->
                                                 fun str' ->
                                                   fun pos' ->
                                                     fun a ->
                                                       let _loc_a =
                                                         locate str pos str'
                                                           pos' in
                                                       fun mt ->
                                                         fun str ->
                                                           fun pos ->
                                                             fun str' ->
                                                               fun pos' ->
                                                                 fun mn ->
                                                                   let _loc_mn
                                                                    =
                                                                    locate
                                                                    str pos
                                                                    str' pos' in
                                                                   fun
                                                                    _default_0
                                                                    ->
                                                                    fun
                                                                    _default_1
                                                                    ->
                                                                    let loc_first
                                                                    =
                                                                    merge2
                                                                    _loc_mn
                                                                    _loc_a in
                                                                    let m =
                                                                    module_declaration
                                                                    ~attributes:(
                                                                    attach_attrib
                                                                    loc_first
                                                                    a)
                                                                    loc_first
                                                                    mn mt in
                                                                    loc_sig
                                                                    _loc
                                                                    (Psig_recmodule
                                                                    (m :: ms))))))))));
           Earley_core.Earley.fsequence module_kw
             (Earley_core.Earley.fsequence
                (Earley_core.Earley.alternatives
                   [Earley_core.Earley.fsequence type_kw
                      (Earley_core.Earley.fsequence_position modtype_name
                         (Earley_core.Earley.fsequence
                            (Earley_core.Earley.option None
                               (Earley_core.Earley.apply (fun x -> Some x)
                                  (Earley_core.Earley.fsequence_ignore
                                     (Earley_core.Earley.string "=" "=")
                                     (Earley_core.Earley.fsequence
                                        module_type
                                        (Earley_core.Earley.empty
                                           (fun mt -> mt))))))
                            (Earley_core.Earley.fsequence
                               post_item_attributes
                               (Earley_core.Earley.empty_pos
                                  (fun __loc__start__buf ->
                                     fun __loc__start__pos ->
                                       fun __loc__end__buf ->
                                         fun __loc__end__pos ->
                                           let _loc =
                                             locate __loc__start__buf
                                               __loc__start__pos
                                               __loc__end__buf
                                               __loc__end__pos in
                                           fun a ->
                                             fun mt ->
                                               fun str ->
                                                 fun pos ->
                                                   fun str' ->
                                                     fun pos' ->
                                                       fun mn ->
                                                         let _loc_mn =
                                                           locate str pos
                                                             str' pos' in
                                                         fun _default_0 ->
                                                           Psig_modtype
                                                             {
                                                               pmtd_name =
                                                                 (id_loc mn
                                                                    _loc_mn);
                                                               pmtd_type = mt;
                                                               pmtd_attributes
                                                                 =
                                                                 (attach_attrib
                                                                    _loc a);
                                                               pmtd_loc =
                                                                 _loc
                                                             })))));
                   Earley_core.Earley.fsequence module_name
                     (Earley_core.Earley.fsequence
                        (Earley_core.Earley.apply (fun f -> f [])
                           (Earley_core.Earley.fixpoint' (fun l -> l)
                              (Earley_core.Earley.fsequence_ignore
                                 (Earley_core.Earley.char '(' '(')
                                 (Earley_core.Earley.fsequence module_name
                                    (Earley_core.Earley.fsequence
                                       (Earley_core.Earley.option None
                                          (Earley_core.Earley.apply
                                             (fun x -> Some x)
                                             (Earley_core.Earley.fsequence_ignore
                                                (Earley_core.Earley.char ':'
                                                   ':')
                                                (Earley_core.Earley.fsequence
                                                   module_type
                                                   (Earley_core.Earley.empty
                                                      (fun mt -> mt))))))
                                       (Earley_core.Earley.fsequence_ignore
                                          (Earley_core.Earley.char ')' ')')
                                          (Earley_core.Earley.empty_pos
                                             (fun __loc__start__buf ->
                                                fun __loc__start__pos ->
                                                  fun __loc__end__buf ->
                                                    fun __loc__end__pos ->
                                                      let _loc =
                                                        locate
                                                          __loc__start__buf
                                                          __loc__start__pos
                                                          __loc__end__buf
                                                          __loc__end__pos in
                                                      fun mt ->
                                                        fun mn ->
                                                          (mn, mt, _loc)))))))
                              (fun x -> fun f -> fun l -> f (x :: l))))
                        (Earley_core.Earley.fsequence_ignore
                           (Earley_core.Earley.char ':' ':')
                           (Earley_core.Earley.fsequence_position module_type
                              (Earley_core.Earley.fsequence
                                 post_item_attributes
                                 (Earley_core.Earley.empty_pos
                                    (fun __loc__start__buf ->
                                       fun __loc__start__pos ->
                                         fun __loc__end__buf ->
                                           fun __loc__end__pos ->
                                             let _loc =
                                               locate __loc__start__buf
                                                 __loc__start__pos
                                                 __loc__end__buf
                                                 __loc__end__pos in
                                             fun a ->
                                               fun str ->
                                                 fun pos ->
                                                   fun str' ->
                                                     fun pos' ->
                                                       fun mt ->
                                                         let _loc_mt =
                                                           locate str pos
                                                             str' pos' in
                                                         fun l ->
                                                           fun mn ->
                                                             let mt =
                                                               List.fold_left
                                                                 (fun acc ->
                                                                    fun
                                                                    (mn, mt,
                                                                    _l) ->
                                                                    mtyp_loc
                                                                    (merge2
                                                                    _l
                                                                    _loc_mt)
                                                                    (Pmty_functor
                                                                    (mn, mt,
                                                                    acc))) mt
                                                                 (List.rev l) in
                                                             Psig_module
                                                               (module_declaration
                                                                  ~attributes:(
                                                                  attach_attrib
                                                                    _loc a)
                                                                  _loc mn mt)))))))])
                (Earley_core.Earley.empty_pos
                   (fun __loc__start__buf ->
                      fun __loc__start__pos ->
                        fun __loc__end__buf ->
                          fun __loc__end__pos ->
                            let _loc =
                              locate __loc__start__buf __loc__start__pos
                                __loc__end__buf __loc__end__pos in
                            fun r -> fun _default_0 -> loc_sig _loc r)));
           Earley_core.Earley.fsequence open_kw
             (Earley_core.Earley.fsequence override_flag
                (Earley_core.Earley.fsequence_position module_path
                   (Earley_core.Earley.fsequence post_item_attributes
                      (Earley_core.Earley.empty_pos
                         (fun __loc__start__buf ->
                            fun __loc__start__pos ->
                              fun __loc__end__buf ->
                                fun __loc__end__pos ->
                                  let _loc =
                                    locate __loc__start__buf
                                      __loc__start__pos __loc__end__buf
                                      __loc__end__pos in
                                  fun a ->
                                    fun str ->
                                      fun pos ->
                                        fun str' ->
                                          fun pos' ->
                                            fun m ->
                                              let _loc_m =
                                                locate str pos str' pos' in
                                              fun o ->
                                                fun _default_0 ->
                                                  loc_sig _loc
                                                    (Psig_open
                                                       {
                                                         popen_lid =
                                                           (id_loc m _loc_m);
                                                         popen_override = o;
                                                         popen_loc = _loc;
                                                         popen_attributes =
                                                           (attach_attrib
                                                              _loc a)
                                                       }))))));
           Earley_core.Earley.fsequence include_kw
             (Earley_core.Earley.fsequence module_type
                (Earley_core.Earley.fsequence post_item_attributes
                   (Earley_core.Earley.empty_pos
                      (fun __loc__start__buf ->
                         fun __loc__start__pos ->
                           fun __loc__end__buf ->
                             fun __loc__end__pos ->
                               let _loc =
                                 locate __loc__start__buf __loc__start__pos
                                   __loc__end__buf __loc__end__pos in
                               fun a ->
                                 fun me ->
                                   fun _default_0 ->
                                     loc_sig _loc
                                       (Psig_include
                                          {
                                            pincl_mod = me;
                                            pincl_loc = _loc;
                                            pincl_attributes =
                                              (attach_attrib _loc a)
                                          })))));
           Earley_core.Earley.fsequence classtype_definition
             (Earley_core.Earley.empty_pos
                (fun __loc__start__buf ->
                   fun __loc__start__pos ->
                     fun __loc__end__buf ->
                       fun __loc__end__pos ->
                         let _loc =
                           locate __loc__start__buf __loc__start__pos
                             __loc__end__buf __loc__end__pos in
                         fun ctd -> loc_sig _loc (Psig_class_type ctd)));
           Earley_core.Earley.fsequence class_specification
             (Earley_core.Earley.empty_pos
                (fun __loc__start__buf ->
                   fun __loc__start__pos ->
                     fun __loc__end__buf ->
                       fun __loc__end__pos ->
                         let _loc =
                           locate __loc__start__buf __loc__start__pos
                             __loc__end__buf __loc__end__pos in
                         fun cs -> loc_sig _loc (Psig_class cs)));
           Earley_core.Earley.fsequence floating_attribute
             (Earley_core.Earley.empty_pos
                (fun __loc__start__buf ->
                   fun __loc__start__pos ->
                     fun __loc__end__buf ->
                       fun __loc__end__pos ->
                         let _loc =
                           locate __loc__start__buf __loc__start__pos
                             __loc__end__buf __loc__end__pos in
                         fun ((s, l) as _default_0) ->
                           loc_sig _loc (Psig_attribute (s, l))));
           Earley_core.Earley.fsequence floating_extension
             (Earley_core.Earley.empty_pos
                (fun __loc__start__buf ->
                   fun __loc__start__pos ->
                     fun __loc__end__buf ->
                       fun __loc__end__pos ->
                         let _loc =
                           locate __loc__start__buf __loc__start__pos
                             __loc__end__buf __loc__end__pos in
                         fun ((s, l) as _default_0) ->
                           loc_sig _loc (Psig_extension ((s, l), []))))])
    let _ =
      set_grammar signature_item
        (Earley_core.Earley.alternatives
           [Earley_core.Earley.fsequence signature_item_base
              (Earley_core.Earley.fsequence_ignore
                 (Earley_core.Earley.option None
                    (Earley_core.Earley.apply (fun x -> Some x)
                       double_semi_col))
                 (Earley_core.Earley.empty_pos
                    (fun __loc__start__buf ->
                       fun __loc__start__pos ->
                         fun __loc__end__buf ->
                           fun __loc__end__pos ->
                             let _loc =
                               locate __loc__start__buf __loc__start__pos
                                 __loc__end__buf __loc__end__pos in
                             fun s -> (attach_sig _loc) @ [s])));
           Earley_core.Earley.fsequence (alternatives extra_signature)
             (Earley_core.Earley.empty_pos
                (fun __loc__start__buf ->
                   fun __loc__start__pos ->
                     fun __loc__end__buf ->
                       fun __loc__end__pos ->
                         let _loc =
                           locate __loc__start__buf __loc__start__pos
                             __loc__end__buf __loc__end__pos in
                         fun e -> (attach_sig _loc) @ e))])
  end

(*
  ======================================================================
  Copyright Christophe Raffalli & Rodolphe Lepigre
  LAMA, UMR 5127 - Université Savoie Mont Blanc

  christophe.raffalli@univ-savoie.fr
  rodolphe.lepigre@univ-savoie.fr

  This software contains implements a parser combinator library together
  with a syntax extension mechanism for the OCaml language.  It  can  be
  used to write parsers using a BNF-like format through a syntax extens-
  ion called pa_parser.

  This software is governed by the CeCILL-B license under French law and
  abiding by the rules of distribution of free software.  You  can  use,
  modify and/or redistribute it under the terms of the CeCILL-B  license
  as circulated by CEA, CNRS and INRIA at the following URL:

            http://www.cecill.info

  The exercising of this freedom is conditional upon a strong obligation
  of giving credits for everybody that distributes a software incorpora-
  ting a software ruled by the current license so as  all  contributions
  to be properly identified and acknowledged.

  As a counterpart to the access to the source code and rights to  copy,
  modify and redistribute granted by the  license,  users  are  provided
  only with a limited warranty and the software's author, the holder  of
  the economic rights, and the successive licensors  have  only  limited
  liability.

  In this respect, the user's attention is drawn to the risks associated
  with loading, using, modifying and/or developing  or  reproducing  the
  software by the user in light of its specific status of free software,
  that may mean that it is complicated  to  manipulate,  and  that  also
  therefore means that it is reserved  for  developers  and  experienced
  professionals having in-depth computer knowledge. Users are  therefore
  encouraged to load and test  the  software's  suitability  as  regards
  their requirements in conditions enabling the security of  their  sys-
  tems and/or data to be ensured and, more generally, to use and operate
  it in the same conditions as regards security.

  The fact that you are presently reading this means that you  have  had
  knowledge of the CeCILL-B license and that you accept its terms.
  ======================================================================
*)

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
open Ast_helper
include Pa_ocaml_prelude

#define LOCATE locate

module Make(Initial : Extension) = struct

include Initial

let ouident = uident
let parser uident   =
  | ouident

let olident = lident
let parser lident   =
  | olident

let oident = ident
let parser ident   =
  | oident

let mk_unary_op loc name loc_name arg =
  match name, arg.pexp_desc with
  | "-", Pexp_constant(Pconst_integer(n,o)) ->
     Exp.constant ~loc (Const.integer ?suffix:o ("-"^n))
  | ("-" | "-."), Pexp_constant(Pconst_float(f,o)) ->
     Exp.constant ~loc (Const.float ?suffix:o ("-" ^ f))
  | "+", Pexp_constant(Pconst_integer _)
  | ("+" | "+."), Pexp_constant(Pconst_float _) ->
     Exp.mk ~loc arg.pexp_desc
  | ("-" | "-." | "+" | "+."), _ ->
     let lid = id_loc (Lident ("~" ^ name)) loc_name in
     let fn = Exp.ident ~loc:loc_name lid in
     Exp.apply ~loc fn [nolabel, arg]
  | _ ->
     let lid = id_loc (Lident name) loc_name in
     let fn = Exp.ident ~loc:loc_name lid in
     Exp.apply ~loc fn [nolabel, arg]

let mk_binary_op loc e' op loc_op e =
  if op = "::" then
    let lid = id_loc (Lident "::") loc_op in
    Exp.construct ~loc lid (Some (Exp.tuple ~loc:(ghost loc) [e';e]))
  else
    let id = Exp.ident ~loc:loc_op (id_loc (Lident op) loc_op) in
    Exp.apply ~loc id [(nolabel, e') ; (nolabel, e)]

let wrap_type_annotation loc types core_type body =
  let exp = Exp.constraint_ ~loc body core_type in
  let exp =
    List.fold_right (fun ty exp -> Exp.newtype ~loc ty exp) types exp
  in
  (exp, Typ.poly ~loc:(ghost loc) types (Typ.varify_constructors types core_type))

type tree = Node of tree * tree | Leaf of string

let string_of_tree (t:tree) : string =
  let b = Buffer.create 101 in
  let rec fn = function
      Leaf s -> Buffer.add_string b s
    | Node(a,b) -> fn a; fn b
  in
  fn t;
  Buffer.contents b

(* Naming labels *)
let label_name = lident

let parser ty_label =
  | '~' - s:lident ':' -> labelled s

let parser ty_opt_label =
  | '?' - s:lident ':' -> optional s

let parser maybe_opt_label =
  | o:'?'? ln:label_name ->
      (if o = None then labelled ln else (optional ln))

(****************************************************************************
 * Names                                                                    *
 ****************************************************************************)

(* FIXME: add antiquotations for all as done for field_name *)

let operator_name =
  alternatives (prefix_symbol Prefix :: List.map infix_symbol infix_prios)

let parser value_name =
  | id:lident -> id
  | '(' op:operator_name ')' -> op

let constr_name     = uident
let parser tag_name = STR("`") c:ident -> c

let typeconstr_name = lident
let field_name      = lident

let smodule_name       = uident
let parser module_name =
  | u:uident -> id_loc (Some u) _loc
  | joker_kw -> id_loc None _loc

let modtype_name       = ident
let class_name         = lident
let inst_var_name      = lident
let parser method_name = id:lident -> id_loc id _loc

let module_path_gen, set_module_path_gen  = grammar_family "module_path_gen"
let module_path_suit, set_module_path_suit  = grammar_family "module_path_suit"

let parser module_path_suit_aux allow_app =
  | STR("(") m':(module_path_gen true) STR(")") when allow_app ->
      (fun a -> Lapply(a, m'))
  | STR(".") m:smodule_name ->
      (fun acc -> Ldot(acc, m))

let _ = set_module_path_suit (fun allow_app ->
    parser
      f:(module_path_suit_aux allow_app) g:(module_path_suit allow_app) -> (fun acc -> g (f acc))
    | EMPTY -> (fun acc -> acc)
    )

let _ = set_module_path_gen (fun allow_app ->
  parser
  | m:smodule_name s:(module_path_suit allow_app) -> s (Lident m)
  )

let module_path = module_path_gen false
let extended_module_path = module_path_gen true

let _ = set_grammar value_path (
  parser
  | mp:{m:module_path STR(".")}? vn:value_name ->
      (match mp with
       | None   -> Lident vn
       | Some p -> Ldot(p, vn)))

let parser constr =
  | mp:{m:module_path STR(".")}? cn:constr_name ->
      (match mp with
       | None   -> Lident cn
       | Some p -> Ldot(p, cn))

let parser typeconstr =
  | mp:{m:extended_module_path STR(".")}? tcn:typeconstr_name ->
      (match mp with
       | None   -> Lident tcn
       | Some p -> Ldot(p, tcn))

let parser field =
  | mp:{m:module_path STR(".")}? fn:field_name ->
      (match mp with
       | None   -> Lident fn
       | Some p -> Ldot(p, fn))

let parser class_path =
  | mp:{m:module_path STR(".")}? cn:class_name ->
      (match mp with
       | None   -> Lident cn
       | Some p -> Ldot(p, cn))

let parser modtype_path =
  | mp:{m:extended_module_path STR(".")}? mtn:modtype_name ->
      (match mp with
       | None   -> Lident mtn
       | Some p -> Ldot(p, mtn))

let parser classtype_path =
  | mp:{m:extended_module_path STR(".")}? cn:class_name ->
      (match mp with
       | None   -> Lident cn
       | Some p -> Ldot(p, cn))

let parser opt_variance =
  | v:RE("[+-]")? ->
      (match v with
       | None     -> Invariant
       | Some "+" -> Covariant
       | Some "-" -> Contravariant
       | _        -> assert false)

let parser override_flag =
    o:STR("!")? -> (if o <> None then Override else Fresh)

let parser attr_id =
  | id:ident l:{ '.' id:ident}* ->
      id_loc (String.concat "." (id::l)) _loc

let parser payload =
  | s:structure -> PStr(s)
  | ':' t:typexpr -> PTyp(t)
  | '?' p:pattern e:{_:when_kw e:expression}? -> PPat(p,e)

let parser attribute =
  | "[@" id:attr_id p:payload ']' -> Attr.mk ~loc:_loc id p

let parser attributes =
  | {a:attribute}*

let parser ext_attributes =
  | a:{'%' a:attribute}? l:attributes -> a, l

let parser post_item_attributes =
  | l:{"[@@" id:attr_id p:payload ']' -> Attr.mk ~loc:_loc id p}*

let parser floating_attribute =
  | "[@@@" id:attr_id p:payload ']' -> Attr.mk ~loc:_loc id p

let parser extension =
  | "[%" id:attr_id p:payload ']' -> Attr.mk ~loc:_loc id p

let parser floating_extension =
  | "[%%" id:attr_id p:payload ']'

(****************************************************************************
 * Type expressions                                                         *
 ****************************************************************************)
let parser only_poly_typexpr =
  | ids:{'\'' id:ident -> id_loc id _loc}+ '.' te:typexpr ->
      Typ.poly ~loc:_loc ids te

let parser poly_typexpr =
  | ids:{'\'' id:ident -> id_loc id _loc}+ '.' te:typexpr ->
      Typ.poly ~loc:_loc ids te
  | te:typexpr ->
       te

let parser poly_syntax_typexpr =
  | type_kw ids:{id:typeconstr_name -> id_loc id _loc_id}+ '.' te:typexpr ->
       (ids, te)

let parser method_type =
  | mn:method_name ':' pte:poly_typexpr        -> Of.tag ~loc:_loc mn pte
  | ty:(typexpr_lvl (next_type_prio DashType)) -> Of.inherit_ ~loc:_loc ty

let parser tag_spec =
  | tn:tag_name te:{_:of_kw '&'? typexpr}? ->
      let amp,t = match te with
              | None   -> true, []
              | Some (amp,l) -> amp<>None, [l]
      in
      let tn = id_loc tn _loc_tn in
      Rf.tag ~loc:_loc tn amp t

  | te:typexpr ->
      Rf.inherit_ ~loc:_loc te

let parser tag_spec_first =
  | tn:tag_name te:{_:of_kw '&'? typexpr}? ->
      let amp,t = match te with
              | None   -> true,[]
              | Some (amp,l) -> amp<>None, [l]
      in
      let tn = id_loc tn _loc_tn in
      [Rf.tag ~loc:_loc tn amp t]
  | te:typexpr? '|' ts:tag_spec ->
      match te with
      | None    -> [ts]
      | Some te -> [Rf.inherit_ ~loc:_loc te; ts]

let parser tag_spec_full =
  | tn:tag_name (amp,tes):{of_kw amp:'&'? te:typexpr
    tes:{'&' te:typexpr}* -> (amp<>None,(te::tes))}?[true,[]] ->
      let tn = id_loc tn _loc_tn in
      Rf.tag ~loc:_loc tn amp tes
  | te:typexpr ->
      Rf.inherit_ ~loc:_loc te

let parser polymorphic_variant_type : core_type grammar =
  | '[' tsf:tag_spec_first tss:{'|' ts:tag_spec}* ']' ->
      Typ.variant ~loc:_loc (tsf @ tss) Closed None
  | "[>" ts:tag_spec? tss:{'|' ts:tag_spec}* ']' ->
      let tss = match ts with
                | None    -> tss
                | Some ts -> ts :: tss
      in
      Typ.variant ~loc:_loc tss Open None
  | "[<" '|'? tfs:tag_spec_full tfss:{'|' tsf:tag_spec_full}*
         tns:{'>' tns:tag_name+}?[[]] ']' ->
      Typ.variant ~loc:_loc (tfs :: tfss) Closed (Some tns)

let parser package_constraint =
  | type_kw tc:typeconstr '=' te:typexpr ->
      let tc = id_loc tc _loc_tc in
      (tc, te)

let parser package_type =
  | mtp:modtype_path
          cs:{with_kw pc:package_constraint
                      pcs:{_:and_kw package_constraint}* -> (pc::pcs)}?[[]]
    -> Typ.package ~loc:_loc (id_loc mtp _loc_mtp) cs

let parser opt_present =
  | STR("[>") l:tag_name+ STR("]") -> l
  | EMPTY -> []

let mkoption loc d =
  let loc = ghost loc in
  Typ.constr ~loc (id_loc (Ldot (Lident "*predef*", "option")) loc) [d]

let extra_types_grammar lvl =
  (alternatives (List.map (fun g -> g lvl) extra_types))

let op_cl = parser d:".." -> Open | EMPTY -> Closed

let _ = set_typexpr_lvl (fun @(allow_par, lvl) ->
  parser
  | [@unshared] e:(extra_types_grammar lvl)
                         -> e
  | "'" id:ident
    -> Typ.var ~loc:_loc id

  | joker_kw
    -> Typ.any ~loc:_loc ()

  | '(' module_kw pt:package_type ')'
    -> loc_typ _loc pt.ptyp_desc

  | '(' te:typexpr at:attribute* ')'
       when allow_par
    -> { te with ptyp_attributes = at }

  | ln:ty_opt_label te:(typexpr_lvl ProdType) arrow_re te':(typexpr_lvl Arr)
       when lvl <= Arr
    -> Typ.arrow ~loc:_loc ln te te'

  | ln:label_name ':' te:(typexpr_lvl ProdType) arrow_re te':(typexpr_lvl Arr)
       when lvl <= Arr
    -> Typ.arrow ~loc:_loc (labelled ln) te te'

  | te:(typexpr_lvl ProdType) arrow_re te':(typexpr_lvl Arr)
       when lvl <= Arr
    -> Typ.arrow ~loc:_loc nolabel te te'

  | tc:typeconstr
    -> Typ.constr ~loc:_loc (id_loc tc _loc_tc) []

  | '(' te:typexpr tes:{',' te:typexpr}+ ')' tc:typeconstr
       when lvl <= AppType
    -> Typ.constr ~loc:_loc (id_loc tc _loc_tc) (te::tes)

  | t:(typexpr_lvl AppType) tc:typeconstr
       when lvl <= AppType
    -> Typ.constr ~loc:_loc (id_loc tc _loc_tc) [t]

  | pvt:polymorphic_variant_type
    -> pvt

  | '<' rv:op_cl '>'
    -> Typ.object_ ~loc:_loc [] rv

  | '<' mts:(Earley.list1 method_type semi_col) rv:{_:semi_col op_cl}?[Closed] '>'
    -> Typ.object_ ~loc:_loc mts rv

  | '#' cp:class_path
    -> Typ.class_ ~loc:_loc (id_loc cp _loc_cp) []

  | te:(typexpr_lvl DashType) '#' cp:class_path
       when lvl <= DashType
    -> Typ.class_ ~loc:_loc (id_loc cp _loc_cp) [te]

  | '(' te:typexpr tes:{',' te:typexpr}* ')' '#' cp:class_path
    -> Typ.class_ ~loc:_loc (id_loc cp _loc_cp) (te::tes)

  | tes:(Earley.list2 (typexpr_lvl DashType) (parser {'*' | "×"}))
       when lvl <= ProdType
    -> Typ.tuple ~loc:_loc tes

  | te:(typexpr_lvl As) as_kw '\'' id:ident
       when lvl <= As
    -> Typ.alias ~loc:_loc te id
)

(****************************************************************************
 * Type and exception definitions                                           *
 ****************************************************************************)

(* Type definition *)
let parser type_param =
  | var:opt_variance id:{_:'\'' ident} ->
      (Name (id_loc id _loc_id), var)
  | var:opt_variance j:'_' ->
      (Joker _loc_j, var)

let parser type_params =
  | EMPTY -> []
  | tp:type_param -> [tp]
  | '(' tp:type_param tps:{',' tp:type_param -> tp}* ')' ->
      tp::tps

let parser type_equation =
  | '=' p:private_flag te:typexpr -> (p,te)

let parser type_constraint =
  | constraint_kw id:{_:'\'' ident} '=' te:typexpr ->
      (Typ.var ~loc:_loc_id id, te, merge2 _loc_id _loc)

let parser constr_name2 =
  | cn:constr_name    -> cn
  | '(' ')' -> "()"

let parser bar with_bar =
  | EMPTY when not with_bar
  | '|'

(** FIXME OCAML: the bar is included in position *)
let parser constr_decl with_bar =
  | (bar with_bar) cn:constr_name2
    (args,res):{ te:{_:of_kw { _:'(' te:typexpr _:')' -> (te,true)
                   | te:typexpr_nopar -> (te,false) }}? ->
               let tes =
                 match te with
                 | None   -> []
                 | Some ({ ptyp_desc = Ptyp_tuple tes }, false) -> tes
                 | Some (t,_) -> [t]
               in
               (Pcstr_tuple tes, None)
             | of_kw '{' fds:field_decl_list '}' -> (Pcstr_record fds, None)
             | ':' tes:{te:(typexpr_lvl (next_type_prio ProdType))
                        tes:{_:'*' (typexpr_lvl (next_type_prio ProdType))}*
                        arrow_re -> (te::tes)}?[[]]
                   te:(typexpr_lvl (next_type_prio Arr)) ->
                (Pcstr_tuple tes, Some te)
             | ':' '{' fds:field_decl_list '}' arrow_re
                   te:(typexpr_lvl (next_type_prio Arr)) ->
                (Pcstr_record fds, Some te)
             } a:post_item_attributes
        -> let name = id_loc cn _loc_cn in
           (name,args,res,a)

let parser type_constr_decl with_bar =
  (name,args,res,a):(constr_decl with_bar) ->
     Type.constructor ~attrs:(attach_attrib _loc a) ~loc:_loc ~args ?res name

let parser type_constr_extn with_bar =
  | (name,args,res,a):(constr_decl with_bar) ->
     Te.decl ~attrs:(attach_attrib _loc a) ~loc:_loc ~args ?res name
  | li:lident '=' cn:constr a:post_item_attributes ->
     Te.rebind ~attrs:(attach_attrib _loc a) ~loc:_loc (id_loc li _loc_li)
                                                       (id_loc cn _loc_cn)

let _ = set_grammar constr_decl_list (
  parser
  | cd:(type_constr_decl false) cds:(type_constr_decl true)* -> cd::cds
  | EMPTY -> []
  )

let parser constr_extn_list =
  | cd:(type_constr_extn false) cds:(type_constr_extn true)* -> cd::cds
  | EMPTY -> []

(* NOTE: OCaml includes the semi column in the position *)
let parser field_decl_semi =
  | m:mutable_flag fn:field_name STR(":") pte:poly_typexpr semi_col ->
     label_declaration ~attributes:(attach_attrib _loc [])
                       _loc (id_loc fn _loc_fn) m pte

let parser field_decl =
  | m:mutable_flag fn:field_name STR(":") pte:poly_typexpr ->
     label_declaration ~attributes:(attach_attrib _loc [])
                       _loc (id_loc fn _loc_fn) m pte

let parser field_decl_aux =
  | EMPTY -> []
  | fs:field_decl_aux fd:field_decl_semi -> fd::fs

let _ = set_grammar field_decl_list (
  parser
    | fs:field_decl_aux -> List.rev fs
    | fs:field_decl_aux fd:field_decl -> List.rev (fd::fs)
  )

let parser type_representation =
  | STR("{") fds:field_decl_list STR("}") -> Ptype_record fds
  | cds:constr_decl_list -> if cds = [] then give_up (); Ptype_variant (cds)
  | ".." -> Ptype_open

let parser type_information =
  | te:type_equation? ptr:{CHR('=') pri:private_flag tr:type_representation}?
    cstrs:type_constraint* ->
      let pri, tkind =
        match ptr with
        | None   -> (Public, Ptype_abstract)
        | Some c -> c
      in
      (pri, te, tkind, cstrs)

let typedef_gen = (fun att constr filter ->
  parser
  | tps:type_params tcn:constr ti:type_information
            a:{post_item_attributes when att | EMPTY when not att -> []}
    -> (fun prev_loc ->
      let _loc = match
           (prev_loc:Location.t option) with None -> _loc
        | Some l -> merge2 l _loc
      in
      let (pri, te, tkind, cstrs) = ti in
      let pri, te = match te with
          None -> pri, None
        | Some(Private, te) ->
           if pri = Private then give_up (); (* ty = private ty' = private A | B is not legal *)
           Private, Some te
        | Some(_, te) -> pri, Some te
      in
      id_loc tcn _loc_tcn,
      type_declaration ~attributes:(if att then attach_attrib _loc a else [])
        _loc (id_loc (filter tcn) _loc_tcn) tps cstrs tkind pri te)
  )

let parser type_extension =
  type_kw params:type_params tcn:typeconstr "+=" priv:private_flag
     cds:constr_extn_list attrs:post_item_attributes ->
        let tcn = id_loc tcn _loc_tcn in
        let params = params_map params in
        Te.mk ~attrs ~params ~priv tcn cds

let parser typedef = (typedef_gen true typeconstr_name (fun x -> x))
let parser typedef_in_constraint = (typedef_gen false typeconstr Longident.last)

let parser type_definition =
  | l:type_kw td:typedef tds:{l:and_kw td:typedef -> snd (td (Some _loc_l))}* ->
                             snd (td (Some _loc_l))::tds

(* Exception definition *)
let parser exception_definition =
  | exception_kw cn:constr_name '=' c:constr ->
      let name = id_loc cn _loc_cn in
      let ex = id_loc c _loc_c in
      let rb = Te.rebind ~loc:_loc name ex in
      let rb = Te.mk_exception ~loc:_loc rb in
      Str.exception_ ~loc:_loc rb
  | exception_kw (name,args,res,a):(constr_decl false) ->
       let cd = Te.decl ~attrs:(attach_attrib _loc a) ~loc:_loc ~args ?res name in
       let cd = Te.mk_exception ~loc:_loc cd in
       Str.exception_ ~loc:_loc cd

(****************************************************************************
 * Classes                                                                  *
 ****************************************************************************)
(* Class types *)
let class_field_spec = declare_grammar "class_field_spec"
let class_body_type = declare_grammar "class_body_type"

let parser virt_mut =
  | v:virtual_flag m:mutable_flag -> (v, m)
  | mutable_kw virtual_kw -> (Virtual, Mutable)

let parser virt_priv =
  | v:virtual_flag p:private_flag -> (v, p)
  | private_kw virtual_kw -> (Virtual, Private)

let _ = set_grammar class_field_spec (
  parser
  | inherit_kw cbt:class_body_type ->
      pctf_loc _loc (Pctf_inherit cbt)
  | val_kw (vir,mut):virt_mut ivn:inst_var_name STR(":") te:typexpr ->
        let ivn = id_loc ivn _loc_ivn in
        pctf_loc _loc (Pctf_val (ivn, mut, vir, te))
  | method_kw (v,pri):virt_priv mn:method_name STR(":") te:poly_typexpr ->
        Ctf.method_ ~loc:_loc mn pri v te
  | constraint_kw te:typexpr CHR('=') te':typexpr ->
                                          pctf_loc _loc (Pctf_constraint (te, te'))
  | attr:floating_attribute -> Ctf.attribute ~loc:_loc attr
  | ext:floating_extension -> Ctf.extension ~loc:_loc ext
  )

let _ = set_grammar class_body_type (
  parser
  | object_kw te:{STR("(") te:typexpr STR(")")}? cfs:class_field_spec*
    end_kw ->
      let self = match te with
                 | None   -> loc_typ _loc_te Ptyp_any
                 | Some t -> t
      in
      let sign =
        { pcsig_self = self
        ; pcsig_fields = cfs
        }
      in
      pcty_loc _loc (Pcty_signature sign)
  | tes:{STR("[") te:typexpr tes:{STR(",") te:typexpr}*
    STR("]") -> (te::tes)}?[[]] ctp:classtype_path ->
      let ctp = id_loc ctp _loc_ctp in
      pcty_loc _loc (Pcty_constr (ctp, tes))
  )

let parser class_type =
  | tes:{l:maybe_opt_label? STR(":") te:typexpr -> (l, te)}* cbt:class_body_type ->
      let app acc (lab, te) =
        match lab with
        | None   -> pcty_loc _loc (Pcty_arrow (nolabel, te, acc))
        | Some l -> pcty_loc _loc (Pcty_arrow (l, (match l with Optional _ -> te | _ -> te), acc))
      in
      List.fold_left app cbt (List.rev tes)

let parser type_parameters =
  | i1:type_param l:{ STR(",") i2:type_param }* -> i1::l

(* Class specification *)
let parser class_spec =
  | v:virtual_flag params:{STR("[") params:type_parameters STR("]")}?[[]]
    cn:class_name STR(":") ct:class_type ->
      class_type_declaration ~attributes:(attach_attrib _loc []) _loc (id_loc cn _loc_cn) params v ct

let parser class_specification =
  | class_kw cs:class_spec css:{_:and_kw class_spec}* -> (cs::css)

(* Class type definition *)
let parser classtype_def =
  | v:virtual_flag params:{STR("[") tp:type_parameters STR("]")}?[[]] cn:class_name
    CHR('=') cbt:class_body_type ->
      (fun _l -> class_type_declaration ~attributes:(attach_attrib _l []) _l (id_loc cn _loc_cn) params v cbt)

let parser classtype_definition =
  | k:class_kw type_kw cd:classtype_def cds:{_:and_kw cd:classtype_def -> cd _loc}* ->
      cd (merge2 _loc_k _loc_cd) ::cds

(****************************************************************************
 * Constants and Patterns                                                   *
 ****************************************************************************)

(* Constants *)
let parser constant =
  | (f,suffix):float_litteral -> Const.float ?suffix f
  | c:char_litteral           -> Const.char c
  | (s,quotation_delimiter):string_litteral -> Const.string ?quotation_delimiter s
  | s:regexp_litteral  -> const_string s
  | s:new_regexp_litteral -> const_string s
  | (i,suffix):int_litteral -> Const.integer ?suffix i

(* we do like parser.mly from ocaml: neg_constant for pattern only *)
let parser neg_constant =
  | '-' - '.'? (f,suffix):float_litteral -> Const.float   ?suffix ("-" ^ f)
  | '-' (i,suffix):int_litteral          -> Const.integer ?suffix ("-" ^ i)

(* Patterns *)

let parser extra_patterns_grammar lvl =
  (alternatives (List.map (fun g -> g lvl) extra_patterns))

let _ = set_pattern_lvl (fun @(as_ok,lvl) -> parser
  | [@unshared] e:(extra_patterns_grammar (as_ok, lvl)) -> e

  | [@unshared] p:(pattern_lvl (as_ok, lvl)) as_kw vn:value_name when as_ok ->
      Pat.alias ~loc:_loc p (id_loc vn _loc_vn)

  | vn:value_name ->
      Pat.var ~loc:_loc (id_loc vn _loc_vn)

  | joker_kw ->
      Pat.any ~loc:_loc ()

  | c1:char_litteral ".." c2:char_litteral ->
      Pat.interval ~loc:_loc (Const.char c1) (Const.char c2)

  | c:{c:constant | c:neg_constant} when lvl <= AtomPat ->
      Pat.constant ~loc:_loc c

  | '(' p:pattern  ty:{_:':' typexpr}? ')' ->
     let p = match ty with
         None -> loc_pat _loc p.ppat_desc
       | Some ty -> Pat.constraint_ ~loc:_loc p ty
     in
     p

  | lazy_kw p:(pattern_lvl (false,ConstrPat)) when lvl <= ConstrPat ->
      Pat.lazy_ ~loc:_loc p

  | exception_kw p:(pattern_lvl (false,ConstrPat)) when lvl <= ConstrPat ->
      Pat.exception_ ~loc:_loc p

  | c:constr p:(pattern_lvl (false, ConstrPat)) when lvl <= ConstrPat ->
      Pat.construct ~loc:_loc (id_loc c _loc_c) (Some p)

  | c:constr ->
      Pat.construct ~loc:_loc (id_loc c _loc_c) None

  | b:bool_lit ->
      Pat.construct ~loc:_loc (id_loc (Lident b) _loc) None

  | c:tag_name p:(pattern_lvl (false, ConstrPat)) when lvl <= ConstrPat ->
      Pat.variant ~loc:_loc c (Some p)

  | c:tag_name ->
      Pat.variant ~loc:_loc c None

  | s:'#' t:typeconstr ->
      Pat.type_ ~loc:_loc (id_loc t _loc_t)

  | s:'{' f:field p:{'=' p:pattern}? fps:{semi_col f:field
                  p:{'=' p:pattern}? -> (id_loc f _loc_f, p)}*
          clsd:{semi_col joker_kw -> ()}? semi_col? '}' ->
      let all = (id_loc f _loc_f, p)::fps in
      let f (lab, pat) =
        match pat with
        | Some p -> (lab, p)
        | None   ->
           let slab = match lab.txt with
                               | Lident s -> id_loc s lab.loc
                               | _        -> give_up ()
                    in (lab, loc_pat lab.loc (Ppat_var slab))
      in
      let all = List.map f all in
      let cl = match clsd with
               | None   -> Closed
               | Some _ -> Open
      in
      Pat.record ~loc:_loc all cl

  | '[' ps:(list1 pattern semi_col) semi_col? c:']' ->
      pat_list _loc _loc_c ps

  | '[' ']' ->
      Pat.construct ~loc:_loc (id_loc (Lident "[]") _loc) None

  | "[|" ps:(list0 pattern semi_col) semi_col? "|]" ->
      Pat.array ~loc:_loc ps

  | '(' ')' ->
      Pat.construct ~loc:_loc (id_loc (Lident "()") _loc) None

  | begin_kw end_kw ->
      Pat.construct ~loc:_loc (id_loc (Lident "()") _loc) None

  | '(' module_kw mn:module_name pt:{':' pt:package_type}? ')'
      when lvl <= AtomPat ->
      let pat =  Pat.unpack ~loc:_loc mn in
      begin
        match pt with
        | None    -> pat
        | Some pt ->
            (* FIXME OCAML: why enlarge and ghost ?*)
            let pt = loc_typ (ghost _loc) pt.ptyp_desc in
            Pat.constraint_ ~loc:_loc pat pt
      end

  | p :(pattern_lvl (true , AltPat)) '|'
    p':(pattern_lvl (false, next_pat_prio AltPat)) when lvl <= AltPat ->
      Pat.or_ ~loc:_loc p p'

  | ps:{ (pattern_lvl (true , next_pat_prio TupPat)) _:','}+
       p:(pattern_lvl (false, next_pat_prio TupPat)) when lvl <= TupPat ->
      Pat.tuple ~loc:_loc (ps @ [p])

  | p :(pattern_lvl (true , next_pat_prio ConsPat)) c:"::"
    p':(pattern_lvl (false, ConsPat)) when lvl <= ConsPat ->
      let cons = id_loc (Lident "::") _loc_c in
      let args = loc_pat (ghost _loc) (Ppat_tuple [p; p']) in
      Pat.construct ~loc:_loc cons (Some args)

)

(****************************************************************************
 * Expressions                                                              *
 ****************************************************************************)

let let_re = "\\(let\\)\\|\\(val\\)\\b"

type assoc = NoAssoc | Left | Right

let assoc = function
  Prefix | Dot | Dash | Opp -> NoAssoc
| Prod | Sum | Eq -> Left
| _ -> Right

let infix_prio s =
  match s.[0] with
  | '*' -> if String.length s > 1 && s.[1] = '*' then Pow else Prod
  | '/' | '%' -> Prod
  | '+' | '-' -> Sum
  | ':' -> if String.length s > 1 && s.[1] = '=' then Aff else Cons
  | '<' -> if String.length s > 1 && s.[1] = '-' then Aff else Eq
  | '@' | '^' -> Append
  | '&' -> if String.length s = 1 ||
                (String.length s = 2 && s.[1] = '&') then Conj else Eq
  | '|' -> if String.length s = 2 && s.[1] = '|' then Disj else Eq
  | '=' | '>' | '$' | '!' -> Eq
  | 'o' -> Disj
  | 'm' -> Prod
  | 'a' -> Pow
  | 'l' -> (match s.[1] with 's' -> Pow | _ -> Prod)
  | _ -> Printf.printf "%s\n%!" s; assert false

let prefix_prio s =
  if s = "-" || s = "-." || s = "+" || s = "+." then Opp else Prefix

let array_function loc str name =
  loc_expr loc (Pexp_ident (id_loc (Ldot(Lident str, name)) loc ))

let bigarray_function loc str name =
  let lid = Ldot(Ldot(Lident "Bigarray", str), name) in
  loc_expr loc (Pexp_ident (id_loc lid loc))

let untuplify exp =
  match exp.pexp_desc with
  | Pexp_tuple es -> es
  | _             -> [exp]

let bigarray_get loc arr arg =
  let apply fn args =
    let fn = Exp.ident (Location.mknoloc (Longident.parse fn)) in
    Exp.apply ~loc fn (List.map (fun e -> (Nolabel, e)) args)
  in
  match untuplify arg with
  | [c1]       -> apply "Bigarray.Array1.get" [arr; c1]
  | [c1;c2]    -> apply "Bigarray.Array2.get" [arr; c1; c2]
  | [c1;c2;c3] -> apply "Bigarray.Array3.get" [arr; c1; c2; c3]
  | cs         -> apply "Bigarray.Genarray.get" [arr; Exp.array cs]

let bigarray_set loc arr arg v =
  let apply fn args =
    let fn = Exp.ident (Location.mknoloc (Longident.parse fn)) in
    Exp.apply ~loc fn (List.map (fun e -> (Nolabel, e)) args)
  in
  match untuplify arg with
  | [c1]       -> apply "Bigarray.Array1.set" [arr; c1; v]
  | [c1;c2]    -> apply "Bigarray.Array2.set" [arr; c1; c2; v]
  | [c1;c2;c3] -> apply "Bigarray.Array3.set" [arr; c1; c2; c3; v]
  | cs         -> apply "Bigarray.Genarray.set" [arr; Exp.array cs; v]

let parser constructor =
  | m:{ m:module_path STR"." }? id:{id:uident -> id | b:bool_lit -> b } ->
      match m with
      | None   -> Lident id
      | Some m -> Ldot(m, id)

let parser argument =
  | '~' id:lident no_colon ->
        (labelled id, loc_expr _loc_id (Pexp_ident(id_loc (Lident id) _loc_id)))
  | id:ty_label e:(expression_lvl (NoMatch ,next_exp App)) ->
       (id, e)
  | '?' id:lident ->
       (optional id, Exp.ident ~loc:_loc_id (id_loc (Lident id) _loc_id))
  | id:ty_opt_label e:(expression_lvl (NoMatch, next_exp App)) ->
       (id, e)
  | e:(expression_lvl (NoMatch, next_exp App)) ->
       (nolabel, e)

let _ = set_parameter (fun allow_new_type ->
  parser
  | pat:(pattern_lvl (false,AtomPat)) -> `Arg (nolabel, None, pat)
  | '~' '(' id:lident t:{ STR":" t:typexpr }? STR")" -> (
      let pat =  loc_pat _loc_id (Ppat_var(id_loc id _loc_id)) in
      let pat = match t with
      | None   -> pat
      | Some t -> loc_pat _loc (Ppat_constraint (pat, t))
      in
      `Arg (labelled id, None, pat))
  | id:ty_label pat:pattern -> `Arg (id, None, pat)
  | '~' id:lident no_colon -> `Arg (labelled id, None, loc_pat _loc_id (Ppat_var(id_loc id _loc_id)))
  | '?' '(' id:lident t:{ ':' t:typexpr -> t }? e:{'=' e:expression -> e}? ')' -> (
      let pat = loc_pat _loc_id (Ppat_var(id_loc id _loc_id)) in
      let pat = match t with
                | None -> pat
                | Some t -> loc_pat (merge2 _loc_id _loc_t) (Ppat_constraint(pat,t))
      in `Arg (optional id, e, pat))
  | id:ty_opt_label STR"(" pat:pattern t:{':' t:typexpr}? e:{'=' e:expression}? ')' -> (
      let pat = match t with
                | None -> pat
                | Some t -> loc_pat (merge2 _loc_pat _loc_t) (Ppat_constraint(pat,t))
      in `Arg (id, e, pat))
  | id:ty_opt_label pat:pattern -> `Arg (id, None, pat)
  | '?' id:lident ->
             (* FIXME OCAML: why is ? treated differently in label position *)
             `Arg (optional id, None, loc_pat _loc_id (Ppat_var (id_loc id _loc_id)))
  | '(' type_kw name:typeconstr_name ')' when allow_new_type ->
      let name = id_loc name _loc_name in
      `Type(name)
  )

let apply_params ?(gh=false) _loc params e =
  let f acc = function
    | `Arg (lbl,opt,pat), _loc' ->
       loc_expr (ghost (merge2 _loc' _loc)) (pexp_fun (lbl, opt, pat, acc))
    | `Type name, _loc' ->
       (** FIXME OCAML: should be ghost, or above should not be ghost *)
       Exp.newtype ~loc:(merge2 _loc' _loc) name acc
  in
  let e = List.fold_left f e (List.rev params) in
  if gh then e else de_ghost e

let apply_params_cls ?(gh=false) _loc params e =
  let ghost _loc' = if gh then merge2 _loc' _loc else _loc in
  let f acc = function
    | `Arg (lbl,opt,pat), _loc' ->
       (** FIXME OCAML: shoud be ghost as above ? *)
       loc_pcl (ghost _loc') (Pcl_fun(lbl, opt, pat, acc))
    | `Type name, _ -> assert false
  in
  List.fold_left f e (List.rev params)

let parser right_member =
  | l:{lb:(parameter true) -> lb, _loc_lb}+ ty:{CHR(':') t:typexpr}? CHR('=') e:expression ->
      let e = match ty with
        None -> e
      | Some ty -> loc_expr (ghost (merge2 _loc_ty _loc)) (pexp_constraint(e, ty))
      in
      apply_params ~gh:true _loc_e l e

let parser eright_member =
  | ty:{CHR(':') t:typexpr}? CHR('=') e:expression ->
      (ty, e)

let _ = set_grammar let_binding (
  parser
  | pat:pattern erm:eright_member a:post_item_attributes l:{_:and_kw let_binding}?[[]] ->
  (    let (_ty, e) = erm in
       let loc = merge2 _loc_pat _loc_erm in
       let pat, e = match _ty with
           None -> (pat, e)
         | Some ty ->
            (* FIXME OCAML: crisper position are possible!*)
            let loc = ghost _loc in
            let poly_ty = Typ.poly ~loc:(ghost _loc(*_ty*)) [] ty in
            (Pat.constraint_ ~loc pat poly_ty, Exp.constraint_ ~loc e ty)
       in
       value_binding ~attributes:(attach_attrib loc a) loc pat e::l)
  | vn:value_name e:right_member a:post_item_attributes l:{_:and_kw let_binding}?[[]] ->
     ( let loc = merge2 _loc_vn _loc_e in
       let pat = pat_ident _loc_vn vn  in
       value_binding ~attributes:(attach_attrib loc a) loc pat e::l)
  | vn:value_name ':' ty:only_poly_typexpr '=' e:expression a:post_item_attributes l:{_:and_kw let_binding}?[[]] ->
      let pat = loc_pat (ghost _loc) (Ppat_constraint(
        loc_pat _loc_vn (Ppat_var(id_loc vn _loc_vn)),
        (** FIXME OCAML: shoud not change the position below *)
        loc_typ (ghost _loc) ty.ptyp_desc))
      in
      let loc = merge2 _loc_vn _loc_e in
      value_binding ~attributes:(attach_attrib loc a) loc pat e::l
  | vn:value_name ':' (ids,ty):poly_syntax_typexpr '=' e:expression a:post_item_attributes l:{_:and_kw let_binding}?[[]] ->
    let loc = merge2 _loc_vn _loc_e in
    let (e, ty) = wrap_type_annotation loc ids ty e in
    let pat = loc_pat (ghost loc) (Ppat_constraint(
        loc_pat _loc_vn (Ppat_var(id_loc vn _loc_vn)),
        ty))
    in
    value_binding ~attributes:(attach_attrib loc a) loc pat e::l
  )

let parser match_case alm lvl =
  | pat:pattern  w:{_:when_kw expression }? arrow_re
                 e:{ (expression_lvl (alm, lvl))
                   | "." -> Exp.unreachable ~loc:_loc () }
  -> make_case pat e w

let _ = set_grammar match_cases (
  parser
  | '|'? l:{(match_case Let Seq) '|'}* x:(match_case Match Seq) no_semi -> l @ [x]
  | EMPTY -> []
  )

let parser type_coercion =
  | STR(":") t:typexpr t':{STR(":>") t':typexpr}? -> (Some t, t')
  | STR(":>") t':typexpr -> (None, Some t')

let parser expression_list =
  | l:{ e:(expression_lvl (NoMatch, next_exp Seq)) _:semi_col -> (e, _loc_e)}* e:(expression_lvl (Match, next_exp Seq)) semi_col?
      -> l @ [e,_loc_e]
  | EMPTY -> []

let parser record_item =
  | f:field CHR('=') e:(expression_lvl (NoMatch, next_exp Seq)) -> (id_loc f _loc_f,e)
  | f:lident -> (let id = id_loc (Lident f) _loc_f in id, loc_expr _loc_f (Pexp_ident(id)))

let parser last_record_item =
  | f:field CHR('=') e:(expression_lvl (Match, next_exp Seq)) -> (id_loc f _loc_f,e)
  | f:lident -> (let id = id_loc (Lident f) _loc_f in id, loc_expr _loc_f (Pexp_ident(id)))

let _ = set_grammar record_list (
  parser
  | l:{ record_item _:semi_col }* it:last_record_item semi_col? -> (l@[it])
  | EMPTY -> [])

(****************************************************************************
 * classes and objects                                                      *
 ****************************************************************************)

let parser obj_item =
  | v:inst_var_name CHR('=') e:(expression_lvl (Match, next_exp Seq)) (* FIXME match always allowed ?*)
     -> (id_loc v _loc_v, e)

(* Class expression *)

let parser class_expr_base =
  | cp:class_path ->
      let cp = id_loc cp _loc_cp in
      loc_pcl _loc (Pcl_constr (cp, []))
  | '[' te:typexpr tes:{',' te:typexpr}* ']' cp:class_path ->
      let cp = id_loc cp _loc_cp in
      loc_pcl _loc (Pcl_constr (cp, te :: tes))
  | STR("(") ce:class_expr STR(")") ->
      loc_pcl _loc ce.pcl_desc
  | STR("(") ce:class_expr STR(":") ct:class_type STR(")") ->
      loc_pcl _loc (Pcl_constraint (ce, ct))
  | fun_kw ps:{p:(parameter false) -> (p, _loc)}+ arrow_re ce:class_expr ->
      apply_params_cls _loc ps ce
  | let_kw r:rec_flag lbs:let_binding in_kw ce:class_expr ->
      loc_pcl _loc (Pcl_let (r, lbs, ce))
  | object_kw cb:class_body end_kw ->
      loc_pcl _loc (Pcl_structure cb)

let _ = set_grammar class_expr (
  parser
  | ce:class_expr_base args:{arg:argument+}? ->
      (match args with
       | None   -> ce
       | Some l -> loc_pcl _loc (Pcl_apply (ce, l)))
  )

let parser class_field =
  | inherit_kw o:override_flag ce:class_expr
               id:{_:as_kw id:lident -> id_loc id _loc_id}? ->
      loc_pcf _loc (Pcf_inherit (o, ce, id))
  | val_kw o:override_flag m:mutable_flag ivn:inst_var_name te:{CHR(':') t:typexpr}?
    CHR('=') e:expression ->
      let ivn = id_loc ivn _loc_ivn in
      let ex =
        match te with
        | None   -> e
        | Some t -> loc_expr (ghost (merge2 _loc_ivn _loc)) (pexp_constraint (e, t))
      in
      loc_pcf _loc (Pcf_val (ivn, m, Cfk_concrete(o,ex)))
  | val_kw m:mutable_flag virtual_kw ivn:inst_var_name
    STR(":") te:typexpr ->
      let ivn = id_loc ivn _loc_ivn in
      loc_pcf _loc (Pcf_val (ivn, m, Cfk_virtual te))
  | val_kw virtual_kw mutable_kw ivn:inst_var_name STR(":") te:typexpr ->
      let ivn = id_loc ivn _loc_ivn in
      loc_pcf _loc (Pcf_val (ivn, Mutable, Cfk_virtual te))
  | method_kw t:{override_flag private_flag method_name}
    STR(":") te:poly_typexpr CHR('=') e:expression ->
      let (o,p,mn) = t in
      let e = loc_expr (ghost (merge2 _loc_t _loc)) (Pexp_poly (e, Some te)) in
      loc_pcf _loc (Pcf_method (mn, p, Cfk_concrete(o,e)))
  | method_kw t:{override_flag private_flag method_name}
    STR(":") (ids,te):poly_syntax_typexpr CHR('=') e:expression ->
      let (o,p,mn) = t in
      let _loc_e = merge2 _loc_t _loc in
      let e, poly =  wrap_type_annotation _loc_e ids te e in
      let e = loc_expr (ghost _loc_e) (Pexp_poly (e, Some poly)) in
      loc_pcf _loc (Pcf_method (mn, p, Cfk_concrete(o,e)))
  | method_kw t:{override_flag private_flag method_name} ps:{p:(parameter true) -> p,_loc_p}*
      te:{STR(":") te:typexpr}? CHR('=') e:expression ->
      let (o,p,mn) = t in
      if ps = [] && te <> None then give_up ();
      let e =
        match te with
          None -> e
        | Some te ->
           loc_expr (ghost (merge2 _loc_te _loc_e)) (pexp_constraint (e, te))
      in
      let e : expression = apply_params ~gh:true _loc_e ps e in
      let e = loc_expr (ghost (merge2 _loc_t _loc_e)) (Pexp_poly (e, None)) in
      loc_pcf _loc (Pcf_method (mn, p, Cfk_concrete(o,e)))
  | method_kw p:private_flag virtual_kw mn:method_name STR(":") pte:poly_typexpr ->
      loc_pcf _loc (Pcf_method (mn, p, Cfk_virtual(pte)))
  | method_kw virtual_kw private_kw mn:method_name STR(":") pte:poly_typexpr ->
      loc_pcf _loc (Pcf_method (mn, Private, Cfk_virtual(pte)))
  | constraint_kw te:typexpr CHR('=') te':typexpr ->
      loc_pcf _loc (Pcf_constraint (te, te'))
  | initializer_kw e:expression ->
      loc_pcf _loc (Pcf_initializer e)
  | attr:floating_attribute -> Cf.attribute ~loc:_loc attr
  | ext:floating_extension  -> Cf.extension ~loc:_loc ext

let _ = set_grammar class_body (
  parser
  | p:pattern? f:class_field* ->
      let p = match p with None -> loc_pat (ghost _loc_p) Ppat_any | Some p -> p in
      { pcstr_self = p; pcstr_fields = f }
  )

(* Class definition *)
let parser class_binding =
  | v:virtual_flag params:{STR("[") params:type_parameters STR("]")}?[[]]
    cn:class_name ps:{p:(parameter false) -> (p,_loc)}* ct:{STR(":") ct:class_type}? CHR('=')
    ce:class_expr ->
      let ce = apply_params_cls ~gh:true (merge2 _loc_ps _loc) ps ce in
      let ce = match ct with
               | None    -> ce
               | Some ct -> loc_pcl _loc (Pcl_constraint(ce, ct))
      in
      (fun _loc ->
        class_type_declaration ~attributes:(attach_attrib _loc [])
                               _loc (id_loc cn _loc_cn) params v ce)

let parser class_definition =
  | k:class_kw cb:class_binding cbs:{_:and_kw cb:class_binding -> cb _loc}*
        -> (cb (merge2 _loc_k _loc_cb)::cbs)

let pexp_list _loc ?loc_cl l =
  if l = [] then
    loc_expr _loc (pexp_construct(id_loc (Lident "[]") _loc, None))
  else
    let loc_cl = ghost (match loc_cl with None -> _loc | Some pos -> pos) in
    List.fold_right (fun (x,pos) acc ->
                     let _loc = ghost (merge2 pos loc_cl) in
                     loc_expr _loc (pexp_construct(id_loc (Lident "::") (ghost _loc), Some (loc_expr _loc (Pexp_tuple [x;acc])))))
                    l (loc_expr loc_cl (pexp_construct(id_loc (Lident "[]") loc_cl, None)))


let apply_lbl _loc (lbl, e) =
  let e = match e with
      None -> loc_expr _loc (Pexp_ident(id_loc (Lident lbl) _loc ))
    | Some e -> e
  in (lbl, e)

let rec mk_seq loc_c final = function
      | [] -> final
      | x::l ->
         let res = mk_seq loc_c final l in
         loc_expr (merge2 x.pexp_loc loc_c) (Pexp_sequence(x,res))

(* Expressions *)
let parser extra_expressions_grammar c =
  (alternatives (List.map (fun g -> g c) extra_expressions))

let structure_item_simple = declare_grammar "structure_item_simple"

let parser functor_parameter =
  | '(' ')'                                   -> (_loc, Unit         )
  | '(' mn:module_name ':' mt:module_type ')' -> (_loc, Named(mn, mt))

let parser left_expr @(alm,lvl) =
  | fun_kw l:{lbl:(parameter true) -> lbl,_loc_lbl}* arrow_re
    when allow_let alm && lvl < App ->
      (Seq, false, (fun e (_loc,_) ->
        loc_expr _loc (apply_params _loc l e).pexp_desc))

  | _:let_kw f:{
    | r:rec_flag l:let_binding ->
        (fun e (_l,_) -> loc_expr _l (Pexp_let (r, l, e)))
    | module_kw mn:module_name l:functor_parameter*
      mt:{':' mt:module_type}? '=' me:module_expr ->
        (fun e (loc,_) ->
               let me =
                 match mt with None -> me | Some mt ->
                 let loc = merge2 _loc_mt _loc_me in
                 Mod.constraint_ ~loc me mt
               in
               let me =
                 List.fold_left (fun acc (loc, p) ->
                   let loc = merge2 loc _loc_me in
                   Mod.functor_ ~loc p acc) me (List.rev l)
               in
               Exp.letmodule ~loc mn me e)
    | open_kw o:override_flag mp:module_path ->
        (fun e (_l,_) ->
                (let mp = id_loc mp _loc_mp in
                 Exp.open_ ~loc:_l (Opn.mk ~override:o (Mod.ident mp)) e))
    } _:in_kw
    when allow_let alm && lvl < App -> (Seq, false, f)

  | if_kw c:expression then_kw e:(expression_lvl (Match, next_exp Seq)) else_kw
       when (allow_let alm || lvl = If) && lvl < App
    -> (next_exp Seq, false, (fun e' (_loc,_) -> Exp.ifthenelse ~loc:_loc c e (Some e')))

  | if_kw c:expression then_kw
       when (allow_let alm || lvl = If) && lvl < App
    -> (next_exp Seq, true, (fun e (_loc,_) -> Exp.ifthenelse ~loc:_loc c e None))

  | ls:{(expression_lvl (NoMatch, next_exp Seq)) _:semi_col }+ when lvl <= Seq ->
       (next_exp Seq, false, (fun e' (_,_l) ->
        mk_seq _l e' ls))

  | v:inst_var_name STR("<-") when lvl <= Aff
    -> (next_exp Aff, false, (fun e (_l,_) -> loc_expr _l (Pexp_setinstvar(id_loc v _loc_v, e))))

  | e':(expression_lvl (NoMatch, Dot)) '.'
      f:{ STR("(") f:expression STR(")") ->
           fun e' e (_l,_) -> exp_apply _l (array_function (ghost (merge2 e'.pexp_loc _l)) "Array" "set") [e';f;e]

        | STR("[") f:expression STR("]") ->
           fun e' e (_l,_) -> exp_apply _l (array_function (ghost (merge2 e'.pexp_loc _l)) "String" "set") [e';f;e]

        | STR("{") f:expression STR("}") ->
           fun e' e (_l,_) ->
             de_ghost (bigarray_set (ghost (merge2 e'.pexp_loc _l)) e' f e)

        | f:field ->
           fun e' e (_l,_) -> let f = id_loc f _loc_f in loc_expr _l (Pexp_setfield(e',f,e))

        } "<-"
      when lvl <= Aff
    -> (next_exp Aff, false, f e')

  | l:{(expression_lvl (NoMatch, next_exp Tupl)) _:',' }+
      when lvl <= Tupl ->
       (next_exp Tupl, false, (fun e' (_l,_) -> Exp.tuple ~loc:_l (l@[e'])))

  | assert_kw when lvl <= App ->
       (next_exp App, false, (fun e (_l,_) -> loc_expr _l (Pexp_assert(e))))

  | lazy_kw when lvl <= App ->
     (next_exp App, false, (fun e (_l,_) -> loc_expr _l (Pexp_lazy e)))

  | (prefix_expr Opp) when lvl <= Opp
  | (prefix_expr Prefix) when lvl <= Prefix

  | (infix_expr Prod) when lvl <= Prod
  | (infix_expr Sum) when lvl <= Sum
  | (infix_expr Append) when lvl <= Append
  | (infix_expr Cons) when lvl <= Cons
  | (infix_expr Aff) when lvl <= Aff
  | (infix_expr Eq) when lvl <= Eq
  | (infix_expr Conj) when lvl <= Conj
  | (infix_expr Disj) when lvl <= Disj
  | (infix_expr Pow) when lvl <= Pow

and parser prefix_expr lvl =
  p:(prefix_symbol lvl) -> (lvl, false, (fun e (_l,_) -> mk_unary_op _l p _loc_p e))

and infix_expr lvl =
  if assoc lvl = Left then
    parser
      e':(expression_lvl (NoMatch, lvl)) op:(infix_symbol lvl) ->
         (next_exp lvl, false,
          fun e (_l,_) -> mk_binary_op _l e' op _loc_op e)
         else if assoc lvl = NoAssoc then
    parser
      e':(expression_lvl (NoMatch, next_exp lvl)) op:(infix_symbol lvl) ->
         (next_exp lvl, false,
          fun e (_l,_) -> mk_binary_op _l e' op _loc_op e)
  else
    parser
      ls:{e':(expression_lvl (NoMatch, next_exp lvl)) op:(infix_symbol lvl)
             -> (_loc,e',op,_loc_op) }+ ->
         (next_exp lvl, false,
          fun e (_l,_) ->
          List.fold_right
            (fun (_loc_e,e',op,_loc_op) acc
             -> mk_binary_op (merge2 _loc_e _l) e' op _loc_op acc) ls e)

let parser prefix_expression =
  | function_kw l:match_cases
    -> Exp.function_ ~loc:_loc l
  | match_kw e:expression with_kw l:match_cases
    -> Exp.match_ ~loc:_loc e l
  | try_kw e:expression with_kw l:match_cases
    -> Exp.try_ ~loc:_loc e l
  | e:(alternatives extra_prefix_expressions)

let parser right_expression @lvl =
  | id:value_path when lvl <= Atom -> loc_expr _loc (Pexp_ident(id_loc id _loc_id))

  | c:constant when lvl <= Atom -> loc_expr _loc (Pexp_constant c)

  | mp:module_path STR(".") STR("(") e:expression STR(")") when lvl <= Atom ->
      let mp = id_loc mp _loc_mp in
      Exp.open_ ~loc:_loc (Opn.mk ~override:Fresh (Mod.ident mp)) e

  | mp:module_path '.' '[' l:expression_list cl: ']' when lvl <= Atom ->
      let mp = id_loc mp _loc_mp in
      Exp.open_ ~loc:_loc (Opn.mk ~override:Fresh (Mod.ident mp))
        (pexp_list _loc ~loc_cl:_loc_cl l)

  | mp:module_path '.' '{' e:{expression _:with_kw}? l:record_list '}' when lvl <= Atom ->
      let mp = id_loc mp _loc_mp in
      Exp.open_ ~loc:_loc (Opn.mk ~override:Fresh (Mod.ident mp))
        (Exp.record ~loc:_loc l e)

  | '(' e:expression? ')' when lvl <= Atom ->
       (match e with
        | Some(e) -> loc_expr _loc e.pexp_desc
        | None ->
           let cunit = id_loc (Lident "()") _loc in
           loc_expr _loc (pexp_construct(cunit, None)))

  | '(' no_parser e:expression t:type_coercion ')'  when lvl <= Atom  ->
       (match t with
           | (Some t1, None) -> loc_expr (ghost _loc) (pexp_constraint(e, t1))
           | (t1, Some t2) -> loc_expr (ghost _loc) (pexp_coerce(e, t1, t2))
           | None, None -> assert false)

  | begin_kw e:expression? end_kw when lvl <= Atom ->
       (match e with
        | Some e -> loc_expr _loc e.pexp_desc
        | None ->
           let cunit = id_loc (Lident "()") _loc in
           loc_expr _loc (pexp_construct(cunit, None)))

  | f:(expression_lvl (NoMatch, next_exp App)) l:argument+ when lvl <= App ->
     loc_expr _loc (match f.pexp_desc, l with
     | Pexp_construct(c,None), [Nolabel, a] ->  Pexp_construct(c,Some a)
     | Pexp_variant(c,None), [Nolabel, a] -> Pexp_variant(c,Some a)
     | _ -> Pexp_apply(f,l))

  | c:constructor no_dot when lvl <= Atom ->
        loc_expr _loc (pexp_construct(id_loc c _loc_c, None))

  | l:tag_name  when lvl <= Atom ->
     loc_expr _loc (Pexp_variant(l, None))

  | "[|" l:expression_list "|]" when lvl <= Atom ->
     loc_expr _loc (Pexp_array (List.map fst l))

  | '[' l:expression_list cl:']' when lvl <= Atom ->
     loc_expr _loc (pexp_list _loc ~loc_cl:_loc_cl l).pexp_desc

  | STR("{") e:{expression _:with_kw}? l:record_list STR("}") when lvl <= Atom ->
      loc_expr _loc (Pexp_record(l,e))

  | while_kw e:expression do_kw e':expression done_kw when lvl <= Atom ->
      loc_expr _loc (Pexp_while(e, e'))

  | for_kw id:pattern CHR('=') e:expression d:downto_flag e':expression do_kw e'':expression done_kw when lvl <= Atom ->
      loc_expr _loc (Pexp_for(id, e, e', d, e''))

  | new_kw p:class_path when lvl <= Atom -> loc_expr _loc (Pexp_new(id_loc p _loc_p))

  | object_kw o:class_body end_kw when lvl <= Atom -> loc_expr _loc (Pexp_object o)

  | "{<" l:{ o:obj_item l:{_:semi_col o:obj_item}* _:semi_col? -> o::l }?[[]] ">}" when lvl <= Atom ->
     loc_expr _loc (Pexp_override l)

  | '(' module_kw me:module_expr pt:{STR(":") pt:package_type}? ')' when lvl <= Atom ->
      let desc = match pt with
                 | None    -> Pexp_pack me
                 | Some pt -> let me = loc_expr (ghost _loc) (Pexp_pack me) in
                              (* FIXME OCAML: why enlarge and ghost ?*)
                              let pt = loc_typ (ghost _loc) pt.ptyp_desc in
                              pexp_constraint (me, pt)
      in loc_expr _loc desc
  | e':{e':(expression_lvl (NoMatch, Dot)) -> e'} '.'
      r:{ STR("(") f:expression STR(")") ->
           fun e' _l -> exp_apply _l (array_function (ghost (merge2 e'.pexp_loc _l)) "Array" "get") [e';f]

        | STR("[") f:expression STR("]") ->
           fun e' _l -> exp_apply _l (array_function (ghost (merge2 e'.pexp_loc _l)) "String" "get") [e';f]

        | STR("{") f:expression STR("}") ->
           fun e' _l -> bigarray_get (ghost (merge2 e'.pexp_loc _l)) e' f

        | f:field ->
           fun e' _l ->
             let f = id_loc f _loc_f in loc_expr _l (Pexp_field(e',f))
        } when lvl <= Dot -> r e' _loc

  | e':(expression_lvl (NoMatch, Dash)) '#' f:method_name when lvl <= Dash ->
       Exp.send ~loc:_loc e' f

let parser semicol @(alm, lvl) =
  | semi_col when lvl = Seq -> true
  | no_semi  when lvl = Seq -> false
  | EMPTY    when lvl > Seq -> false

let parser noelse @b =
  | no_else when b
  | EMPTY when not b

let parser debut lvl alm = s:(left_expr (alm,lvl)) ->
  let (lvl0, no_else, f) = s in
  ((lvl0,no_else), (f, _loc_s))

let parser suit lvl alm (lvl0,no_else) =
  | e:(expression_lvl (alm,lvl0)) c:(semicol (alm,lvl)) (noelse no_else) ->
      fun (f, _loc_s) ->
        let _l = merge2 _loc_s _loc_e in
        (* position of the last semi column for sequence only *)
        let _loc_c = if c then _loc_c else _loc_e in
        f e (_l, _loc_c)

let _ = set_expression_lvl (fun (alm, lvl as c) -> parser

  | e:(extra_expressions_grammar c) (semicol (alm,lvl)) -> e

  | (Earley.dependent_sequence (debut lvl alm) (suit lvl alm))

  | r:(right_expression lvl) (semicol (alm,lvl)) -> r

  | r:prefix_expression (semicol (alm,lvl)) when allow_match alm -> r
  )



(****************************************************************************
 * Module expressions (module implementations)                              *
 ****************************************************************************)

let parser module_expr_base =
  | mp:module_path ->
      let mid = id_loc mp _loc in
      mexpr_loc _loc (Pmod_ident mid)
  | {struct_kw -> push_comments ()}
    ms:{ms:structure -> ms @ attach_str _loc}
    {end_kw ->  pop_comments ()} ->
      mexpr_loc _loc (Pmod_structure(ms))
  | functor_kw p:functor_parameter arrow_re me:module_expr
    -> Mod.functor_ ~loc:_loc (snd p) me
  | '(' me:module_expr mt:{':' mt:module_type}? ')' ->
      (match mt with
       | None    -> me
       | Some mt -> mexpr_loc _loc (Pmod_constraint (me, mt)))
  | '(' val_kw e:expression pt:{STR(":") pt:package_type}? ')' ->
      let e = match pt with
              | None    -> Pmod_unpack e
              | Some pt -> Pmod_unpack (loc_expr (ghost _loc) (pexp_constraint (e, pt)))
      in
      mexpr_loc _loc e

let _ = set_grammar module_expr (
  parser
    m:module_expr_base l:{STR("(") m:module_expr STR(")") -> (_loc, m)}* ->
      List.fold_left (fun acc (_loc_n, n) -> mexpr_loc (merge2 _loc_m _loc_n) (Pmod_apply(acc, n))) m l
  )

let parser module_type_base =
  | mp:modtype_path ->
      Mty.ident ~loc:_loc (id_loc mp _loc)
  | {sig_kw -> push_comments ()}
    ms:{ms:signature -> ms @ attach_sig _loc}
    {end_kw -> pop_comments () } ->
      Mty.signature ~loc:_loc ms
  | functor_kw p:functor_parameter arrow_re me:module_type no_with ->
      Mty.functor_ ~loc:_loc (snd p) me
  | '(' mt:module_type ')'
  | module_kw type_kw of_kw me:module_expr ->
      Mty.typeof_ ~loc:_loc me

let parser mod_constraint =
  | t:type_kw tf:typedef_in_constraint -> let (tn,ty) = tf (Some _loc_t) in
     Pwith_type(tn,ty)
  | module_kw m1:module_path CHR('=') m2:extended_module_path ->
     let name = id_loc m1 _loc_m1 in
     Pwith_module(name, id_loc m2 _loc_m2 )
  | type_kw tps:type_params tcn:typeconstr STR(":=") te:typexpr ->
      let tcn0 = id_loc (Longident.last tcn) _loc_tcn in
      let _tcn = id_loc tcn _loc_tcn in
      let td = type_declaration _loc tcn0 tps [] Ptype_abstract Public (Some te) in
      Pwith_typesubst (_tcn,td)
  | module_kw mn:module_path STR(":=") emp:extended_module_path ->
      let mn = id_loc mn _loc_mn in
      Pwith_modsubst(mn, id_loc emp _loc_emp)

let _ = set_grammar module_type (
  parser
    m:module_type_base l:{_:with_kw m:mod_constraint l:{_:and_kw mod_constraint}* -> m::l } ? ->
      (match l with
         None -> m
       | Some l -> mtyp_loc _loc (Pmty_with(m, l)))
  )

let parser structure_item_base =
  | RE(let_re) r:rec_flag l:let_binding ->
      Str.value ~loc:_loc r l
  | external_kw n:value_name ':' ty:typexpr '=' prim:string_litteral+
    a:post_item_attributes ->
      if List.length prim > 3 then give_up ();
      let prim = List.map fst prim in
      let attrs = attach_attrib _loc a in
      Str.primitive ~loc:_loc (Val.mk ~loc:_loc ~attrs ~prim (id_loc n _loc_n) ty)
  | td:type_definition ->
      Str.type_ ~loc:_loc Recursive td (* FIXME for NonRec *)
  | te:type_extension  ->
      Str.type_extension ~loc:_loc te
  | exception_definition
  | _:module_kw r:{
      | rec_kw mn:module_name mt:{':' mt:module_type}? '=' me:module_expr ms:{
          and_kw mn:module_name mt:{':' mt:module_type}? '=' me:module_expr ->
            module_binding (merge2 _loc_mt _loc_me) mn mt me
        }* ->
          let m = (module_binding (merge2 _loc_mt _loc_me) mn mt me) in
          Str.rec_module ~loc:_loc (m::ms)
      | mn:module_name l:functor_parameter* mt:{':' mt:module_type }? '=' me:module_expr ->
          let me =
            let loc = merge2 _loc_mt _loc_me in
            match mt with None -> me | Some mt -> Mod.constraint_ ~loc me mt
          in
          let fn acc (loc, p) =
            let loc = merge2 loc _loc_me in
            Mod.functor_ ~loc p acc
          in
          let me = List.fold_left fn me (List.rev l) in
          Str.module_ ~loc:_loc (module_binding _loc mn None me)
      | type_kw mn:modtype_name mt:{STR"=" mt:module_type}?
        a:post_item_attributes ->
          let attrs = attach_attrib _loc a in
          Str.modtype ~loc:_loc (Mtd.mk ~loc:_loc ~attrs ?typ:mt (id_loc mn _loc_mn))
    }
  | open_kw o:override_flag me:module_expr a:post_item_attributes ->
      let attrs = attach_attrib _loc a in
      Str.open_ ~loc:_loc (Opn.mk ~loc:_loc ~attrs ~override:o me)
  | include_kw me:module_expr a:post_item_attributes ->
      let attrs = attach_attrib _loc a in
      Str.include_ ~loc:_loc (Incl.mk ~loc:_loc ~attrs me)
  | ctd:classtype_definition -> Str.class_type ~loc:_loc ctd
  | cds:class_definition     -> Str.class_ ~loc:_loc cds
  | attr:floating_attribute  -> Str.attribute ~loc:_loc attr
  | ext:floating_extension   -> Str.extension ~loc:_loc ext

(* FIXME ext_attributes *)
let parser structure_item_aux =
  | _:ext_attributes -> []
  | _:ext_attributes e:expression -> attach_str _loc @ [loc_str _loc_e (pstr_eval e)]
  | s1:structure_item_aux double_semi_col?[()] _:ext_attributes f:{
             | e:(alternatives extra_structure) ->
                 (fun s1 -> List.rev_append e (List.rev_append (attach_str _loc_e) s1))
             | s2:structure_item_base ->
                  (fun s1 -> s2 :: (List.rev_append (attach_str _loc_s2) s1)) } -> f s1
  | s1:structure_item_aux double_semi_col _:ext_attributes e:expression
      -> loc_str _loc_e (pstr_eval e) :: (List.rev_append (attach_str _loc_e) s1)

let _ = set_grammar structure_item
  (parser l:structure_item_aux _:double_semi_col? -> List.rev l)
let _ = set_grammar structure_item_simple
  (parser ls:{l:structure_item_base -> l}* -> ls)

let parser signature_item_base =
  | val_kw n:value_name ':' ty:typexpr a:post_item_attributes ->
      let attrs = attach_attrib _loc a in
      Sig.value ~loc:_loc (Val.mk ~loc:_loc ~attrs (id_loc n _loc_n) ty)
  | external_kw n:value_name ':' ty:typexpr '=' prim:string_litteral+
    a:post_item_attributes ->
      if List.length prim > 3 then give_up ();
      let prim = List.map fst prim in
      let attrs = attach_attrib _loc a in
      Sig.value ~loc:_loc (Val.mk ~loc:_loc ~attrs ~prim (id_loc n _loc_n) ty)
  | td:type_definition ->
      Sig.type_ ~loc:_loc Recursive td (* FIXME non rec *)
  | te:type_extension  ->
      Sig.type_extension ~loc:_loc te
  | exception_kw (name,args,res,a):(constr_decl false) ->
      let cd = Te.decl ~attrs:(attach_attrib _loc a) ~loc:_loc ~args ?res name in
      Sig.exception_ ~loc:_loc (Te.mk_exception ~loc:_loc cd)
  | module_kw rec_kw mn:module_name ':' mt:module_type a:post_item_attributes
    ms:{and_kw mn:module_name ':' mt:module_type a:post_item_attributes ->
            module_declaration ~attributes:(attach_attrib _loc a) _loc mn mt
    }* ->
      let loc_first = merge2 _loc_mn _loc_a in
      let m = (module_declaration ~attributes:(attach_attrib loc_first a) loc_first mn mt) in
      Sig.rec_module ~loc:_loc (m::ms)
  | _:module_kw r:{
      | mn:module_name l:functor_parameter* ':' mt:module_type a:post_item_attributes ->
          let fn acc (loc, p) =
            let loc = merge2 loc _loc_mt in
            Mty.functor_ ~loc p acc
          in
          let mt = List.fold_left fn mt (List.rev l) in
          let attrs = attach_attrib _loc a in
          Sig.module_ ~loc:_loc (Md.mk ~loc:_loc ~attrs mn mt)
      | type_kw mn:modtype_name mt:{'=' mt:module_type }? a:post_item_attributes ->
                let attrs = attach_attrib _loc a in
          Sig.modtype ~loc:_loc (Mtd.mk ~loc:_loc ~attrs ?typ:mt (id_loc mn _loc_mn))
                }
  | open_kw o:override_flag m:module_path a:post_item_attributes ->
      let attrs = attach_attrib _loc a in
      Sig.open_ ~loc:_loc (Opn.mk ~loc:_loc ~attrs ~override:o (id_loc m _loc_m))
  | include_kw me:module_type a:post_item_attributes ->
    loc_sig _loc (Psig_include {pincl_mod = me; pincl_loc = _loc; pincl_attributes = attach_attrib _loc a })
  | ctd:classtype_definition -> loc_sig _loc (Psig_class_type ctd)
  | cs:class_specification -> loc_sig _loc (Psig_class cs)
  | attr:floating_attribute -> Sig.attribute ~loc:_loc attr
  | ext:floating_extension  -> Sig.extension ~loc:_loc ext

let _ = set_grammar signature_item (
  parser
  | e:(alternatives extra_signature) -> attach_sig _loc @ e
  | s:signature_item_base _:double_semi_col? -> attach_sig _loc @ [s]
  )

end

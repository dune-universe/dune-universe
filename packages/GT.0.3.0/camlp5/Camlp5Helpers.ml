(*
 * Generic Transformers: Camlp5 syntax extension.
 * Copyright (C) 2016-2019
 *   Dmitrii Kosarev aka Kakadu
 * St.Petersburg State University, JetBrains Research
 *)

(**
   Implementation of the [GTHELPERS_sig.S] interface which
   allows construction of Camlp5 parse tree
 *)
#load "q_MLast.cmo";;

(* Camlp5 AST
   https://github.com/camlp5/camlp5/blob/master/main/mLast.mli
   Camlp5 doc
   https://camlp5.github.io/doc/htmlc/ast_strict.html
*)

open GTCommon
open Ploc
open MLast

module Located = struct
  type t = Ploc.t
  let mk ~loc lident = lident
end
type loc = Located.t

let loc_from_caml camlloc =
  let open Ppxlib.Location in
  let { loc_start; loc_end} = camlloc in
  Ploc.make_loc loc_start.pos_fname loc_start.pos_lnum loc_start.pos_bol
    (loc_start.pos_bol, loc_start.pos_bol + loc_start.pos_cnum) ""

let noloc = Ploc.dummy

type type_arg = MLast.type_var
let named_type_arg ~loc s : type_arg = (Ploc.VaVal (Some s), None)

type lab_decl = (loc * string * bool * ctyp)
let lab_decl ~loc name is_mut typ = (loc, name, is_mut, typ)

type case = patt * expr option * expr
let case ~lhs ~rhs : case = (lhs, None, rhs)

let sep_last l =
  match List.rev l with
    last :: rev_pfx -> (last, List.rev rev_pfx)
  | [] -> failwith "sep_last must be called with nonempty list"

let capitalized s =
  assert (s <> "");
  let c1 = String.get s 0 in
  Char.(equal (uppercase_ascii c1) c1)

module Longid = struct
  type t = MLast.longid
  let of_longident ~loc lid =
    let open Ppxlib.Longident in
    let rec trec = function
        Lident s when capitalized s -> <:extended_longident< $uid:s$ >>
      | Ldot (li, s) when capitalized s -> <:extended_longident< $longid:trec li$ . $uid:s$ >>
      | Ldot (_, s) when not (capitalized s) ->
        Ploc.raise loc (Failure "Longid.of_longident: should not be called with lowercase ids(1)")
      | Lident s when not (capitalized s) ->
        Ploc.raise loc (Failure "Longid.of_longident: should not be called with lowercase ids(2)")
      | Lapply _ -> Ploc.raise loc (Failure "Longid.of_longident: should not be called with Lapply")
      | _ -> assert false
    in trec lid
end

module Pat = struct
  type t = MLast.patt
  let any ~loc = <:patt< _ >>
  let lid ~loc s  = <:patt< $lid:s$ >>
  let var = lid
  let sprintf ~loc fmt = Printf.ksprintf (fun s -> <:patt< $lid:s$ >>) fmt
  let of_longident ~loc lid =
    let is_lident s = not (capitalized s) in
    match lid with
      Longident.Lident s when is_lident s -> <:patt< $lid:s$ >>
    | Ldot(li, s) when is_lident s ->
      let li = Longid.of_longident ~loc li in
      <:patt< $longid:li$ . $lid:s$ >>
    | li ->
      let li = Longid.of_longident ~loc li in
      <:patt< $longid:li$ >>

  let access2 ~loc m n = of_longident ~loc (Ldot (Lident m, n))
  let constraint_ ~loc p t = <:patt< ($p$ : $t$) >>

  let constr_record ~loc uid ps =
    failwith "Record constructors are not available for camlp5"

  let constr ~loc uid ps =
    assert (uid <> "");
    let c = <:patt< $uid:uid$ >> in
    match ps with
    | [] -> c
    | [x] -> <:patt< $c$ $x$ >>
    | _  -> let args = <:patt< ($list:ps$) >> in
      <:patt< $c$ $args$ >>

  let type_ ~loc lident =
    <:patt< # $lilongid:Asttools.longident_lident_of_string_list loc (Longident.flatten lident)$ >>

  let record ~loc fs =
    <:patt< { $list:List.map (fun (l,r) -> (of_longident ~loc l, r) ) fs$ } >>
  let record1 ~loc ident =
    record ~loc [ ident, of_longident ~loc @@ HelpersBase.lident_tail ident ]

  let tuple ~loc ps = <:patt< ($list:ps$) >>
  let variant ~loc name args =
    let v = <:patt< ` $name$ >> in
    match args with
    | []  -> v
    | [x] -> <:patt< $v$ $x$ >>
    | _ ->
      let tup = tuple ~loc args in
      <:patt< $v$ $tup$ >>

  let alias ~loc p1 name =
    let right = <:patt< $lid:name$ >> in
    <:patt< (($p1$) as $right$) >>

  let optional ~loc p1 e = <:patt< ?{$p1$ = $e$} >>
  let unit ~loc = <:patt< () >>
end


let use_new_type ~loc name e =
  let p = <:patt< (type $lid:name$) >> in
  <:expr< fun [ $p$ -> $e$ ] >>

module Exp = struct
  type t = MLast.expr

  let ident ~loc s =
    if Base.Char.is_uppercase s.[0] || Base.String.equal s "[]"
    then <:expr< $uid:s$ >>
    else <:expr< $lid:s$ >>
  let lid = ident
  (* let uid ~loc s = <:expr< $uid:s$ >> *)
  let unit ~loc =  <:expr< () >>
  let sprintf ~loc fmt =
    Printf.ksprintf (fun s -> <:expr< $lid:s$ >>) fmt

  let string_const ~loc s = <:expr< $str:s$ >>
  let int_const ~loc n = <:expr< $int:string_of_int n$ >>
  let assert_false ~loc = <:expr< assert False >>
    [@@ocaml.warning "-32"]

  let of_longident ~loc l =
    let rec helper = function
      (* | Longident.Lident s when Char.equal s.[0] (Char.uppercase_ascii s.[0]) -> uid ~loc s *)
      | Longident.Lident s ->
          assert (s <> "");
          ident ~loc s
      | Ldot (l, r) ->
          let u = helper l in
          <:expr< $u$ . $ident ~loc r$ >>
      | _ -> assert false
    in
    helper l

  let acc ~loc e l = <:expr< $e$ . $of_longident ~loc l$ >>

  let access ~loc mname iname =
    let u = <:expr< $uid:mname$ >> in
    <:expr< $u$ . $ident ~loc iname$ >>

  let app ~loc l r = <:expr< $l$ $r$ >>
  let app_lab ~loc l lab r =
    let p = Pat.var ~loc lab in
    let arg = <:expr< ~{$p$ = $r$} >> in
    <:expr< $l$ $arg$ >>
  let app_list ~loc l xs =
    List.fold_left (app ~loc) l xs
  let match_ ~loc e (xs: case list) =
    let xs = List.map (fun (a,b,c) -> (a, Ploc.VaVal b, c)) xs in
    <:expr< match $e$ with [ $list:xs$ ] >>

  let fun_ ~loc pat e =
    <:expr< fun [ $list:[ (pat,Ploc.VaVal None,e) ]$ ] >>
  let fun_list ~loc pats body =
    List.fold_right (fun x acc -> fun_ ~loc x acc) pats body

  let fun_list_l ~loc pats body =
    List.fold_right (fun (lab,opt) acc ->
        <:expr< fun [ $list:[ (Pat.optional ~loc (Pat.lid ~loc lab) opt, Ploc.VaVal None, acc) ]$ ] >>
      ) pats body


  let construct ~loc lident args =
    app_list ~loc (of_longident ~loc lident) args

  let variant ~loc s args =
    match args with
    | []  -> <:expr< ` $s$ >>
    | [x] -> app ~loc <:expr< ` $s$ >> x
    | le  ->  <:expr< ` $s$ ($list:le$) >>

  let tuple ~loc le =
    match le with
    | [] -> failwith "Exp.tuple: bad argument"
    | [x] -> x
    | le  -> <:expr< ($list:le$) >>

  let new_ ~loc lident =
    <:expr< new $lilongid: Asttools.longident_lident_of_string_list loc (Longident.flatten lident)$ >>
  let object_ ~loc (pat, fields) =
    <:expr< object ($pat$) $list:fields$ end >>
  let send ~loc left s = <:expr< $left$ # $s$ >>

  let record ~loc lpe =
    let lpe = List.map (fun (l,r) -> Pat.of_longident ~loc l, r) lpe in
    <:expr< {$list:lpe$} >>
  let record1 ~loc lident expr = record ~loc [lident, expr]

  let field ~loc e lident = acc ~loc e lident
  let let_ ~loc ?(rec_=false) lpe ewhere =
    let lpe = List.map (fun (p,e) -> (p,e, <:vala< [] >>)) lpe in
    if rec_
    then <:expr< let rec $list:lpe$ in $ewhere$ >>
    else <:expr< let $list:lpe$ in $ewhere$ >>
  let let_one ~loc ?(rec_=false) pat e1 ewhere = let_ ~loc ~rec_ [pat, e1] ewhere

  let from_caml e = failwith "from_caml not implemented"
  let assert_false ~loc = <:expr< assert False >>
  let failwith_ ~loc s  = <:expr< failwith $str:s$ >>
  let objmagic_unit ~loc = <:expr< Obj.magic () >>
  let true_  ~loc = <:expr< True >>
  let false_ ~loc = <:expr< False >>

  let list ~loc xs =
    let rec helper acc = function
      | [] -> acc
      | x::xs -> helper (app_list ~loc <:expr< $uid:"::"$ >> [x; acc]) xs
    in
    helper <:expr< $uid:"[]"$ >> (List.rev xs)

  (* let new_type ~loc = failwith "Not implemented" *)
  let constraint_ ~loc e t = <:expr< ($e$:$t$) >>
end

module Typ = struct
  type t = MLast.ctyp

  let of_longident ~loc lid =
    let open Ppxlib.Longident in
    match lid with
      Lident s when not (capitalized s) -> <:ctyp< $lid:s$ >>
    | Ldot(li, s) when not (capitalized s) ->
      let li = Longid.of_longident ~loc li in
      <:ctyp< $longid:li$ . $lid:s$ >>
    | Lapply _ -> failwith "Typ.of_longident: should not be called with Lapply"
    | _ -> assert false

  let sprintf ~loc fmt =
    Printf.ksprintf (fun s -> <:ctyp< $lid:s$ >>) fmt
  let ident ~loc s = <:ctyp< $lid:s$ >>
  let string ~loc = <:ctyp< string >>
  let unit ~loc = <:ctyp< unit >>
  let pair ~loc l r = <:ctyp< ( $list:[l;r]$ ) >>

  let access2 ~loc mname tname =
    assert (Base.Char.is_uppercase mname.[0]);
    of_longident ~loc (Ldot (Lident mname, tname))

  let var  ~loc s = <:ctyp< '$s$ >>
  let app  ~loc l r = <:ctyp< $l$ $r$ >>
  let any  ~loc = <:ctyp< _ >>
  let alias ~loc t s =
    let p = var ~loc s in
    <:ctyp< $t$ as $p$ >>
  let tuple ~loc lt = <:ctyp< ( $list:lt$ ) >>
  let constr ~loc lident =
    let init = of_longident ~loc lident in
    function
    | []    -> init
    | lt ->
      List.fold_left (app ~loc) init lt

  let class_ ~loc lident  =
    let init = <:ctyp< # $lilongid:Asttools.longident_lident_of_string_list loc (Longident.flatten lident)$ >> in
    function
    | []    -> init
    (* | [r]   -> <:ctyp< $init$ $r$ >> *)
    | lt ->
      List.fold_left (app ~loc) init lt

  let of_type_arg ~loc (s,_) = match s with
    | VaVal (Some s) -> var ~loc s
    | VaAnt _ -> assert false
    | VaVal None -> failwith "bad type arg"

  let object_ ~loc flg lst =
    let lst = List.map (fun (s,t) -> (Some s, t, <:vala< [] >>)) lst in
    <:ctyp< < $list:lst$ $flag:(match flg with Ppxlib.Open -> true | Ppxlib.Closed -> false)$ > >>
  let arrow ~loc t1 t2 = <:ctyp< $t1$ -> $t2$ >>
  let chain_arrow ~loc = function
  | [] -> assert false
  | xs ->
    let r = List.rev xs in
    let init = List.hd r in
    List.fold_left (fun acc x -> arrow ~loc x acc) init (List.tl r)

  let from_caml root_typ =
    let rec helper typ =
      let loc = loc_from_caml typ.Ppxlib.ptyp_loc in
      match typ.ptyp_desc with
      | Ptyp_any   -> <:ctyp< _ >>
      | Ptyp_var s -> <:ctyp< '$s$ >>
      | Ptyp_arrow (lab, l, r) -> arrow ~loc (helper l) (helper r)
      | Ptyp_constr ({txt;_}, ts) -> constr ~loc txt (List.map helper ts)
      | Ptyp_tuple ts -> <:ctyp< ( $list:(List.map helper ts)$ ) >>
      | Ptyp_variant (_,_,_)
      | _ -> failwith "Not implemented: conversion from OCaml ast to Camlp5 Ast"
    in
    helper root_typ

  (* this might need to be changed *)
  let variant ~loc ?(is_open=false) fs =
    let vs = fs |> List.map (fun rf -> match rf.Ppxlib.prf_desc with
        | Ppxlib.Rinherit core_typ -> PvInh (loc, from_caml core_typ)
        | Rtag (lb, is_open, args) ->
          PvTag (loc, VaVal lb.txt, VaVal is_open, VaVal (List.map from_caml args), <:vala< [] >> )
      ) in
    if is_open
    then <:ctyp< [ > $list:vs$ ] >>
    else <:ctyp< [ < $list:vs$ ] >>

  let variant_of_t ~loc typ =
    <:ctyp< [ > $list:[PvInh (loc, typ)]$ ] >>

  let openize ~loc ?as_ t =
    let ans = variant_of_t ~loc t in
    match as_ with
    | Some name -> alias ~loc ans name
    | None ->  ans

  let use_tdecl tdecl =
    let loc = loc_from_caml tdecl.Ppxlib.ptype_loc in
    let c = ident ~loc tdecl.ptype_name.txt in
    List.fold_left (fun acc (t,_) -> match t.Ppxlib.ptyp_desc with
        | Ptyp_var s -> app ~loc acc (var ~loc s)
        | _ -> assert false
      )
      c
      tdecl.ptype_params

  let poly ~loc names t = <:ctyp< ! $list:names$ . $t$ >>

  let map ~onvar t = t

  let to_type_arg = function
    | <:ctyp< '$s$ >> -> Some (named_type_arg ~loc:noloc s)
    | _ -> None
  let to_type_arg_exn = function
    | <:ctyp< '$s$ >> -> named_type_arg ~loc:noloc s
    | _ -> failwith "bad argument of to_type_arg_exn"

end

type type_declaration = MLast.type_decl
type class_declaration = class_expr class_infos

let class_declaration  ~loc ~name ?(virt=false) ?(wrap=(fun x -> x)) ~params fields =
    let c = { ciLoc = loc; ciVir = Ploc.VaVal virt;
              ciPrm = (loc, Ploc.VaVal params);
              ciNam = Ploc.VaVal name;
              ciExp = wrap @@ CeStr (loc, Ploc.VaVal None, Ploc.VaVal fields) ;
              ciAttributes = <:vala< [] >>}
    in
    c

module Str = struct
  type t = MLast.str_item
  let of_tdecls ~loc td =
    let open Ppxlib in
    let tdPrm = HelpersBase.map_type_param_names td.ptype_params
        ~f:(fun s -> named_type_arg ~loc s)
    in
    let tdDef =
      match td.ptype_kind with
      | Ptype_variant cds ->
        let llslt = List.map (fun cd ->
            let args =
              match cd.pcd_args with
              | Pcstr_record _ -> assert false
              | Pcstr_tuple ts ->  List.map Typ.from_caml ts
            in
            <:constructor< $uid:cd.pcd_name.txt$ of $list:args$ >>
          ) cds
        in
        <:ctyp< [ $list:llslt$ ] >>
      | _ -> assert false

    in
    let t = <:type_decl< $tp:(loc, VaVal td.ptype_name.txt)$ $list:tdPrm$ = $tdDef$ >>
    in
    <:str_item< type $list:[t]$ >>
    (* TODO *)

  let single_value ~loc pat body =
    <:str_item< value $pat$ = $body$ >>

  let values ~loc ?(rec_flag=Ppxlib.Recursive) vbs =
    let vbs = List.map (fun (p,e) -> (p,e,<:vala< [] >>)) vbs in
    match rec_flag with
    | Recursive -> <:str_item< value rec $list:vbs$ >>
    | Nonrecursive -> <:str_item< value $list:vbs$ >>

  let of_vb ~loc ?(rec_flag=Ppxlib.Recursive) vb = values ~loc ~rec_flag [vb]

  let class_single ~loc ~name ?(virt=false) ?(wrap=(fun x -> x)) ~params fields =
    let c = { ciLoc = loc; ciVir = Ploc.VaVal virt;
              ciPrm = (loc, Ploc.VaVal params);
              ciNam = Ploc.VaVal name;
              ciExp = wrap @@ CeStr (loc, Ploc.VaVal None, Ploc.VaVal fields) ;
             ciAttributes = <:vala< [] >> }
    in

    <:str_item< class $list:[c]$ >>

  let tdecl ~loc ~name ~params rhs =
    let tdPrm = List.map (fun s -> (VaVal (Some s),None)) params in
    let t = <:type_decl< $tp:(loc, VaVal name)$ $list:tdPrm$ = $rhs$ >>
    in
    <:str_item< type $list:[t]$ >>

  let of_class_declarations ~loc (lcice: class_declaration list) =
    <:str_item< class $list:lcice$ >>

  let tdecl_record ~loc ~name ~params llsbt =
    let llsbt = List.map (fun (a,b,c,d) -> (a,b,c,d,<:vala< [] >>)) llsbt in
    let t = <:ctyp< { $list:llsbt$ } >> in
    tdecl ~loc ~name ~params t

  (* let functor1 ~loc name ~param sigs strs = failwith "not_implemented" *)
  let simple_gadt : loc:loc -> name:string -> params_count:int -> (string * Typ.t) list -> t =
    fun ~loc ~name ~params_count ts ->
    let ltv =
      List.init params_count (fun n ->
          (VaVal (Some (Printf.sprintf "dummy%d" n)), None)) in
    let ls = (loc, VaVal name) in
    let ltt = [] in
    let t =
      let llslt = List.map (fun (name,typ) ->
          <:constructor< $uid:name$ : $typ$ >>
        ) ts in
      <:ctyp< [ $list:llslt$ ] >>
    in
    let ltd = <:type_decl< $tp:ls$ $list:ltv$ = $t$ $list:ltt$ >>
    in
    <:str_item< type $list:[ltd]$ >>

  let module_ ~loc name me =
    <:str_item< module $uid:name$ = $mexp:me$ >>

  let modtype ~loc (_,name,topt) =
    match topt with
    | Some mt -> <:str_item< module type $name$ = $mt$ >>
    | None   -> failwith "Should not happen?"
  let tdecl_abstr ~loc = assert false
  let include_ ~loc me = <:str_item< include $me$ >>
end

module Me = struct
  type t = MLast.module_expr
  let structure ~loc lsi =
    <:module_expr< struct $list:lsi$ end >>
  let ident ~loc lident =
    let rec helper = function
      | Ppxlib.Lident s -> <:module_expr< $uid:s$ >>
      | Ppxlib.Ldot (p, s) -> <:module_expr< $helper p$ . $uid:s$ >>
      | _ -> failwith "Me.ident not_implemented"
    in
    helper lident
  let apply ~loc me1 me2 =
    <:module_expr< $me1$ $me2$ >>
  let functor_ ~loc name typ_opt me =
    match typ_opt with
    | Some mt -> <:module_expr< functor ($name$ : $mt$) -> $me$ >>
    | None -> assert false
end

module Mt = struct
  type t = MLast.module_type
  let ident ~loc (lid : Ppxlib.longident) : MLast.module_type =
    let open Ppxlib in
    match lid with
      Lident s ->
      if capitalized s then <:module_type< $uid:s$ >>
      else <:module_type< $lid:s$ >>
    | Ldot(li, s) ->
      let li = Longid.of_longident ~loc li in
      if capitalized s then <:module_type< $longid:li$ . $uid:s$ >>
      else <:module_type< $longid:li$ . $lid:s$ >>
    | Lapply _ -> failwith "Mt.ident: cannot call with an Lapply"

  let signature ~loc lsi =
    <:module_type< sig $list:lsi$ end >>

  let functor_ ~loc name typ_opt me =
    match typ_opt with
    | Some mtl -> <:module_type< functor ($name$ : $mtl$) -> $me$ >>
    | None -> assert false
  let with_ ~loc mt lwc =
    <:module_type< $mt$ with $list:lwc$ >>

end

type module_declaration = loc * string * Mt.t
let module_declaration ~loc ~name t : module_declaration  = (loc,name,t)

type module_type_declaration = loc * string * Mt.t option
let module_type_declaration ~loc ~name topt : module_type_declaration =
  (loc,name,topt)

module Sig = struct
  type t = MLast.sig_item
  let of_tdecls ~loc td =
    let open Ppxlib in
    let tdPrm = HelpersBase.map_type_param_names td.ptype_params
        ~f:(fun s -> named_type_arg ~loc s)
    in
    let tdDef =
      match td.ptype_kind with
      | Ptype_variant cds ->
        let llslt = List.map (fun cd ->
            let args =
              match cd.pcd_args with
              | Pcstr_record _ -> assert false
              | Pcstr_tuple ts -> List.map Typ.from_caml ts
            in
            <:constructor< $uid:cd.pcd_name.txt$ of $list:args$ >>
          ) cds
        in
        <:ctyp< [ $list:llslt$ ] >>
      | Ptype_abstract -> begin
          match td.ptype_manifest with
          | None -> assert false
          | Some t -> Typ.from_caml t
        end
      | _ -> assert false

    in
    let t = <:type_decl< $tp:(loc, VaVal td.ptype_name.txt)$ $list:tdPrm$ = $tdDef$ >>
    in
    <:sig_item< type $list:[t]$ >>

  let value ~loc ~name typ =
    SgVal (loc, Ploc.VaVal name, typ, <:vala< [] >>)
    (* let type_ ~loc recflg *)

  let class_ ~loc ~name ~params ?(virt=false) ?(wrap=(fun x -> x)) fields =
    (* TODO: wrap *)
    let c = { ciLoc = loc; ciVir = Ploc.VaVal virt;
              ciPrm = (loc, Ploc.VaVal params);
              ciNam = Ploc.VaVal name;
              ciExp = wrap @@ CtSig (loc, Ploc.VaVal None, Ploc.VaVal fields) ;
              ciAttributes = <:vala< [] >>
            }
    in
    <:sig_item< class $list:[c]$ >>

  let functor1 ~loc name ~param sigs_arg sigs_r =
    let mt1 = <:module_type< sig $list:sigs_arg$ end >> in
    let mt2 = <:module_type< sig $list:sigs_r$ end >> in
    let mt  = <:module_type< functor ($param$ : $mt1$) -> $mt2$ >> in
    <:sig_item< module $uid:name$ : $mtyp:mt$ >>


  let simple_gadt (* : loc:loc -> name:string -> params_count:int -> (string * Typ.t) list -> t *) =
    fun ~loc ~name ~params_count constructors ->

    let tdDef =
      (* TODO: error about gadts may be here *)
      let cs =
        List.map (fun (name,t) -> <:constructor< $uid:name$ : $t$ >> )
          constructors in
      <:ctyp< [ $list:cs$ ] >>
    in
    let tdPrm = List.init params_count (fun n ->
            (VaVal (Some (Printf.sprintf "dummy%d" n)), None)) in
    let td = <:type_decl< $tp:(loc, VaVal name)$ $list:tdPrm$ = $tdDef$ >>
    in
    <:sig_item< type $list:[td]$ >>



  let tdecl_abstr: loc:loc -> string -> string option list -> t = fun ~loc name params ->

    let tdPrm = List.map (fun s -> (VaVal s,None)) params in
    let td = <:type_decl< $tp:(loc, VaVal name)$ $list:tdPrm$ = 'abstract >>
    in
    <:sig_item< type $list:[td]$ >>

  let module_ ~loc (_,name,mtyp) =
    <:sig_item< module $uid:name$ : $mtyp:mtyp$ >>

  (* TODO: Kakadu, what is this?  I don't recognize this.  It doesn't seem to exist in Ocaml, and the construct appears to be something leftover in camlp5. *)
  let modtype ~loc (_loc,s,mt_opt) =
    let mt =
      match mt_opt with
      | Some mt -> mt
      | None -> <:module_type< 'abstract >>
    in
    <:sig_item< module type $s$ = $mt$ >>
end

module WC = struct
  type t = MLast.with_constr
  let typ ~loc ~params name t =
    let ls = (None, <:vala< name >>) in
    let ltv = List.map (fun s -> named_type_arg ~loc s) params in
    <:with_constr< type $lilongid:ls$ $list:ltv$ = $t$ >>
end

module Vb = struct
  type t = Pat.t * Exp.t
end

let value_binding ~loc ~pat ~expr = (pat, expr)

module Cf = struct
  type t = MLast.class_str_item
  let method_concrete ~loc name body_expr =
    <:class_str_item< method $lid:name$ = $body_expr$ >>

  let method_virtual ~loc name typ =
    <:class_str_item< method virtual $lid:name$ : $typ$ >>

  let inherit_ ~loc ?(as_=None) ce =
    <:class_str_item< inherit $ce$ $opt:as_$ >>
  let constraint_ ~loc t1 t2 = <:class_str_item< type $t1$ = $t2$ >>
end

module Ctf = struct
  type t = class_sig_item
  let constraint_ ~loc t1 t2 = <:class_sig_item< type $t1$ = $t2$ >>
  let method_ ~loc ?(virt=false) s t =
    if virt
    then <:class_sig_item< method virtual $lid:s$ : $t$ >>
    else <:class_sig_item< method $lid:s$ : $t$ >>

  let inherit_ ~loc cty = <:class_sig_item< inherit $cty$ >>
end

module Cty = struct
  type t = class_type
  let of_longident ~loc l =
    let open Ppxlib in
    match l with
      Lident s when not (capitalized s) -> <:class_type< $lid:s$ >>

    | Ldot(li,s) when capitalized s ->
      failwith "class_type must end in uncapitalized ident"

    | Ldot(li,s) when not (capitalized s) ->
      let li = Longid.of_longident ~loc li in
      <:class_type< $longid:li$ . $lid:s$ >>
    | Lapply _ -> failwith "Cty.of_longident: cannot call with an Lapply"
      | _ -> assert false

  let arrow ~loc t ct = <:class_type< [ $t$ ] -> $ct$ >>
  let constr ~loc longident lt =
    let ct = of_longident ~loc longident in
    <:class_type< $ct$ [ $list:lt$ ] >>
end

module Cl = struct
  type t = class_expr
  let constr ~loc lident args =
    let ls = Asttools.longident_lident_of_string_list loc (Longident.flatten lident) in
    <:class_expr< [ $list:args$ ] $lilongid:ls$ >>
  let apply ~loc l xs =
    List.fold_left (fun acc r -> <:class_expr< $acc$ $r$ >>) l  xs
  let fun_ ~loc p ce = <:class_expr< fun $p$ -> $ce$ >>
  let fun_list ~loc ps ce =
    List.fold_right (fun_ ~loc) ps ce
end


let class_structure ~self ~fields = (self, fields)
type class_structure = Pat.t * Cf.t list

let typ_arg_of_core_type t =
  match t.Ppxlib.ptyp_desc with
  | Ptyp_any -> failwith "wildcards are not supported"
  | Ptyp_var s -> named_type_arg ~loc:(loc_from_caml t.ptyp_loc) s
  | _ -> assert false

let openize_poly ~loc t =
  let lpv = [ PvInh (loc, t) ] in
  <:ctyp< [ > $list:lpv$ ] >>
  (*
  match t with
  | MLast.TyVrn (loc, name, flg) -> begin
      (*
      let flg = match flg with
      | None -> Some None
      | Some None -> Some None
      | Some (Some xs) -> Some None (* It could be a bug here *)
      in
      *)
      let flg = Some None in
      MLast.TyVrn (loc, name, flg)
    end
  | t -> t
*)

let closize_poly t =
  match t with
  | MLast.TyVrn (loc, name, flg) ->
    (fun flg -> MLast.TyVrn (loc, name, flg) )
      (
      match flg with
      | Some (Some xs) -> Some (Some xs)
      | _ -> Some (Some (VaVal []))
    )
  | t -> t



(* Need to be synchronized with Expander.params_of_interface_class *)
let prepare_param_triples ~loc ~extra
    ?(inh=fun ~loc:loc s -> Typ.var ~loc @@ "i" ^ s)
    ?(syn=fun ~loc:loc s -> Typ.var ~loc @@ "s" ^ s)
    ?(default_inh = Typ.var ~loc "syn")
    ?(default_syn = Typ.var ~loc "inh")
    names  =

  let ps = List.concat @@ List.map (fun s ->
      [ inh ~loc s; Typ.var ~loc s; syn ~loc s])
      names
  in
  ps @ [ default_inh; extra; default_syn ]

let typ_vars_of_typ t =
  let open Base in
  let rec helper acc = function
    | <:ctyp< $longid:_$ . $lid:_$ >>  -> acc
    | <:ctyp< $t1$ as $t2$ >> -> helper acc t1 (* ??? *)
    | <:ctyp< _ >>            -> acc
    | <:ctyp< $t1$ $t2$ >> -> helper (helper acc t1) t2
    | <:ctyp< $t1$ -> $t2$ >> -> helper (helper acc t1) t2
    | <:ctyp< # $lilongid:_$  >> -> acc (* I'm not sure *)
    | <:ctyp<  ~$s$:$t$   >> -> helper acc t
    | <:ctyp< $lid:s$      >> -> acc
    | <:ctyp< $t1$ == private $t2$ >>
    | <:ctyp< $t1$ ==  $t2$        >> -> helper (helper acc t1) t2
    | <:ctyp< < $list:lst$ $flag:b$ > >> ->
      List.fold ~init:acc ~f:(fun acc (_,t, _) -> helper acc t) lst
    | <:ctyp< ?$s$: $t$     >> -> helper acc t
    | <:ctyp< (module $mt$) >> -> acc
    | <:ctyp< ! $list:ls$ . $t$ >> -> failwith "not implemented"
    | <:ctyp< '$s$ >> -> s :: acc
    | <:ctyp< { $list:llsbt$ } >>  ->
      List.fold ~init:acc ~f:(fun acc (_,_,_,t, _) -> helper acc t) llsbt
    | <:ctyp< [ $list:llslt$ ] >> -> failwith "sum"
    | <:ctyp< ( $list:lt$ )    >> -> List.fold ~init:acc ~f:helper lt
    | <:ctyp< [ = $list:lpv$ ] >>  -> failwith "polyvariant"
    | _ -> acc (* This could be wrong *)
  in
  List.dedup_and_sort ~compare:String.compare @@ helper [] t

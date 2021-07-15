(* Time-stamp: <modified the 29/08/2019 (at 15:35) by Erwan Jahier> *)


open Lxm
open AstPredef
open AstV6
open AstCore
open Format
(***********************************************************************************)
(* exported *)


let rec (op2string : AstCore.by_pos_op -> string) =
  fun op ->  
    match op with 
      (* unaires *)
      | Predef_n(op) -> (AstPredef.op2string op.it)
      | (CALL_n ne) -> string_of_node_exp ne.it
      | (PRE_n    ) -> "pre"
      | (CURRENT_n) -> "current"
	   (* binaires *)
      | (ARROW_n  ) -> "->"
      | (WHEN_n _ce) -> "when"
      | (HAT_n    ) -> "^"
      | (CONCAT_n ) -> "|"
      | (IDENT_n idref) -> Lv6Id.string_of_idref false idref
      | (FBY_n    ) -> "fby"
      | (WITH_n(_,_,_)) -> "with" 
      | (TUPLE_n  ) -> assert false
      | (ARRAY_n  ) -> assert false
      | (ARRAY_ACCES_n _ ) ->  assert false
      | (ARRAY_SLICE_n _sl) -> assert false
      | (STRUCT_ACCESS_n _fld) -> assert false


(***********************************************************************************)
(* exported *)
and t (os: Format.formatter) (tree: AstV6.t) = 
  match tree with     
  | PRPackBody(_sl,pbody) ->  packbody os pbody
  | PRPack_or_models(_,_) -> assert false

and packbody (os: Format.formatter) (pkg: AstV6.packbody) = 
  let dump_def (d: item_ident) =
    try ( 
      (match d with
	     | ConstItem id -> dump_const os (Hashtbl.find pkg.pk_const_table id)
	     | TypeItem  id -> dump_type os  (Hashtbl.find pkg.pk_type_table  id)
	     | NodeItem (id,sparams) ->
	       let {src = lxm ; it = ninfo } = Hashtbl.find pkg.pk_node_table id in
	       dump_node os {src = lxm ; it = ninfo };
	       fprintf os " <<@\n" ;
	       fprintf os "@[<b 3>@   %a@]@\n" dump_static_param_list sparams;
	       fprintf os ">>\n"
      );
      Format.fprintf os "@\n"
    ) 
    with Not_found ->
      print_string ("*** unable to find a definition for " ^
		                 (match d with 
			                  ConstItem id 
			                | TypeItem  id 
			                | NodeItem (id,_) -> Lv6Id.to_string id
		                 )
		);
      flush stdout;
      assert false
  in 
  (* Format.fprintf os "@?@[<b 0>" ; *)
  List.iter dump_def pkg.pk_def_list  ;
(* Format.fprintf os "@]@."  *)

(*******************************)
(* dump d'une def de constante *)
(*******************************)
and dump_const (os: Format.formatter) (x: const_info srcflagged) = (
  let lxm = x.src and info = x.it in
  fprintf os "-- %s@\n" (Lxm.details lxm) ;
  fprintf os "const %s " (Lxm.str lxm) ;
  dump_const_def os info ;
  fprintf os " ;@\n" ;
)
and dump_const_def (os: Format.formatter) (info: const_info) = (
  match info with
    | ExternalConst (_id, ty,None) -> fprintf os ": %a " dump_type_exp ty
    | ExternalConst (_id, ty, Some v) -> 
	   fprintf os ": %a = %a" dump_type_exp ty dump_val_exp v
    | DefinedConst  (_id, None, exp) -> fprintf os "= %a " dump_val_exp exp
    | DefinedConst  (_id, Some ty, exp) -> 
	   fprintf os ": %a = %a " dump_type_exp ty dump_val_exp exp
    | EnumConst  (_id, _ty) -> fprintf os "XXX printme!"
)
(**************************)
(* dump d'une def de type *)
(**************************)
and dump_type (os: Format.formatter) (x: type_info srcflagged) = (
  let lxm = x.src and info = x.it in
  let id = Lxm.str lxm in
  fprintf os "-- %s@\n" (Lxm.details lxm) ;
  fprintf os "type %s " id ;
  dump_type_def os info ;
  fprintf os " ;@\n" ;
)
and dump_type_def (os: Format.formatter) (info: type_info ) = (
  match info with
    | ExternalType _id -> () 
    | AliasedType (_id, te) -> fprintf os "= %a" dump_type_exp te
    | EnumType (_id, clist) ->
	   (* fprintf os "= enum {@[<b 3>@,%a@,@]}" dump_id_list clist *)
	   fprintf os "= enum { %a}" dump_id_list clist
        
    | StructType {st_name = _id; st_flist = fl; st_ftable = ft} ->
	   let filst = List.map (function id -> (Hashtbl.find ft id).it) fl in
	   fprintf os "= struct {@\n@[<b 3>   %a@]@\n}"
	     dump_field_list filst

    | ArrayType (_id, texp, vexp) ->
	   fprintf os " = %a" dump_type_exp texp;
	   fprintf os "^%a"   dump_val_exp vexp;
	   
)
(****************************)
(* dump d'une liste de noms *)
(****************************)
and dump_id_list (os : formatter) (idlst : Lv6Id.t srcflagged list) = (
  match idlst with 
      [] -> () 
    | h::[] -> ( fprintf os "%s" (Lv6Id.to_string h.it))
    | h::t  -> ( fprintf os "%s, %a" (Lv6Id.to_string h.it) dump_id_list t)
)
(*****************************)
(* dump d'une liste de field *)
(*****************************)
and dump_field_list (os: Format.formatter) (filst: field_info list) = (
  match filst with
      [] -> ()
    | h::[] -> ( fprintf os "%a" dump_field h )
    | h::t	-> ( fprintf os "%a;@\n%a" dump_field h dump_field_list t)
)
and dump_field (os: Format.formatter) (finfo: field_info) = (
  match finfo with
      {fd_name=id; fd_type=ty; fd_value=None} -> (
	     fprintf os "%s : %a" (Lv6Id.to_string id) dump_type_exp ty
      ) 
    |
	     {fd_name=id; fd_type=ty; fd_value=Some ex} -> (
	       fprintf os "%s : %a = %a" (Lv6Id.to_string id) 
	         dump_type_exp ty dump_val_exp ex
	     )
)
and _dump_param_list 
    (os: Format.formatter)
  (plst: (Lv6Id.t option * type_exp) list) = 
  (
    match plst with
	     [] -> ()
      | h::[] -> ( fprintf os "%a" _dump_param h )
      | h::t  -> ( fprintf os "%a; %a" _dump_param h _dump_param_list t) 
  )
and _dump_param (os: Format.formatter) (p: (Lv6Id.t option * type_exp)) = (
  match p with
      (None, ty) -> (fprintf os "%a" dump_type_exp ty)
    | (Some id, ty) -> (fprintf os "%s : %a" (Lv6Id.to_string id) dump_type_exp ty) 
) 
(**************************)
(* dump d'une eq. de node *)
(**************************)
and dump_item_info_list 
    (os: Format.formatter) 
  (lst: item_info srcflagged list) =
  (
    match lst with
	     [] -> ()
      | h::[]	-> dump_item_info os h
      | h::t  -> fprintf os "%a;@\n%a" dump_item_info h dump_item_info_list t 
  )
and dump_item_info
    (os: Format.formatter)
  (ie: item_info srcflagged) = (
  match ie.it with
	 | ConstInfo ci -> dump_const os (Lxm.flagit ci ie.src)
	 | TypeInfo ti -> dump_type os (Lxm.flagit ti ie.src)
	 | NodeInfo ni -> dump_node os (Lxm.flagit ni ie.src)
)
  

and dump_static_param_list 
    (os: Format.formatter) 
  (lst: static_param srcflagged list) =
  (
    match lst with
	     [] -> ()
      | h::[]	-> dump_static_param os h
      | h::t  -> fprintf os "%a;@\n%a" dump_static_param h dump_static_param_list t 
  )
and dump_static_param 
    (os: Format.formatter) 
  (sp: static_param srcflagged) =
  (
    match sp.it with
      | StaticParamType id ->  fprintf os "type %s" (Lv6Id.to_string id)
      | StaticParamConst (id, exp) -> fprintf os "const %s : %a" 
	     (Lv6Id.to_string id) dump_type_exp exp
      | StaticParamNode (id, ins, outs, has_mem,is_safe) -> (
	     fprintf os "%s%s %s(@,%a@,)returns(@,%a@,)" 
	       (if is_safe then "" else "unsafe ")
	       (if has_mem then "node" else "function")
	       (Lv6Id.to_string id)
	       dump_line_var_decl_list ins dump_line_var_decl_list outs
	   )
  )
(**************************)
(* dump d'une def de node *)
(**************************)
and dump_node (os: Format.formatter) (x: node_info srcflagged) = (
  let lxm = x.src and ninfo = x.it in
  fprintf os "-- %s" (Lxm.details lxm) ;
  fprintf os " (node definition)@\n" ;
  fprintf os "node %s" (Lv6Id.to_string ninfo.name);
  fprintf os " <<@\n" ;
  fprintf os "@[<b 3>@   %a@]@\n" dump_static_param_list ninfo.static_params;
  fprintf os ">>\n";
  (match ninfo.vars with
    | None -> ()
    | Some {
	   inlist  = inlist;
	   outlist = outlist;
	   loclist = loclist_opt;
	   vartable = vartab;
	 } -> 
	   let get_info (id: Lv6Id.t) = (Hashtbl.find vartab id).it in
	   let inlst = List.map get_info inlist in
	   let outlst = List.map get_info outlist in
	   fprintf os "(@\n" ;
	   fprintf os "@[<b 3>@   %a@]@\n" dump_var_decl_list inlst ;
	   fprintf os ") returns (@\n" ;
	   fprintf os "@[<b 3>@   %a@]@\n" dump_var_decl_list outlst ;
	   fprintf os ");@\n" ;
	   match loclist_opt with
	     | None -> ()
	     | Some loclist -> 
		    let loclst = List.map get_info loclist in
		    fprintf os "var@\n";
		    fprintf os "@[<b 3>   %a;@]@\n" dump_var_decl_list loclst;
  );
  
  (match ninfo.def with
    | Extern    -> fprintf os "extern"
    | Abstract  -> fprintf os "abstract" 
    | Body body -> dump_node_body os body
    | Alias {it = _nexp; src = _lxm} -> assert false
  (* 	   fprintf os " = @,%a;@\n" dump_by_pos_exp nexp *)

  );
  if ninfo.has_mem then () else ();
  if ninfo.is_safe then () else ();

)

and dump_node_body (os: Format.formatter) (nbody: node_body) = (
  fprintf os "@[<b 3>let" ;
  (* les assertions *)
  dump_assert_list os nbody.asserts ;	
  (* les equations *)
  dump_eq_list os nbody.eqs ;
  fprintf os "@]@\ntel" ;
)
(* déclarations sur plusieurs lignes, indentées ... *)
and dump_var_decl_list (os: Format.formatter) (lst: var_info list) = (
  match lst with
      [] -> ()
    | h::[] -> ( fprintf os "%a" dump_var_decl h )
    | h::t -> ( fprintf os "%a;@\n%a" dump_var_decl h dump_var_decl_list t )
)
(* déclarations sur une ligne AVEC SOURCE ... *)
and dump_line_var_decl_list (os: Format.formatter) (lst: var_info srcflagged list) = (
  match lst with
      [] -> ()
    | h::[] -> ( fprintf os "%a" dump_var_decl h.it )
    | h::t -> ( fprintf os "%a;@,%a" dump_var_decl h.it dump_line_var_decl_list t )
)
and dump_var_decl (os: Format.formatter) (vinfo: var_info  ) = (
  fprintf os "%s : %a" (Lv6Id.to_string vinfo.var_name) dump_type_exp vinfo.var_type ;
  (
    match vinfo.var_clock with
	     Base -> ()
      | NamedClock({it=cc,id;src=_lxm}) ->
        let cc_str = Lv6Id.string_of_long false cc in
        let id_str = Lv6Id.to_string id in
        let clk_str = 
          if cc_str = "true" then id_str
          else if cc_str = "false" then ("not " ^ id_str) 
          else (cc_str ^ "(" ^ id_str ^ ")")
        in
        (fprintf os " when %s" ) clk_str
  )
)
(**************************)
(* dump d'un type immédiat*)
(**************************)
and dump_type_exp (os: Format.formatter) (x: type_exp) = (
  match x.it with
      Bool_type_exp -> fprintf os "bool"
    |  Int_type_exp  -> fprintf os "int"
    |  Real_type_exp -> fprintf os "real"
    |  Named_type_exp id -> fprintf os "%s" (Lv6Id.string_of_idref false id)
    |  Array_type_exp (te, sz) -> (
	   dump_type_exp os te ;
	   fprintf os "^" ;
	   dump_val_exp os sz
    )
)
(**************************)
(* dump des assertions    *)
(**************************)
and dump_assert_list (os: Format.formatter) (af: (val_exp srcflagged) list) = (
  match af with
      [] -> ()
    | a :: reste -> (
	   Format.fprintf os "@\nassert %a;" dump_val_exp a.it ;
	   Format.fprintf os "%a" dump_assert_list reste
	 )
)
(**************************)
(* dump des equations     *)
(**************************)
and dump_eq_list (os: Format.formatter) (eqfs: (eq_info srcflagged) list) = (
  match eqfs with
      [] -> ()
    | {it=(lflst,exp); src=_lxm} :: reste -> (
	   Format.fprintf os "@\n%a = %a;%a"
	     dump_left_part_list lflst
	     dump_val_exp exp
	     dump_eq_list reste
	 )
)
and dump_left_part_list (os: Format.formatter) (lfts: left_part list) =
  (
    match lfts with
	     l::[] ->     dump_left_part os l
      | l::reste ->  fprintf os "%a,@,%a" dump_left_part l dump_left_part_list reste
      | _ -> assert false
	     
  )
and dump_left_part (os: Format.formatter) (lft: left_part) =
  (
    match lft with
	     LeftVar idflg ->  fprintf os "%s" (Lv6Id.to_string idflg.it)
      | LeftField (l, idflg) ->  fprintf os "%a.%s" dump_left_part l (Lv6Id.to_string idflg.it)
      | LeftArray (l, expflg) ->
	     fprintf os "%a[@,%a@,]" dump_left_part l dump_val_exp expflg.it
      | LeftSlice (l, slcflg) ->
	     fprintf os "%a[@,%a@,]" dump_left_part l dump_slice_info slcflg.it
  )
(**************************)
(* dump d'une expression  *)
(**************************)
and dump_val_exp (os: Format.formatter) (x: val_exp) = (
  match x with
    | CallByPos ( {it=oper; src=_lxm} , pars ) -> dump_by_pos_exp os oper pars
    | CallByName ({it=oper; src=_lxm},nm_pars) -> dump_by_name_exp os oper nm_pars
    | Merge_n (id, _) -> (* finish me *)
      fprintf os "merge  %a (...) " dump_val_exp id.it
    | Merge_bool_n({it=id;_}, t, f) -> 
      fprintf os "merge %a (true -> %a) (false -> %a)"  
        dump_val_exp id dump_val_exp t dump_val_exp f
)
and dump_val_exp_list (os : formatter) (xl: val_exp list) = (
  match xl with 
      [] -> () 
    | h::[] -> ( fprintf os "%a" dump_val_exp h ) 
    | h::t  -> ( fprintf os "%a,@,%a" dump_val_exp h dump_val_exp_list t)
)
(****************************)
(* Appels "par position"    *)
(****************************)
and dump_by_pos_exp (os: Format.formatter) (oper: by_pos_op) (pars: operands) =
  (
    match (oper, pars) with 
      | (IDENT_n  id,Oper [])  -> dump_leaf_exp os (Lv6Id.string_of_idref false id)
      | (PRE_n,      Oper [p0]) -> dump_unary_exp os "pre" p0
      | (CURRENT_n,  Oper [p0]) -> dump_unary_exp os "current" p0
      | (ARROW_n,  Oper [p0;p1]) -> dump_binary_exp os "->" p0 p1
      | (FBY_n,    Oper [p0;p1]) -> dump_binary_exp os "fby" p0 p1
      | (WHEN_n _,   Oper [p0;p1]) -> dump_binary_exp os "when" p0 p1

      | (Predef_n (x), _) -> (
        match (x.it, pars) with
	       | (TRUE_n,     Oper [])  -> dump_leaf_exp os "true" 
          | (FALSE_n,    Oper [])  -> dump_leaf_exp os "false"
          | (ICONST_n s, Oper []) -> dump_leaf_exp os (Lv6Id.to_string s) 
          | (RCONST_n s, Oper []) -> dump_leaf_exp os (Lv6Id.to_string s)
	       (* unaires *)
          | (NOT_n,      Oper [p0]) -> dump_unary_exp os "not" p0
          | (UMINUS_n,   Oper [p0]) -> dump_unary_exp os "-" p0
          | (RUMINUS_n,  Oper [p0]) -> dump_unary_exp os "-" p0
          | (IUMINUS_n,  Oper [p0]) -> dump_unary_exp os "-" p0
          | (REAL2INT_n, Oper [p0]) -> dump_unary_exp os "int" p0
          | (INT2REAL_n, Oper [p0]) -> dump_unary_exp os "real" p0
	       (* binaires *)
          | (AND_n,    Oper [p0;p1]) -> dump_binary_exp os "and" p0 p1
          | (OR_n,     Oper [p0;p1]) -> dump_binary_exp os "or" p0 p1
          | (XOR_n,    Oper [p0;p1]) -> dump_binary_exp os "xor" p0 p1
          | (IMPL_n,   Oper [p0;p1]) -> dump_binary_exp os "=>" p0 p1
          | (EQ_n,     Oper [p0;p1]) -> dump_binary_exp os "=" p0 p1
          | (NEQ_n,    Oper [p0;p1]) -> dump_binary_exp os "<>" p0 p1
          | (LT_n,     Oper [p0;p1]) -> dump_binary_exp os "<" p0 p1
          | (LTE_n,    Oper [p0;p1]) -> dump_binary_exp os "<=" p0 p1
          | (GT_n,     Oper [p0;p1]) -> dump_binary_exp os ">" p0 p1
          | (GTE_n,    Oper [p0;p1]) -> dump_binary_exp os ">=" p0 p1
          | (DIV_n,    Oper [p0;p1]) -> dump_binary_exp os "div" p0 p1
          | (MOD_n,    Oper [p0;p1]) -> dump_binary_exp os "mod" p0 p1
          | (MINUS_n,  Oper [p0;p1]) -> dump_binary_exp os "-" p0 p1
          | (RMINUS_n,  Oper [p0;p1]) -> dump_binary_exp os "-" p0 p1
          | (IMINUS_n,  Oper [p0;p1]) -> dump_binary_exp os "-" p0 p1
          | (PLUS_n,   Oper [p0;p1]) -> dump_binary_exp os "+" p0 p1
          | (RPLUS_n,   Oper [p0;p1]) -> dump_binary_exp os "+" p0 p1
          | (IPLUS_n,   Oper [p0;p1]) -> dump_binary_exp os "+" p0 p1
          | (SLASH_n,  Oper [p0;p1]) -> dump_binary_exp os "/" p0 p1
          | (RSLASH_n,  Oper [p0;p1]) -> dump_binary_exp os "/" p0 p1
          | (ISLASH_n,  Oper [p0;p1]) -> dump_binary_exp os "/" p0 p1
          | (TIMES_n,  Oper [p0;p1]) -> dump_binary_exp os "*" p0 p1
          | (RTIMES_n,  Oper [p0;p1]) -> dump_binary_exp os "*" p0 p1
          | (ITIMES_n,  Oper [p0;p1]) -> dump_binary_exp os "*" p0 p1
          | (IF_n,   Oper [p0;p1;p2]) -> dump_ternary_exp os "if" "then" "else" p0 p1 p2
          | (NOR_n,    Oper pl) -> dump_nary_exp os "nor" pl
          | (DIESE_n,  Oper pl) -> dump_nary_exp os "#" pl
          | (_,_) -> (
            Lv6errors.print_internal_error "AstV6Dump.dump_by_pos_exp"
               (Printf.sprintf "unexpected op '%s'" (AstPredef.op2string_long x.it));
            assert false
          )
      )
      | (HAT_n,    Oper [p0;p1]) -> dump_binary_exp os "^" p0 p1
      | (CONCAT_n, Oper [p0;p1]) -> dump_binary_exp os "|" p0 p1
      | (WITH_n(_), Oper [p0;p1;p2]) -> dump_ternary_exp os "with" "then" "else" p0 p1 p2
      | (TUPLE_n,  Oper pl) -> dump_nary_exp os "" pl
      | (CALL_n s, Oper pl) -> fprintf os "%a(@,%a@,)"
	     dump_node_exp s.it dump_val_exp_list pl 
      | (ARRAY_n,  Oper pl) -> fprintf os "[@,%a@,]" dump_val_exp_list pl
      | (ARRAY_ACCES_n ix, Oper [p0]) ->  fprintf os "%a[@,%a@,]" 
	     dump_val_exp p0 dump_val_exp ix 
      | (ARRAY_SLICE_n sl, Oper [p0]) ->  fprintf os "%a[@,%a@,]" 
	     dump_val_exp p0 dump_slice_info sl 
      | (STRUCT_ACCESS_n fld, Oper [p0]) -> fprintf os "%a.%s"
	     dump_val_exp p0 (Lv6Id.to_string fld)

      | (FBY_n, _) -> assert false

      | (STRUCT_ACCESS_n _, _) -> assert false
      | (ARRAY_SLICE_n _, _) -> assert false
      | (ARRAY_ACCES_n _, _) -> assert false
      | (WITH_n(_), _) -> assert false
      | (CONCAT_n, _) -> assert false
      | (HAT_n, _) -> assert false


      | (WHEN_n _, _) -> assert false
      | (ARROW_n, _) -> assert false
      | (CURRENT_n, _) -> assert false
      | (PRE_n, _) -> assert false
      | (IDENT_n _, _) -> assert false

  )
(* les procs standard pour les operateurs predefs *)
and dump_leaf_exp (os : Format.formatter) (s : string) = (
  fprintf os "%s" s
)
and dump_unary_exp
    (os : Format.formatter)
  (s : string)
  (op0: val_exp)
    = (
  fprintf os "(@,%s %a@,)" s dump_val_exp op0
)
and dump_binary_exp
    (os : Format.formatter)
  (s : string)
  (op0: val_exp)
  (op1: val_exp)
    = (
  fprintf os "(@,%a %s %a@,)" dump_val_exp op0 s dump_val_exp op1
)
and dump_ternary_exp
    (os : Format.formatter)
  (s : string)
  (t : string)
  (e : string)
  (op0: val_exp)
  (op1: val_exp)
  (op2: val_exp)
    = (
  fprintf os "(@,%s %a %s %a %s %a@,)"
	 s dump_val_exp op0 t dump_val_exp op1 e dump_val_exp op2
)
and dump_nary_exp
    (os : Format.formatter)
  (s: string)
  (ops: val_exp list)
    = (
  fprintf os "%s(@,%a@,)" s dump_val_exp_list ops
)

and string_of_node_exp (id, sal) = 
  (Lv6Id.string_of_idref false id) ^
	 (if sal = [] then "" else 
	     "<<" ^ (String.concat ", " (List.map static_arg_to_string sal)) ^ ">>")

and static_arg_to_string arg = 
  match arg.it with
    | StaticArgLv6Id id -> Lv6Id.string_of_idref false id
    | StaticArgConst _ve -> "const xxx"
    | StaticArgType  _te -> "type xxx"
    | StaticArgNode  op -> "node "^(op2string op)

and dump_node_exp  os ne =
  fprintf os "%s" (string_of_node_exp ne)

and _dump_static_sarg_list 
    (os : Format.formatter)
  (lst: (static_arg srcflagged) list)
    = (
  match lst with
	 | [] -> ()
	 | [sa] -> fprintf os "%a" _dump_static_sarg sa.it
	 | sa::reste ->
	   fprintf os "%a, @,%a" _dump_static_sarg sa.it _dump_static_sarg_list reste
)
and _dump_static_sarg
    (os : Format.formatter)
  (sa: static_arg)
    =
  match sa with
	 | StaticArgLv6Id id -> fprintf os "%s" (Lv6Id.string_of_idref false id)
	 | StaticArgConst ve -> fprintf os "const %a"    dump_val_exp ve
	 | StaticArgType  te -> fprintf os "type %a"     dump_type_exp te
	 | StaticArgNode  op -> fprintf os "node %s"     (op2string op)

and dump_static_arg_list 
    (os : Format.formatter)
  (lst: (Lv6Id.t * static_arg srcflagged) list)
    = (
  match lst with
	 | [] -> ()
	 | [sa] -> fprintf os "%a" dump_static_arg sa
	 | sa::reste ->
	   fprintf os "%a, @,%a" dump_static_arg sa dump_static_arg_list reste
)
and dump_static_arg
    (os : Format.formatter)
  ((id,sa): Lv6Id.t * static_arg srcflagged) 
    =
  fprintf os "%s = " (Lv6Id.to_string id);
  match sa.it with
	 | StaticArgLv6Id id -> fprintf os "%s" (Lv6Id.string_of_idref false id)
	 | StaticArgConst ve -> fprintf os "const %a"    dump_val_exp ve
	 | StaticArgType  te -> fprintf os "type %a"     dump_type_exp te
	 | StaticArgNode  op -> fprintf os "node %s"     (op2string op)
(* 	| StaticArgFunc  ne -> fprintf os "function %a" dump_node_exp ne *)
		
and dump_slice_info 
    (os : Format.formatter)
  (sl: slice_info)
    = (
  fprintf os "%a@, .. @,%a" dump_val_exp sl.si_first dump_val_exp sl.si_last ;
  match sl.si_step with
	   Some e -> fprintf os "@, step %a" dump_val_exp e
	 | None -> ()
)
(****************************)
(* Appels "par noms"        *) 
(****************************)
and dump_by_name_exp
    (os: Format.formatter)
  (oper: by_name_op)
  (pars: (Lv6Id.t srcflagged * val_exp) list) =
  (
    match (oper, pars) with
      | (STRUCT_n id, pl) -> (
	     fprintf os "%s{@,%a@,}" (Lv6Id.string_of_idref false id) dump_named_pars pl
	   ) 
      | (STRUCT_WITH_n (id1,id2), pl) -> (
	     fprintf os "%s{ %s with @,%a@,}" (Lv6Id.string_of_idref false id1) 
          (Lv6Id.string_of_idref false id2) dump_named_pars pl
	   ) 
      | (STRUCT_anonymous_n, pl) -> (
	     fprintf os "{@,%a@,}" dump_named_pars pl
	   )
  )
and dump_named_pars
    (os: Format.formatter)
  (pars: (Lv6Id.t srcflagged * val_exp) list) =
  ( match pars with
	   [] -> () 
    |(v,e)::[] -> fprintf os "%s = %a" (Lv6Id.to_string v.it) dump_val_exp e
    |(v,e)::l  ->
	   fprintf os "%s = %a;@,%a" (Lv6Id.to_string v.it) dump_val_exp e 
	     dump_named_pars l
  )



let dump_packinstance (os: Format.formatter) (pi: AstV6.pack_instance) = (
      Format.fprintf os "= %s(%a);@\n" 
	(Lv6Id.to_string pi.pi_model)
	dump_static_arg_list pi.pi_args ;
    )

let dump_packgiven (os: Format.formatter) (pg: AstV6.pack_given) = (
  (
    match (pg.pg_provides) with
	Some pl -> (
	  Format.fprintf os "provides@\n@[<b 3>   %a@]@,;@\n"
	    dump_item_info_list pl
	) | _ -> ()
  );
  Format.fprintf os "body@\n@[<b 3>   %a@]@\nend@\n"
    packbody  pg.pg_body ;
)

(***********************************************************************************)
(* exported *)
let packinfo (os: Format.formatter) (pf: AstV6.pack_info srcflagged) = (
  let (p, lxm) = (pf.it, pf.src) in
    Format.fprintf os "@?@[<b 0>" ;
    Format.fprintf os "-----------------------------\n";
    Format.fprintf os "-- PACKAGE DEF \"%s\"\n" (Lv6Id.pack_name_to_string p.pa_name);
    Format.fprintf os "-----------------------------\n";
    Format.fprintf os "-- %s\n" (Lxm.details lxm) ;
    (
      match (p.pa_def) with
	  PackGiven pg -> dump_packgiven os pg 
	| PackInstance pi -> dump_packinstance os pi 
    );
    Format.fprintf os "-----------------------------\n";
    Format.fprintf os "@]@." 
)
  

(***********************************************************************************)
(* exported *)
let modelinfo (os: Format.formatter) (mf: AstV6.model_info srcflagged) = (
      let (m, lxm) = (mf.it, mf.src) in
	Format.fprintf os "@?@[<b 0>" ;
	Format.fprintf os "-----------------------------\n";
	Format.fprintf os "-- MODEL DEF \"%s\"\n" (Lv6Id.pack_name_to_string m.mo_name);
	Format.fprintf os "-----------------------------\n";
	Format.fprintf os "-- %s\n" (Lxm.details lxm) ;
	Format.fprintf os "model %s@\n" (Lv6Id.pack_name_to_string m.mo_name);

	( match (m.mo_needs) with
	      [] -> ()
	    | _ -> (
		Format.fprintf os "needs@\n@[<b 3>   %a@]@,;@\n"
		  dump_static_param_list m.mo_needs
	      )
	);
	( match (m.mo_provides) with
	      Some pl -> (
		Format.fprintf os "provides@\n@[<b 3>   %a@]@,;@\n"
		  dump_item_info_list pl
	      ) | _ -> ()
	);
	Format.fprintf os "body@\n@[<b 3>   %a@]@\nend@\n"
	  packbody  m.mo_body ;
	Format.fprintf os "-----------------------------\n";
	Format.fprintf os "@]@." 
    )

let print_val_exp oc ve =
	let os = Format.formatter_of_out_channel oc in
	dump_val_exp os ve;
	pp_print_flush os ()


let print_node_exp oc ne =
	let os = Format.formatter_of_out_channel oc in
	dump_node_exp os ne;
	pp_print_flush os ()

(* on one line for debug ... *)

let print_short_val_exp oc ve =
  let os = Format.formatter_of_out_channel oc in
  let fof : Format.formatter_out_functions =
    pp_get_formatter_out_functions os ()
  in
  let fof = {
    fof
    with 
      Format.out_string  = (fun s p n -> output_string oc (String.sub s p n));
      Format.out_newline = (fun () -> ());
      Format.out_spaces  = (fun _ -> ());
      (* This one has been introduced in ocaml 4.06; hence we use 
         the default formater ti ovoid braking backward compatility
         Format.out_indent  = (fun _ -> ()); *)
      Format.out_flush   = (fun () -> flush oc);
  }
  in
  Format.pp_set_formatter_out_functions os fof;
  dump_val_exp os ve;
  pp_print_flush os ()



(* Time-stamp: <modified the 29/08/2019 (at 15:28) by Erwan Jahier> *)


open Lxm
open AstPredef
open AstCore
open Lic
open IdSolver
open Lv6errors
open Lv6Id

(** debug flag: on prend le meme que LicTab ... *)
let dbg = (Lv6Verbose.get_flag "lazyc")

(******************************************************************************)
(* exception Ast2licType_error of string *)

(* exported *)
let rec (of_type: IdSolver.t -> AstCore.type_exp -> Lic.type_) =
  fun env texp ->
    match texp.it with
      | Bool_type_exp -> Bool_type_eff
      | Int_type_exp  -> Int_type_eff
      | Real_type_exp -> Real_type_eff
      | Named_type_exp s -> env.id2type s texp.src
      | Array_type_exp (elt_texp, szexp) ->
        let elt_teff = of_type env elt_texp in
        try
          let sz = EvalConst.eval_array_size env szexp in
          Array_type_eff (elt_teff, sz)
        with EvalConst.EvalArray_error msg -> 
          let lxm = AstCore.lxm_of_val_exp szexp in
          raise (Compile_error(lxm, "can't eval type: "^msg))



let (_add_pack_name : IdSolver.t -> Lxm.t -> Lv6Id.idref -> Lv6Id.idref) =
  fun id_solver lxm cc -> 
    try
      match Lv6Id.pack_of_idref cc with
        | Some _ -> cc
        | None ->
          let id = Lv6Id.of_idref false cc in
          let pn = 
            AstTabSymbol.find_pack_of_const id_solver.global_symbols id lxm 
          in
          Lv6Id.make_idref pn id
    with _ -> cc (* raise en error? *)


(* exported *)
let (of_clock : IdSolver.t -> AstCore.var_info -> Lic.id_clock)=
  fun id_solver v ->
    match v.var_clock with
      | Base -> v.var_name, BaseLic
      | NamedClock({ it=(cc,cv) ; src=lxm }) ->
        let vi = id_solver.id2var cv lxm in
        let _, clk = vi.var_clock_eff in
        let ct = vi.var_type_eff in
        v.var_name, On((cc,cv,ct), clk)

(******************************************************************************)
(* Checks that the left part has the same type as the right one. *)
and (type_check_equation: IdSolver.t -> Lxm.t -> Lic.left list -> 
     Lic.val_exp -> unit) =
  fun id_solver lxm lpl_eff ve_eff -> 
    let lpl_teff = List.map Lic.type_of_left lpl_eff in
    let ve_eff, right_part = EvalType.f id_solver ve_eff in
    if (List.length lpl_teff <> List.length right_part) then
      raise (Compile_error(lxm, 
                           "tuple size error: \n*** the tuple size is\n***\t"^ 
                             (string_of_int (List.length lpl_teff)) ^
                             " for the left-hand-side, and \n***\t" ^ 
                             (string_of_int (List.length right_part)) ^
                             " for the right-hand-side (in " ^
                               (String.concat
                                  "," 
                                  (List.map (LicDump.string_of_leff false) lpl_eff))
                               ^ " = " ^
                             (LicDump.string_of_val_exp_eff false ve_eff) ^ ")\n"   
      ))
    else
      List.iter2
        (fun le re -> 
          if le <> re then
            let msg = "type mismatch: \n***\t'" 
              ^ (Lic.string_of_type le) ^ 
                "' (left-hand-side) \n*** is not compatible with \n***\t'" 
              ^ (Lic.string_of_type re) ^ "' (right-hand-side)"
            in
            raise (Compile_error(lxm, msg))
        )
        lpl_teff
        right_part

(* Checks that the left part has the same clock as the right one. *)
and (clock_check_equation:IdSolver.t -> Lxm.t -> UnifyClock.subst -> 
     Lic.left list -> Lic.id_clock list -> Lic.val_exp -> Lic.val_exp) =
  fun _id_solver _lxm s lpl_eff right_part_clks ve_eff ->
    let lxms = List.map Lic.lxm_of_left lpl_eff in
    EvalClock.check_res lxms s lpl_eff right_part_clks;
    ve_eff

(******************************************************************************)
(* 
ICI : BEQUILLE(S)
on fait un lookup dans la table des operateurs
pour rechercher leurs (éventuels) parametres statiques : 

TRAITER LES MACROS PREDEF :
- ici, on juste besoin de fabriquer les arguments statiques effectifs
  à partir des arguments donnés et des args attendus.
- on cherche pas à faire rentrer dans le moule, on délègue 

- 2015/07 -> probleme des node avec param statiques identifies par pack::node 
  c'etait pas prevu du  tout ...
  rajout du champs "all_srcs" dans le id solver qui premet de retrouver
  n'importe quelle info source (un peu extreme comme solution ...) 
*)

(* pour abstraire la nature des params statiques *)
type abstract_static_param =
   | ASP_const of Lv6Id.t
   | ASP_type of Lv6Id.t
   | ASP_node of Lv6Id.t

let do_abstract_static_param x = 
match x.it with
   | StaticParamType id -> ASP_type id
   | StaticParamConst (id,_) -> ASP_const id
   | StaticParamNode (id,_,_,_,_) -> ASP_node id


let get_abstract_static_params
   (srcs: AstTab.t)
   (symbols: AstTabSymbol.t)
   (lxm: Lxm.t)
   (idref: Lv6Id.idref)
: abstract_static_param list =
                     
   Lv6Verbose.exe ~flag:dbg (fun () ->
     Printf.fprintf stderr "#DBG: Ast2lic.get_abstract_static %s\n"
       (Lv6Id.raw_string_of_idref idref)
   ) ;
   match (idref.id_pack, idref.id_id) with
      | (Some "Lustre", "map")
      | (Some "Lustre", "red")
      | (Some "Lustre", "fill")
      | (Some "Lustre", "fillred") -> [ ASP_node "oper"; ASP_const "size" ]
      | (Some "Lustre", "boolred") -> [ ASP_const "min"; ASP_const "max"; ASP_const "size"]
      | (Some "Lustre", "condact") -> [  ASP_node "oper";  ASP_const "dflt" ]
      | (Some pck, nid) -> (
         (* 2015/07 -> nouveau cas, on cherche les params statiques en tapant
            directement dans le source *)
         let packsrc = match AstTab.pack_prov_env srcs pck with
         | Some ps -> ps
         | None -> AstTab.pack_body_env srcs pck
         in
         let spl = match AstTabSymbol.find_node packsrc nid lxm with
         | AstTabSymbol.Local ni -> ni.it.static_params
         | _ -> assert false
         in List.map do_abstract_static_param spl
      )
      | (None, nid) -> (
          (* try *)
            (* let spl = match AstTabSymbol.find_node symbols (Lv6Id.name_of_idref idref) lxm with *)
            let spl = match AstTabSymbol.find_node symbols nid lxm with
            | AstTabSymbol.Local ni -> ni.it.static_params
            | AstTabSymbol.Imported(_imported_node, params) -> params
            in List.map do_abstract_static_param spl
              (*          with Compile_error(_,_)  -> *)
            (* can occur for static node parameters, which cannot
               themselves have static parameters.  A better solution ougth
               to be to add node static parameters in the AstTabSymbol.t
               however (in Lazycompiler.node_check_do most probably). 
      
               OUI MAIS GROS BUG : qu'est-ce-qui se passe si si le
               'static node parameter' porte le meme nom qu'un noeud
               existant dans AstTabSymbol ???
      
               C'est clairement pas la bonne méthode ...
               Voir + bas ...
      
            *)
            (*             [] *)
     ) 


(* exported *)

let rec of_node
    (id_solver : IdSolver.t) (ne: AstCore.node_exp srcflagged) : Lic.node_exp =

  Lv6Verbose.exe ~flag:dbg (fun () ->
      Printf.fprintf stderr "\n\n#DBG: ENTERING Ast2lic.of_node \'";
      AstV6Dump.print_node_exp stderr ne.it;
      Printf.fprintf stderr "'\n\n";

    );
  let lxm = ne.src in
  let (idref, static_args) = ne.it in
  (* pas tres beau : on corrige le idref des predefs ... *)
  let idref = match (idref.id_pack, idref.id_id) with
    | (None, "map")
    | (None, "red")
    | (None, "fill")
    | (None, "fillred")
    | (None, "boolred")
    | (None, "condact") -> {idref with id_pack = Some "Lustre"}
    | _ -> idref
  in
  (* BUG des param statique node avec le meme nom
     qu'un node template global : 
     pis-aller : si static_args = [],
     on a peut-etre affaire à un static param node, donc
     on appelle directement id_solver.id2node et c'est lui
     qui plantera si ce n'est pas le cas et qu'il fallait
     des static_args...
     si static_args <> [], de toute maniere ca ne peut PAS
     etre un static param node
  *)

  (* NOUVELLE VERSION ÉPURÉE :
     ON ne fait AUCUNE vérif de cohérence de types pour les param statiques,
     on ne vérifie QUE la nature (pour pouvoir résoudre les args qui sont des idents
     A FAIRE + TARD ? !!
  *)
  let static_args_eff = match static_args with
    | [] -> []
    | _ ->
      let static_params =
        get_abstract_static_params id_solver.all_srcs id_solver.global_symbols lxm idref
      in
      let sp_l = List.length static_params 
      and sa_l = List.length static_args in
      if (sp_l <> sa_l) then
        let msg = "Bad number of (static) arguments: " ^ 
                  (string_of_int sp_l) ^ " expected, and " ^ 
                  (string_of_int sa_l) ^ " provided."
        in
        raise (Compile_error(lxm, msg))
      else
        List.map2 (check_static_arg id_solver) 
          static_params 
          static_args
  in
  let res = id_solver.id2node idref static_args_eff lxm in
  Lv6Verbose.exe ~flag:dbg (fun () ->
      Printf.fprintf stderr "\n#DBG: LEAVING Ast2lic.of_node \'";
      AstV6Dump.print_node_exp stderr ne.it;
      Printf.fprintf stderr "'\n";
      Printf.fprintf stderr "    RESULT:\n%s\n" (Lic.string_of_node_exp res);
    );
  res

and check_static_arg
    (node_id_solver: IdSolver.t)
    (asp: abstract_static_param) 
    (sa: AstCore.static_arg srcflagged) 
  : Lic.static_arg =
  (
    (* 1ere passe :
       on utilise expected juste pour résoudre la nature,
       on "compile" les args 
    *)
    let nature_error nat =
      let msg = Printf.sprintf "Bad static argument nature, a %s was expected" nat in
      raise (Compile_error(sa.src, msg))
    in
    let res = match (sa.it, asp) with
      (* ident vs type *)
      | (StaticArgLv6Id idref, ASP_type id) ->
        let teff = node_id_solver.id2type idref sa.src in
        TypeStaticArgLic (id, teff)
      (* type_exp vs type *)
      | (StaticArgType te, ASP_type id) ->
        let teff = of_type node_id_solver te in
        TypeStaticArgLic (id, teff)
      (* ident vs const *)
      | (StaticArgLv6Id idref, ASP_const id) ->
        let ceff = node_id_solver.id2const idref sa.src in
        ConstStaticArgLic (id, ceff)
      (* val_exp vs const *)
      | (StaticArgConst ce, ASP_const id) -> (
          let ceff = EvalConst.f node_id_solver ce in
          match ceff with
          | [ceff] -> ConstStaticArgLic (id,ceff)
          | _ -> ConstStaticArgLic (id,Tuple_const_eff ceff)
        )
      (* id vs node *)
      | (StaticArgLv6Id idref, ASP_node id) ->
        let sargs = [] in
        let neff = node_id_solver.id2node idref sargs sa.src in
        NodeStaticArgLic (id, neff.node_key_eff)
      (* node exp vs node *)
      | (StaticArgNode (CALL_n ne), ASP_node id) ->
        let neff = of_node node_id_solver ne in
        NodeStaticArgLic (id, neff.node_key_eff)
      (* node exp vs node *)
      | (StaticArgNode (Predef_n (op)), ASP_node id) ->
        let opeff = LicEvalType.make_node_exp_eff node_id_solver None true op.it sa.src in
        NodeStaticArgLic (id, opeff.node_key_eff)
      | (_, ASP_type  _) -> nature_error "type"
      | (_, ASP_const _) -> nature_error "constant"
      | (_, ASP_node  _) -> nature_error "node"
    in res
  )

(******************************************************************************)


(* exported *)
and (of_eq: IdSolver.t -> AstCore.eq_info srcflagged -> Lic.eq_info srcflagged) =
  fun id_solver eq_info -> 
    let (lpl, ve) = eq_info.it in
    let lpl_eff = List.map (translate_left_part id_solver) lpl in
    let exp_clks = List.map Lic.clock_of_left lpl_eff  in
    let cs = UnifyClock.empty_subst in
    let ve_eff,right_part_clks,cs = translate_val_exp_check id_solver exp_clks cs ve in
    let ve_eff =
      type_check_equation id_solver eq_info.src lpl_eff ve_eff;
      clock_check_equation id_solver eq_info.src cs lpl_eff right_part_clks ve_eff
    in
    flagit (lpl_eff, ve_eff) eq_info.src


and (translate_left_part : IdSolver.t -> AstCore.left_part -> Lic.left) =
  fun id_solver lp_top -> 
    match lp_top with
    | LeftVar id -> 
      let vi_eff = id_solver.id2var id.it id.src in
      LeftVarLic (vi_eff, id.src)
    | LeftField (lp, id) -> (
        let lp_eff = translate_left_part id_solver lp in
        let teff = Lic.type_of_left lp_eff in
        (* check that [lp_eff] is a struct that have a field named [id] *)
        match teff with
        | Struct_type_eff(_, fl) -> (
            try let (teff_field,_) = List.assoc id.it fl in
              LeftFieldLic(lp_eff, id.it, teff_field)
            with Not_found ->
              raise (Compile_error(id.src, "bad field name in structure"))
          )
        | _  -> raise (Compile_error(id.src, "a structure was expected"))
      )
    | LeftArray (lp, vef) -> (
        let lp_eff = translate_left_part id_solver lp in
        let teff = Lic.type_of_left lp_eff in
        let lxm = vef.src in
        match teff with
        | Abstract_type_eff(_,Array_type_eff(teff_elt, _size))
        | Array_type_eff(teff_elt, _size) ->
          let index = EvalConst.eval_array_index id_solver vef.it lxm in
          LeftArrayLic(lp_eff, index, teff_elt)

        | _ -> raise (Compile_error(vef.src, "an array was expected"))
      )
    | LeftSlice (lp, sif) -> (
        let lp_eff = translate_left_part id_solver lp in
        let teff = Lic.type_of_left lp_eff in
        match teff with  
        | Abstract_type_eff(_,Array_type_eff(teff_elt, _size))
        | Array_type_eff(teff_elt, _size) -> 
          let sieff = translate_slice_info id_solver sif.it sif.src in
          let size_slice = sieff.se_width in
          let teff_slice = Array_type_eff(teff_elt, size_slice) in
          LeftSliceLic(lp_eff, sieff, teff_slice)
        | _ -> raise (Compile_error(sif.src, "an array was expected"))
      )


(* Translate and performs the checks *)
and (translate_val_exp_check  : IdSolver.t -> Lic.clock list -> UnifyClock.subst -> 
     AstCore.val_exp -> Lic.val_exp * Lic.id_clock list * UnifyClock.subst) =
  fun id_solver exp_clks s ve ->
    let s,vef = translate_val_exp id_solver s ve in
    let lxm = AstCore.lxm_of_val_exp ve in
    let lxms = List.map (fun _ -> lxm) exp_clks in
    (* let vef, tl   = EvalType.f id_solver vef in *)
    EvalClock.f id_solver s vef lxms exp_clks  


and (translate_val_exp : IdSolver.t -> UnifyClock.subst -> AstCore.val_exp
     -> UnifyClock.subst * Lic.val_exp) =
  fun id_solver s ve ->
    (match ve with
     | CallByPos({it=WITH_n(c,e1,e2);_}, Oper vel) ->
       assert (vel=[]);
       if EvalConst.f id_solver c = [ Bool_const_eff true ] 
       then translate_val_exp id_solver s e1 
       else translate_val_exp id_solver s e2
     | _ -> 
       let s, vef_core, lxm =
         match ve with
         | Merge_n(ve, cl) ->  
           let lxm_ve = ve.src in
           let ve = ve.it in
           let s,ve = translate_val_exp id_solver s ve in
           let s, cl =
             List.fold_left
               (fun (s,cl) (id,ve) -> 
                  let s, ve = translate_val_exp id_solver s ve in
                  let const = id_solver.id2const id.it id.src in
                  s,(flagit const id.src, ve)::cl
               )
               (s, [])
               cl
           in
           s, Lic.Merge(ve, List.rev cl), lxm_ve
         | Merge_bool_n(ve, t, f) ->
           let lxm_ve = ve.src in
           let ve = ve.it in
           let s,ve = translate_val_exp id_solver s ve in
           let s,case_true  = translate_val_exp id_solver s t in
           let s,case_false = translate_val_exp id_solver s f in
           let case_true = (flagit (Bool_const_eff true) lxm_ve, case_true) in
           let case_false = (flagit (Bool_const_eff false) lxm_ve, case_false) in
           s, Lic.Merge(ve, [case_true; case_false]), lxm_ve

         | CallByName(by_name_op, field_list) ->
           let s,fl = List.fold_left 
               (fun (s,fl) f -> 
                  let s,f = translate_field id_solver s f in
                  s,f::fl
               )
               (s,[])
               field_list
           in
           let fl = List.rev fl in
           let s, by_name_op = translate_by_name_op id_solver by_name_op s in
           s, 
           (CallByNameLic(by_name_op, fl)), by_name_op.src

         | CallByPos(by_pos_op, Oper vel) ->
           let s, vel_eff = 
             List.fold_left 
               (fun (s,vel) ve -> 
                  let s, ve = translate_val_exp id_solver s ve in
                  s,ve::vel
               )
               (s,[]) vel 
           in
           let vel_eff = List.rev vel_eff in
           let lxm = by_pos_op.src in
           let by_pos_op = by_pos_op.it in
           let mk_by_pos_op by_pos_op_eff =
             CallByPosLic(flagit by_pos_op_eff lxm, vel_eff)
           in
           let mk_nary_pos_op by_pos_op_eff = 
             (* For nor and diese: internally, nor and diese takes an array of val_exp,
                to make it easier the translation into boolred.

                It is the good spot to do that? what could be a better spot?
             *)
             let array_val_exp =
               let lxm = Lxm.override_name "[ ]" lxm in
               { ve_core = CallByPosLic(flagit Lic.ARRAY lxm, vel_eff);
                 ve_typ = [Array_type_eff(List.hd (List.hd vel_eff).ve_typ,
                                          List.length vel_eff)];
                 ve_clk = (List.hd vel_eff).ve_clk;
                 ve_src =  lxm 
               }
             in
             CallByPosLic(flagit by_pos_op_eff lxm, [array_val_exp])
           in

           let s, vef_core =
             match by_pos_op with
             | WITH_n(_,_,_) -> assert false (* handled at the top of the function *)
             (* put that in another module ? yes, see above.*)
             | Predef_n({it=TRUE_n;_})  -> s,mk_by_pos_op(Lic.CONST (Bool_const_eff true))
             | Predef_n({it=FALSE_n;_}) -> s,mk_by_pos_op(Lic.CONST (Bool_const_eff false))
             | Predef_n({it=RCONST_n r;_}) -> s,mk_by_pos_op(Lic.CONST (Real_const_eff r))
             | Predef_n({it=ICONST_n i;_}) -> s, mk_by_pos_op(Lic.CONST (Int_const_eff i))


             | Predef_n({it=NOR_n;src=lxm}) -> s, mk_nary_pos_op(
                 Lic.PREDEF_CALL (flagit (AstPredef.op_to_long NOR_n,[]) lxm))
             | Predef_n({it=DIESE_n;src=lxm}) -> s, mk_nary_pos_op(
                 Lic.PREDEF_CALL (flagit (AstPredef.op_to_long DIESE_n,[]) lxm))
             | Predef_n(op) -> s, mk_by_pos_op(
                 Lic.PREDEF_CALL (flagit (AstPredef.op_to_long op.it,[]) op.src))
             | CALL_n node_exp_f -> 
               let neff = of_node id_solver node_exp_f in
               let ceff = Lic.CALL (flagit neff.node_key_eff node_exp_f.src) in
               Lv6Verbose.exe ~flag:dbg (fun () ->
                   Printf.fprintf stderr "#DBG: Ast2lic.translate_val_exp CALL_n ";
                   AstV6Dump.print_node_exp stderr node_exp_f.it;
                   Printf.fprintf stderr " gives type: %s\n%!"
                     (Lic.string_of_type_profile (profile_of_node_exp neff))
                 ) ;
               (s, mk_by_pos_op ceff)
             | IDENT_n idref -> (
                 try
                   let var = id_solver.id2var idref.id_id lxm in
                   s, mk_by_pos_op(Lic.VAR_REF var.var_name_eff)
                 with _ ->
                   let s, const = UnifyClock.const_to_val_eff lxm false s
                       (id_solver.id2const idref lxm)
                   in
                   s, const.ve_core
               )
             | CURRENT_n -> s, mk_by_pos_op (Lic.CURRENT None)
             | PRE_n -> s, mk_by_pos_op Lic.PRE

             | ARROW_n -> s, mk_by_pos_op Lic.ARROW

             | FBY_n -> (* XXX temporary crutch: translate "e1 fby e2" into "e2 -> pre(e2)" *)
               (match vel_eff with
                | [e1;e2] -> 
                  let ve_pre = CallByPosLic(flagit Lic.PRE lxm, [e2]) in
                  let ve_pre = { e2 with ve_core=ve_pre } in
                  let lxm = Lxm.override_name "->" lxm in
                  s,CallByPosLic(flagit Lic.ARROW lxm, [e1;ve_pre]) 
                | _ -> assert false
               )
             (*                   | FBY_n ->   s, mk_by_pos_op Lic.FBY *)
             | CONCAT_n -> s, mk_by_pos_op Lic.CONCAT
             | TUPLE_n -> s, mk_by_pos_op Lic.TUPLE
             | ARRAY_n -> s, CallByPosLic(flagit Lic.ARRAY lxm, vel_eff)
             | STRUCT_ACCESS_n fid ->
               s, mk_by_pos_op (Lic.STRUCT_ACCESS (fid))

             | WHEN_n Base -> s, mk_by_pos_op (Lic.WHEN BaseLic)
             | WHEN_n (NamedClock { it = (cc,c) ; src = lxm }) -> 
               let var_info = id_solver.id2var c lxm in
               let _, clk = var_info.var_clock_eff in
               let ct =  var_info.var_type_eff in
               s, mk_by_pos_op (Lic.WHEN (On((cc,c,ct), clk)))

             | ARRAY_ACCES_n ve_index ->
               s, mk_by_pos_op (Lic.ARRAY_ACCES(
                   EvalConst.eval_array_index id_solver ve_index lxm))

             | ARRAY_SLICE_n si ->
               s, mk_by_pos_op (Lic.ARRAY_SLICE(
                   EvalConst.eval_array_slice id_solver si lxm))

             | HAT_n -> (
                 match vel with
                 | [_exp; ve_size] -> 
                   let size_const_eff = EvalConst.f id_solver ve_size in 
                   (match size_const_eff with
                    | [Int_const_eff sz] -> s, mk_by_pos_op (Lic.HAT(int_of_string sz))
                    | _ -> assert false)
                 | _ -> assert false
               )
           in
           s, vef_core, lxm
       in
       let vef = { ve_core=vef_core; ve_typ=[]; ve_clk = []; ve_src = lxm } in
       let vef, _tl = EvalType.f id_solver vef in
       s,vef
    )    


and translate_by_name_op id_solver op s = 
  let to_long idref =
    match Lv6Id.pack_of_idref idref with
    | None -> (* If no pack name is provided, we lookup it in the symbol table *)
      let id = Lv6Id.of_idref false idref in
      let pn = AstTabSymbol.find_pack_of_type id_solver.global_symbols id op.src in
      Lv6Id.make_long pn idref.id_id
    | Some pn -> Lv6Id.make_long pn idref.id_id
  in
  let s, nop =
    match op.it with
    | STRUCT_anonymous_n -> s, STRUCT_anonymous
    | STRUCT_n idref ->     s, STRUCT (to_long idref)
    | STRUCT_WITH_n (idref1, idref2) -> 
      s, STRUCT_with (to_long idref1, idref2.id_id)
  in
  s, flagit nop op.src

and translate_field id_solver s (id, ve) = 
  let s, ve = translate_val_exp id_solver s ve in
  s, (id, ve)

(* XXX autre nom, autre module ? 
   node_of_static_arg : appelé QUAND ON SAIT qu'un sarg doit etre un NODE
   const_of_static_arg : appelé QUAND ON SAIT qu'un sarg doit etre une CONST

   -> sert pour les macros predefs
   ca fait partie de la definition des iterateurs d'une certaine maniere...
   -> créer 2 modules, Iterator + IteratorSemantics 
*)
and _const_of_static_arg id_solver const_or_const_ident lxm = 
  match const_or_const_ident with
  | StaticArgConst(c) -> (
	   match EvalConst.f id_solver c with
	   | [x] -> x
	   | xl -> 
		  (* EvalConst.f ne fabrique PAS de tuple, on le fait ici *)
		  Tuple_const_eff xl
    )
  | StaticArgLv6Id(id) -> id_solver.id2const id lxm 
  | StaticArgType _
  | StaticArgNode _ -> raise (Compile_error(lxm, "a constant was expected"))



and _node_of_static_arg id_solver node_or_node_ident lxm =
  match node_or_node_ident with
  | StaticArgLv6Id(id) -> 
    let sargs = [] in (* it is an alias: no static arg *)
    id_solver.id2node id sargs lxm 

  | StaticArgNode(CALL_n ne) -> of_node id_solver ne
  | StaticArgNode(Predef_n (op)) ->
    LicEvalType.make_node_exp_eff id_solver None true op.it lxm

  | StaticArgNode(_) -> assert false

  | StaticArgType _ 
  | StaticArgConst _ -> raise (Compile_error(lxm, "a node was expected"))

and (translate_slice_info  : IdSolver.t -> AstCore.slice_info -> 
     Lxm.t -> Lic.slice_info) =
  fun id_solver si lxm ->
    EvalConst.eval_array_slice id_solver si lxm


(**********************************************************************************)
(* exported *)
let (of_assertion : IdSolver.t -> AstCore.val_exp Lxm.srcflagged -> 
      Lic.val_exp Lxm.srcflagged) =
  fun id_solver vef ->
    let s = UnifyClock.empty_subst in
    let exp_clks = [BaseLic] in (* assertions are on the base clock *)
    let val_exp_eff, _ ,s = translate_val_exp_check id_solver exp_clks s vef.it in
    (* Check that the assert is a bool. *)
    let val_exp_eff, evaled_exp = EvalType.f id_solver val_exp_eff in
      List.iter
        (fun ve -> 
           if ve <> Bool_type_eff then
             let msg = "type mismatch: \n\tthe content of the assertion is of type " 
               ^ (Lic.string_of_type ve) 
               ^ " whereas it shoud be a Boolean\n"
             in
               raise (Compile_error(vef.src, msg))
        )
        evaled_exp;
      (* type is ok *)

      (* Clock check the assertion*)
      let _, clock_eff_list, _s = 
        EvalClock.f id_solver s val_exp_eff [vef.src] [BaseLic]
      in
        match clock_eff_list with
          | [_id, BaseLic] 
          | [_id, On(_,BaseLic)]
          | [_id, ClockVar _] -> Lxm.flagit val_exp_eff vef.src
          | [_id, ce] -> 
              let msg = "clock error: assert should be on the base clock, "^
                "but it is on "^ (LicDump.string_of_clock2 ce) ^ "\n"
              in
                raise (Compile_error(vef.src, msg))

          | _ -> assert false
      

(******************************************************************************)
(******************************************************************************)

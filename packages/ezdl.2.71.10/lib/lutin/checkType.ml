(** TYPE/BINDING CHECK : main

-------------------------------------------------------------
Cette passe a essentiellement pour but de donner un
type effectif aux différents items syntaxiques du
programme (lets, expressions).

Conjointement, elle résout les "bindings" des identificateurs :
- toute référence à un identificateur est identifiée
de manière UNIQUE par son ident (décoré de son info source)

On utilise trois tables :
- une table GLOBALE de binding "ident -> ident_info"
- une table "dynamique" de scoping "string -> ident_info" 
- une table de typage des exp "val_exp -> type_eff"


------------------------------------------------------------*)
open Lexeme;;
open LutErrors;;
open Format;;
open Syntaxe;;
open CkIdentInfo;;

let type_error lxm get expect = (
	let msg = sprintf "type error, get (%s) while (%s) was expected"
		(CkTypeEff.list_to_string get) (CkTypeEff.list_to_string expect) in
	raise (Compile_error (lxm, msg))
)
let arity_error lxm get expect = (
	let msg = sprintf "arity error, get %d while %d was expected"
		get expect in
	raise (Compile_error (lxm, msg))
)

(*--------------------------------------------*)
(* utilitaire pour faciliter le type checking *)
(*--------------------------------------------*)

(* util : check l'eventuelle init d'une var *)
let rec check_var_decl  (env : CheckEnv.t)  (i, t, vopt,range_opt) =
  let tdecl = (CkTypeEff.of_texp t) in
  (match vopt with
   | None -> ()
   | Some e -> (
       let tcalc = check_exp env e in
       if CkTypeEff.lifts_to tcalc tdecl then () else
         type_error i.src [tcalc] [tdecl] 
     )
  );
  (match range_opt with
   | None -> ()
   | Some (low, high) -> 
     let tlow = check_exp env low
     and thigh= check_exp env high in
     if(CkTypeEff.lifts_to tlow tdecl) then 
       if (CkTypeEff.lifts_to thigh tdecl) then () 
       else ( type_error i.src [thigh] [tdecl] )
     else  ( type_error i.src [tlow] [tdecl] )
  );
  (i,tdecl)
and
  (* erun vars: opt type and init val, NO RANGE, + expected type *) 
  check_erun_var_decl
    (env : CheckEnv.t)
    (i, topt, vopt)
    (txpc)
  =
  (
    let teff = match topt with
      | Some t -> 
        let tf = (CkTypeEff.of_texp t) in
        if(CkTypeEff.lifts_to txpc tf) then tf
        else ( type_error i.src [txpc] [tf] )
      | None -> txpc 
    in
    let _ = (
      match vopt with
      | None -> ()
      | Some e -> (
          let tcalc = check_exp env e in
          if(CkTypeEff.lifts_to tcalc teff) then ()
          else ( type_error i.src [tcalc] [teff] )
        )

    ) in
    (i,teff)
  )
and
  (* run result: id MUST be Support (controlable checked later) *)
  check_run_var_decl
    (env : CheckEnv.t)
    (id)
    (txpc)
  =
  (
    let _ = match (CheckEnv.nature_of_ident env id) with
      | Support_var -> ()
      | _ -> (
          raise (Compile_error (id.src,"identifier "^id.it^" not allowed as run result"))
        ) in
    let tf = CheckEnv.type_of_ident env id in
    let teff = if(CkTypeEff.lifts_to tf txpc) then txpc
      else ( type_error id.src [tf] [txpc])
    in
    (id,teff)
  )
and
  check_exp
    (env : CheckEnv.t)
    (e : Syntaxe.val_exp)  = (* CheckEnv.type_eff *)
  (
    (*DBG
      printf "check_exp\n";
    *)
    let rec_call e = ( check_exp env e) in
    let rec_list_call el = ( List.map rec_call el) in
    let e_teff = (
      (* Début du calcul de e_teff *)
      match e.it with
        TRUE_n -> CkTypeEff.boolean
      |  FALSE_n -> CkTypeEff.boolean
      |  ICONST_n _id -> CkTypeEff.integer
      |  RCONST_n _id -> CkTypeEff.real
      |  IDENT_n  id -> (
          (* tout sauf une macro ... (01/2012 : meme zeroaire !) *)
          match (CheckEnv.nature_of_ident env id) with
            Macro_ident (_, _prof) -> (
              let msg = sprintf "arity error, get a macro where a scalar was expected" in
              raise (Compile_error (e.src, msg))
 (*
match CkTypeEff.split_prof prof with
([], [t]) -> t
|   (til, _) -> (
arity_error e.src 0 (List.length til)
)
*)
            )
          | _ -> CheckEnv.type_of_ident env id
        )
      |  PRE_n id -> (
          (* que Support_var ou Formal_param (au benefice du doute) *)
          match (CheckEnv.nature_of_ident env id) with
            Support_var
          |   Formal_param -> (
              let te = CheckEnv.type_of_ident env id in
              try (
                CkTypeEff.lift_ref te
              ) with Failure _ -> (
                  let msg = sprintf
                      "type error, get (%s) while ('a ref) was expected"
                      (CkTypeEff.to_string te)
                  in
                  raise (Compile_error (e.src, msg))
                )
            ) |
            _ -> (
              raise (Compile_error (e.src,
                                    "identifier "^id.it^" not allowed as pre argument"))
            ) 
        )
      |  FBY_n  (e1,e2) -> (
          let tel = rec_list_call [e1;e2] in 
          match_type_profile tel CkTypeEff.prof_tt_t e.src
        )
      |  LOOP_n (_,e1) -> (
          let tel = rec_list_call [e1] in 
          match_type_profile tel CkTypeEff.prof_t_t e.src
        )
(*
|  LOOPE_n (n, e1) -> (
let tel = rec_list_call [n;e1] in 
match_type_profile tel CkTypeEff.prof_it_t e.src
)
*)
      |  LOOPI_n (min, max, e1) -> (
          let tel = rec_list_call [min;max;e1] in 
          match_type_profile tel CkTypeEff.prof_iit_t e.src
        )
      |  LOOPA_n (av, Some ec, e1) -> (
          let tel = rec_list_call [av;ec;e1] in 
          match_type_profile tel CkTypeEff.prof_iit_t e.src
        )
      |  LOOPA_n (av, None, e1) -> (
          let tel = rec_list_call [av;e1] in 
          match_type_profile tel CkTypeEff.prof_it_t e.src
        )
      |  ASSERT_n (_, c, e1) -> (
          let tel = rec_list_call [c;e1] in 
          match_type_profile tel CkTypeEff.prof_bt_t e.src
        )
   	|  CALL_n (id, elst) -> (
			 (* doit être une macro (ou un predef op) ... *)
			 match (CheckEnv.nature_of_ident env id) with
	       | Macro_ident (None (* it is a predef op ! *),  prof) -> ( 
				  let tel = rec_list_call elst in
				  match_type_profile tel prof e.src
			   )
			 | Macro_ident (Some _ (* it is a user def macro *), prof) -> (
				  let tel = rec_list_call elst in
				  match_type_profile tel prof e.src
			   )
			 (* ... ou une fonction externe *)
			 | External_func (_lio, _eio, prof) -> (
				  let tel = rec_list_call elst in
				  match_type_profile tel prof e.src
			   )
			 | _ -> (
				  raise (Compile_error (e.src,
					                     "identifier "^id.it^" cannot be used as a function"))
			   )
		  )
      |  CHOICE_n clist -> (
          let check_one_choice = ( function
                (e, None) -> (
                  let t = check_exp env e in
                  let _ = match_type_profile [t] CkTypeEff.prof_t_t e.src in
                  ()
                ) |
                (e, Some w) -> (
                  let te = check_exp env e in
                  let tw = check_exp env w.it in
                  let _ = match_type_profile [te;tw] CkTypeEff.prof_tw_t w.src in
                  ()
                )
            ) in
          List.iter check_one_choice clist ;
          CkTypeEff.trace
        )
      | PRIO_n plist -> (
          let check_one_prio e = (
            let t = check_exp env e in
            let _ =  match_type_profile [t] CkTypeEff.prof_t_t e.src in
            ()
          ) in
          List.iter check_one_prio plist ;
          CkTypeEff.trace
        )
      | PARA_n plist -> (
          let check_one_para e = (
            let t = check_exp env e in
            let _ =  match_type_profile [t] CkTypeEff.prof_t_t e.src in
            ()
          ) in
          List.iter check_one_para plist ;
          CkTypeEff.trace
        )
      (* EXIST => modifie l'environnement *)
      |  EXIST_n (tidlst, e1) -> (
          (* on évalue les inits *)
          let checked_ids = List.map (check_var_decl env) tidlst in
          (* on ajoute les vars dans env *)
          let rkey = CheckEnv.add_support_vars env checked_ids in
          let res = check_exp env e1 in
          CheckEnv.restore env rkey ;
          res
        )
      (* ERUN => modifie l'environnement *)
      |  ERUN_n (varlst, edef, e1) -> (
          (* edef doit etre un node call (pour l'instant !) *)
          let expected_types = (
            match edef.it with
            | CALL_n (id, elst) -> (
                (* doit être un node ... *)
                match (CheckEnv.nature_of_ident env id) with
                | Node_ident (_, prof) -> (
                    let tel = rec_list_call elst in
                    match_run_type_profile tel prof e.src
                  )
                (* ... ou une fonction externe *)
                | External_func (_lio, _eio, prof) -> (
                    let tel = rec_list_call elst in
                    [ match_type_profile tel prof e.src ]
                  )
                | _ -> (
                    raise (Compile_error
                             (e.src, "identifier "^id.it^" cannot be used in run statement"))
                  )
              )
            | _ -> raise (Compile_error
                            (edef.src, "only node calls are supported in run statement"))
          ) in
          (* on checke les ids wrt expected_types *)
          let checked_ids = List.map2 (check_erun_var_decl env) varlst expected_types in
          (* expected_types is associated to edef *)
          CheckEnv.set_exp_type env edef (CkTypeEff.get_data_tuple expected_types);
          (* on ajoute les vars dans env *)
          let rkey = CheckEnv.add_support_vars env checked_ids in
          let res = check_exp env e1 in
          CheckEnv.restore env rkey ;
          res
        )
      (* definitive RUN each id in idlst MUST be an existing  controlable var
      *)
      |  RUN_n (idlst, edef, e1opt) -> (
          (* edef doit etre un node call (pour l'instant !) *)
          let expected_types = (
            match edef.it with
            | CALL_n (id, elst) -> (
                (* doit être un node ... *)
                match (CheckEnv.nature_of_ident env id) with
                | Node_ident (_, prof) -> (
                    let tel = rec_list_call elst in
                    match_run_type_profile tel prof e.src
                  )
                (* ... ou une fonction externe *)
                | External_func (_lio, _eio, prof) -> (
                    let tel = rec_list_call elst in
                    [ match_type_profile tel prof e.src ]
                  )
                | _ -> (
                    raise (Compile_error (e.src,
                                          "identifier "^id.it^" cannot be used in run statement"))
                  )
              )
            | _ -> raise (Compile_error
                            (edef.src, "only node calls are supported in run statement"))
          ) in
          (* on checke les ids wrt expected_types *)
          let checked_ids = List.map2 (check_run_var_decl env) idlst expected_types in
          (* expected_types is associated to edef *)
          CheckEnv.set_exp_type env edef (CkTypeEff.get_data_tuple expected_types);
          match e1opt with
          | Some e1 -> 
            (* on ajoute les vars dans env *)
            let rkey = CheckEnv.add_support_vars env checked_ids in
            let res = check_exp env e1 in
            CheckEnv.restore env rkey ;
            res
          | None -> CkTypeEff.trace
        )
      (* LET => modifie l'environnement *)
      |  LET_n (li, e1) -> (
          (* on checke la def dans env ...*)   
          let tres = check_let env li in
          (* on ajoute l'association dans env *)
          let rkey = CheckEnv.add_let env li tres li.lti_ident in
          (* check de l'arg dans cet env ... *)
          let res = check_exp env e1 in
          (* restauration de l'ancien env *)
          CheckEnv.restore env rkey ;
          res   
        )
      (* raise ... *)
      | RAISE_n id -> (
          (*printf "RAISE %s\n" (Lexeme.to_string id.src); *)
          match (CheckEnv.nature_of_ident env id) with
            Const_ident -> (
              let t = CheckEnv.type_of_ident env id in
              if (t = CkTypeEff.except) then (
                CkTypeEff.trace   
              ) else (
                type_error id.src [t] [CkTypeEff.except]
              )
            ) | _ -> (
              raise (Compile_error (e.src,
                                    "identifier "^id.it^" not allowed as raise argument"))
            )
        )
      (* déclaration d'exceptions locales => modifie l'environnement *)
      |  EXCEPT_n (xlst, e1) -> (
          (* on ajoute les xlst dans env *)
          let f i = (
            CheckEnv.add_local_cst env i (CkTypeEff.except)
          ) in
          let ekeyl = List.map f xlst in 
          let res = check_exp env e1 in
          List.iter (CheckEnv.restore env) ekeyl ;
          res
        )
      (* catch *)
      |   CATCH_n (id, e1, e2opt) -> (
          (* type de l'id *)
          let id_type = ( match (CheckEnv.nature_of_ident env id) with
                Const_ident -> (
                  CheckEnv.type_of_ident env id
                ) | _ -> (
                  raise (Compile_error (e.src,
                                        "identifier "^id.it^" not allowed as catch argument"))
                )
            ) in
          match e2opt with
            None -> (
              let tel = (id_type)::(rec_list_call [e1]) in
              match_type_profile tel CkTypeEff.prof_et_t e.src
            ) |
            Some e2 -> (
              let tel = (id_type)::(rec_list_call [e1;e2]) in
              match_type_profile tel CkTypeEff.prof_ett_t e.src
            )
        )
      (* trap: exception + catch *)
      |   TRAP_n (id, e1, e2opt) -> (
          (* on déclare l'exception ... *)
          let ekey =
            CheckEnv.add_local_cst env id (CkTypeEff.except) 
          in
          (* Bien qu'on sache que id est bien une exception,
             on doit QUAND MEME CHECKER pour etablir le bibding !
          *)
          let _id_type = ( match (CheckEnv.nature_of_ident env id) with
                Const_ident -> (
                  CheckEnv.type_of_ident env id
                ) | _ -> assert false
            ) in
          let res = (
            match e2opt with
              None -> (
                let tel = (CkTypeEff.except)::(rec_list_call [e1]) in
                match_type_profile tel CkTypeEff.prof_et_t e.src
              ) |
              Some e2 -> (
                let tel = (CkTypeEff.except)::(rec_list_call [e1;e2]) in
                match_type_profile tel CkTypeEff.prof_ett_t e.src
              )
          ) in
          (* on restaure *)
          CheckEnv.restore env ekey ;
          res
        )
      (* try *)
      | TRY_n (e1, e2opt ) -> (
          match e2opt with
            None -> (
              let tel = rec_list_call [e1] in 
              match_type_profile tel CkTypeEff.prof_t_t e.src
            ) |
            Some e2 -> (
              let tel = rec_list_call [e1;e2] in 
              match_type_profile tel CkTypeEff.prof_tt_t e.src
            )
        )
    ) in
    (* FIN DU CALCUL DE e_teff *)
    CheckEnv.set_exp_type env e e_teff;
    e_teff
  )
(* Old version : expect a single result type
   kept to avoid a match (almost) everywhere
*)
and match_type_profile tel prof lxm = (
  try (
    match CkTypeEff.match_prof tel prof with
    | [t] -> t
    | _ -> assert false
  ) with _ -> 
    type_error lxm tel (CkTypeEff.params_of_prof prof)
)
(* General version, returns a list, used (only ?) for run's
*)
and match_run_type_profile tel prof lxm = (
  try (
    CkTypeEff.match_prof tel prof
  ) with _ -> 
    type_error lxm tel (CkTypeEff.params_of_prof prof)
)
and check_let
    (env : CheckEnv.t)
    (li : Syntaxe.let_info) = (* unit *)
  (
    (*DBG printf "check_let\n"; *)
    let lxm = li.lti_ident.src in
    (* calcul du type (et check par effet de bord) *)
    match (li.lti_def) with
      Some exp -> (
        (* macro definie *)
        (* mise en place de l'env *)
        let rkey = CheckEnv.add_formal_params env li.lti_inputs in
        (* 01/2012 lift ref ! *)
        let tcalc = check_exp env exp in
        let tcalc = try CkTypeEff.lift_ref tcalc with Failure _ -> tcalc in
        let res = (
          match li.lti_type with
            Some te -> (
              let tdecl = (CkTypeEff.of_texp te) in
              if(CkTypeEff.lifts_to tcalc tdecl) then ( tdecl)
              else ( type_error lxm [tcalc] [tdecl] )
            ) | None -> ( tcalc)
        ) in
        CheckEnv.restore env rkey ;
        res
      ) |
      None -> (
        (* IMPOSSIBLE !!! *)
        assert false
      )
  ) and check_extern
    (_env : CheckEnv.t)
    (li : Syntaxe.let_info) = (* unit *)
      (
        (*DBG printf "check_extern\n"; *)
        let lxm = li.lti_ident.src in
        (* calcul du type (et check par effet de bord) *)
        match (li.lti_def) with
          Some _exp -> (
            assert false
          ) |
          (* check data is made later *)
          None -> (
            match li.lti_type with
            |   Some te -> (
                let res = CkTypeEff.of_texp te in
                Verbose.exe ~level:3
                  (fun () -> Printf.printf "CheckType.check_extern \"%s\", return type \"%s\"\n"
                      li.lti_ident.it
                      (CkTypeEff.to_string res));
                res
              )
            |   None -> raise ( Compile_error (lxm,
                                               "external profiles must be fully declared")
                              )
          )
      )

(* Returns the complete profile *)
let check_node 
   (env : CheckEnv.t)
   (ni : Syntaxe.node_info) =
   (* unit *)
(
   let lxm = ni.ndi_ident.src in
   (* mise en place de l'env *)
   let ins = List.map (check_var_decl env) ni.ndi_inputs in
   let outs = List.map (check_var_decl env) ni.ndi_outputs in
   let rkey = CheckEnv.add_support_profile env ins outs in
   (* calcul du type (et check par effet de bord *)
   let tcalc = check_exp env ni.ndi_def in
   (* extract the type for creating the profile *)
   let zeprof = if(CkTypeEff.lifts_to tcalc CkTypeEff.trace) then (
      let teff_of_param = function (_, t) -> t in
      let tins = List.map teff_of_param ins in
      let touts = List.map teff_of_param outs in
      CkTypeEff.get_prof tins touts
   ) else (
      type_error lxm [tcalc] [CkTypeEff.trace]
   )  in
   CheckEnv.restore env rkey ;
   zeprof
)

(*
   Returns CheckEnv.t,
   if libs is non empty, checks external refs
*)
let check_pack 
    (libs: string list option)
    (p : Syntaxe.package) = (
      (* on checke dans l'ordre de déclaration,
         en démarrant avec LutPredef.lutin_env
         let env = CheckEnv.create () in
      *)
  let env0 = CheckEnv.copy LutPredef.lutin_env in
  let env = match libs with
    | None -> env0
    | Some ll -> CheckEnv.add_libs env0 ll
  in
  let check_def_item =
    function
    LetDef s -> (
      let m = (Util.hfind p.pck_lettab s.it) in
      let tres = check_let env m in
      ignore (CheckEnv.add_let env m tres m.lti_ident)
    ) 
      | ExternDef x -> (
        let m = (Util.hfind p.pck_lettab x.it) in
        let tres = check_extern env m in
        ignore (CheckEnv.add_extern env m tres m.lti_ident)
      ) 
      | NodeDef s -> (
        let n = (Util.hfind p.pck_nodetab s.it) in
        let nprof = check_node env n in
        ignore (CheckEnv.add_node env n nprof n.ndi_ident)
      )
      |  ExceptDef s -> (
               (* équivalent à une constante
                  abstraite GLOBALE de type except *)
        ignore (CheckEnv.add_global_cst env s (CkTypeEff.except))
      )
  in
  List.iter check_def_item p.pck_deflist ;
  env
)

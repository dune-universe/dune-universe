
(** After this lic2lic pass, there is only one operator per equation.

Source 2 source transformation :

CONDITION :
- après L2lRmPoly (et DoAliasType ?)
*)

open Lxm
open Lic

let dbg = (Lv6Verbose.get_flag "split")
let profile_info = Lv6Verbose.profile_info

(********************************************************************************)

let new_var type_eff clock_eff = 
  let id = Lv6Id.of_string (FreshName.local_var "split") in 
  let var =
    { 
      var_name_eff   = id;
      var_nature_eff = AstCore.VarLocal;
      var_number_eff = -1;
      var_type_eff   = type_eff;
      var_clock_eff  = id,clock_eff;
    } in
    var

(********************************************************************************)
(* functions that deal with tuple breaking *)
let rec (get_vel_from_tuple : val_exp -> val_exp list) =
  function
    | { ve_core = CallByPosLic({it=Lic.TUPLE ;_}, vel)  ;_} -> 
        List.flatten (List.map get_vel_from_tuple vel)
    | ve -> [ve]

let (remove_tuple : val_exp list -> val_exp list) =
  fun vel -> 
    List.flatten (List.map get_vel_from_tuple vel)

let (remove_tuple_from_eq : eq_info srcflagged -> eq_info srcflagged) =
  (* transform "...=((x1,x2),x3)" into "...=(x1,x2,x3)" *)
  fun {src=lxm;it=(lhs,ve)} -> 
    let ve =
      match ve.ve_core with
        | CallByPosLic({it=op;src=lxm }, vel) -> 
          { ve with 
            ve_core = CallByPosLic({it=op;src=lxm}, (remove_tuple vel)) }
        | _ -> ve
    in
    {src=lxm;it=(lhs,ve)}
    
let to_be_broken = function
    (* We are only interested in operators that can deal with tuples! *)
  | CallByPosLic({ it = Lic.ARROW ;_ }, _) -> true
  | CallByPosLic({ it = Lic.FBY ;_ }, _) -> true
  | CallByPosLic({ it = Lic.PRE ;_ }, _) -> true
  | CallByPosLic({ it = Lic.CURRENT _  ;_}, _) -> true
  | CallByPosLic({ it = Lic.TUPLE  ;_}, _) -> true
  | CallByPosLic({ it = Lic.WHEN _  ;_}, _) -> true
  | CallByPosLic({ it = Lic.PREDEF_CALL({ it = (("Lustre","if"),[])  ;_}) ;_}, _) -> true
  | _e -> false



let (break_it_do : val_exp -> val_exp list) =
  fun ve -> 
  let nvel = 
    match ve.ve_core with
    | CallByPosLic({it=Lic.PREDEF_CALL({ it = (("Lustre","if"),[]) ;_ });src=lxm}, [c;ve1;ve2]) ->
       let vel1 = get_vel_from_tuple ve1
       and vel2 = get_vel_from_tuple ve2 
       in
       assert (List.length vel1 = List.length vel2);
       List.map2
	 (fun ve1 ve2 -> 
	   { ve_core = 
               CallByPosLic({it=Lic.PREDEF_CALL(
                                    { it = (("Lustre","if"),[]);src=lxm });src=lxm}, 
                            [c;ve1;ve2]);
             ve_typ = ve1.ve_typ;
             ve_clk = ve1.ve_clk;
             ve_src = lxm
           }
	 )
	 vel1
	 vel2
    | CallByPosLic({it=WHEN clk; src=lxm}, vel) -> (
      let vel = List.flatten (List.map get_vel_from_tuple vel) in
      List.map 
	(fun ve -> 
          { ve with 
            ve_core=CallByPosLic({it=WHEN clk ; src=lxm }, [ve])}) 
	vel
    )
    | CallByPosLic({it=Lic.TUPLE ; src=_lxm }, vel) -> (remove_tuple vel)
    | CallByPosLic({it=op ; src=lxm }, [ve]) ->
       let vel = get_vel_from_tuple ve in
       List.map 
         (fun ve -> { ve with ve_core=CallByPosLic({it=op;src=lxm}, [ve])})
         vel
    | CallByPosLic({it=CURRENT c ; src=lxm }, [clk;ve]) ->
       let vel = get_vel_from_tuple ve in
       List.map 
         (fun ve -> { ve with ve_core=CallByPosLic({it=CURRENT c;src=lxm}, [clk;ve])})
         vel
    | CallByPosLic({it=op ; src=lxm }, [ve1;ve2]) ->
       let vel1 = get_vel_from_tuple ve1
       and vel2 = get_vel_from_tuple ve2 
       in
       assert (List.length vel1 = List.length vel2);
       List.map2
	 (fun ve1 ve2 -> 
	   { ve_core = CallByPosLic({it=op ; src=lxm}, [ve1;ve2]);
             ve_typ = ve1.ve_typ;
             ve_clk = ve1.ve_clk;
             ve_src = lxm
           }
	 )
	 vel1
	 vel2
    | _ -> [ve]
             (*           assert false (* ougth to be dead code (guarded by to_be_broken...) *) *)
  in
  let tl = ve.ve_typ
  and cl = ve.ve_clk in
  assert (List.length ve.ve_typ = List.length nvel);
  let nvel = List.map2 (fun nve t -> { nve with ve_typ = [t]; ve_clk=cl } ) nvel ve.ve_typ in   
  assert(ve.ve_typ = tl);
  nvel

let rec (break_it : val_exp -> val_exp list) =
  fun ve -> 
    let vel = break_it_do ve in
    if List.length vel = 1 then [ve] else 
      (* fixpoint *)
       (List.flatten (List.map break_it vel))

let (is_an_access : Lic.by_pos_op -> bool) = function
  | STRUCT_ACCESS _
  | ARRAY_ACCES _  -> true
  | _ -> false

      
let (split_tuples:Lic.eq_info Lxm.srcflagged list -> Lic.eq_info Lxm.srcflagged list) =
  fun eql -> 
    let split_one_eq eq = 
      let { src = lxm_eq ; it = (lhs, n_rhs) } = eq in
        if List.length lhs > 1 && (to_be_broken n_rhs.ve_core) then
          let vel = break_it n_rhs in
          let eqs = 
            try List.map2 (fun lhs ve -> [lhs], ve) lhs vel 
            with _ -> 
              assert false
          in
          let eqs = List.map (fun eq -> Lxm.flagit eq lxm_eq) eqs in
            eqs
        else
          [eq]
    in
    List.fold_left (fun acc eq -> List.rev_append (split_one_eq eq) acc) [] eql 

(********************************************************************************)
(* The functions below accumulate 
   (1) the new equations 
   (2) the fresh variables. 
*)
type split_acc = (Lic.eq_info srcflagged) list * Lic.var_info list

let rec (eq : LicPrg.t -> Lic.eq_info Lxm.srcflagged -> split_acc) =
  fun lic_prg { src = lxm_eq ; it = (lhs, rhs) } ->
  let n_rhs, (neqs, nlocs) = split_val_exp lic_prg false true rhs in
  { src = lxm_eq ; it = (lhs, n_rhs) }::neqs, nlocs

and (split_eq_acc : LicPrg.t -> split_acc -> Lic.eq_info srcflagged -> split_acc) =
  fun lic_prg (eqs, locs) equation ->
  let (neqs, nlocs) = eq lic_prg equation in 
  let neqs = split_tuples neqs in
  List.rev_append neqs eqs, List.rev_append nlocs locs

and (split_val_exp : LicPrg.t -> bool -> bool -> Lic.val_exp -> Lic.val_exp * split_acc) =
  fun lic_prg when_flag top_level ve -> 
  (* [when_flag] is true is the call is made from a "when" statement.
       We need this flag in order to know if it is necessary to add
       a when on constants. Indeed, in Lustre V6, it is not necessary
       to write " 1 when clk + x " if x in on clk (it's more sweet). 
       So we need to add it  (because if we split "1+1+x", then it
       is hard to know the "1" are on the clock of x ; moreover, lustre
       v4 (and the other backends) cannot infer such clock).

       But is is not forbidden either! so we need to make sure that there
       is no "when"...
   *)
  match ve.ve_core with
  | Merge(ce,cl) -> (
    let ce,(eql1, vl1) = split_val_exp lic_prg false false ce in
    let const_l, vel = List.split cl in
    let vel,(eql2, vl2) = split_val_exp_list lic_prg false false vel in
    let eql, vl = eql1@eql2, vl1@vl2 in
    let cl = List.combine const_l vel in
    let rhs = { ve with ve_core = Merge(ce,cl)} in
	 if top_level then rhs, (eql, vl) else
      (* create the var for the current call *)
      let clk_l = ve.ve_clk in 
      let typ_l = ve.ve_typ in  
      assert (List.length typ_l = List.length clk_l);
      let nv_l = List.map2 new_var typ_l clk_l 
      in
      let lxm = lxm_of_val_exp ve in
      let vi2val_exp nv = 
        let _,clk = nv.var_clock_eff in
        { ve with
          ve_core = CallByPosLic(Lxm.flagit (Lic.VAR_REF (nv.var_name_eff)) lxm,[]);
          ve_typ = [nv.var_type_eff];
          ve_clk = [clk];
        }
      in
      let nve = match nv_l with
        | [] -> assert false (* SNO *)
        | [nv] -> vi2val_exp nv 
        | _ -> { ve with ve_core =
                           CallByPosLic(Lxm.flagit Lic.TUPLE lxm, List.map vi2val_exp nv_l)
               }
      in
      let lpl = List.map (fun nv -> LeftVarLic(nv, lxm)) nv_l in
      let eq = Lxm.flagit (lpl, rhs) lxm in
		nve, (eql@[eq], vl@nv_l)
  )
  | CallByPosLic({it=Lic.VAR_REF _;_}, _) -> ve, ([],[])
  | CallByPosLic({src=_lxm;it=Lic.CONST_REF _idl}, _vel) -> ve, ([],[])
  | CallByPosLic({src=lxm;it=Lic.CONST _}, _)
    -> if not when_flag then
         let clk = ve.ve_clk in
         match (clk) with
         | On(clock,clk)::_ -> 
            { ve with ve_core = 
                        CallByPosLic({src=lxm;it=Lic.WHEN(On(clock,clk))},[ve])},
            ([],[])                  
         | (ClockVar _)::_ (* should not occur *)
         | BaseLic::_  -> ve, ([],[])
         | [] -> assert false (* should not occur *)
       else
         ve, ([],[])
  | CallByNameLic (by_name_op_eff, fl) -> (
    let lxm = by_name_op_eff.src in 
    let fl, eql, vl = 
      List.fold_left
        (fun (fl_acc, eql_acc, vl_acc) (fn, fv) ->
         let fv, (eql, vl) = split_val_exp lic_prg false false fv in
         ((fn,fv)::fl_acc, eql@eql_acc, vl@vl_acc)
        )
        ([],[],[])
        fl
    in
    let rhs = { ve with ve_core = CallByNameLic (by_name_op_eff, List.rev fl) } in
	 if top_level then
      rhs, (eql, vl)
    else
      (* create the var for the current call *)
      let clk_l = ve.ve_clk in 
      let typ_l = ve.ve_typ in  
      assert (List.length typ_l = List.length clk_l);
      let nv_l = List.map2 new_var typ_l clk_l  in
      let nve = match nv_l with
        | [nv] -> { ve with ve_core = 
                              CallByPosLic(
                                  Lxm.flagit (Lic.VAR_REF (nv.var_name_eff)) lxm,
                                  [] 
                                )}
        | _ -> assert false
      in
      let lpl = List.map (fun nv -> LeftVarLic(nv, lxm)) nv_l in
      let eq = Lxm.flagit (lpl, rhs) lxm in
		nve, (eql@[eq], vl@nv_l)
  )                                                          
  | CallByPosLic(by_pos_op_eff, vel) -> (
    (* recursively split the arguments *) 
    let lxm = by_pos_op_eff.src in
    let (rhs, (eql,vl)) =
      match by_pos_op_eff.it with 
      | Lic.HAT(i) ->
         let vel, (eql, vl) = split_val_exp_list lic_prg false false vel in
         let by_pos_op_eff = Lxm.flagit (Lic.HAT(i)) lxm in
         let rhs = CallByPosLic(by_pos_op_eff, vel) in
         rhs, (eql, vl)
      | Lic.WHEN ve -> (* should we create a var for the clock? *)
         let vel,(eql, vl) = split_val_exp_list lic_prg true false vel in
         let by_pos_op_eff = Lxm.flagit (Lic.WHEN(ve)) lxm in
         let rhs = CallByPosLic(by_pos_op_eff, vel) in
         rhs, (eql, vl)
      | _ -> 
         let vel, (eql, vl) = split_val_exp_list lic_prg false false vel in
         let rhs = CallByPosLic(by_pos_op_eff, vel) in
         rhs, (eql, vl)
    in
    let rhs = { ve with ve_core = rhs } in
	 if top_level || by_pos_op_eff.it = TUPLE ||
         (* useless (I hope) and would sometimes create combinatory loops *)
         is_an_access by_pos_op_eff.it
    then 
      rhs, (eql, vl) 
    else
      (* create the var for the current call *)
      let clk_l = ve.ve_clk in 
      let typ_l = ve.ve_typ in
      assert (List.length typ_l = List.length clk_l);
      let nv_l = List.map2 new_var typ_l clk_l  in
      let nve = 
        match nv_l with
        | [nv] -> { ve with
                    ve_typ = [nv.var_type_eff];
                    ve_clk = clk_l; 
                    ve_core = CallByPosLic(
                                  Lxm.flagit (Lic.VAR_REF (nv.var_name_eff)) lxm,
                                  [])		
	               }
        | _ -> { ve with
                 ve_typ = List.map (fun v -> v.var_type_eff) nv_l;
                 ve_clk = clk_l;
		           ve_core =
                   CallByPosLic(
                       Lxm.flagit Lic.TUPLE lxm, 
                       (List.map 
                          (fun nv -> 
                           let nnv = {
                             ve_core = CallByPosLic 
                                         (Lxm.flagit (Lic.VAR_REF (nv.var_name_eff)) lxm,
                                          []);
                             ve_typ = [nv.var_type_eff];
                             ve_clk = [snd nv.var_clock_eff];
                             ve_src = lxm
                           }
			                  in
			                  nnv
			                 )
                          nv_l
			              )
                     )
               }
	   in
	   let lpl = List.map (fun nv -> LeftVarLic(nv, lxm)) nv_l in
      let eq = Lxm.flagit (lpl, rhs) lxm in
		nve, (eql@[eq], vl@nv_l)
  )

and (split_val_exp_list : LicPrg.t -> bool -> bool -> Lic.val_exp list ->
                          Lic.val_exp list * split_acc) =
  fun lic_prg when_flag top_level vel ->
  let vel, accl = 
    List.split (List.map (split_val_exp lic_prg when_flag top_level) vel) 
  in
  let eqll,vll = List.split accl in
  let eql, vl = List.flatten eqll, List.flatten vll in
  (vel,(eql,vl))

and split_node (lic_prg:LicPrg.t) (opt:Lv6MainArgs.t) (n: Lic.node_exp) : Lic.node_exp =
  Lv6Verbose.exe
    ~flag:dbg (fun () ->
               Printf.eprintf "*** Splitting node  %s\n"
                              (LicDump.string_of_node_key_iter false n.node_key_eff);
               flush stderr);
  let res = match n.def_eff with
    | ExternLic 
    | MetaOpLic
    | AbstractLic None -> n
    | AbstractLic (Some pn) -> 
       { n with def_eff = AbstractLic (Some (split_node lic_prg opt pn)) }
    | BodyLic b -> 
       let loc = match n.loclist_eff with None -> [] | Some l -> l in
       let (neqs, nv) = List.fold_left (split_eq_acc lic_prg) ([], loc) b.eqs_eff in
       profile_info (Printf.sprintf "Split %i equations into %i ones\n"
                                    (List.length b.eqs_eff) (List.length neqs));
       let (nasserts,neqs, nv) = 
         if Lv6MainArgs.global_opt.Lv6MainArgs.gen_autotest then
           (* do not split assertions when generating Lutin because we
             would handle less assertions *)
           (b.asserts_eff,neqs, nv) 
         else
           let asserts = List.map (fun x -> x.it) b.asserts_eff in 
           let lxm_asserts = List.map (fun x -> x.src) b.asserts_eff in 
           let nasserts,(neqs_asserts,nv_asserts) = 
             split_val_exp_list lic_prg false false
                                (* force the creation of a new var for
                                  assert so that it is easier to
                                  check in SocExec *) asserts
           in
           assert (List.length nasserts = List.length lxm_asserts); 
           let nasserts = List.map2 Lxm.flagit nasserts lxm_asserts in 
           (nasserts,neqs@neqs_asserts, nv@nv_asserts) 
       in 
       let neqs = List.map remove_tuple_from_eq neqs in
       let nb = { eqs_eff = neqs ; asserts_eff = nasserts  } in
       let n = 
         { n with
           loclist_eff = Some nv;
           def_eff = BodyLic nb }
       in
       n
  in
  res

let doit (opt:Lv6MainArgs.t) (inprg : LicPrg.t) : LicPrg.t =
   (* n.b. on fait un minumum d'effet de bord pour
      pas avoir trop d'acummulateur ... *)
   let res = ref LicPrg.empty in

   (* TRAITE LES TYPES *)
   let do_type k (te:Lic.type_) =
      res := LicPrg.add_type k te !res
   in
   LicPrg.iter_types do_type inprg;

   (* TRAITE LES CONSTANTES *)
   let do_const k (ec: Lic.const) =
      res := LicPrg.add_const k ec !res
   in 
   LicPrg.iter_consts do_const inprg ;

   (* TRAITE LES NOEUDS : *)
   let do_node k (ne:Lic.node_exp) =
      (* On passe en parametre un constructeur de nouvelle variable locale *)
     profile_info (Printf.sprintf  "#DBG: split equations of '%s'\n"
                                   (Lic.string_of_node_key k));
      let ne' = split_node inprg opt ne in
      res := LicPrg.add_node k ne' !res
   in
   (*LET's GO *)
   LicPrg.iter_nodes do_node inprg;
   !res

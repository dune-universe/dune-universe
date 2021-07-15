(* Time-stamp: <modified the 29/08/2019 (at 16:03) by Erwan Jahier> *)

(*
Source 2 source transformation :
élimine polymorphisme et surcharge

*)

open Lxm
open Lic

let dbg = (Lv6Verbose.get_flag "poly")

let (is_predef_overloaded : Lic.node_key -> bool) = 
  fun nk -> 
    match fst nk with
      | ("Lustre",("times"|"slash"|"uminus"|"minus"|"plus"|"lt"|"lte"|"gt"|"gte")) -> true
      | _ -> false

(** utile : on ne traite que les poly non externe *)
let node_is_poly ne =
   (Lic.node_is_poly ne) && not (Lic.node_is_extern ne)

let types_of_operands ops =
   match ops with vl ->
   List.flatten (List.map Lic.type_of_val_exp vl)

(* transforme un type match en pseudo-arg statique
   plus homogene ... *)
let static_args_of_matches matches =
   List.map (fun (tv, te) ->
      let tid = Lic.string_of_type_var tv in
      TypeStaticArgLic (tid, te)
   ) matches


(* tranform "plus" into "iplus", etc. *)
let (instanciate_node_key: Lic.type_matches -> Lic.node_key -> Lic.node_key) =
  fun tmatches nk -> 
    if is_predef_overloaded nk then (
      let ((_m,n),sargs) = nk in
      try if List.assoc AnyNum tmatches = Int_type_eff then 
          ("Lustre","i"^n),sargs
        else
          ("Lustre","r"^n),sargs
      with Not_found -> nk
    ) else 
      nk


let doit (inprg : LicPrg.t) : LicPrg.t =
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
  let rec do_node k (ne:Lic.node_exp) = (
    if node_is_poly ne then
      (* pour les noeuds *NON* polymorphes/surchagés, on fait rien du tout.
         pour les noeuds Lustre polymorphe (if, eq, neq) non plus.
      *)
      Lv6Verbose.exe ~flag:dbg (fun() -> Printf.printf
                                   "### Warning: no code generated for polymorphic/overloaded node '%s'\n"
                                   (Lic.string_of_node_key ne.node_key_eff))
    else
      let def' = match ne.def_eff with
        | MetaOpLic
        | ExternLic -> ne.def_eff 
        | AbstractLic _ -> assert false
        | BodyLic nb ->    BodyLic (do_body [] nb)
      in
      res := LicPrg.add_node k { ne with def_eff = def'} !res
  )
  (* TRAITEMENT DES BODY *)
  and do_body (m: Lic.type_matches) (nb: Lic.node_body) : Lic.node_body =
    (* parcours les expressions du body
       à la recherche d'appel de noeuds poly *)
    let do_assert a = Lxm.flagit (do_exp m a.it) a.src
    and do_eq eq =
      Lxm.flagit (List.map (do_left m) (fst eq.it), 
                  do_exp m (snd eq.it)) 
        eq.src
    in
    {
      asserts_eff = List.map do_assert nb.asserts_eff;
      eqs_eff = List.map do_eq nb.eqs_eff;
    }

  and do_left (m: Lic.type_matches) (l: Lic.left) : Lic.left =
    let rec aux l =
      match l with
      | LeftVarLic  (var_info, lxm) -> LeftVarLic(do_var_info m var_info, lxm) 
      | LeftFieldLic(left, id,  t)  -> LeftFieldLic(aux left, id, Lic.subst_matches m t)
      | LeftArrayLic(left, int, t)  -> LeftArrayLic(aux left, int, Lic.subst_matches m t)
      | LeftSliceLic(left, si,  t)  -> LeftSliceLic(aux left, si, Lic.subst_matches m t)
    in
    aux l

  and do_var_info (m: Lic.type_matches) (vi:Lic.var_info) : Lic.var_info =
    { vi with var_type_eff = Lic.subst_matches m vi.var_type_eff }

  (* TRAITEMENT DES EXP : on passe en parametre un Lic.type_matches *)
  and do_exp (m: Lic.type_matches) (e: Lic.val_exp) : Lic.val_exp =
    let typ' = Lic.apply_type_matches m e.ve_typ in
    let core' = match e.ve_core with
      | CallByPosLic (posop, ops) -> (
          let ops' = (List.map (do_exp m) ops) in
          match posop.it with
          | PREDEF_CALL (_pop) -> CallByPosLic (posop, ops')
          | CALL nk ->
            let ne = 
              match LicPrg.find_node inprg nk.it with 
              | Some n -> n
              | None -> assert false 
            in
            let nk' = if is_predef_overloaded nk.it then
                (Lxm.flagit (instanciate_node_key m nk.it) nk.src)
              else  if node_is_poly ne 
              then (
                Lv6Verbose.exe ~flag:dbg (fun () ->
                    Printf.fprintf stderr "#DBG: CALL poly node %s\n" (Lxm.details posop.src));
                let intypes = types_of_operands ops' in
                let (inpars, _) = Lic.profile_of_node_exp ne in
                let tmatches =  UnifyType.is_matched inpars intypes in
                {it=solve_poly tmatches nk.it ne; src=nk.src}
              )
              else nk 
            in
            let posop' = Lxm.flagit (CALL nk') posop.src in
            CallByPosLic (posop', ops')
          | _x ->
            (* dans tout les autre cas, raf ? *)
            CallByPosLic (posop, ops')
        )
      | CallByNameLic (namop, idops) ->
        let idops' = List.map (fun (id, ve) -> (id, (do_exp m) ve)) idops in
        CallByNameLic (namop, idops')
      | Merge (ce,cl) -> 
        let cl = List.map (fun (id, ve) -> (id, (do_exp m) ve)) cl in
        Merge (ce, cl)
    in
    { e with ve_core = core'; ve_typ = typ' }
  (* TRAITEMENT DES PARAMS STATIQUES *)
  and do_static_arg (m: Lic.type_matches) (a: Lic.static_arg) : Lic.static_arg =
    match a with
    | ConstStaticArgLic (_id, _cst) -> a
    | TypeStaticArgLic (_id, _ty) -> a
    | NodeStaticArgLic (id, nk) -> (
        match nk with
        | (("Lustre",_),[]) -> NodeStaticArgLic (id, instanciate_node_key m nk)
        | _ ->
          let ne = 
            match LicPrg.find_node inprg nk with 
            | Some n -> n
            | None -> assert false 
          in
          let nk' = solve_poly m nk ne in
          NodeStaticArgLic (id, nk')
      )
  (* Gros du boulot :
      soit un noeud poly, soit un profil attendu,
      fabrique s'il n'existe pas déjà, un noeud non poly adéquat ...
  *)
  and solve_poly (tmatches: Lic.type_matches) (nk: Lic.node_key) (ne: Lic.node_exp)
    : Lic.node_key = 
    Lv6Verbose.exe ~flag:dbg (fun () ->
        Printf.printf 
          "#DBG: L2lRmPoly.solve_poly nk='%s'\n#  prof=%s'\n# matches='%s'\n"
          (Lic.string_of_node_key nk)
          (Lic.string_of_type_profile (Lic.profile_of_node_exp ne))
          (Lic.string_of_type_matches tmatches)
      );
    let do_var vi =
      let nt = Lic.subst_matches tmatches vi.var_type_eff in
      assert(not (Lic.type_is_poly nt));
      { vi with var_type_eff = nt }
    in
    let (nid, sargs) = nk in
    (* nouvelle clé unique = ancienne + tmatches *)
    (*     let sargs' = sargs@(static_args_of_matches tmatches) in *)
    let sargs' = (List.map (do_static_arg tmatches) sargs)
                 @(static_args_of_matches tmatches) 
    in
    let nk' = (nid, sargs') in
    let def' = match ne.def_eff with
      | ExternLic
      | AbstractLic _ -> assert false 
      | MetaOpLic  -> MetaOpLic
      | BodyLic nb -> BodyLic(do_body tmatches nb)
    in 
    let ne' = {
      node_key_eff = nk';
      inlist_eff = List.map do_var ne.inlist_eff;
      outlist_eff = List.map do_var ne.outlist_eff;
      loclist_eff = (match ne.loclist_eff with
          | None -> None
          | Some vl -> Some (List.map do_var vl)
        );
      def_eff = def';
      has_mem_eff = ne.has_mem_eff;
      is_safe_eff = ne.is_safe_eff;
      lxm = ne.lxm;
    } in
    res := LicPrg.add_node nk' ne' !res;
    nk'
  in
  (*LET's GO *)
  LicPrg.iter_nodes do_node inprg;
  !res

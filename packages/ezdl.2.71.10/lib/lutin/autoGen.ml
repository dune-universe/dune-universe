(** compilation/generation d'automate
-------------------------------------

La génération statique d'un automate n'est pas forcément
la meilleure solution : l'idée d'un générateur dynamique
qui énumère les contraintes à la volée est sans doute plue
efficace.

Il s'agit donc essentiellement de faire des test de branchement
sur Lucky pour peaufiner le travail. Une solution plus efficace
sera étudiée plus tard. 

L'algo général est du type ``calcul des dérivées régulières'',
par opposition aux algos de style reglo/Thompson (trop compliqués
pour le langage étendu avec exception et parallélisme).

On procède en plusieurs étapes :
- génération d'un automate abstrait avec transitions "branchues"
- traduction de cet automate dans la syntaxe Lucky


La compilation en automate branchu est basée sur un schéma simple
qui rappèle le calcul des dérivées des e.r.

*)

open LutErrors;;
open Printf;;
open CoTraceExp ;;
open Expand ;;

let dbg = Verbose.get_flag "AutoGen"

(** N.B. On utilise des AlgExp.t de type "CkTypeEff.weight" pour
représenter les poids dans les transitions *)

(* tree-like transitions:
- leaves:
	"Vanish" for normal termination
	"Raise exception" for abnormal termination
	"Goto" for an actual transition
- n-ary node with weights
*)

(** WEIGHTS *)
type weightexp = 
|  W_huge 
|  W_exp of CoAlgExp.t

let opt_weight_of_opt_algexp = function
|  None -> None
|  Some e -> Some (
   if (e = CoAlgExp.of_huge_weight) then W_huge
   else W_exp e
)

let huge_weight = W_huge

(** EXCEPTIONS *)
type raise_desc = string

(** GOTO *)
type stable_state = Expand.tbl CoTraceExp.t

(* type cond = CoAlgExp.t list *)
type cond = Guard.t

type goto = cond * stable_state

(** TRANSITION TREE *)
type ttree =
   Vanish
|  Raise of raise_desc
|  Goto of goto
|  Split of (ttree * weightexp option * cond) list

let rec string_of_ttree topt = (  
   match topt with
      None -> sprintf "%s" LutPredef.kw_deadlock
   |  Some t -> (
		match t with
		| Vanish -> "Vanish"
		| Raise x -> sprintf "Raise %s" x
		| Goto (c, ss) -> (
			let cl = List.map CoAlgExp.lus_dumps (Guard.to_exp_list c) in
			sprintf "Goto (%s, %s)" (String.concat " and " cl) (CoTraceExp.dumps ss)
		)
		| Split l -> (
			let soc (t, wo, _c) = (
				let ws = match wo with
				|	Some W_huge -> "<INF>"
				|  None        -> "< 1 >"
				|  Some   _    -> "< ? >"
				in
				sprintf "%s : %s" ws (string_of_ttree (Some t))
			) in
			String.concat ";\n            " (List.map soc l)
		)
	)
)


(* DATA CONTEXT managment (for simulation)
NB used here ONLY for pre re-initialisation (cf. exist)
*)
type config = { data: Guard.store option; control: string }

let make_config state = { data = None; control = state }

(** STATE MANAGMENT *)
type state_info =
   SS_stable of Expand.tbl CoTraceExp.t
|  SS_transient
|  SS_final of string

(* POINT TO POINT TRANSITION:
   intermediate representation
   for full automaton generation *)
type trans = {
   src: string;
   wgt: weightexp option; 
   form: cond;
   dest: string; 
}

module TraceMap = struct
  include Map.Make(struct type t = Expand.tbl CoTraceExp.t let compare = compare end)
end

module ConfigMap = struct
  include Map.Make(struct type t = config let compare = compare end)
end

open Util

(* THE MAIN TYPE
	- (control) states are CoTraceExp.t
	- (control) states are hashed, and labelled by a unique string
	- a config is a variable config + a (control) state
*)
type t = {
  source_code : Expand.t;
  nb_stables : int;
  nb_transients : int;
  init_control : string;
  final_control : string;
  states : state_info StringMap.t ;
  transitions : trans list;

  (* Gestion des puits *)
  nb_sinks : int;
  _state2trace : Expand.tbl CoTraceExp.t StringMap.t ;
  _trace2state : string TraceMap.t ;
  _config2ttree : ttree ConfigMap.t;

  (* liste des control inexplorés *)
  todo : string list;

  (* mode global/dynamique *)   
}

(* Misc infos *)
let source (it:t) = it.source_code
let states (it:t) = it.states
let init_control (it:t) = it.init_control
let transitions (it:t) = it.transitions

(* ADD/REPLACE pre values in data strore *)
let add_pres (unalias:Guard.unalias) (data:Guard.store option) (pctx:CoTraceExp.escope) = (
	match pctx with
	| [] -> data
	|	_ -> (
		let ctx = match data with
		|	Some d -> d
		|	None -> Guard.empty_store
		in
		let addp accin (id, eo) = (
			match eo with
			| None -> accin
			| Some e -> (
				let v = try (
					Guard.value_of_algexp unalias ctx e 
				) with Guard.Not_constant _e -> raise (Internal_error ("AutoExplore.add_pres",
            	("initial value of \""^id^"\" must be a constant expression")))
				in
				Value.OfIdent.add accin (id, v)
			)
		) in
		let new_pres = List.fold_left addp ctx.Guard.pres pctx in
		Some {Guard.curs=ctx.Guard.curs; Guard.pres=new_pres}
	)
) 

let get_cpt i = CoIdent.of_cpt i

let dynamic_weight_exp (f : string) (c : int) (args : CoAlgExp.t list) = (
   let pcpt = CoAlgExp.of_pre (get_cpt c) CkTypeEff.integer in
   CoAlgExp.of_call (CoIdent.from_string f) CkTypeEff.integer (pcpt::args) 
)

let incr_loop_cpt (c : int) = (
   let nme = get_cpt c in
   let precpt = CoAlgExp.of_pre nme CkTypeEff.integer in
   let cpt = CoAlgExp.of_support nme CkTypeEff.integer true in
   CoAlgExp.of_call (CoIdent.from_string LutPredef.kw_eq) CkTypeEff.boolean [
      cpt ;
      CoAlgExp.of_call (CoIdent.from_string LutPredef.kw_plus) CkTypeEff.integer [
         precpt;
         CoAlgExp.of_iconst "1"
      ]
   ]
)
let reset_loop_cpt (c : int) = (
   let nme = get_cpt c in
   let cpt = CoAlgExp.of_support nme CkTypeEff.integer true in
   CoAlgExp.of_call (CoIdent.from_string LutPredef.kw_eq) CkTypeEff.boolean [
      cpt ;
      CoAlgExp.of_iconst "0"
   ]
)

(** dump des transition branchues pour debug *)
let dump_weight = ( function
|  Some W_huge    -> printf "huge"
|  Some (W_exp w) -> CoAlgExp.dump w
|  None           -> printf "1"
)

(* obsolete
let (algexp_of_weight: weightexp -> CoAlgExp.t) = (
   function w -> w
)
*)

let dump_ttree topt = (
   let rec rdump t pfx = (
      match t with
      Vanish -> (
         printf "%sVANISH\n" pfx;
      ) |
      Raise e -> (
         printf "%sRAISE %s\n" pfx e;
      ) |
      Goto (c, n) -> (
         printf "%sGOTO:[\n" pfx;
         printf "%s   cond:(" pfx;
         (* Utils.iter_sep (CoAlgExp.dump) (function _ -> printf " & ") c ; *)
			Guard.dumpf stdout c;
         printf " )\n%s   next: <" pfx;
         CoTraceExp.dump n;
         printf ">\n%s]\n" pfx
      ) |
      Split twl -> (
         printf "%sSPLIT:\n" pfx;
         let recpfx = sprintf "%s   " pfx in
         let dchoice (t,wo,cl) = (
            printf "%s[\n" pfx;
            (match wo with
               Some _w -> (
                  printf "%sWEIGHT: " recpfx;
                  dump_weight wo ; 
                  printf "\n"
               ) | None -> ()
            );
            printf "%sCOND: (" recpfx;
				Guard.dumpf stdout cl;
            printf ")\n";
            rdump t recpfx ;
            printf "%s]\n" pfx
         )  in
         List.iter dchoice twl
      )
   ) in
   (
   match topt with
      None -> printf "%s" LutPredef.kw_deadlock
   |  Some t -> rdump t ""
   );
   printf "\n" 
)

(****************************************************************************
TRACE -> TRANSITIONS BRANCHUES
-----------------------------------------------------------------------------
C'est essentiellement une fonction :
   CkTypeExp.t -> ttree
Le traitement des "feuilles" est réalisé par des fonctions
passées en paramètre (callback) :
- terminaisons normales (vanish)
- terminaisons anormales (raise)
- une pour traiter les continuations (goto)

N.B. ces deux fonctions peuvent ne pas rendre de résultat en cas
de ``DeadLock'' statique.
****************************************************************************)

(** utilitaire : mise en séquence tenant compte du TE_eps *)
let put_in_seq te1 te2 = (
   if(te1 = TE_eps) then te2
   else if(te2 = TE_eps) then te1
   else (TE_fby (te1,te2))
)
(* utilitaire : mise en parallèle *)
let put_in_para te1 te2 = (
   match (te1,te2) with
   | (TE_eps, x) -> x
   | (x, TE_eps) -> x
   | (x, TE_para yl) -> TE_para (x::yl)
   | (x,y) -> TE_para [x; y]
)


(* For old algo with several callbacks *)
(** type de la fonction de traitement des vanish *)
(* type goto_callback = goto -> ttree option *)

(** type de la fonction de traitement des raise *)
(* type raise_callback = raise_desc -> ttree option *)

(** type de la fonction de traitement des goto *)
(* type vanish_callback = unit -> ttree option *)

(* For NEW algo with a UNIQUE callback *)
type callback = ttree option -> ttree option 

(** CoTraceExp.t -> ttree *)

(* MAIN (RECURSIVE) TRAVERSAL OF A LUTIN STATE
	New algo -> 2009 semantics, with:
	- inherited acc-umulator for constraints
	- all the rest (mainly control) performed by cont-inuation

	N.B. cases that impact acc-umulator are:
		- TE_constraint
		- TE_para and also TE_assert (special case)

   with partial evaluation of contraints
*)

let _dump_cont _str tt _x = (
	
	dump_ttree tt;
)

let gentrans 
    (xenv : Expand.t)
	 (data : Guard.store option)  (* data env = inputs + pres *)
    (x : Expand.tbl CoTraceExp.t)     (* control = lutin trace *)
  = (
    (*-------------------------------------------*)
    (* Correspondance id de trace -> trace exp
       N.B. on traque les récursions ?  *)
    (*-------------------------------------------*)
    let id2trace s = (StringMap.find s (Expand.trace_tab xenv)).ti_def_exp in
    let unalias  s = (StringMap.find s (Expand.alias_tab xenv)).ai_def_exp in

    (*-------------------------------------------*)
    (* LA FONCTION RÉCURSIVE *)
    (*-------------------------------------------*)
    let rec rec_gentrans
		  (data : Guard.store option)  (* data env = inputs + pres *)
        (x : Expand.tbl CoTraceExp.t)
        (acc : cond)
        (cont : callback)
      = (
        Verbose.exe ~flag:dbg 
          (fun () -> Printf.printf "++rec_gentrans \"%s\"\n" (CoTraceExp.dumps x));
        match x with
          (TE_erun (_, _, _, _)
          | TE_dyn_erun (_, _, _, _, _)
          | TE_dyn_erun_ldbg (_, _, _, _, _)
          | TE_run (_, _, _, _, _, _)
          | TE_dyn_run (_, _, _, _, _, _, _)
          | TE_dyn_run_ldbg (_, _, _, _, _, _, _)) -> assert false
        (***** EPSILON => vanish ... *****)
        | TE_eps -> (
            cont (Some Vanish)
          )
        (***** TRACE NOMMÉE => ON CHERCHE SA DEF ******) 
        | TE_ref s -> (
            rec_gentrans data (id2trace s) acc cont
          )
        (***** CONTRAINTE => on appelle le callback *)
        | TE_constraint (ae,si) -> (
			   (* HERE DO EVAL ! *)
			   let new_acc = Guard.add ~context:data ae acc si in
			   cont (Some (Goto (new_acc, TE_eps)))
          )
        (***** SEQUENCE *****)
        (* on génère la trace de te1 avec :
           - goto(cl,n) => goto(cl,n fby te2)
           - vanish => te2
        *)
        | TE_fby (te1, te2) -> (
			   let fby_cont a = (
				  Verbose.exe ~flag:dbg 
                (fun () -> Printf.printf  "-- fby_cont (%s)\n  in context %s\n"
                    (string_of_ttree a) (CoTraceExp.dumps x));
				  match a with
				  |	None -> None
				  |	Some c -> (
					   match c with
					   | Split _ -> assert false
					   | Goto (cl,n) -> cont (Some (Goto (cl, put_in_seq n te2)))
					   | Vanish -> rec_gentrans data te2 acc cont
					   | _ -> cont (Some c)
         	    )
			   ) in
            rec_gentrans data te1 acc fby_cont
          )
        (**** CHOIX *****)
        (* chaque choix est traité dans
           l'environnement global,
           on décore la branche avec le poids
           correspondant et une action vide *)

        (* ICI *)

        | TE_choice wtel -> (
            let res = ref [] in
            let tc (te, we) = (
              match (rec_gentrans data te acc cont) with
                None -> ()
              | Some tt -> res := (tt, opt_weight_of_opt_algexp we,Guard.empty)::!res
            ) in
            List.iter tc wtel;
            match !res with
              [] -> None
            |  ttlist -> Some (Split ttlist)
          )
        (*** PRIORITE ****)
        | TE_prio tel -> (
            let rec doit = ( function
                  [] -> assert false
                |  [te] -> (
                    rec_gentrans data te acc cont
                  )
                |  te::tail -> (
                    let first = rec_gentrans data te acc cont in
                    let others = doit tail in 
                    match (first, others) with
                      (None,None) -> None
                    |  (Some f, None) -> Some f
                    |  (None, Some o) -> Some o
                    |  (Some f, Some o) ->
                      Some
                        (Split [(f, Some huge_weight, Guard.empty) ;
                                (o, None, Guard.empty)])
                  )
              ) in doit tel
          )
        (*** BOUCLE "INFINIE" ***)
        | TE_loop te -> (
			   let loop_goon_cont a = (
				  Verbose.exe ~flag:dbg
                (fun () -> Printf.printf "-- loop_goon_cont (%s)\n   in context %s\n"
                    (string_of_ttree a) (CoTraceExp.dumps x));
				  match a with
				  |	None -> None
				  |	Some c -> (
					   match c with
					   | Split _ -> assert false
					   | Goto (cl,n) -> cont (Some (Goto (cl, put_in_seq n x)))
					   | Vanish -> None
					   | _ -> cont (Some c)
				    )
            ) in
            (* on génère en priorité les cas ``boucle'' *)
            let goon = rec_gentrans data te acc loop_goon_cont in
			   Verbose.exe ~flag:dbg 
              (fun () -> Printf.printf  "==== loop goon branch of %s\n     gives: %s\n"
                  (CoTraceExp.dumps x) (string_of_ttree goon));
            (* on considère aussi le cas vanish *)
            let stop = cont (Some Vanish) in
			   Verbose.exe ~flag:dbg
              (fun () -> Printf.printf "==== loop stop branch of %s\n     gives: %s\n"
                  (CoTraceExp.dumps x) (string_of_ttree stop));
            match (goon,stop) with
              (None,None) -> None
            |  (Some g, None) -> Some g
            |  (None, Some s) -> Some s
            |  (Some g, Some s) -> Some
                                     (Split [(g, Some huge_weight, Guard.empty) ; (s, None, Guard.empty)])
          )
        (*** BOUCLE intervale ****)
        | TE_loopi (cpt,min,max,te,si) -> (
            (* similaire à la boucle sauf pour les poids et les effets
               de bord :
               goon -> interval_goon(pre cpt, min, max)
               + loop_incr_exp cpt
               stop -> interal_stop(pre cpt, min, max)
               + loop_reset_exp cpt
            *)
			   let loopi_goon_cont a = (
				  Verbose.exe ~flag:dbg 
                (fun () -> Printf.printf "-- loopi_goon_cont (%s)\n   in context %s\n"
                    (string_of_ttree a) (CoTraceExp.dumps x));
				  match a with
				  |	None -> None
				  |	Some c -> (
					   match c with
					   | Split _ -> assert false
					   | Goto (cl,n) -> cont (Some (Goto (cl, put_in_seq n x)))
					   | Vanish -> None
					   | _ -> cont (Some c)
				    )
            ) in
            let goon = rec_gentrans data te acc loopi_goon_cont in
            let stop = cont (Some Vanish) in
            match (goon,stop) with
              (None,None) -> None
            |  (Some g, None) -> Some g
            |  (None, Some s) -> Some s
            |  (Some g, Some s) -> (
                let gw = dynamic_weight_exp LutPredef.kw_interval_goon cpt [min; max]  in
                let ga = incr_loop_cpt cpt in
                let sw = dynamic_weight_exp LutPredef.kw_interval_stop cpt [min; max]  in
                let sa = reset_loop_cpt cpt in
                Some
                  (Split [(g, Some (W_exp gw), Guard.of_exp ga si) ;
                          (s, Some (W_exp sw), Guard.of_exp sa si)])
              )
          )
        (*** BOUCLE moyenne ****)
        | TE_loopa (cpt,av, ecopt,te,si) -> (
            (* similaire à la boucle sauf pour les poids :
               goon -> loopa_goon(pre cpt, min, max)
               stop -> loopa_stop(pre cpt, min, max)
            *)
			   let loopa_goon_cont a = (
				  Verbose.exe ~flag:dbg
                (fun () -> Printf.printf "-- loopi_goon_cont (%s)\n   in context %s\n"
                    (string_of_ttree a) (CoTraceExp.dumps x));
				  match a with
				  |	None -> None
				  |	Some c -> (
					   match c with
					   | Split _ -> assert false
					   | Goto (cl,n) -> cont (Some (Goto (cl, put_in_seq n x)))
					   | Vanish -> None
					   | _ -> cont (Some c)
				    )
            )  in
            let goon = rec_gentrans data te acc loopa_goon_cont in
            let stop = cont (Some Vanish) in
            match (goon,stop) with
              (None,None) -> None
            |  (Some g, None) -> Some g
            |  (None, Some s) -> Some s
            |  (Some g, Some s) -> (
                let ec = match ecopt with
                    Some x -> x
                  |  None -> CoAlgExp.of_iconst "0" 
                in 
                let gw = dynamic_weight_exp LutPredef.kw_average_goon cpt [av; ec]  in
                let ga = incr_loop_cpt cpt in
                let sw = dynamic_weight_exp LutPredef.kw_average_stop cpt [av; ec]  in
                let sa = reset_loop_cpt cpt in
                Some
                  (Split [(g, Some (W_exp gw), Guard.of_exp ga si) ; 
                          (s, Some (W_exp sw), Guard.of_exp sa si)])
              )
          )
        (* Assert 
        *)
        | TE_assert (a, te, si) -> (
			   let assert_cont act = (
				  Verbose.exe ~flag:dbg
                (fun () -> Printf.printf  "-- assert_cont (%s)\n   in context %s\n"
                    (string_of_ttree act) (CoTraceExp.dumps x));
				  match act with
				  |  None -> None
				  |  Some c -> (
					   match c with
					   | Split _ -> assert false
					   | Goto (cl,n) -> cont (Some (Goto (cl, TE_assert (a,n,si))))
					   | _ -> cont (Some c)
				    )
			   ) in
			   let rec_acc = Guard.add ~context:data a acc si in
            rec_gentrans data te rec_acc assert_cont
          )
        (* init_pre
			   recursive call with a new store
        *)
        | TE_exist (ectx, te) -> (
			   let new_data = add_pres unalias data ectx in 
            rec_gentrans new_data te acc cont  
          )
        (* RAISE 
            on appèle simplement le callback avec le nom de l'exception *)
        | TE_raise s -> (
			   cont (Some (Raise s))
          )
        (* on appèle recursivement le traitement de e avec un nouveau raise callback :
            - si le callback est levé avec i, appèle recursivement :
            * si eco = Some ec : ec avec les callbacks de l'appel
            * sinon le vanish de l'appel
            - sinon appèle le callback raise de l'appel
            ET avec un nouveau goto callback :
            * goto(cl,n) => goto(cl, catch(i,n,eco))
        *)
        | TE_catch (i,e,eco) -> (
			   let catch_cont a = (
				  Verbose.exe ~flag:dbg
                (fun () -> Printf.printf  "-- catch_cont (%s)\n   in context %s\n"
                    (string_of_ttree a) (CoTraceExp.dumps x));
				  match a with
				  |	None -> None
				  |	Some c -> (
					   match c with
					   | Split _ -> assert false
					   | Goto (cl,n) -> cont (Some (Goto (cl, TE_catch(i, n, eco))))
					   | Raise x -> (
            		    if ( x == i) then (
               		   match eco with
               		     Some ec -> (
                  		    rec_gentrans data ec acc cont
               		     ) |
               		     None -> cont (Some Vanish)
            		    ) else (
               		   cont (Some (Raise x))
            		    )  
					     )
					   | _ -> cont (Some c)
				    )
            ) in
            rec_gentrans data e acc catch_cont
          )
        (* TRY *)  
        (* optimisation : try(eps,_) = eps *)
        | TE_try (TE_eps,_) -> cont (Some Vanish)
        | TE_try (e,eco) -> (
            (* on créé un choix binaire entre :
                - priorité (poids infini) e avec un nouveau goto callback :
                * goto(cl,n) -> goto(cl, try(n,eco))
                - sinon :
                * si "eco = Some ec" alors appel rec. sur ec
                * si "eco = None" alors appel du vanish 
            *)
            (* la branche prio *)
			   let try_cont a = (
				  Verbose.exe ~flag:dbg 
                (fun () -> Printf.printf "-- try_cont (%s)\n   in context %s\n"
                    (string_of_ttree a) (CoTraceExp.dumps x));
				  match a with
				  |	None -> None
				  |	Some c -> (
					   match c with
					   | Split _ -> assert false
					   | Goto (cl,n) -> cont (Some (Goto (cl, TE_try(n,eco))))
					   | _ -> cont (Some c)
				    )
            ) in
            let prio = rec_gentrans data e acc try_cont in

            (* la branche par defaut *) 
            let defaut = ( match eco with
                | Some ec -> (
                    rec_gentrans data ec acc cont
                  )
			       | None -> cont (Some Vanish)
              ) in
            match (prio,defaut) with
              (None,None) -> None
            |  (Some p, None) -> Some p
            |  (None, Some d) -> Some d
            |  (Some p, Some d) -> (
                Some 
                  (Split [(p, Some huge_weight, Guard.empty) ; (d, None, Guard.empty)])
              )
          )
        (* Parallèle : il y en a toujours au - un !  *)
        | TE_para ([]) -> assert false
        | TE_para ([e]) -> (
            (* le dernier continue son chemin *)
            rec_gentrans data e acc cont
          )
        | TE_para (e::el) -> (
			   (* continuation for the head statement *)
			   let para_head_cont a = (
				  Verbose.exe ~flag:dbg 
                (fun () -> Printf.printf "-- para_head_cont (%s)\n   in context %s\n"
                    (string_of_ttree a) (CoTraceExp.dumps x));
				  match a with
				  |	None -> None
				  |	Some c -> (
					   match c with
					   | Split _ -> assert false
					   | Vanish -> (
         			    (* 1st vanishes: others continue *)
						    rec_gentrans data (TE_para(el)) acc cont
					     )
					   | Raise s -> (
         			    (* 1st raises s: whole raises s *)
						    cont (Some (Raise s))
					     )
					   | Goto (cl,n) -> (
					       (* 1st REQUIRES TO DO cl,
					          That is, if cl is satisfiable, THEN 
					          the others MUST find a behavior compatible
					          HERE: PROBLEM, impossible
					          to do that with Lucky ! (see the case Raise below)
					          *)
						    (* HERE: DO EVAL! *) 
						    let tail_acc = Guard.merge cl acc in
						    let para_tail_cont a = (
							   Verbose.exe ~flag:dbg 
                          (fun () -> Printf.printf 
                              "-- para_tail_cont (%s)\n   in context %s\n"
                              (string_of_ttree a) (CoTraceExp.dumps x));
							   match a with
							   | None -> None
							   | Some c -> (
								    match c with
								    | Split _ -> assert false
								    | Vanish -> (
									     (* others vanish, 1st continue *)
									     cont (Some (Goto (cl,n)))
								      )
								    | Raise s -> (
									     (* SEMANTICS PROBLEM:
										     should the others be able to raise exception
										     IF cl is satisfiable ?
										     - no: we consider that the head has priority
										     and then others must find a way to respect it
										     either with a trans, or vanish (if possible) 
										     - yes: we state that raising an exception is
										     a acceptable way (though strange !) to respect
										     the head 
									        => the "no" seems more acceptable ??
									        TECHNICAL PROBLEM:  none of these solutions
									        can be expressed with "wt" because satisfiability
									        check are ONLY performed at leaves.
                                                                       => the "real" semantics will be implemented in 
									        the "lutExe" only.
									        Lucky-wt based algos are FALSE in this case
									        For the time being:
									        - emit a warning
									        - choose an "intermediate" solution consisting
									        in rejecting locally this case	
									        *)
									     (* cont (Some (Raise s)) *)
                                LutErrors.warning None 
	                               ("raising exception "^s^" in a parallel cannot be safely compiled into Lucky");
									     cont None
								      )
								    | Goto (tcl, tn) -> (
									     (* HERE -> something to do ? *)
									     cont (Some (Goto (tcl, put_in_para n tn)))
								      )
							     )
						    ) in
            		    rec_gentrans data (TE_para(el)) tail_acc para_tail_cont
					     )
					   (* | _ -> cont (Some c) *)
				    )
			   ) in
            rec_gentrans data e acc para_head_cont
          )
		  | TE_strong_assert (_, _, _)
		  | TE_dyn_loop (_, _, _)
		  | TE_omega _
		  | TE_noeps _
		  | TE_dyn_choice (_, _)
			 -> assert false
      ) in
    (* top-level cont *)
	 let top_cont a = (
		Verbose.exe ~flag:dbg 
        (fun () -> Printf.printf "-- top_cont (%s)\n   in context %s\n"
            (string_of_ttree a) (CoTraceExp.dumps x));
		match a with 
		| None -> None
		| Some c -> (
			 match c with
			 | Split _ -> assert false
			 | _ -> Some c
		  )
	 ) in
    Verbose.exe ~flag:dbg  (fun () -> Printf.printf "=================\ngentrans \"%s\"\n"
                               (CoTraceExp.dumps x));
    let res = rec_gentrans data x Guard.empty top_cont in
    (*dump_ttree res;*)
    res
  )

(****************************************************************************
GENERATION DE L'AUTOMATE COMPLET
-----------------------------------------------------------------------------
Nommage des états :
- les stables sont "state<index de stable>
- les transients sont "<father>_<sous-index>

- N.B. il existe toujours un état final potentiel
  qu'on nomme "vanish"

****************************************************************************)

(** dump des transitions *)
let dump_trans tr = (
printf "TRANSITION [\n";
printf "   SRC: %s\n" tr.src;
printf "   WEIGHT: ";
   dump_weight tr.wgt ;
printf "\n";
printf "   COND: " ;
   Guard.dumpf stdout tr.form ;
printf "\n";
printf "   DEST: %s\n" tr.dest;
printf "]\n"
)

let new_stable_state (it: t) (e : Expand.tbl CoTraceExp.t) = (
  let ssi = it.nb_stables in
  let res = sprintf "state%d" ssi in
  let it = { it with
             nb_stables =  it.nb_stables + 1;
             states = StringMap.add  res (SS_stable e) it.states
           }
  in
  res, it
)

let new_transient_state (it: t) (father: string) (index: int) = (
  let res = sprintf "%s_%d" father index in
  let it = { it with
             nb_transients = it.nb_transients + 1;
             states = StringMap.add res SS_transient it.states;
           }
  in
  res, it
)

(** recherche/crée une association trace/state *)
let get_stable (it:t) e = (
  try TraceMap.find e it._trace2state, it
  with Not_found -> (
      let res, it = new_stable_state it e in
      Verbose.exe ~level:3
        (fun () -> Printf.printf "##new state=\"%s\" exp=%s\n" res (CoTraceExp.dumps e));
      let it = { it with
                 _trace2state = TraceMap.add e res it._trace2state;
                 _state2trace = StringMap.add res e it._state2trace;
                 todo = res :: it.todo
               }
      in
      res, it
    )
)


(** recherche/crée un état puits
   N.B, on garde tel que l'ident qui est suppose être unique !
*)
let get_sink (it:t) x =
  match StringMap.find_opt x it.states with
  | None ->
    Verbose.put ~level:3 "##new sink=\"%s\"\n" x ;
    x, { it with
         nb_sinks = it.nb_sinks + 1;
         states = StringMap.add  x (SS_final x) it.states;
       }
  | Some _ -> x, it

let init (xenv : Expand.t) =
  let res = {
    source_code = xenv;
    nb_stables = 0;
    nb_transients = 0;
    init_control = "";
    (* L'état final est un puit *)
    final_control = "";
    states = StringMap.empty;
    transitions = [];

    nb_sinks = 0;
    _state2trace = StringMap.empty; 
    _trace2state = TraceMap.empty;
    _config2ttree = ConfigMap.empty;

    (* liste des inexplorés *)
    todo = [];
  }
  in
  let is = Expand.main_trace xenv in
  let ie = (Util.StringMap.find is (Expand.trace_tab xenv)).ti_def_exp in
  let init_control, res = get_stable res ie in
  let final_control, res = get_sink res "vanish" in
  { res with
    init_control = init_control;
    final_control =final_control;
  }


let rec ttree2trans (it:t) (src: string) (tt : ttree) = (
  match tt with
  | Vanish -> 
    [ { src = src; wgt = None ; form = Guard.empty; dest = it.final_control } ], it
  | Raise x ->
    let dest, it = get_sink it x in
    [ { src = src; wgt = None ; form = Guard.empty; dest = dest } ], it
  | Goto (cl, n) ->
    let  dest, it = get_stable it n in
    [ { src = src; wgt = None ; form = cl; dest = dest ; } ], it 
  | Split twl -> (
      let child_cpt = ref 0 in
      let treat_choice (_trs,it) (t, wo, a) =
        let dest, it = new_transient_state it src !child_cpt in
        incr child_cpt;
        let t0 = { src = src; wgt = wo ; form = a ; dest = dest; } in
        let trs, it = ttree2trans it dest t in
        trs @ (t0 :: trs), it
      in
      List.fold_left treat_choice ([],it) twl
    )
)


let get_state_def (it:t) (ix: string) =  StringMap.find ix it._state2trace

let get_state_info (it:t) (ix: string) = StringMap.find ix it.states 

(*
*)
let config2ttree (it:t) (cfg: config) = (
  let ix = cfg.control in
  let e = StringMap.find ix it._state2trace in
  let data = cfg.data in
  (* use cash *)
  try 
    let tt = ConfigMap.find cfg it._config2ttree in
    Verbose.put ~level:2 "##config2ttree: \"%s\" cached\n" ix ;
    if (Utils.paranoid ()) then (
      let tt' = gentrans it.source_code data e in
      assert (tt' = (Some tt))
    );
    tt, it
  with Not_found -> (
      Verbose.exe ~level:2
        (fun () -> Printf.printf "##config2ttree: \"%s\" = %s\n" 
            ix (CoTraceExp.dumps e));
      match ( gentrans it.source_code data e) with
      (* match ( gentrans_old it.source_code e) with *)
        Some tt ->
        let it = { it with _config2ttree = ConfigMap.add cfg tt it._config2ttree } in
        (* (* TODO:  *)tree2trans it ix tt *)
        tt, it
      | None -> raise (Failure "unexpected toplevel Deadlock")
    )
)


type gtree = string * gtree_node 
and gtree_node =
|  GT_leaf of (cond * string)
|  GT_choice of (weightexp option * gtree) list
|  GT_stop of string

(* debug : size ? *)
let rec gtree_size (_,gt) = (
  match gt with
  |  GT_leaf _ -> 1
  |  GT_stop _ -> 1
  |  GT_choice ll -> (
    List.fold_left (fun a (_, g) -> a + (gtree_size g)) 1 ll
  )
)
    
let rec ttree2gtree (it:t) (src: string) (acc: cond) (tt : ttree) =
  match tt with
  | Vanish -> (src, GT_stop it.final_control), it
  | Raise x ->
    let sink, it = get_sink it x in 
    (src, GT_stop sink), it
  | Goto (cl, n) ->
    let st, it = get_stable it n in
    (src, GT_leaf (Guard.merge acc cl, st)), it
  | Split twl -> (
      (* |  Split of (ttree * weightexp option * CoAlgExp.t list) list *)
      let child_cpt = ref 0 in
      let treat_choice :  (weightexp option * gtree) list * t -> 
        (ttree * weightexp option * cond) -> (weightexp option * gtree) list * t =
        fun (choices, it) (t, wo, a) ->
          let dest, it = new_transient_state it src !child_cpt in
          incr child_cpt;
          let cht, it = ttree2gtree it dest (Guard.merge acc a) t in
          (wo, cht)::choices, it
      in
      let choices, it = List.fold_left treat_choice ([],it) twl in
      (src, GT_choice (List.rev choices)), it
    )

let config2gtree (it:t) (cfg: config) = (
  let ix = cfg.control in
   let tt, it = config2ttree it cfg in
   ttree2gtree it ix Guard.empty tt
)

let config2trans (it:t) (cfg: config) = (
  let ix = cfg.control in
   let tt, it = config2ttree it cfg in
   ttree2trans it ix tt
)

(*
Builds a full automaton from an expanded Lutin program
the "store" in config if always EMPTY
*)
let make (xenv : Expand.t) = (
  let it = init xenv in
  let rec explore (tlist, it) = 
    match it.todo with
    | [] -> (tlist, it) 
    | s::tail -> (
        (* on l'enlève *)
        let it = { it with todo = tail } in
        let curconf = { data = None; control = s} in
        let trs, it = config2trans it curconf in
        let tlist = trs @ tlist in
        (* on continue *)
        explore (tlist, it) 
      )
  in
  let tlist, it = explore ([],it) in
  { it with transitions = List.rev tlist }
)

let dump (auto : t) = (
   printf "AUTOMATON (init: %s stables: %d transients: %d)\n"
      auto.init_control auto.nb_stables auto.nb_transients
   ;
   List.iter dump_trans auto.transitions
)

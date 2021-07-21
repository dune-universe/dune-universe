(**

LUTIN2 : 
	identificateurs prédéfinis
	(dont opérateurs algébriques)

- Les opérations algébriques sont traitées
  comme des opérateurs (i.e. macros) prédéfinies.
  (par oposition aux statements qui manipulent des trace)

- Pour le pretty-print Lucky, on garde une table
  qui permet de retrouver la notation infixée 
  correspondante
  
*)

let lutin_env = CheckEnv.create ()

let (infixed_tab : (string, string list) Hashtbl.t) = Hashtbl.create 50

let as_infixed_syntax (op: string) = (
  try (
	 Some (Util.hfind infixed_tab op)
  ) with Not_found -> None
	 
)

(* poids prédef *)
let kw_tiny = "tiny"
let _ = CheckEnv.add_predef_cst lutin_env kw_tiny CkTypeEff.weight
let kw_huge = "huge"
let _ = CheckEnv.add_predef_cst lutin_env kw_huge CkTypeEff.weight

(* poids dynamiques predefs *)

(** poids dynamique des boucles intervalle :
les params sont 
- le nombre de boucles déjà effectuées (pre cpt)
- le nombre min 	
- le nombre max 	
*) 
let kw_interval_goon = "interval_continue"
let _ = CheckEnv.add_predef_op lutin_env kw_interval_goon CkTypeEff.prof_iii_i
let kw_interval_stop = "interval_stop"
let _ = CheckEnv.add_predef_op lutin_env kw_interval_stop CkTypeEff.prof_iii_i

let kw_average_goon = "gauss_continue"
let _ = CheckEnv.add_predef_op lutin_env kw_average_goon CkTypeEff.prof_iii_i
let kw_average_stop = "gauss_stop"
let _ = CheckEnv.add_predef_op lutin_env kw_average_stop CkTypeEff.prof_iii_i
(* exception prédef *)
let kw_deadlock = "Deadlock"
let _ = CheckEnv.add_predef_cst lutin_env kw_deadlock CkTypeEff.except


(* utilitaire *)
let declare_op
	(nme:string)
	(prof:CkTypeEff.profile)
	(infix:string list) = 
(
	let _ = CheckEnv.add_predef_op lutin_env nme prof in
	match infix with
		[] -> ()
	|	lst -> (
		Hashtbl.add infixed_tab	nme lst
	)
)


(* strictement booléens *)
let kw_not = "not"
let _ = declare_op kw_not CkTypeEff.prof_b_b ["not ";""] 

let kw_or = "or"
let _ = declare_op kw_or CkTypeEff.prof_bb_b ["";" or ";""] 

let kw_xor = "xor"
let _ = declare_op kw_xor CkTypeEff.prof_bb_b ["";" xor ";""] 

let kw_nxor = "nxor"
let _ = declare_op kw_nxor CkTypeEff.prof_bl_b [] 

let kw_nor = "nor"
let _ = declare_op kw_nor CkTypeEff.prof_bl_b [] 

let kw_diese = "#"
let _ = declare_op kw_diese CkTypeEff.prof_bl_b []  

let kw_and = "and"
let _ = declare_op kw_and CkTypeEff.prof_bb_b ["";" and ";""] 

let kw_impl = "impl"
let _ = declare_op kw_impl CkTypeEff.prof_bb_b ["";" => ";""] 

(* polymorphes *)
let kw_eq = "eq"
let _ = declare_op kw_eq CkTypeEff.prof_xx_b ["";" = ";""]

let kw_neq = "neq"
let _ = declare_op kw_neq CkTypeEff.prof_xx_b ["";" <> ";""]

let kw_ite = "ite"
let _ = declare_op kw_ite CkTypeEff.prof_bxx_x ["if ";" then ";" else ";""]

(* strictement entiers *)
let kw_div = "div"
let _ = declare_op kw_div CkTypeEff.prof_ii_i ["";" div ";""]

let kw_mod = "mod"
let _ = declare_op kw_mod CkTypeEff.prof_ii_i ["";" mod ";""]

(* surchargés n -> n *) 
let kw_uminus = "uminus"
let _ = declare_op kw_uminus CkTypeEff.prof_n_n ["-";""] 

(* surchargés nn -> n *) 
let kw_plus = "plus"
let _ = declare_op kw_plus CkTypeEff.prof_nn_n ["";" + ";""]

let kw_minus = "minus"
let _ = declare_op kw_minus CkTypeEff.prof_nn_n ["";" - ";""]

let kw_times = "times"
let _ = declare_op kw_times CkTypeEff.prof_nn_n ["";" * ";""]

let kw_slash = "slash"
let _ = declare_op kw_slash CkTypeEff.prof_nn_n ["";" / ";""]

(* surchargés nn -> b *) 
let kw_lt = "lt"
let _ = declare_op kw_lt CkTypeEff.prof_nn_b ["";" < ";""]

let kw_gt = "gt"
let _ = declare_op kw_gt CkTypeEff.prof_nn_b ["";" > ";""]

let kw_lte = "lte"
let _ = declare_op kw_lte CkTypeEff.prof_nn_b ["";" <= ";""]

let kw_gte = "gte"
let _ = declare_op kw_gte CkTypeEff.prof_nn_b ["";" >= ";""]


(** EXPANSION : main

------------------------------------------------------------

L'expansion consiste essentiellement à construire 3 tables globales
indexées par des ident target :

- Table des variables (support) : elle contient les entrées/sorties
  et les locales (cf. exist) remontées au top via un nommage unique.

- 2010/04/15 : pour faciliter le traitement des pre
        (en particulier pour les exist avec init), on cree systematiquement
   un ident pour les pre :
        - Input/Output: (pre in/pre out) on crée un alias (e.g. prein = pre in)
        NON (CA MARCHE PAS) - Local : (pre x) on crée une var support prex,
     avec comme default "pre x"
        - Local : il faut aussi un alias, mais plus compliqué car le pre
        est potentiellement réinitialisé dans le support_scope.
   L'idée est qu'on associe une variable spéciale booléenne associée
   au support_scope soit enter_support_scope.
        si la variable x a une valeur init (e), l'alias est :
      pre_x = if enter_support_scope then e else pre x
        sinon, c'est pas la peine, on peut avoir simplement : 
      pre_x = pre x
        La variable enter_support_scope est une variable support avec
        valeur par défaut "false", et qui est forcée à vrai
        sur entrée dans le support_scope.

        Problème : il faut se débrouiller pour ne créer le enter_support_scope
        que si besoin !

        ATTENTION : ne pas confondre "sscope" avec les scopes de l'expansion
          
- Table des alias : associe à des idents cible des expressions algébriques.
    Les idents cible correpondent au instances de macros utilisées
    dans le corps du node.

- Table des traces : à chaque trace nommée est associée une expression
  de trace expansée.

----------------------------------------------------------*)

open Lexeme;;
open LutErrors;;
open Printf;;
open Syntaxe;;
open CkIdentInfo;;
open CoIdent;;


let dbg = Verbose.get_flag "Expand"

(**************************************************************
Type des infos associées aux variables support, aux alias et
aux traces.
**************************************************************)

(**
L'info associée à une variable support contient sa nature,
l'expression algébrique associée (celle qui permet de faire
référence à la variable dans les expressions algébriques),
ainsi que la "pile" permettant le retour au source.

2010/04, rajout :
- éventuelle référence au pre
- éventuel support_scope 
- éventuelle valeur initiale 
- éventuelle valeur par défaut 
*)
type support_nature =
    Input
  | Output
  | LocalIn   (* hidden input *)
  | LocalOut  (* hidden output *)
    
type support_info = {
  si_ident : CoIdent.t ;
  si_nature : support_nature ;
  si_type : CkTypeEff.t ;
  si_ref_exp : CoAlgExp.t ;
  si_src : CoIdent.src_stack;
  (* on ne la crée qu'à la demande *)
  si_pre_ref_exp : CoAlgExp.t option ;
  si_default : CoAlgExp.t option ;
  si_scope : support_scope option ;
  si_init : CoAlgExp.t option ;
  si_range : (CoAlgExp.t * CoAlgExp.t) option;
}

(**************************************************************
   LOCAL SCOPE HANDLING (see EXIST_n)
   ---------------------------------------------------------------
   A scope contains a pre (re)init context 
 **************************************************************)

and support_scope = {
  sco_src : CoIdent.src_stack;
  sco_escope : CoTraceExp.escope;
}

let new_support_scope (src : CoIdent.src_stack) = (
  let res = {
    sco_src = src;
    (* made on demand *)
    sco_escope = CoTraceExp.new_escope () ;
  }
  in
  res
)

(**
Alias : L'info contient le type, l'expression à utiliser pour faire
référence à l'alias, et celle qui correspond à sa définition
*)
type alias_info = {
  ai_type : CkTypeEff.t;
  ai_def_exp : CoAlgExp.t;
  ai_ref_exp : CoAlgExp.t;
  ai_src : CoIdent.src_stack
}


(**
Exception :  pas grand chose à stocker : c'est juste un ident ...
*)
type except_info = {
  ei_src : CoIdent.src_stack ;
}

(**
Externe : il s'agit surtout de garder l'info pour utilisation
    ulterieure
*)
type extern_info = {
  xi_decl : Syntaxe.let_info;
  xi_prof : CkTypeEff.profile;
  xi_src : Lexeme.t ;
}

(**************************************************************
On utilise de plus une table ``dynamique'' qui fait l'association entre
ident source et ident target. Les associations varient au cours de
l'expansion, mais la table est tout de même globale~:
on se contente d'écraser au fur et à mesure les associations, sans
se soucier d'avoir une cohérence globale de la table (le binding a déjà été
fait, donc tout est correct).

---------------------------------------------------------------------
Notes sur les instances de macros
---------------------------------------------------------------------
Macros sans paramètres :
------------------------
        Pour les macros sans paramètres, le corps est expansé une fois pour toute
        et le même alias/trace est utilisé pour toutes
        les références.
Macros avec paramètres :
------------------------
        Pour les macros avec paramètres, on ne fait pas d'équivalence
        syntaxique. Par exemple, pour~:

                let f (x,y,z : ...) = E in
                ...
                ... f(a,b,c) ... -- instance f1 
                ...
                ... f(a,b,c) ... -- instance f2 
                ...

        La macro f va être expansée 2 fois, c'est-à-dire qu'on aurra
        les def. d'alias ou de trace suivantes~:

                f1 = expand(E/x <- f1_x, y <- f1_y, z <- f1_z
                avec f1_x = expand(a), f1_y = expand(b), f1_z = expand(c).

                et idem pour [[f2]].

**************************************************************)


(**************************************************************
Table d'association source/target
***************************************************************
On utilise une table globale. Les associations sont écrasées
au grès de l'évolution du scope, sans aucune vérification
particulière puisqu'on sait que le programme est correct
coté binding.

N.B. la clé (unique) utilisée pour désigner les identificateurs
source correspond à l'instance de la déclaration de l'ident.
Pour savoir à quelle instance de déclaration est associée
une INSTANCE DE RÉFÉRENCE, il faut IMPÉRATIVEMENT
passer par la fonction CheckEnv.binding
**************************************************************)

type target_nat =
   TN_support
|  TN_alias
|  TN_trace
|  TN_except

open Util
module SrcIdentMap = struct
  include Map.Make(struct type t = Syntaxe.ident let compare = compare end)
end

(* MAIN STRUCTURE (no more global var) *)
type t = {
  nme : string;
  (* unique ident space *)
  idspace : CoIdent.space;
  (* lists, in the order of declaration *)
  alist : CoIdent.t list;
  inlist : CoIdent.t list;
  outlist : CoIdent.t list;
  locinlist : CoIdent.t list;
  locoutlist : CoIdent.t list;
  prelist : (CoIdent.t * support_info) list;
  (* info tabs *)
  stab : support_info StringMap.t;
  atab : alias_info StringMap.t;
  ttab : trace_info StringMap.t;
  etab : extern_info StringMap.t;
  xtab : except_info StringMap.t;
  (* run instance table *)
  runcpt : int;
  runtab: t StringMap.t;
  (* src -> target tab *) 
  src2target : (target_nat * CoIdent.t) SrcIdentMap.t ;
  mtrace : CoIdent.t;

  (* global pack info, should be in some global struct shared by several t *)
  gxlist : Syntaxe.except_info list;
}
and  tbl = {
  arg_opt: MainArg.t;
  expanded_code: t;
  (* translation CoIdent -> Exp.var is done once *)
  in_vars: Exp.var list;
  out_vars: Exp.var list;
  loc_vars: Exp.var list;
  (* (partial) initial values of global vars (ins/outs *)
  init_pres: Value.OfIdent.t;

  id2var_tab: Exp.var Util.StringMap.t;

  (* REQUIRED BY SolveR.solve_formula *)
  (* list of names for outputs ... *)
  out_var_names: string list;
  (* ... formula (and) for bool vars ... *)
  bool_vars_to_gen: Exp.formula;
  (* ... and var list for nums ! *)
  num_vars_to_gen: Exp.var list;
  snt: Solver.t;
}

(**
Trace : L'info contient juste l'expression associée
à la définition.
*)
and trace_info = {
  ti_def_exp : tbl CoTraceExp.t;
  ti_src : CoIdent.src_stack ;
}

(* get a run def *)
let get_run_expanded_code it rid = StringMap.find rid it.runtab

(*
Initialize with global infos
only gxlist for the time being ?
*)
let new_empty_t nme mainid gxlist = {
   nme = nme;
   idspace = CoIdent.new_space ();
   alist = [];
   inlist = [];
   outlist = [];
   locinlist = [];
   locoutlist = [];
   prelist = [];
   stab = StringMap.empty;
   atab = StringMap.empty; 
   ttab = StringMap.empty; 
   etab = StringMap.empty; 
   gxlist = gxlist;
   xtab = StringMap.empty;
   runcpt = 0;
   runtab = StringMap.empty;
   src2target = SrcIdentMap.empty;
   mtrace = mainid;
}

(* gestion des instances de run *)
let add_run_instance: t -> string -> t -> string * t = 
  fun x nnme e ->
    let runcpt = x.runcpt + 1 in
    let inme = Printf.sprintf "%s_%02d" nnme runcpt in
    let x = { x with
              runcpt = runcpt;
              runtab = StringMap.add inme e x.runtab;
            }
    in
    inme, x

let src_decl_to_target_info (zeres:t) id =
  SrcIdentMap.find id zeres.src2target

let src_decl_to_target_id (zeres:t) id =
  snd (SrcIdentMap.find id zeres.src2target)


(* L'utilitaire suivant enchaîne les étapes, 
   À N'UTILISER QUE SI ON EST SUR QUE TOUT EST CORRECT ! *)

let src_ref_to_target_id (zeres:t) (env : CheckEnv.t) (id : Syntaxe.ident) = (
  let info = CheckEnv.get_binding env id in
  let id_decl = CkIdentInfo.def_ident info in
  src_decl_to_target_id zeres id_decl
)

let add_support_target_id (zeres:t) (src: Syntaxe.ident) (tgt: CoIdent.t):t = (
   { zeres with src2target = SrcIdentMap.add src (TN_support, tgt) zeres.src2target }
)

let add_alias_target_id (zeres:t) (src: Syntaxe.ident) (tgt: CoIdent.t):t = (
Verbose.put ~flag:dbg " Add alias %s <- %s\n" (src.it) (CoIdent.to_string tgt);
   { zeres with src2target = SrcIdentMap.add src (TN_alias, tgt) zeres.src2target }
)

let add_trace_target_id (zeres:t) (src: Syntaxe.ident) (tgt: CoIdent.t):t = (
Verbose.put ~flag:dbg " Add trace %s <- %s\n" (src.it) (CoIdent.to_string tgt);
   { zeres with src2target = SrcIdentMap.add src (TN_trace, tgt) zeres.src2target }
)

let add_except_target_id (zeres:t) (src: Syntaxe.ident) (tgt: CoIdent.t):t = (
   { zeres with src2target = SrcIdentMap.add src (TN_except, tgt) zeres.src2target }
)

(* "expansion" des parametres ref :
    on associe au pa_src le tgtid
    qui DOIT ETRE deja associe a l'arg
*)
let set_ref_param (zeres: t) (env : CheckEnv.t) (pa_src: Syntaxe.ident)
   (arg: Syntaxe.val_exp) : t =
  match (arg.it) with     
    IDENT_n id -> (
      let info = CheckEnv.get_binding env id in
      let id_decl = CkIdentInfo.def_ident info in
      let tgtid = match (src_decl_to_target_info zeres id_decl) with
          (TN_support, ti) -> ti | _ -> assert false
      in
      add_support_target_id zeres pa_src tgtid
    )
  | _ -> assert false



(**************************************************************
Table des exceptions 
---------------------------------------------------------------
Ce sont des idents particuliers qui n'apparaîssent
que dans des constructions particulières (catch, raise)
Inutile (voire dangereux ?) de les mélanger avec les
autres idents.
**************************************************************)

let new_global_except (zeres: t)   (ex: Syntaxe.ident) : t =
  (* on garde l'ident comme target *)     
  let tgtid = CoIdent.get ex.it in
  let zeres = {
    zeres with
    xtab = StringMap.add tgtid { ei_src = CoIdent.base_stack () } zeres.xtab }
  in
  let zeres = add_except_target_id zeres ex tgtid in
  zeres
  

let new_local_except (zeres:t) (ex: Syntaxe.ident) (sstack: CoIdent.scope_stack):t =
  (* on crée un ident target ``frais'' *) 
  let tgtid = CoIdent.get_fresh zeres.idspace ex.it in
  let src = CoIdent.get_src_stack ex.src sstack None in
  let zeres = { zeres with
                xtab = StringMap.add tgtid { ei_src = src } zeres.xtab
              }
  in
  add_except_target_id zeres ex tgtid


(**************************************************************
Table des variables support
---------------------------------------------------------------
**************************************************************)

(* Utilitaires : renvoient une CoAlgExp.t adéquate *)
let alg_exp_of_support_ref (zeres:t) tgtid =
  (StringMap.find tgtid zeres.stab).si_ref_exp

(** Déclaration d'une entrée *)
(* input (de node) => on garde l'ident, incontrôlable *)
let new_support_input (zeres: t) (* info optionnelles *) (init: CoAlgExp.t option)
    (i : Syntaxe.ident) (te : CkTypeEff.t) (sstack : CoIdent.scope_stack):t =
  let tgtid = CoIdent.get i.it in
  let _ = try (
    let oldinfo = StringMap.find tgtid zeres.stab in
    let oldlxm = CoIdent.head_of_src_stack oldinfo.si_src in
    let msg = "input ident already used at " ^
              (lexeme_line_col oldlxm)
    in
    raise (Compile_error (i.src, msg))
  ) with Not_found -> ()
  in
  let src = CoIdent.get_src_stack i.src sstack None in
  let zeres = {
    zeres with
    stab =
      StringMap.add tgtid {
        si_ident = tgtid ;
        si_nature = Input ;
        si_type = te;
        si_ref_exp = CoAlgExp.of_support tgtid te false;
        (* a priori pas utilisé *)
        (* si_pre_ref_exp = CoAlgExp.of_pre tgtid te ; *)
        si_pre_ref_exp = None ;

        si_src = src;
        si_default = None;
        si_scope = None;
        si_init  = init;
        si_range = None;
      } zeres.stab;
    (* on garde la liste pour les sortir dans l'ordre *)
    inlist = tgtid::zeres.inlist ;
  }
  in
  add_support_target_id zeres i tgtid
  

(** Déclaration d'une sortie *)
(* output (de node) => on garde l'ident, contrôlable *)
let new_support_output
    (zeres: t)
    (* info optionnelles *)
    (init: CoAlgExp.t option)
    (range_opt: (CoAlgExp.t * CoAlgExp.t) option)
    (i : Syntaxe.ident)
    (te : CkTypeEff.t)
    (sstack : CoIdent.scope_stack) : t =
  (
    let tgtid = CoIdent.get i.it in
    let _ = try (
      let oldinfo = StringMap.find tgtid zeres.stab in
      let oldlxm = CoIdent.head_of_src_stack oldinfo.si_src in
      let msg = "output ident already used at " ^
                (lexeme_line_col oldlxm)
      in
      raise (Compile_error (i.src, msg))
    ) with Not_found -> ()
    in
    let src = get_src_stack i.src sstack None in
    let zeres = {
      zeres with
      stab =
        StringMap.add tgtid {
          si_ident = tgtid ;
          si_nature = Output ;
          si_type = te;
          si_ref_exp = CoAlgExp.of_support tgtid te true;

          (* a priori pas utilisé *)
          (* si_pre_ref_exp = CoAlgExp.of_pre tgtid te ; *)
          si_pre_ref_exp = None ;

          si_src = src;
          si_default = None;
          si_scope = None;
          si_init = init;
          si_range = range_opt;
        }  zeres.stab;
    (* on garde la liste pour les sortir dans l'ordre *)
      outlist = (tgtid) :: zeres.outlist ;
    }
    in
    add_support_target_id zeres i tgtid
  )

(** Déclaration d'une locale *)
(* local (de node) => on garde l'ident, contrôlable *)
let new_support_local (zeres: t) (* in or out ? *) (nat: support_nature)
    (* info optionnelles *) (scope: support_scope option) (init: CoAlgExp.t option)
    (range_opt: (CoAlgExp.t * CoAlgExp.t) option)  (* info générales *)
    (i : Syntaxe.ident)  (te : CkTypeEff.t) (sstack : CoIdent.scope_stack)
  : string * support_scope option * t =
  let isout = (nat = LocalOut) in
  let tgtid = CoIdent.get_fresh zeres.idspace i.it in
  let src = get_src_stack i.src sstack None in
  let zeres = {
    zeres with
    stab =
      StringMap.add tgtid {
        si_ident = tgtid; 
        si_nature = nat; 
        si_type = te;
        (* controlable if LocalOut *)
        si_ref_exp = CoAlgExp.of_support tgtid te isout;

        (* a priori pas utilisé *)
        (* si_pre_ref_exp = CoAlgExp.of_pre tgtid te ; *)
        si_pre_ref_exp = None ;

        si_src = src;
        si_default = None;
        si_scope = scope;
        si_init = init;
        si_range = range_opt;
      } zeres.stab;
    (* on garde la liste pour les sortir dans l'ordre *)
    locoutlist = if isout then (tgtid) :: zeres.locoutlist else zeres.locoutlist;
    locinlist  = if isout then  zeres.locinlist else (tgtid) :: zeres.locinlist;
  }
  in
  (* adds the def to the exist-scope if any *)
  let scope = match scope with
    | None -> None
    | Some ctx ->
      Some ( { ctx with
               sco_escope = CoTraceExp.add_escope ctx.sco_escope (tgtid, init) })
  in
  let zeres = add_support_target_id zeres i tgtid in
  tgtid, scope, zeres
  

(**************************************************************
Table des alias
---------------------------------------------------------------

**************************************************************)

(* Utilitaire, renvoie une CoAlgExp.t adéquate *)
let alg_exp_of_alias_ref zeres tgtid =
  (StringMap.find tgtid zeres.atab).ai_ref_exp


(* Unicité des idents target d'alias *)
let fresh_alias_id zeres s = ( CoIdent.get_fresh zeres.idspace s )

(* creation d'un alias :
   - creation instance->def
   - association source->instance
ATTENTION:
   new_alias enchaîne les deux !
*) 
let create_alias (zeres: t)
    (i : Syntaxe.ident) (tgtid : CoIdent.t ) (edef : CoAlgExp.t) (te : CkTypeEff.t )
    (sstack : CoIdent.scope_stack) : t = 
  Verbose.exe ~flag:dbg (fun () -> Printf.printf " Create alias %s = %s\n   type: %s\n"
                            (CoIdent.to_string tgtid)
                            (CoAlgExp.lus_dumps edef)
                            (CkTypeEff.to_string te));
  assert (CkTypeEff.is_data te);
  let src = get_src_stack i.src sstack None in
  let ctrl = CoAlgExp.is_controlable edef in
  let eref = CoAlgExp.of_alias tgtid te ctrl in
  let zeres = { zeres with
                atab = StringMap.add tgtid {
                    ai_type = te ;
                    ai_def_exp = edef ;
                    ai_ref_exp = eref ;
                    ai_src = src ;
                  } zeres.atab ;
                alist = tgtid::(zeres.alist)
              }
  in
  zeres

let new_alias (zeres: t) (i : Syntaxe.ident) (tgtid : CoIdent.t ) (edef : CoAlgExp.t)
  (te : CkTypeEff.t ) (sstack : CoIdent.scope_stack) : t = 
  let zeres = create_alias zeres i tgtid edef te sstack in
  add_alias_target_id zeres i tgtid


(*------------------------------------------
Treatment of "pre X":
--------------------------------------------
- In all case:
  an ident preX is created

- then, if X does not belong to a local scope:
  preX is aliased to "pre X"
  (N.B. the possible init val is handled by the declaration)

- else, if X belongs to a local scope, but has no init:
  preX is aliased to "pre X"

- else, if X belongs to a local scope, and has init:
  preX is DECLARED as support 
  "preX = init" is added to the support init constraint
  "preX = pre X" is added to the support other constraint

*)
let new_pre_handler (id : CoIdent.t) (id_info : support_info) = 
  let te = id_info.si_type in
  let pre_X = CoAlgExp.of_pre id te in
  pre_X

let alg_exp_of_support_pre_ref (zeres: t) tgtid : CoAlgExp.t * t =
  let zevar = StringMap.find tgtid zeres.stab in
  match zevar.si_pre_ref_exp with
  | Some pe -> pe, zeres
  | None -> (
      (* nouveau pre *)
      let pe = new_pre_handler tgtid zevar in
      let zevar = { zevar with si_pre_ref_exp = Some pe } in
      let zeres = { zeres with stab = StringMap.add tgtid zevar zeres.stab } in
      pe, zeres 
    )

(**************************************************************
Table des traces
---------------------------------------------------------------
**************************************************************)

(* Unicité des idents target de traces *)
let fresh_trace_id zeres s = ( CoIdent.get_fresh zeres.idspace s )
let fixed_trace_id s = ( CoIdent.get s )

(* creation d'une trace :
   - creation instance->def
   - association source->instance
ATTENTION:
   new_trace enchaîne les deux !
*) 
let create_trace  (zeres: t) (i : Syntaxe.ident) (tgtid : CoIdent.t )
    (edef : tbl CoTraceExp.t) (sstack : CoIdent.scope_stack)
  : t =
  let src = get_src_stack i.src sstack None in
  { zeres with
    ttab = StringMap.add tgtid { ti_def_exp = edef; ti_src = src } zeres.ttab
  }

let new_trace (zeres: t)  (i : Syntaxe.ident) (tgtid : CoIdent.t )
        (edef : tbl CoTraceExp.t)  (sstack : CoIdent.scope_stack) :t =
   let zeres = create_trace zeres i tgtid edef sstack in
   add_trace_target_id zeres i tgtid

(**************************************************************
Table des externes (utilisees)
---------------------------------------------------------------
**************************************************************)

let add_used_extern (zeres: t) (info : CkIdentInfo.t) : t =
  let id = (CkIdentInfo.def_ident info) in
  if (StringMap.mem id.it zeres.etab) then zeres
  else (
    match CkIdentInfo.get_nature info with
    (* HERE: because of (new) handling of external libs
       surely something to modify here ??? *)
      External_func (Some leti, _, p) -> (
        let ei = {
          xi_decl = leti;
          xi_prof = p;
          xi_src = id.src
        }
        in
        { zeres with etab = StringMap.add id.it ei zeres.etab }
      )
    | _ -> assert false
  )
  


(**************************************************************
DUMP
---------------------------------------------------------------
Pour debug etc ...
**************************************************************)
let (dump_src_ctx : t -> string) = 
  fun x -> 
    (String.concat "\n"
       (List.map 
          (fun id -> Lexeme.to_string id.src)
          x.gxlist
          ))


let rec dump (x:t) = (
  let dump_extern_info (tgt : string) (xi : extern_info) = (
    printf "extern %s : %s" tgt 
      (CkTypeEff.prof_to_string xi.xi_prof) ;
    printf "\n" ;
  )
  in
  let dump_support_info (tgt : CoIdent.t) (si : support_info) = (
    let ns = match si.si_nature with
      | Input -> "Input"
      | Output -> "Output"
      | LocalIn -> "LocalIn"
      | LocalOut -> "LocalOut"
    in
      printf "%s (%s):\n" (CoIdent.to_string tgt) ns ;
      printf "  type : %s\n" (CkTypeEff.to_string si.si_type);
      printf "  ref  : "; CoAlgExp.dump si.si_ref_exp ;
      printf "\n  pre  : ";
      let _ = match si.si_pre_ref_exp with
        | None   -> printf "(never used)"
        | Some e -> CoAlgExp.dump e
      in
        printf "\n  dflt : ";
        let _ = match si.si_default with
          | None   -> printf "(not defined)"
          | Some e -> CoAlgExp.dump e
        in
          printf "\n  init : ";
          let _ = match si.si_init with
            | None   -> printf "(not defined)"
            | Some e -> CoAlgExp.dump e
          in
            printf "\n  range : ";
            let _ = match si.si_range with
              | None   -> printf "(not defined)"
              | Some (e1,e2) -> (
                  printf "[ ";
                  CoAlgExp.dump e1;
                  printf " ; ";
                  CoAlgExp.dump e2;
                  printf "]";
                )
            in
              printf "\n  src  :\n" ;
              CoIdent.print_src_stack si.si_src ;
              printf "\n" ;
  )
  in
  let dump_alias_info (tgt : CoIdent.t) (ai : alias_info) = (
    printf "%s : %s = "
      (CoIdent.to_string tgt) (CkTypeEff.to_string ai.ai_type);
    CoAlgExp.dump ai.ai_def_exp ;
    printf "\n";
    CoIdent.print_src_stack ai.ai_src
  )
  in
  let dump_trace_info (tgt : CoIdent.t) (ti : trace_info) = (
    printf "%s = " (CoIdent.to_string tgt);
    CoTraceExp.dump ti.ti_def_exp ;
    printf "\n";
    CoIdent.print_src_stack ti.ti_src
  ) 
in
    printf "$$$$$$$$$ INFO FOR %s $$$$$$$$$$\n" x.nme;
    printf "--- EXTERN TABLE of %s\n" x.nme  ;
    StringMap.iter dump_extern_info x.etab;
    printf "--- SUPPORT TABLE of %s\n" x.nme ;
    StringMap.iter dump_support_info x.stab;
    printf "--- ALIAS TABLE of %s\n" x.nme ;
    StringMap.iter dump_alias_info x.atab;
    printf "--- TRACE TABLE of %s\n" x.nme ;
    StringMap.iter dump_trace_info x.ttab;
    let dump_run_info id ex = (
      printf "\n--- RUN TABLE of %s CONTAINS %s\n" x.nme id;
      dump ex
    ) in
      StringMap.iter dump_run_info x.runtab;

)

(**************************************************************
EXPANSION (main)
**************************************************************)

(** Expansion des identificateurs dans une expression algébriques

        On 2 (+1) cas :

  1) variable support ou paramètre formel :

  Les deux cas sont similaires car ils correspondent aux
        "feuilles" (on n'a rien à expanser).
        Dans les deux cas, l'ident ``cible'' a été créé
        un bonne fois pour toute à la déclaration (cf.
        [[let]], [[exist]] etc).
        On n'a rien à faire~: on renvoie l'expression
        "cible" de référence déjà créée.

        2) identificateur défini :

        il s'agit d'un identificateur défini en amont
        par un [[let x = e ...]]. Un tel identificateur
        est expansé  une seule fois, quand on rencontre
        sa première référence. On traite (expanse)
        alors récursivement sa définition, on crée
        une entrée cible (alias ou trace suivant le type).
        Si on rencontre une autre référence, on rend le résultat
        déjà calculé.

        3??) identificateur prédéfini :

        Pour l'instant il n'y a rien de tel dans le langage

*)
let rec treat_exp_ident_ref (zeres: t) (env : CheckEnv.t) (sstack : scope_stack)
    (id : Syntaxe.ident)  : CoAlgExp.t * t
  = (
    (* info associée à id au cours du check *)
    let info = CheckEnv.get_binding env id in
    (* predef ? *)
    if (CkIdentInfo.is_predef info) then (
      (* pas grand chose à faire .. *)
      CoAlgExp.of_const id.it (CkIdentInfo.get_type info), zeres
    ) else (
      (* déjà traité ? *)
      let id_decl = CkIdentInfo.def_ident info in
      try (
        match (src_decl_to_target_info zeres id_decl) with
        | (TN_support, tgtid) -> (
            alg_exp_of_support_ref zeres tgtid, zeres
          )
        | (TN_alias, tgtid) -> (
            alg_exp_of_alias_ref zeres tgtid, zeres
          )
        | (TN_except, tgtid) -> raise (
            Internal_error ("Expand.treat_exp_ident_ref",
                            "unexpected exception ident"^tgtid)
          )
        | (TN_trace, tgtid) -> raise (
            Internal_error ("Expand.treat_exp_ident_ref",
                            "unexpected trace ident"^tgtid)
          )
      ) with Not_found -> (
          (* pas encore vu ... *)
          match (CkIdentInfo.get_nature info) with
            Def_ident li -> (
              (* forcement defini ! *)
              let def = match li.lti_def with
                  Some d -> d
                |  _ -> assert false
              in
              let newscope = get_scope id_decl.src
                  (get_src_stack id.src sstack None) in
              let edef,zeres = treat_exp zeres env newscope def in
              let tgtid = fresh_alias_id zeres id.it in
              let te = (CkIdentInfo.get_type info) in 
              let zeres = new_alias zeres id_decl tgtid edef te sstack in
              alg_exp_of_alias_ref zeres tgtid, zeres
            )
          |  _ -> (assert false)
        )
    )
  )
and
  (* Expansion des identificateurs dans une expression trace

      Le traitement est similaire au cas algébrique, à part que
      les cas "impossibles" ne sont pas les mêmes et qu'il faut
      expliciter un éventuel "cast" bool -> trace

      N.B. la notion de trace "predef" n'existe pas
      pour l'instant, mais on prévoie quand même le cas ...
  *)
  treat_trace_ident_ref 
    (zeres: t)
    (env : CheckEnv.t)
    (sstack : scope_stack)
    (id : Syntaxe.ident) 
    (e:Syntaxe.val_exp) : tbl CoTraceExp.t * t
  = (
    (* info associée à id au cours du check *)
    let info = CheckEnv.get_binding env id in
    Verbose.put ~flag:dbg "treat_trace_ident_ref %s\n" (CkIdentInfo.to_string info);
    (* predef ? *)
    if (CkIdentInfo.is_predef info) then (
      (* rien à faire *)
      assert false
    ) else (
      (* déjà traité ? *)
      let id_decl = CkIdentInfo.def_ident info in
      try (
        match (src_decl_to_target_info zeres id_decl) with
        | (TN_trace, tgtid) -> (
            CoTraceExp.of_ref tgtid, zeres
          )
        | (TN_support, tgtid) -> (
            (* cast *)
            CoTraceExp.of_constraint (alg_exp_of_support_ref zeres tgtid)
              (id.src, get_src_stack id.src sstack (Some e)), zeres
          )
        | (TN_alias, tgtid) -> (
            (* cast *)
            CoTraceExp.of_constraint (alg_exp_of_alias_ref zeres tgtid)
              (id.src, get_src_stack id.src sstack (Some e)), zeres
          )
        | (TN_except, _tgtid) -> (
            (* erreur interne ! on n'a rien à faire là !!! *)
            assert false
          )
      ) with Not_found -> (
          (* pas encore vu ... *)
          match (CkIdentInfo.get_nature info) with
          | Def_ident li -> (
              (* Forcement defini ! *)
              let def = match li.lti_def with
                  Some d -> d
                |  _ -> assert false
              in
              let newscope = get_scope id_decl.src
                  (get_src_stack id.src sstack (Some e)) in
              let edef, zeres = treat_trace zeres env newscope def in
              let tgtid = fresh_trace_id zeres id.it in
              let zeres = new_trace zeres id_decl tgtid edef sstack in
              CoTraceExp.of_ref tgtid, zeres
            )
          | _ -> (
              Printf.fprintf stderr
                "unexpected ident:\n %s\n" (CkIdentInfo.to_string info);   
              assert false
            )
        )
    )
  )
(* Expansion des appels  (Généralités)
    ------------------------------------------------------

    Le traitement est assez similaire, que l'on soit en train
    d'expanser une trace ou une expression algébrique.

    Dans les deux cas, on expanse chaque instance, sans chercher
    à tenir compte d'une éventuelle équivalence
    (c'est donc différent du traitement des identificateurs
    simples qui sont expansés une seule fois).

    Le principe est le suivant, on est sur une instance de la forme :
    idref (e1, e2, e3, ...)
    On retrouve (via CheckEnv) que cette instance est liée :
    - à un opérateur prédéfini, auquel ca on ne fait rien,
    - à une macro définie de la forme : iddef (x1, x2, x3, ...) = e

    On commence par traiter les arguments~; pour chaque xi :
    - on expanse [[ei]] dans l'environnement courant : [[expand-ei]],
    - on crée un ident target uniq-xi (trace/alias),
    - on associe [[expand-ei]] à [[uniq-xi]] dans la table
    correspondante (trace/alias),

    Puis on expanse [[e]]~:
    - dans un nouvel environnement ou chaque ident (de déclaration)
    [[xi]] est associé à l'ident cible [[uniq-xi]] correspondant,
    - on expanse [[e]],
    - on crée un ident target [[uniq-idref]], qu'on associe
    à [[expand-e]] dans la table correspondante (trace/alias),
    - on renvoie un référence à [[uniq-idref]] (trace ou alias suivant le cas).

*)
and treat_exp_call
    (zeres: t)
    (env : CheckEnv.t)
    (_e : Syntaxe.val_exp) (* only for retrieving the right type *)
    (sstack : scope_stack)
    (id : Syntaxe.ident) 
    (args : Syntaxe.val_exp list) 
    (e : Syntaxe.val_exp) : CoAlgExp.t * t
  = (
    (* Expansion des appels  (cas des expression algébriques)
        On n'a pas de soucis de cast implicite bool/trace : on
        est assuré de ne manipuler que des expressions algébriques.
    *)
    (* info associée à id au cours du check *)
    let info = CheckEnv.get_binding env id in
    Verbose.put ~flag:dbg "treat_exp_call %s (%s)\n" (Lexeme.to_string id.src)
      (CkIdentInfo.to_string info);
    (* predef/extern => on n'a rien à expanser !
       - les extern sont mises dans la table
       - les arguments d'externes doivent être non-contrôlables     
       - TODO: les arguments de modulo, div devrait etre jeté ici aussi !
    *)
    if (CkIdentInfo.is_predef info) then (
      let aelist, zeres = List.fold_left
          (fun (l,zeres) exp ->
             let x,zeres = treat_exp zeres env sstack exp in
             x::l, zeres
          )
          ([],zeres)
          args
      in
      let aelist = List.rev aelist in
      (CoAlgExp.of_call
         (CoIdent.from_string id.it) (CheckEnv.get_exp_type env e) aelist),
      zeres
    )
    else if (CkIdentInfo.is_extern info) then (
      let aelist,zeres = List.fold_left
          (fun (l,zeres) e ->
             let x, zeres = treat_exp zeres env sstack e in
             x::l,zeres
          )
          ([],zeres)
          args
      in
      Verbose.put ~flag:dbg "Expand.treat_exp_call: external func \"%s\"\n" info.ii_name;
      let zeres = add_used_extern zeres info in
      (* non contrôlable ? *)
      let check_unctl esrc eexp = 
        if (CoAlgExp.is_controlable eexp) then (
          Verbose.exe ~flag:dbg (fun () -> CoAlgExp.dumpf stderr eexp);
          raise ( Compile_error (esrc.src,
                                 "extern. func. arguments must be uncontrollable"))
        );
      in
      List.iter2 check_unctl args aelist;
      let ii = CheckEnv.get_ident_info env id in 
      match ii.ii_nature with
      | External_func (_, Some ei, prof) -> 
        CoAlgExp.of_external_call
          (CoIdent.from_string id.it)
          ei
          prof
          (CheckEnv.get_exp_type env e)
          aelist,
        zeres
      | External_func (_, None, _prof) ->
        raise ( Compile_error (
            id.src,
            "don't know where to find external function, use -L option"
          ))
      | _ -> assert false
    ) else (
      (* ident de déclaration : utile ? *)
      let iddecl = CkIdentInfo.def_ident info in
      (* info est une Macro_ident avec forcement 
         une def (le cas extern est deja traite) *)
      let (params, def) = (
        match (CkIdentInfo.get_nature info) with
        | Macro_ident (Some li, _) -> (
            match li.lti_inputs with
            | Some tilst -> (
                match li.lti_def with
                |  Some d ->
                  (* (List.map fst tilst, d) *)
                  (tilst, d)
                | None -> (assert false)
              )
            | _ -> (assert false)
          ) | _ -> assert false   
      ) in 
      (* Il y a forcement une def !
         l'instance de call induit
         un nouveau "scope" dans lequel on va pouvoir
         situer les références aux paramètres formels,
         sinon, similaire a un appel predef 
      *)
      let newscope = get_scope iddecl.src (get_src_stack id.src sstack  (Some e)) in
        (*
           Passage par valeur :
           on crée/associe les alias correspondants,
           pour chaque couple (pa_decl, arg) :
           - on expanse pa_arg -> pa_val
           - on crée un ident cible pa_target
           - on crée une définition d'alias
           N.B. ON POSTPONE l'association d'alias 
               pour ne pas interférer avec l'évaluation
               des param suivants ! 
           pa_target = pa_val
           Passage par reference :
           la déclaration du param est DIRECTEMENT liée au tgtid passé en parametre
         *)
      let postponed_aliases = ref [] in
      let treat_param zeres (par : Syntaxe.ident * Syntaxe.type_exp)
          (arg : Syntaxe.val_exp) = (
        let pa_decl = fst par in
        Verbose.put ~flag:dbg "  treat_param %s\n" pa_decl.it;
        let pa_type = snd par in
        let te = CkTypeEff.of_texp pa_type in
        if (CkTypeEff.is_ref te) then (
          set_ref_param zeres env pa_decl arg
        ) else (
          (* passage par valeur *)
          let pa_val, zeres = treat_exp zeres env sstack arg in
          let pa_target = fresh_alias_id zeres pa_decl.it in    
          postponed_aliases := (pa_decl,pa_target)::!postponed_aliases;
          create_alias zeres pa_decl pa_target pa_val te newscope 
        )
      )
      in
      let zeres = List.fold_left2 treat_param zeres params args in
      (* on traite les alias postponé *)
      let zeres =
        List.fold_left
          (fun zeres (s,t) -> add_alias_target_id zeres s t)
          zeres
          !postponed_aliases
      in
      (* on expanse l'expression de id dans ce nouveau contexte *)
      let edef, zeres = treat_exp zeres env newscope def in
      let tgtid = fresh_alias_id zeres id.it in
      let te = (CkIdentInfo.get_type info) in
      let zeres = new_alias zeres id tgtid edef te sstack in
      alg_exp_of_alias_ref zeres tgtid, zeres
    )
  )
and
  (* Expansion des appels  (cas des expression de trace)

      Le cas est très similaire à celui des algébriques, si ce n'est
      qu'on doit faire attention au cast implicite entre bool et trace :

      - si le call lui-même est un bool, on passe par [[treat_exp_call]]
      et on fait un cast explicite du résultat. On n'a pas besoin (?) de
      se soucier du type
      des arguments, vu qu'une trace ne peut pas participer au calcul
      d'une valeur algébrique !

      - si le call est une trace, il faut encore passer en revue le type
      DÉCLARÉ de chaque paramètre pour lui associer la bonne information
      (alias ou trace).

      En fait, comme on sait que le programme est correct, on se contente
      de se poser la question du cast :
      - pour chaque param: suivant son type on l'expanse comme une trace
      ou une expression algébrique, et on met le résultat dans la table
      des traces ou des alias,
      - pour le résultat : pareil, suivant son type on l'expanse comme une
      trace ou une exp. alg., on le met dans la bonne table, et finalement
      on caste en trace si ce n'est en est pas une.

      REMARQUE : il y a beaucoup de duplication entre les deux cas,
      en particulier toute la partie traitement des arguments
      pourrait être partagée~; en effet le cas "exp" n'est qu'un
      cas simplifié du cas "trace"
  *)
  treat_trace_call
    (zeres: t)
    (env : CheckEnv.t)
    (_e : Syntaxe.val_exp) (* only for retrieving the right type *)
    (sstack : scope_stack)
    (id : Syntaxe.ident) 
    (args : Syntaxe.val_exp list) 
    (e : Syntaxe.val_exp) : tbl CoTraceExp.t * t
  = (
    (* info associée à id au cours du check *)
    let info = CheckEnv.get_binding env id in
    Verbose.put ~flag:dbg "treat_trace_call %s (%s)\n"
      (Lexeme.to_string id.src) (CkIdentInfo.to_string info);
    (* predef/extern => c'est forcement un cast ! *)
    if ( (CkIdentInfo.is_predef info) || (CkIdentInfo.is_extern info)) then (
      let aelist,zeres =
        List.fold_left (fun (aelist,zeres) e ->
            let x, zeres = treat_exp zeres env sstack e in
            x::aelist, zeres
          )
          ([],zeres)
          args
      in
      let aelist = List.rev aelist in (* necessary? *)
      let zeres = if (CkIdentInfo.is_extern info) then
          let zeres = add_used_extern zeres info in
          (* non contrôlable ? *)
          let check_unctl esrc eexp =
            if (CoAlgExp.is_controlable eexp) then (
              raise ( Compile_error (esrc.src,
                                     "extern. func. arguments must be uncontrollable"))
            );
          in
          List.iter2 check_unctl args aelist;
          zeres
        else zeres
      in
      let ae = CoAlgExp.of_call
          (CoIdent.from_string id.it)
          (CheckEnv.get_exp_type env e)
          (* (CkIdentInfo.get_type info) *)
          aelist
      in
      CoTraceExp.of_constraint ae (id.src, get_src_stack id.src sstack (Some e)), zeres
    ) else (
      (* ident de déclaration : utile ? *)
      let iddecl = CkIdentInfo.def_ident info in
      (* info est une Macro_ident avec forcement 
         une def (le cas extern est deja traite) *)
      let (params, def) = (
        match (CkIdentInfo.get_nature info) with
        | Macro_ident (Some li, _) -> (
            match li.lti_inputs with
            | Some tilst -> (
                match li.lti_def with
                | Some d -> (tilst, d)
                (* (List.map fst tilst, d) *)
                | None -> (assert false)
              )
            | _ -> (assert false)
          ) | _ -> assert false   
      ) in 
      (* l'instance de call induit un nouveau "scope"
         dans lequel on va pouvoir situer les références
         aux paramètres formels
      *)
      let newscope = get_scope iddecl.src (get_src_stack id.src sstack (Some e)) in
      (* on crée/associe les alias correspondants,
         pour chaque couple (pa_decl, arg) :
         - on expanse pa_arg -> pa_val (suivant le type)
         - on crée un ident cible pa_target
         - on crée une définition (alias ou trace)
           NB. on postpone l'insertion (alias ou trace)
           pa_target = pa_val
      *)
      let postponed_aliases = ref [] in
      let postponed_traces = ref [] in
      let treat_param zeres
          (pa : Syntaxe.ident * Syntaxe.type_exp)
          (arg : Syntaxe.val_exp) : t
        = (
          let pa_decl = fst pa in
          let pa_type = snd pa in
          let te = CkTypeEff.of_texp pa_type in
          if (CkTypeEff.is_ref te) then (
            (* c'est forcement un data ... *)
            set_ref_param zeres env pa_decl arg
          ) else (
            (* trace ou data ... *)
            if (te = CkTypeEff.trace) then (
              let pa_val, zeres = treat_trace zeres env sstack arg in
              let pa_target = fresh_trace_id zeres pa_decl.it in
              postponed_traces := (pa_decl,pa_target)::!postponed_traces;
              let zeres = new_trace zeres pa_decl pa_target pa_val newscope in
              zeres
            ) else (
              let pa_val,zeres = treat_exp zeres env sstack arg in
              let pa_target = fresh_alias_id zeres pa_decl.it in
              postponed_aliases := (pa_decl,pa_target)::!postponed_aliases;
              create_alias zeres pa_decl pa_target pa_val te newscope 
            )
          )
        ) 
      in
      let zeres = List.fold_left2 treat_param zeres params args in
      (* on traite les alias/traces postponés *)
      let zeres =
        List.fold_left
          (fun zeres (s,t) -> add_alias_target_id zeres s t)
          zeres
          !postponed_aliases
      in
      let zeres =
        List.fold_left (fun zeres (s,t) -> add_trace_target_id zeres s t)
          zeres
          !postponed_traces
      in
      (* on expanse l'expression de id dans ce nouveau contexte *)
      let te = (CkIdentInfo.get_type info) in
      if (te =  CkTypeEff.trace) then (
        let edef,zeres = treat_trace zeres env newscope def in
        let tgtid = fresh_trace_id zeres id.it in
        let zeres = new_trace zeres id tgtid edef
            (e.src, get_src_stack e.src sstack (Some e))
        in
        CoTraceExp.of_ref tgtid, zeres
      ) else (
        let edef,zeres = treat_exp zeres env newscope def in
        let tgtid = fresh_alias_id zeres id.it in
        let zeres = new_alias zeres id tgtid edef te sstack in
        CoTraceExp.of_constraint(alg_exp_of_alias_ref zeres tgtid) newscope, zeres
      )
    )
  )
and
  (* Expansion des expressions algébrique (cas général)

      C'est essentiellement une fonction val_exp -> CoAlgExp.t
      Les autres paramètres sont:
      - env : pour récupérer les infos relatives aux idents source,
      - sstack : la "pile" des infos source pour localiser
      la trace en cours de traitement (messages d'erreur).

      Le typage ayant réussi, on est sur de ne pas avoir affaire
      à une expression de trace.
  *)
  treat_exp (zeres: t) (env : CheckEnv.t) (sstack : scope_stack) (e : Syntaxe.val_exp)
  : CoAlgExp.t * t
  = (
    match e.it with   
    (* constantes *)
    | TRUE_n -> CoAlgExp.of_true, zeres
    | FALSE_n -> CoAlgExp.of_false, zeres
             (*
               |  ICONST_n id -> (CoAlgExp.of_iconst id)
               |  RCONST_n id -> (CoAlgExp.of_rconst id)
             *)
    | ICONST_n id -> (CoAlgExp.of_iconst id.it), zeres
    | RCONST_n id -> (CoAlgExp.of_rconst id.it), zeres
    (* pre => forcément un support
    *)
    | PRE_n id -> (
        let info = CheckEnv.get_binding env id in
        let id_decl = CkIdentInfo.def_ident info in
        let tgtid = src_decl_to_target_id zeres id_decl in 
        alg_exp_of_support_pre_ref zeres tgtid
      )
    (* ident/pre *)
    | IDENT_n id -> (
        treat_exp_ident_ref zeres env sstack id
      )
    (* nouvelle macro :
       rien à faire, on expanse "à la demande"
    *)
    | LET_n (_, e) -> (
        treat_exp zeres env sstack e
      )
    (* opérateur *)
    |  CALL_n (id, args) -> (
        treat_exp_call zeres env e sstack id args e
      )
    (* traces => impossible *)
    |  EXIST_n _ 
    |  RUN_n  _ 
    |  ERUN_n  _ 
    |  EXCEPT_n _ 
    |  FBY_n _
    |  CHOICE_n _
    |  PRIO_n _
    |  PARA_n _
    |  LOOP_n _
    |  LOOPI_n _
    |  LOOPA_n _
    |  RAISE_n _
    |  TRY_n _
    |  CATCH_n _
    |  TRAP_n _
    |  ASSERT_n _ -> raise (
        Internal_error ("Compile.treat_exp","unexpected case")
      ) 
  )
and
  (* Expansion des expressions de trace (cas général)

      C'est essentiellement une fonction val_exp -> CoTraceExp.t
      Les autres paramètres sont :
      - env : pour récupérer les infos relatives aux idents source,
      - sstack : la "pile" des infos source pour localiser
      la trace en cours de traitement (messages d'erreur).

  *)
  (* utile : évalue l'éventuelle valeur initiale dans
     une déclaration d'ident
  *)
  eval_init_in_var_decl
    (zeres: t)
    (env : CheckEnv.t)
    (sstack : scope_stack) 
    (i, t, vopt, range_opt) =
  (
    let range_exp, zeres =
      (match range_opt with
       | None -> None, zeres
       | Some (low, high) -> 
         let low_exp,zeres  = treat_exp zeres env sstack low in
         let high_exp,zeres = treat_exp zeres env sstack high in

         if (CoAlgExp.is_controlable low_exp) then (
           raise ( Compile_error (i.src, "range values should not be controllable"))
         ) else 
         if (CoAlgExp.is_controlable high_exp) then (
           raise ( Compile_error (i.src, "range values should not be controllable"))
         ) else Some(low_exp, high_exp), zeres
      )
    in
    let iexp, zeres = match vopt with
      | None -> (
          None, zeres
        )
      | Some e -> ( 
          let res, zeres = treat_exp zeres env sstack e in
            (*
              Verbose.exe ~level:2 (fun () ->
              Verbose.put " oui : " ;
              SyntaxeDump.set_err ();
              SyntaxeDump.dump_exp e;
              Verbose.put  " -> " ;
              CoAlgExp.dumpf stderr res;
              Verbose.put  "\n" ;
              );
            *)
          if (CoAlgExp.is_controlable res) then (
            raise ( Compile_error (i.src, "init values should not be controllable"))
          ) else Some res, zeres
        )
    in
    let te = CkTypeEff.of_texp t in
    (i, te, iexp, range_exp), zeres
  )
and
  (* similar to eval_init_in_var_decl except that :
     - no range
     - types are given as a tuple
  *)
  eval_init_in_run_decl
    (zeres: t)
    (env : CheckEnv.t)
    (sstack : scope_stack) 
    (i, _topt, vopt)
    te
  =
  let iexp, zeres = match vopt with
    | None -> None, zeres
    | Some e -> (
        let res, zeres = treat_exp zeres env sstack e in
            (*
              Verbose.exe ~level:2 (fun () ->
              Verbose.put " oui : " ;
              SyntaxeDump.set_err ();
              SyntaxeDump.dump_exp e;
              Verbose.put  " -> " ;
              CoAlgExp.dumpf stderr res;
              Verbose.put  "\n" ;
              );
            *)
        if (CoAlgExp.is_controlable res) then (
          raise ( Compile_error (i.src, "init values should not be controllable"))
        ) else Some res, zeres
      )
  in
  (i, te, iexp, None), zeres
and
  treat_trace (zeres: t) (env : CheckEnv.t) (sstack : scope_stack) (e : Syntaxe.val_exp)
  : tbl CoTraceExp.t * t
  = (
    match e.it with   
    (* certainement cast implicite bool -> trace *)
    |  TRUE_n
    |  FALSE_n
    |  PRE_n _ -> (
        (* FAUX POUR CE QUI EST DU CALL !!!! *)
        let ae,zeres = treat_exp zeres env sstack e in
        CoTraceExp.of_constraint ae (e.src, get_src_stack e.src sstack (Some e)), zeres
      )
    (* impossible après le type-checking ! *)
    |  ICONST_n _id
    |  RCONST_n _id -> 
      raise (Internal_error ("Compile.treat_trace", "trace exp expected"))
    (* nouvelle macro *)
    |  LET_n (_, e) -> (
        (* Traitement des LET_n :
           Les [[let x = ...]], sans paramètres (à ne pas confondre avec les
           [[let x () = ...]]) sont expansés une seule fois,
           en un alias (ou une trace) unique, valable pour toutes
           les références à [[x]].

           Le traitement n'est pas éffectué ici, mais à la première
           utilisation. De cette manière, on ne fait pas de différence
           avec les [[let x = ...]] définis en dehors du noeud principal.

           En bref, sur un "let x" on n'a rien de spécial à faire !
        *)
        treat_trace zeres env sstack e
      )
    (* ident/call => dépend du type *)
    | IDENT_n id -> (
        treat_trace_ident_ref zeres env sstack id e
      )
    |  CALL_n (id, args) -> (
        treat_trace_call zeres env e sstack id args e
      )
    (* Nouveau support *)
    |  EXIST_n (idlst,ee) -> (
        (* Traitement des EXIST_n :
            Pour chaque identificateur de [[idlst]], on crée
            un target ident unique qu'on insère dans la table support,
            puis on appele recursivement le traitement sur [[ee]]
        *)
        (* on évalue D'ABORD les vopt dans env
           (voir eval_init_in_var_decl)
        *)
        let newidlist, zeres =
          List.fold_left (fun (l,zeres) a ->
              let x,zeres = eval_init_in_var_decl zeres env sstack a in
              x::l,zeres
            )
            ([], zeres)
            idlst
        in
        let newidlist = List.rev newidlist in
        let src_scope = CoIdent.get_src_stack e.src sstack (Some e) in 

        (* The exist-scope is created EMPTY ... *)
        let zescope = new_support_scope src_scope in
        (* ... then filled by added var one by one *)
        let f (zescope_opt, zeres) (i, t, ivopt, range_opt) =
          let (_, zescope_opt, zeres) =
            new_support_local zeres LocalOut zescope_opt ivopt range_opt i t sstack
          in
          (zescope_opt, zeres)
        in
        let (zescope_opt, zeres) = List.fold_left f (Some zescope, zeres) newidlist in
        let zescope = match zescope_opt with Some sc -> sc | None -> assert false in
        (* on continu dans ce nouvel env ... *)
        let res, zeres = treat_trace zeres env sstack ee in
        CoTraceExp.of_exist zescope.sco_escope res, zeres
      )
    |  ERUN_n (varlist, edef, e1) -> (
        (* edef doit etre un node call (pour l'instant !) *)
        match edef.it with
        | CALL_n (id, elst) -> (
            (* doit être un node ... *)
            match (CheckEnv.nature_of_ident env id) with
            | Node_ident (Some ni, _prof) -> (
                (* COMPILE le noued XXX *)
                let recexpand = treat_node env ni zeres.gxlist in
                let instanceid, zeres = add_run_instance zeres id.it recexpand in

                (* EVALUE LES ARGUMENTS du run dans l'env courant *)
                let eval_arg (l,zeres) e =
                  let ce,zeres = treat_exp zeres env sstack e in
                  if (CoAlgExp.is_controlable ce) then (
                    raise (Compile_error (e.src, "erun args sould not be controllable"))
                  );
                  ce::l,
                  zeres 
                in
                let args,zeres = List.fold_left eval_arg ([],zeres) elst in
                let args = List.rev args in (* necessary? *)

                (* EVALUE les eventuels init des resultat  dans l'env courant *)
                let outtypes =
                  CkTypeEff.tuple_to_data_list (CheckEnv.get_exp_type env edef)
                in
                (* on évalue D'ABORD les vopt dans env (voir eval_init_in_var_decl) *)
                let newvarlist, zeres =
                  List.fold_left2 (fun (l,zeres) v o ->
                      let x,zeres = eval_init_in_run_decl zeres env sstack v o in
                      x::l, zeres
                    )
                    ([],zeres)
                    varlist outtypes
                in
                let newvarlist = List.rev newvarlist in
                let src_scope = CoIdent.get_src_stack e.src sstack (Some e) in 

                (* CREE un nouvel env *)
                (* The run-scope is created EMPTY ... *)
                let zescope = new_support_scope src_scope in
                (* ... then filled by added var one by one *)
                let f (zeres,zescope_opt) (i, t, ivopt, range_opt) =
                  let  (_,zescope_opt,zeres) =                    
                    new_support_local zeres LocalIn zescope_opt ivopt
                      range_opt i t sstack
                  in
                  zeres,zescope_opt
                in
                let zeres,zescope_opt =
                  List.fold_left f (zeres,Some zescope) newvarlist
                in
                let zescope = match zescope_opt with
                    Some sc -> sc | None -> assert false
                in
                (* on continu dans ce nouvel env ... *)
                let res, zeres = treat_trace zeres env sstack e1 in
                CoTraceExp.of_erun instanceid zescope.sco_escope args res , zeres

              )
            (* ... ou une fonction externe 
               | External_func (lio, eio, prof) -> (
               let tel = rec_list_call elst in
               [ match_type_profile tel prof e.src ]
               )
            *)
            | _ -> (
                raise (Compile_error (e.src,
                                      "identifier "^id.it^" cannot be used in run statement"))
              )
          )
        | _ -> raise (Compile_error
                        (edef.src, "only node calls are supported in run statement"))
        (* traces *)
      )
    |  RUN_n (idlist, edef, e1opt) -> (
        (* edef doit etre un node call (pour l'instant !) *)
        match edef.it with
        | CALL_n (id, elst) -> (
            (* doit être un node ... *)
            match (CheckEnv.nature_of_ident env id) with
            | Node_ident (Some ni, _prof) -> (
                (* COMPILE le noued *)
                let recexpand = treat_node env ni zeres.gxlist in
                let instanceid,zeres = add_run_instance zeres id.it recexpand in

                (* EVALUE LES ARGUMENTS du run dans l'env courant *)
                let eval_arg (cel,zeres) e =
                  let ce,zeres = treat_exp zeres env sstack e in
                  if (CoAlgExp.is_controlable ce) then (
                    raise ( Compile_error (e.src, "run args sould not be controllable"))
                  );
                  ce::cel, zeres
                in
                let args,zeres = List.fold_left eval_arg ([],zeres) elst in
                let args = List.rev args in (* necessary? *)
                (* on se resert de ce qui a ete fait pour ERUN,
                   pour chaque id de idlist
                   - on verifie qu'il correspond a un CTRL dans l'env courant
                   - on cree un scope ou l'ident devient un LocalIn
                *)
                let src_scope = CoIdent.get_src_stack e.src sstack (Some e) in 
                let zescope = new_support_scope src_scope in
                let treat_res_id (l, zeres, zescope) id = (
                  let info = CheckEnv.get_binding env id in
                  let id_decl = CkIdentInfo.def_ident info in
                  let res, zeres, zescope =
                    match (src_decl_to_target_info zeres id_decl) with
                    | (TN_support, tgtid) -> (
                        let curbinding = StringMap.find tgtid zeres.stab in
                        if (not (CoAlgExp.is_controlable curbinding.si_ref_exp)) then (
                          raise (Compile_error(id.src,"run result must be controllable"))
                        ) ;
                        (* ICI : l'expression d'init est le pre de tgtid *)
                        let init,zeres = alg_exp_of_support_pre_ref zeres tgtid in
                        let tgtid', zescope, zeres =
                          new_support_local zeres LocalIn zescope
                            (* curbinding.si_init curbinding.si_range
                               id curbinding.si_type sstack; *)
                            (Some init) None id curbinding.si_type sstack
                        in
                        let newbinding = StringMap.find tgtid' zeres.stab in
                        CoAlgExp.of_eq curbinding.si_ref_exp newbinding.si_ref_exp,
                        zeres,
                        zescope
                      )
                    | _ -> assert false
                  in
                  res::l, zeres, zescope
                )
                in
                (* l'expression AND(xi = xi') est construite une fois pour toute *)
                let zeconstraints, zeres, zescope =
                  (List.fold_left treat_res_id ([],zeres, Some zescope) idlist)
                in 
                let zeconstraint = CoAlgExp.of_big_and zeconstraints in
                let res,zeres = match e1opt with
                  | Some e1 ->   
                    (* on continu dans ce nouvel env ... *)
                    treat_trace zeres env sstack e1
                  | None ->
                    (* ICI : pas efficace, faire un TE_run avec in optionnel *)
                    CoTraceExp.of_loop (CoTraceExp.of_constraint (CoAlgExp.of_true) 
                                          (sstack)), zeres
                in
                let zescope = match zescope with Some x -> x | None -> assert false in
                CoTraceExp.of_run instanceid zeconstraint zescope.sco_escope args res  
                  (edef.src, get_src_stack e.src sstack (Some e)), zeres
              )
            (* ... ou une fonction externe 
               | External_func (lio, eio, prof) -> (
               let tel = rec_list_call elst in
               [ match_type_profile tel prof e.src ]
               )
            *)
            | _ -> (
                raise (Compile_error (
                    e.src, "identifier "^id.it^" cannot be used in run statement"))
              )
          )
        | _ -> raise (Compile_error
                        (edef.src, "only node calls are supported in run statement"))
        (* traces *)
      )
    | FBY_n (e1,e2) -> (
        let arg1, zeres = treat_trace zeres env sstack e1 in
        let arg2, zeres = treat_trace zeres env sstack e2 in
        CoTraceExp.of_fby arg1 arg2, zeres      
      )
    (* liste de priorité, rien de spécial ... *)
    | PRIO_n elst -> (
        let args, zeres =
          List.fold_left (fun (l,zeres) trace ->
              let x,zeres = treat_trace zeres env sstack trace in
              x::l,zeres
            )
            ([], zeres)
            elst
        in
        let args = List.rev args in
        CoTraceExp.of_prio args, zeres
      )       
    (* idem pour parallèle ... *)
    | PARA_n elst -> (
        let args, zeres = List.fold_left (fun (l,zeres) trace ->
            let x,zeres = treat_trace zeres env sstack trace in
            x::l,zeres
          )
            ([],zeres)
            elst
        in
        let args = List.rev args in
        CoTraceExp.of_para args, zeres
      )       
    (* choix => il faut vérifier la controlabilité des poids *)
    |  CHOICE_n chargs -> (
        (* chargs : (val_exp * val_exp srcflaged option) list *)
        let doit (l,zeres) (et, ewo) =
          let tres, zeres = treat_trace zeres env sstack et in
          let wres = match ewo with
              None -> None
            | Some ew -> (
                let wval,_zeres = treat_exp zeres env sstack ew.it in
                if (CoAlgExp.is_controlable wval) then (
                  raise ( Compile_error (ew.src, "weights sould not be controllable"))
                ) else Some wval
              )
          in
          (tres, wres)::l, zeres
        in
        let choices, zeres = List.fold_left doit ([], zeres) chargs in
        let choices = List.rev choices in
        CoTraceExp.of_choice choices, zeres
      )
    | LOOP_n (f,ee) -> (
        let arg, zeres = treat_trace zeres env sstack ee in
        match f with
        | Weak -> CoTraceExp.of_loop arg, zeres
        | Strong -> CoTraceExp.of_omega arg, zeres
      )
    | LOOPI_n (emin,emax,ee) -> (
        let minres,zeres = treat_exp zeres env sstack emin in
        let maxres,zeres = treat_exp zeres env sstack emax in
        let eres,zeres   = treat_trace zeres env sstack ee in
        if (CoAlgExp.is_controlable minres) then (
          raise ( Compile_error (emin.src, "loop params sould not be controllable"))
        ) else if (CoAlgExp.is_controlable maxres) then (
          raise ( Compile_error (emax.src, "loop params sould not be controllable"))
        ) else (
          CoTraceExp.of_loopi minres maxres eres 
            (ee.src, get_src_stack ee.src sstack (Some e)), zeres
        )
      )
    |  LOOPA_n (emoy,Some eect,ee) -> (
        let moyres,zeres = treat_exp zeres env sstack emoy in
        let ectres,zeres = treat_exp zeres env sstack eect in
        let eres,zeres   = treat_trace zeres env sstack ee in
        if (CoAlgExp.is_controlable moyres) then (
          raise ( Compile_error (emoy.src, "loop params sould not be controllable"))
        ) else if (CoAlgExp.is_controlable ectres) then (
          raise ( Compile_error (eect.src, "loop params sould not be controllable"))
        ) else (
          CoTraceExp.of_loopa  moyres (Some ectres) eres 
            (ee.src, get_src_stack ee.src sstack (Some e)), zeres
        )
      )
    |  LOOPA_n (emoy,None,ee) -> (
        let moyres,zeres  = treat_exp zeres env sstack emoy in
        let eres,zeres  = treat_trace zeres env sstack ee in
        if (CoAlgExp.is_controlable moyres) then (
          raise ( Compile_error (emoy.src, "loop params sould not be controllable"))
        ) else (
          CoTraceExp.of_loopa  moyres None eres 
            (ee.src, get_src_stack ee.src sstack (Some e)), zeres
        )
      )
    |  ASSERT_n (f,c,ee) -> (
        let cres,zeres  = treat_exp zeres env sstack c in
        let eres,zeres  = treat_trace zeres env sstack ee in
        match f with
        | Weak -> CoTraceExp.of_assert cres eres 
                    (ee.src, get_src_stack ee.src sstack (Some e)), zeres
        | Strong -> CoTraceExp.of_strong_assert cres eres 
                      (ee.src, get_src_stack ee.src sstack (Some e)), zeres
      )
    | RAISE_n id -> (
        (* le programme est correct, DONC tgtid existe !! *)
        let tgtid = src_ref_to_target_id zeres env id in 
        CoTraceExp.of_raise (CoIdent.to_string tgtid) , zeres
      )
    (* Nouvelles exceptions *)
    | EXCEPT_n (exlst,ee) -> (
        (* Traitement des EXCEPTION_n :
            Pour chaque identificateur de [[exlst]], on crée
            un target ident unique qu'on insère dans la table
            des exceptions, puis on appele recursivement
            le traitement sur [[ee]]
        *)
        (* on crée les target ident associés *)
        let f zeres ex = new_local_except zeres ex sstack in
        let zeres = List.fold_left f zeres exlst in
        (* on continu dans ce nouvel env ... *)
        treat_trace zeres env sstack ee
      )
    (* Dans la syntaxe intermédiaire, on n'a que 
       la construction catch
    *)
    | TRY_n (e1, e2opt) -> (
        let r1, zeres = treat_trace zeres env sstack e1 in
        let r2opt, zeres = match e2opt with
            None -> None, zeres 
          | Some e2 ->
            let trace, zeres = treat_trace zeres env sstack e2 in
            Some trace, zeres
        in
        CoTraceExp.of_try r1 r2opt, zeres
      )
    | CATCH_n (id, e1, e2opt) -> (
        (* le programme est correct, DONC tgtid existe !! *)
        let tgtid = src_ref_to_target_id zeres env id in 
        let tgtstr = (CoIdent.to_string tgtid) in
        let r1, zeres = treat_trace zeres env sstack e1 in
        let r2opt, zeres  = match e2opt with
            None -> None, zeres 
          | Some e2 ->
            let trace, zeres = treat_trace zeres env sstack e2 in
            Some trace, zeres
        in
        (* Pour simplifier : on INTERDIT
           en interne le catch du "Deadlock" prédéfini ->
           on le remplace par la forme "try"
        *)
        if (tgtstr = LutPredef.kw_deadlock) then
          CoTraceExp.of_try r1 r2opt, zeres 
        else
          CoTraceExp.of_catch tgtstr r1 r2opt, zeres 
      )
    | TRAP_n (id, e1, e2opt) -> (
        (* la seule difference avec le catch est la portée
           de id : une fois créé le target unique, on peu
           expanser en catch classique 
        *)
        (* on crée le target ident unique de id *)
        let zeres = new_local_except zeres id sstack in
        (* le programme est correct, DONC tgtid existe !! *)
        let tgtid = src_ref_to_target_id zeres env id in 
        let tgtstr = (CoIdent.to_string tgtid) in
        let r1, zeres = treat_trace zeres env sstack e1 in
        let r2opt, zeres = match e2opt with
            None -> None, zeres
          | Some e2 ->
            let trace,zeres = treat_trace zeres env sstack e2 in 
            Some trace, zeres
        in
        (* Pour simplifier : on INTERDIT
           en interne le catch du "Deadlock" prédéfini ->
           on le remplace par la forme "try"
        *)
        if (tgtstr = LutPredef.kw_deadlock) then
          CoTraceExp.of_try r1 r2opt, zeres
        else
          CoTraceExp.of_catch tgtstr r1 r2opt, zeres
      ) 

(*
   |  LOOPE_n (en,ee) -> (
        let nres = treat_exp zeres env sstack en in
        let eres = treat_trace zeres env sstack ee in
          if (CoAlgExp.is_controlable nres) then (
       raise (Compile_error (en.src, "loop params should not be controllable"))
          ) else (
       CoTraceExp.of_loope nres eres
          )
           )
*)
  )
(* 
   2011 march
   Separated from the exported "make" function in order to allow recursive calls.
   The only global info from the package is the list of global except
   N.B. => adding other global defs (e.g. data types) will require
   to BETTER reorganized the infos !!
 **)
and treat_node (env : CheckEnv.t) (ni : Syntaxe.node_info)
    (gxlist: Syntaxe.except_info list) : t = 
  (* create a fresh empty expanded program *)
  (* the main trace name is the name of the node *)
  let n = ni.ndi_ident.it in
  let mainid = (fixed_trace_id n) in
  let zeres = new_empty_t n mainid gxlist in

  (*********************************)
  (* EXCEPTIONS GLOBALES           *)
  (* => on garde leur nom tel quel *)
  (*********************************)
  let zeres = List.fold_left new_global_except zeres gxlist in

  (*******************************)
  (* NOEUD PRINCIPAL             *)
  (*******************************)
  let nodelxm = ni.ndi_ident.src in
  let sstack = CoIdent.main_scope nodelxm in
  (*******************************)
  (* INPUTS ET OUTPUTS           *)
  (*******************************)
  (* pour les i/o, on a CoIdent.t = Syntaxe.ident *)
  (* évalue d'abord les init *)

  let f (l,zeres) a =
    let x,zeres = eval_init_in_var_decl zeres env sstack a in
    x::l,zeres
  in
  let inlist, zeres = List.fold_left f ([], zeres) ni.ndi_inputs in
  let inlist = List.rev inlist in
  let add_input zeres (i, t, vopt, _range_opt) =
    new_support_input zeres vopt i t sstack
  in
  let zeres = List.fold_left add_input zeres inlist in
  let outlist, zeres =  List.fold_left f ([], zeres) (ni.ndi_outputs) in
  let outlist = List.rev outlist in
  let add_output zeres (i, t, vopt, range_opt) =
    new_support_output zeres vopt range_opt i t sstack
  in
  let zeres = List.fold_left add_output zeres outlist in
  (***********************************)
  (* MAIN TRACE                      *)
  (***********************************)
  let maintrace, zeres = treat_trace zeres env sstack ni.ndi_def in
  (* on donne à la trace le nom du main node *)
  let zeres = new_trace zeres ni.ndi_ident mainid maintrace sstack in
  let pl = StringMap.fold
      (fun id si acc -> match si.si_pre_ref_exp with
         | None -> acc
         | Some _ -> (id,si)::acc )
      zeres.stab
      []
  in
  (* Final result: need to reverse the lists ... *)
  let zeres = {
    zeres with
    prelist = pl;
    alist = List.rev zeres.alist;
    inlist = List.rev zeres.inlist;
    outlist = List.rev zeres.outlist;
    locoutlist = List.rev zeres.locoutlist;
    locinlist = List.rev zeres.locinlist;
  }
  in
  zeres

(**************************************************************
EXPANSION : procédure principale
---------------------------------------------------------------
Pour faire propre, il serait bon d'éviter les variables globales !
Pour l'instant, on se contente  de les masquer ...
**************************************************************)

let node_name (x:t) = x.nme

(** accès aux résultats *)

let support_pres (x:t) = x.prelist
let support_tab (x:t) = x.stab
let alias_tab (x:t) = x.atab
let alias_list (x:t) = x.alist
let input_list (x:t) = x.inlist
let output_list (x:t) = x.outlist
let local_in_list (x:t) = x.locinlist
let local_out_list (x:t) = x.locoutlist
let trace_tab (x:t) = x.ttab
let extern_tab (x:t) = x.etab
let main_trace (x:t) = x.mtrace

let ident_space (x:t) = x.idspace

let get_trace_info it id = StringMap.find id it.ttab

let make (env : CheckEnv.t) (p : Syntaxe.package) (n : string) : t = (
  let ni =
    try Syntaxe.pack_get_node p n
    with Not_found -> raise (
      Global_error (sprintf "node \"%s\" not found" n)
    )
  in
    treat_node env ni (Syntaxe.pack_except_list p)
)


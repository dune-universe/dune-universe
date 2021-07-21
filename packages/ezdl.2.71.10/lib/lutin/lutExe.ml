(* 

A) Tout ce qu'il faut pour intégrer facilement Lutin à Lurette
--------------------------------------------------------------

- création/initialisation à partir d'un Expand.t
  (ou directement d'un fichier Lutin)
  résultat : le control_state initial

- (1) génération de contraintes satisfiables, avec :
    une valeur des ins * une valeur des pres * un control state
    -> un comportement REALISABLE (goto, vanish, raise programmé)
       + une fonction pour obtenir d'autres comportements satisfiables alternatifs 
         (test épais du contrôle)
    -> plus/pas de comportements

N.B. c'est un peu une liste "lazzy" des comportements possibles

- (2) pour les comportements REALISABLES de type "Goto",
  (i.e. un couple contrainte réalisable * control_state suivant) :
  génération d'une (liste) de solutions concrètes :
  (valeur des sorties, valeur des locales)
N.B. pourrait être lazzy ? ce qui n'est pas le cas pour lucky, où
on demande explicitement "n" solutions.

- pour rappeler (1), un utilitaire qui "merge" (en élagant) :
  valeur des ins * valeur des outs * valeur des locales
  -> valeur des pres 

B) Pour faire un Lutin "réactif" simple
---------------------------------------

C'est juste une version simplifiée où on fournit juste
un step qui fait une réaction complète :
valeur des ins * prg -> valeur des outs * prg suivant

N.B. dans un premier temps, c'est un step simple qu'on
  utilisera pour les appels interne "exist outs = node(ins)",
  On verra plus tard pour la version Lurette récursive !

*)

open LutErrors;;
open CoTraceExp ;;

let dbg = Verbose.get_flag "LutExe"
let dbgrun = Verbose.get_flag "Run"


(* store = current (given) values + past values *)
type store = { curs: Var.env; pres: Var.env }


(* "lucky" out list + loc list  *)
type support = Exp.var list * Exp.var list

(** type t holds several global useful info *)

open Reactive

open Expand
type t = Expand.tbl 

let in_var_list it = it.in_vars
let out_var_list it = it.out_vars
let loc_var_list it = it.loc_vars

let (clear: t -> t) =
  fun t ->
     { t with snt = Bddd.clear t.snt } 

let (_tbl_to_string: t -> string) =
  fun t -> 
    Printf.sprintf "- registers: %s\n- tables: %s\n"
      (Value.OfIdent.to_string ""  t.init_pres)
      (Solver.tbl_to_string t.snt)
    
(* TRANSLATION Lutin -> Lucky

   N.B. necessary to use the Lucky formula solver

   How it works:
   Glue provides a generic lutin exp to lucky exp
   (i.e. CoAlgExp.t to Exp.t) parameterized by:
   - a bool specifying if partial eval must be done
     (i.e. constant propagation)
   - an "ident" solver, wich translate ident into Exp.t
     (maybe mutually recursive with lucky_exp_of !)
 
*)

let (to_event_var:'a Var.t -> RdbgEvent.var) = 
  fun v -> 
    Var.name v, Type.to_data_t (Var.typ v) 


(**
   Basic unalias id2exp to use with Glue.lucky_exp_of
   - Requires a LutExe.t structure
   - aliases are unaliased
   - support vars are kept as it 

=> No longer used ? 
*)
let rec _unalias (it:t) (eval:bool) (idref:CoAlgExp.node) = (
   match idref with
   | CoAlgExp.AE_alias id -> (
      let xenv = it.expanded_code in
      (* let nme = CoIdent.to_string id in *)
      let e = (Util.StringMap.find id (Expand.alias_tab xenv)).Expand.ai_def_exp in
      Glue.lucky_exp_of eval (_unalias it) e
   )
   | CoAlgExp.AE_support id -> (
      Glue.lucky_exp_var_ref (id2var it id)
   )
   | CoAlgExp.AE_pre _id -> assert false
   | _ -> assert false
) 
(*
   Static unalias id2exp to use with Glue.lucky_exp_of
   - Requires just a source program Expant.t
   - aliases are unaliased
   - support vars not supported (error)

=> mainly used for evaluating compile-time constants
   (init, range of in/out) 
*)
and static_unalias (xenv:Expand.t) (eval:bool) (idref:CoAlgExp.node) = (
   match idref with
   | CoAlgExp.AE_alias id -> (
      (* let nme = CoIdent.to_string id in *)
      let e = (Util.StringMap.find id (Expand.alias_tab xenv)).Expand.ai_def_exp in
      Glue.lucky_exp_of eval (static_unalias xenv) e
   )
   | CoAlgExp.AE_support id -> (
      raise (
      Internal_error ("LutExe.static_unalias",
         "unexpected support variable("^(CoIdent.to_string id)^")"
   )))
   | CoAlgExp.AE_pre _id -> assert false
   | _ -> assert false
)
(*
   Create a Var.t and store it in table
=> this table is supposed to become the id2var_tab
   of a LutExe.t
*)
and new_id2var
   (tab:  Exp.var Util.StringMap.t)
   (xenv: Expand.t)
   (id:CoIdent.t) : Exp.var * Exp.var Util.StringMap.t =
(
   let info = Util.StringMap.find id (Expand.support_tab xenv) in
   let var = Glue.lucky_var_of (static_unalias xenv) info in
   let tab = Util.StringMap.add id var tab in
   var, tab
)
(*
   Retrieve a Var.t ina LutExe.t
*)
and id2var (it:t) (id:CoIdent.t) = (
  
   (* let info = Util.StringMap.find id (Expand.support_tab xenv) in *)
   (* Glue.lucky_var_of unalias info *)
   Util.StringMap.find id it.id2var_tab
)


(**
   Builds a LutExe.t structure
   from a Expand.t structure
   (i.e. a pre-compiled Lutin program)
*)
let of_expanded_code (opt:MainArg.t) (exped: Expand.t):t = (
  (* computes and stores info for the execution *)
  let id2var_tab = Util.StringMap.empty in
  (* lucky-like vars are created once for all *)
  let f (vs,t) id =
    let v,t = new_id2var t exped id in
    v::vs, t
  in
  let ivs, id2var_tab = List.fold_left f ([],id2var_tab) (Expand.input_list exped) in
  let ivs = List.rev ivs in
  let ovs, id2var_tab = List.fold_left f ([],id2var_tab) (Expand.output_list exped) in
  let ovs = List.rev ovs in
  
  let ovns = List.map (Var.name) ovs in
  let lvs, id2var_tab = List.fold_left f ([],id2var_tab) (Expand.local_out_list exped) in
  (* all uncontrolables ... *)
  let uvs = ovs @ lvs in
  let isbool v = ((Var.typ v) = Type.BoolT) in
  let isnum v = (match (Var.typ v) with Type.IntT | Type.FloatT -> true | _ -> false) in
  let bvs = List.filter isbool uvs in 
  let nvs = List.filter isnum uvs in 
  let bvf = match bvs with
    | [] -> Exp.True
    | h::t -> List.fold_left (fun acc v -> (Exp.And(Exp.Bvar(v), acc))) (Exp.Bvar h) t
  in
  (* init pres *)
  let addpre acc x = (
    match Var.init x with
    | None -> acc
    | Some e -> (
        let v = Exp.to_value e in
        Value.OfIdent.add acc (Var.name x, v)
      )
  ) in
  let ip = List.fold_left addpre (Value.OfIdent.empty) ivs in
  let ip = List.fold_left addpre ip ovs in
  {
    arg_opt = opt;
    expanded_code = exped;
    out_vars = ovs;
    in_vars = ivs;
    loc_vars = lvs;
    id2var_tab = id2var_tab;
    init_pres = ip;
    out_var_names = ovns;
    bool_vars_to_gen = bvf;
    num_vars_to_gen = nvs;
    snt = Solver.init();
  }
)

(**
   Builds a LutExe.t structure
   from a lutin file + a main node

   N.B. some fields are redundant, they are stored
   to ease te connection with the lucky solver.
*)
let make opt infile mnode = (
  (* open the file, compile and expand the main node ... *)
    let libs = MainArg.libs opt in 
    let mainprg = 
      assert (infile <> []); 
      Parsers.reinit_parser ();
      Parsers.read_lut infile
    in
    Verbose.put ~flag:dbg "LutExe.make: Parsers.read_lut infile [%s] OK\n"
                (String.concat ";" infile);
    let tlenv = CheckType.check_pack libs mainprg in
    Verbose.put ~flag:dbg "LutExe.make: CheckType.check_pack OK\n";

    let mnode = if mnode <> "" then mnode else (
      let all_nodes = Hashtbl.fold
        (fun n _ acc -> n::acc)
        mainprg.Syntaxe.pck_nodetab
        []
      in
        (* It is not necessary to build to complete list to take the first
           one, but I'm sure that list will be useful in the future... R1.*)
      let mnode = if all_nodes = [] then
        (* shouldn't that be checked before? *)
        raise (LutErrors.Global_error ("Lutin files '"^(String.concat "," infile)^ 
                                      "' contains no node"))
      else List.hd all_nodes 
      in
        Verbose.put ~level:1 "# No node is specified: will use %s \n" mnode;
        mnode
    ) in
    let exped = Expand.make tlenv mainprg mnode in
    Verbose.put ~flag:dbg "LutExe.make: Expand.make %s OK\n" mnode;
    (* actual result .... *)
(*     Verbose.put ~flag:dbg "RdbgEvent.set_seed %i\n"(MainArg.seed opt); *)
    MainArg.set_seed opt (Some (MainArg.seed opt));
    if MainArg.run opt then 
      of_expanded_code opt exped
    else
      exit 0
  
)

(** Execution *)

(** control state = Lutin trace exp *)
(* type control_state =  (t * CoTraceExp.t) *)
type control_state = t CoTraceExp.t

let string_of_control_state t =  CoTraceExp.dumps t
(* initial control state *)
let get_init_state it = (
   (* ident de la trace principale *)
   let xenv = it.expanded_code in
   let init_id = Expand.main_trace xenv in
   let init_info = Expand.get_trace_info xenv init_id in
   init_info.Expand.ti_def_exp
)

(** initial pre values *)
let get_init_pres it = it.init_pres



(** A "guard" is a SATISFIABLE constraint used in a goto *)
(* => for the time being, a lucky bool exp (Exp.formula) 
TODO: 
   => hold a normalized version (BDD) ?
   => or even a solution ?
   => hold the support of the constraint ?
      (for the time being, all local vars are generated,
      even out of their scope)
*)

(** keep the solution(s) *) 
type solutions = (Var.env_out * Var.env_loc) list
type guard = {
   g_form : Exp.formula;
   g_sol : solutions option;
   g_bdd : Bdd.t option;
   g_src : CoIdent.src_stack list
}

let rm_ctrl_nt = Str.global_replace (Str.regexp "[\n\t]") " " 

let (guard_to_string : guard -> string) =
 fun g -> 
   rm_ctrl_nt(Exp.formula_to_string g.g_form)

let empty_guard = {g_form = Exp.True; g_sol = None; g_bdd=None; g_src=[] }

let ws = Str.regexp "[ \t\n]+"
let string_of_guard g = (
   let s = Exp.formula_to_string g.g_form in
   let sl = Str.split ws s in
   String.concat " " sl
)

(**
   Full eval id2exp for Glue.lucky_exp_of
   - unaliasing
   - replace uncontrolable by their values
   - partial eval (always)
=> requires a LutExe.t for unaliasing and a data context
   (uncontrol. values)
*)

let rec contextual_id2exp (it:t) data (_eval:bool) (idref:CoAlgExp.node) = (
  let unalias s =
    (Util.StringMap.find s (Expand.alias_tab it.expanded_code)).Expand.ai_def_exp
  in
  match idref with
  | CoAlgExp.AE_alias id -> (
      let e' = unalias id in
      contextual_lutexp2exp it data e'
    )
  | CoAlgExp.AE_support id -> (
      (* does the value exist in data.curs ? *)
      let res =
        try 
          let v = Value.OfIdent.get data.curs id in
          Glue.lucky_exp_of_value v
        with Not_found -> (
            Glue.lucky_exp_var_ref (id2var it id)
          )
      in
      res
    )
  | CoAlgExp.AE_pre id -> (
      (* the value MUST be data.pres *)
      try 
        let v = Value.OfIdent.get data.pres id in
        Glue.lucky_exp_of_value v
      with Not_found ->  
        raise (Global_error (
            "Can't find the value of pre("^(CoIdent.to_string id)^
            ") in the current context"))
    )
  | _ -> assert false
) and
  contextual_lutexp2exp (it:t) data e = (
    (* PARTIAL EVAL *)
    Glue.lucky_exp_of true (contextual_id2exp it data) e
  )

(** exceptions and type *)
exception Deadlock of int (* Attach the event nb to ease debugging when this exc 
                             is raised at top-level *)

exception Stop
exception Exception of string
type internal_state = control_state * Var.env

let get_init_internal_state (it:t) = (get_init_state it, get_init_pres it)


let make_pre :  Var.env_in -> Var.env_out -> Var.env_loc -> Var.env =
fun             ins           outs           locs        -> (
   (* for the time being we keep all variables, even if they are not used in a pre ... *)
   Value.OfIdent.union ins (Value.OfIdent.union outs locs)
)

(** Interface with the solver
=> the lucky solver was designed to do more than we need here !
*)

(* reuse Solver.solve_formula, returns solutions *)
let find_some_sols (it:t) (tfdn: Thickness.formula_draw_nb) (tn: Thickness.numeric)
    (g : guard) : t * guard * solutions
  = match g.g_sol with
  | Some s -> it, g, s
  | None ->
    let snt, zesol,bdd =
      let solver_vl = ref 0 in
      Verbose.exe ~flag:dbg (fun _ -> solver_vl := 3);
      let snt, is_bool_sat, bdd = Solver.is_satisfiable_bdd it.snt
          Value.OfIdent.empty      (* input: Var.env_in *)
          Value.OfIdent.empty      (* memory: Var.env *)
          (*       !solver_vl  *)
          (Verbose.level())
          "LutExe.is_bool_sat"     (* ctx_msg: string *)
          g.g_form
          "LutExe.is_bool_sat"     (* msg: string (?) *)
      in
      if not is_bool_sat then (
        snt, [],bdd
      ) else (
        let snt, sols = Solver.solve_formula snt
            Value.OfIdent.empty      (* input: Var.env_in *)
            Value.OfIdent.empty      (* memory: Var.env *)
            (*          !solver_vl                (* vl: int *) *)
            (Verbose.level())
            "LutExe.solve_guard"     (* msg: string *)
            [it.out_var_names]       (* output_var_names: Var.name list list *)
            tfdn                     (* p: Thickness.formula_draw_nb *)
            tn                       (* num_thickness Thickness.numeric *)
            it.bool_vars_to_gen      (* bool_vars_to_gen_f: formula *)
            it.num_vars_to_gen       (* num_vars_to_gen: var list *)
            g.g_form                 (* f: formula *)
        in 
        snt, sols, bdd
      )
    in
    let it = { it with snt = snt } in
    let g = { g with g_sol = Some zesol; g_bdd = Some bdd } in
    it, g, zesol
    


let find_one_sol it g =
  let thick =  match MainArg.step_mode it.arg_opt with
    | Lucky.StepInside -> (1,0,Thickness.AtMost 0)
    | Lucky.StepEdges ->  (0,1,Thickness.AtMost 0)
    | Lucky.StepVertices ->  (0,0,Thickness.AtMost 1)
  in
    match find_some_sols it 1 thick g with
      | it, g, s::_ -> it, g, s
      | _,_, [] -> raise Not_found
          

(* check_sat *)
let check_satisfiablity (it:t) (g: guard) : t * guard * bool = 
    Verbose.exe ~flag:dbg
      (fun () -> 
         Printf.printf
           "--check satisfiablility of \"%s\"\n" (string_of_guard g));
    try 
      let it, g,_ = find_one_sol it g in
      it, g, true
    with Not_found -> it, g, false


let min_max_src (bl1,el1,bc1,ec1,bchar1,echar1) (bl2,el2,bc2,ec2,bchar2,echar2) =
  let bl,bc =
    if bl1 < bl2 then bl1,bc1 else if
       bl1 > bl2 then bl2,bc2 else bl1, min bc1 bc2
  in
  let el,ec =
    if el1 > el2 then el1,ec1 else if
       el1 < el2 then el2,ec2 else el1, max ec1 ec2
  in
  (bl,el,bc,ec, min bchar1 bchar2, max echar1 echar2) 
  
  
(* Returns the begin/end line/char of a val_exp *)
  let (cstr_src_info_of_val_exp : Syntaxe.val_exp -> int * int * int * int * int * int) =
    fun e -> 
      let rec aux (bl,el,bc,ec,bchar,echar) e =
        let acc =
          min_max_src (bl,el,bc,ec,bchar,echar)
            (e.Lexeme.src.Lexeme.line,
             e.Lexeme.src.Lexeme.line,
             e.Lexeme.src.Lexeme.cstart,
             e.Lexeme.src.Lexeme.cend,
             e.Lexeme.src.Lexeme.chstart,
             e.Lexeme.src.Lexeme.chend
            )
        in
        match e.Lexeme.it with
        | Syntaxe.PRE_n id -> min_max_src acc
          (id.Lexeme.src.Lexeme.line,
           id.Lexeme.src.Lexeme.line,
           id.Lexeme.src.Lexeme.cstart,
           id.Lexeme.src.Lexeme.cend,
           id.Lexeme.src.Lexeme.chstart,
           id.Lexeme.src.Lexeme.chend)
        | Syntaxe.CALL_n (_,vel) -> List.fold_left aux acc vel
        | Syntaxe.ERUN_n(_, ve1, ve2) -> aux (aux acc ve1) ve2
        | Syntaxe.RUN_n (_, ve1, _) ->  aux acc ve1
        | Syntaxe.ASSERT_n (_, ve1, _) ->  aux acc ve1
        | _ -> acc
      in
      aux (e.Lexeme.src.Lexeme.line, e.Lexeme.src.Lexeme.line, 
           e.Lexeme.src.Lexeme.cstart, e.Lexeme.src.Lexeme.cend, 
           e.Lexeme.src.Lexeme.chstart, e.Lexeme.src.Lexeme.chend) e
      
exception No_src_info

let rec (to_src_info:  CoIdent.src_stack -> RdbgEvent.src_info_atom) =
  fun l ->
    match l with
      | [] -> raise No_src_info
      | (lxm, _,None)::tl ->
        {
          RdbgEvent.file = lxm.Lexeme.file;
          RdbgEvent.line = lxm.Lexeme.line, lxm.Lexeme.line;
          RdbgEvent.char = lxm.Lexeme.cstart, lxm.Lexeme.cend;
          RdbgEvent.stack = if tl=[] then None else Some (to_src_info tl);
          RdbgEvent.str = lxm.Lexeme.str;
        }
      | (lxm,_,Some ve)::tl -> 
        let line_b, line_e, col_b, col_e, char_b, char_e = cstr_src_info_of_val_exp ve in
        let file = lxm.Lexeme.file in
        let filecontent = Mypervasives.readfile file in
        {
          RdbgEvent.str =
            (try String.sub filecontent char_b (char_e - char_b + 1)
             with _ ->
             try String.sub filecontent char_b (String.length filecontent - char_b)
             with _ ->
               Printf.sprintf "%s: fail to get chars %i-%i" file char_b char_e);
          RdbgEvent.file = file;
          RdbgEvent.line = line_b, line_e;
          RdbgEvent.char = col_b, col_e;
          RdbgEvent.stack = if tl=[] then None else Some (to_src_info tl);
        }


let add_to_guard_nc it data (e:CoAlgExp.t) (acc:guard) (si:CoTraceExp.src_info) =
(* translate e into an Exp.t using lucky_exp_of with an id2exp:
   - that performs unalias
   - that replace input/pre values
*)
   (* necessarily a formula (bool exp), if we rely on type checking ! *)
   (* let xenv = it.expanded_code in *)
  let le = contextual_lutexp2exp it data e in
  let f = match le with
    | Exp.Formu f -> f
    | _ -> assert false
  in
  let nf = match acc.g_form with
    | Exp.True -> f
    | x -> Exp.And (f, x)
  in
    {
      g_form = nf;
      g_sol = None;
      g_bdd = None;
      g_src = if snd si = [] then acc.g_src else (snd si)::acc.g_src
    } 

exception LocalDeadlock of t

(** Add a new constraint to an existing guard *)
let add_to_guard it data (e:CoAlgExp.t) (acc:guard) (si:CoTraceExp.src_info) = (
  let res = add_to_guard_nc it data e acc si in
  let it, res, ok = check_satisfiablity it res in
    if ok then it, res
    else raise (LocalDeadlock it)
)

(** Tries to compute a value in a context *)
exception Not_a_constant of string 
let compute_exp (it:t) data e = (
   let le = contextual_lutexp2exp it data e in
   try Exp.to_value le
   with _ -> raise (Not_a_constant (Exp.to_string le)) 
)
(* same but accept non existing *)
let try_compute_exp (it:t) data e = (
   let le = contextual_lutexp2exp it data e in
   Exp.to_value le
)
(* i.e. but must be int *)
let compute_int (it:t) data e = (
   match (compute_exp it data e) with
   | Value.N ( Value.I i) -> Util.int_of_num i
   | _ -> assert false
)

(** A behavior (step) of a program *)
type behavior =
| Goto of guard * control_state 
| Raise of string
| Vanish

let string_of_behavior = function
| Goto (g,c) -> 
   Printf.sprintf "Goto (%s, %s)" (string_of_guard g) (string_of_control_state c)
| Vanish -> "Vanish"
| Raise x -> Printf.sprintf "Raise %s" x

type behavior_gen =
|  NoMoreBehavior of int
|  SomeBehavior of behavior * (unit -> behavior_gen)


(*****************************************************************************
genpath est la fonction récursive avec continuation :

- un env pour résoudre les alias
- une valeur des data (courants et pres)
- un accumulateur de contrainte (?)
- une continuation
Retourne :
1 behavior + 1 behavior gen


La fonction récursive:
- réalise un parcours "lazzy" de l'arbre de priorité des comportements
- un point dans ce parcours est parfaitement déterminé par :
  * un point de contrôle (statement lutin : CoTraceExp.t)
  * les attributs qui résume le chemin ayant conduit à ce point de contrôle :
    - la valeur des non-contôlables (ins + pres)
    - l'ensemble des non-contrôlable à traiter
      n.b. ces 2 infos sont dynamiques à cause des scopes de vars. locales,
    - un accumulateur de contraintes, qui permet d'OPTIMISER
      le parcours (assert, et para) mais N'EST PAS STRICTEMENT
      nécéssaire  
    - la continuation qui dit comment traiter les feuilles
- un point peut s'avérer être un cul de sac, auquel cas il faut
  faut explorer une autre branche. C'est pourquoi la fonction
  récursive prend en argument UNE LISTE de points d'exploration :
  * initialement, il n'y a qu'un point (le top level)
  * au cours de la descente, des points "alternatifs" sont insérés
   (opérateur priorité)
  * en cas de deadlock, on passe au point suivant dans la
    liste de priorité, s'il n'y en a pas, on a un deadlock général

N.B. Pour l'instant, ON SE CONCENTRE SUR LA SEMANTIQUE
DETERMINSITE : i.e. on génère exactement UN comportement :

- les choix INDETERMINISTES (probabilistes) sont transformés
  en choix DETERMINSTES (prioritaires)
- les autres possibilités ne sont pas considérées (pour l'instant)
  donc l'interface "facon lurette" pour faire du test épais dans
  le contrôle est pour l'instant purement "cosmétique"

Traitement des supports locaux:

L'idéal serait de gérer finement les variables locales et leurs pre's
au niveau des scopes (exists), typiquement :
- exist porte la liste des vars + leurs valeurs pres:
  * initialement, les pres sont optionnels (programmés)
  * puis après, ils découlent de la réaction précédente.
Le problème est que la valeur des pres n'est pas connue
au moment puisque la résolution des contraintes est reportée
au top-level, il faudrait donc quand on traite un exist :
  - propager les valeurs des pres,
  - évaluer avec une continuation qui répercute un exists avec
    pres INCONNUS
  - une fois la réaction top-level effectuée, répercuter les 
    valeurs tirées sur les exists "pendants"
=> c'est un peu lourd, on fait à l'ancienne en se basant 
sur l'hypothèse (la propriété !) d'unicité des noms des
variables locales :
  - toutes les variables locales sont considérées "vivantes",
    même en dehors de leur scope. Ca ne pose pas de problème
    puisque qu'on sait par analyse statique (binding) qu'elle
    ne seront jamais utilsée en dehors de leur scope.
  - en conséquence, le scope n'est utilisé QUE pour la
    réinitialisation des pres (après on l'oublie)
  - les pres sont recherchés dans le paquet global
  - une conséquence est que TOUTES les locales sont générées
    et stockées, même quand elles ne servent pas ...

********************************************************************************)

(* A continuation "explore" a leaf
   according to the calling context,
   returns a behavior or raise Deadlock

Debug: a continuation holds a mnemo to help debug

*)
type cont_mnemo =
  | Cnoeps
  | Cfby of control_state
  | Ctry of control_state option 
  | Cpara_head of control_state
  | Cpara_tail of guard * control_state
  | Ccatch of string * control_state option
  | Crun of string

let string_of_cont_mnemo = function
  | Cnoeps  -> "/e" 
  | Cfby cs -> Printf.sprintf ".%s" (string_of_control_state cs)
  | Ctry None -> Printf.sprintf "?"
  | Ctry (Some cs) -> Printf.sprintf "?/%s" (string_of_control_state cs)
  | Ccatch (x, None) -> Printf.sprintf "?%s" x
  | Ccatch (x, Some cs) -> Printf.sprintf "?%s/%s" x (string_of_control_state cs)
  | Cpara_head cs -> Printf.sprintf "&(%s)" (string_of_control_state cs)
  | Cpara_tail (g, cs) -> 
     Printf.sprintf "&&(%s, %s)" (string_of_guard g) (string_of_control_state cs)
  | Crun (s) -> Printf.sprintf "!%s" s


type e = RdbgEvent.t
type ctx = RdbgEvent.t

type continuation = {
   doit: behavior -> behavior;
   dbg: cont_mnemo list
}
type continuation_ldbg = {
  doit_ldbg:ctx -> t -> behavior -> (ctx -> t -> behavior -> e)
  ->  (ctx -> t -> e) -> (ctx -> t -> string -> e) -> e;
    dbg_ldbg: cont_mnemo list
}

let (mk_cont : (behavior -> behavior) -> cont_mnemo -> continuation -> continuation) = 
  fun f d cin -> {
    doit = f;
    dbg = d::(cin.dbg);
  }

let (mk_cont_ldbg : ctx -> t -> 
     (ctx -> t -> behavior -> (ctx -> t -> behavior -> e) -> 
      (ctx -> t -> e) -> (ctx -> t -> string -> e) -> e) -> 
     cont_mnemo -> continuation_ldbg ->  
     (ctx -> t -> continuation_ldbg -> e) -> e) =
  fun ctx t f d cin cont -> 
    cont ctx t {           
      doit_ldbg = f;
      dbg_ldbg = d::(cin.dbg_ldbg);
    }

(* a "virtual" branch in the exploration priority tree *)
type branch = {
   br_ctrl: t CoTraceExp.t;
   br_data: store;   
   br_supp: support;
   br_acc: guard;
   br_cont: continuation;
}
type branch_ldbg = {
   br_ctrl_ldbg: t CoTraceExp.t;
   br_data_ldbg: store;   
   br_supp_ldbg: support;
   br_acc_ldbg: guard;
   br_cont_ldbg: continuation_ldbg;
}

let string_of_branch b = (
   "  control: "^(string_of_control_state b.br_ctrl)^"\n"^
   "  data.ins: "^(Value.OfIdent.to_short_string b.br_data.curs)^"\n"^
   "  data.pres: "^(Value.OfIdent.to_short_string b.br_data.pres)
)
let string_of_branch_ldbg b = (
   "  control: "^(string_of_control_state b.br_ctrl_ldbg)^"\n"^
   "  data.ins: "^(Value.OfIdent.to_short_string b.br_data_ldbg.curs)^"\n"^
   "  data.pres: "^(Value.OfIdent.to_short_string b.br_data_ldbg.pres)
)


(* misc. utils *)
let put_in_seq te1 te2 = (
   if(te1 = TE_eps) then te2
   else if(te2 = TE_eps) then te1
   else (TE_fby (te1,te2))
)
let put_in_para te1 te2 = (
  match (te1,te2) with
   | (TE_eps, x) -> x
   | (x, TE_eps) -> x
   | (x, TE_para yl) -> TE_para (x::yl)
   | (x,y) -> TE_para [x; y]
)

let (event_incr : ctx -> MainArg.t -> ctx) =
  fun ctx opt -> 
  MainArg.event_incr opt;
  RdbgEvent.incr_event_nb ctx


let rec genpath (t : t) (data : store)  (* data env = inputs + pres *)
    (x : t CoTraceExp.t)  (* control = lutin trace *)
  : t * behavior
  = (
    (*-------------------------------------------*)
    (* Correspondance id de trace -> trace exp
           N.B. on traque les récursions ?  *)
    (*-------------------------------------------*)
    let xenv = t.expanded_code in
    let id2trace s =
      (Util.StringMap.find s (Expand.trace_tab xenv)).Expand.ti_def_exp
    in
    let it = ref t in
    (* it's too much work to make it a monad.
       well, i've did it for genpath_ldbg! 
    *) 
    (*-------------------------------------------*
      * Fonction récursive :
     * --------------------------------------------*)
    let rec rec_genpath (br:branch) : behavior = (
      let data = br.br_data in
      let x = br.br_ctrl in
      let acc = br.br_acc in
      let cont = br.br_cont in
      Verbose.exe
        ~level:3 ~flag:dbg
        (fun _ -> 
           Printf.fprintf stderr "++REC_GENTRANS:\n" ;
           Printf.fprintf stderr "|> CTRL: %s\n" (string_of_control_state br.br_ctrl);   
           Printf.fprintf stderr "   CONT:\n";
           List.iter (fun c -> Printf.fprintf 
                         stderr "    %s;\n" (string_of_cont_mnemo c)) br.br_cont.dbg;
           Printf.fprintf stderr "--------\n";
        );
      match br.br_ctrl with

      (* Aliased trace *)
      | TE_ref s -> (rec_genpath ({br with br_ctrl = id2trace s}))

      (* Leaves: apply cont *)
      | TE_raise s -> cont.doit (Raise s)
      | TE_eps -> cont.doit Vanish
      (* No eps: forbids e to vanish (but not to raise !) *)
      | TE_noeps e -> (
          let noeps_cont = 
            mk_cont (fun a ->
                Verbose.exe ~flag:dbg 
                  (fun () -> 
                     Printf.printf
                       "-- noeps_cont (%s)\n   in context %s\n"
                       (string_of_behavior a) (string_of_control_state x));
                match a with
                | Vanish -> raise (Deadlock (MainArg.get_event_nb !it.arg_opt))
                | z -> cont.doit z
              ) (Cnoeps) cont 
          in
          rec_genpath ({br with br_ctrl=e; br_cont=noeps_cont})
        )
      (* Constraint: ~same but solve the conjunction first *)
      | TE_constraint (ae,si) -> (
          try 
            MainArg.event_incr !it.arg_opt; (* try *)
            let nit, new_acc = add_to_guard !it data ae acc si in
            it := nit;
            MainArg.event_incr !it.arg_opt; (* sat ou usat*)
            cont.doit (Goto (new_acc, TE_eps))
          with LocalDeadlock nit ->
            it := nit;
            raise (Deadlock (MainArg.get_event_nb !it.arg_opt))
            (* n.b. raise Deadlock if impossible *)
        )
      (* Sequence *)
      | TE_fby (te1, te2) -> (
          let fby_cont = mk_cont (fun a ->
              Verbose.exe ~flag:dbg 
                (fun () -> 
                   Printf.printf
                     "-- fby_cont (%s)\n  in context %s\n"
                     (string_of_behavior a) 
                     (string_of_control_state x));
              match a with
              | Goto (cl,n) -> cont.doit (Goto (cl, put_in_seq n te2))
              | Vanish -> rec_genpath ({br with br_ctrl=te2 })
              | Raise _ -> cont.doit a
            ) (Cfby te2) cont
          in
          rec_genpath ({br with br_ctrl=te1; br_cont=fby_cont})
        )
      | TE_prio [] ->
        (* XXX attach it to this excpt ? *)
        raise (Deadlock (MainArg.get_event_nb !it.arg_opt))
      | TE_prio (te::tel) -> (
          (* Priority: Deadlock is catched HERE *)
          try (rec_genpath ({br with br_ctrl=te})) with 
          | Deadlock _ -> (
              MainArg.event_incr !it.arg_opt; (* try *) 
              MainArg.event_incr !it.arg_opt; (* sat ou usat *)
              rec_genpath ({br with br_ctrl=(TE_prio tel)})
            )
        )
      (* Try similar to a recurse priority *)
      | TE_try (e,eco) -> (
          let try_cont = mk_cont (fun a ->
              Verbose.exe ~flag:dbg 
                (fun () -> 
                   Printf.printf 
                     "-- try_cont (%s)\n  in context %s\n"
                     (string_of_behavior a) 
                     (string_of_control_state x));
              match a with
              | Goto (cl,n) -> cont.doit (Goto (cl, TE_try (n,eco)))
              | _ -> cont.doit a 
            ) (Ctry eco) cont in
          try rec_genpath ({br with br_ctrl=e; br_cont=try_cont})
          with 
          | Deadlock _ -> (
              let ec = match eco with
                | Some e' -> e'
                | None -> TE_eps
              in
              rec_genpath ({br with br_ctrl=ec})
            )
        )
      (* INFINITE WEAK LOOP *)
      (* must behaves exactly as: (te\eps fby loop te) |> eps *)
      | TE_loop te -> (
          let e' =
            TE_prio [
              put_in_seq (TE_noeps te) (TE_loop te)
              ;
              TE_eps
            ]
          in
          rec_genpath ({br with br_ctrl=e'})
        )
      (* INFINITE STRONG LOOP *)
      (* must behaves exactly as: (te\eps fby omega te) *)
      | TE_omega te -> (
          let e' = put_in_seq (TE_noeps te) (TE_omega te)
          in
          rec_genpath ({br with br_ctrl=e'})
        )
      (* ASSERT *) 
      (* default assert is WEAK for backward compatibility
         must behave EXACTLY as 
         trap STOP in (te fby raise STOP) &> omega a
      *)
      | TE_assert (a, te, si) -> (
          let stopid = CoIdent.get_fresh (Expand.ident_space xenv) "Stop_loop" in
          let e' = TE_catch (
              stopid,
              put_in_para
                (put_in_seq te (TE_raise stopid))
                (TE_omega (TE_constraint (a,si)))
              ,
              None
            ) in
          rec_genpath ({br with br_ctrl=e'})
        )
      (* STRONG ASSERT *) 
      (* must behave EXACTLY as
         trap STOP in omega a &> (te fby raise STOP)
      *)
      | TE_strong_assert (a, te, si) -> (
          let stopid = CoIdent.get_fresh (Expand.ident_space xenv) "Stop_loop" in
          let e' = TE_catch (
              stopid,
              put_in_para
                (TE_omega (TE_constraint (a,si)))
                (put_in_seq te (TE_raise stopid))
              ,
              None
            ) in
          rec_genpath ({br with br_ctrl=e'})
        )
      (* Exist: problem modifies the data and support, and the cont *)
      | TE_exist (ectx, te) -> (
          let addp inpres (id, eo) = ( 
            match eo with
            | None -> inpres
            | Some e -> (
                (* first translate to lucky ... *)
                let v = try compute_exp !it data e
                  with Not_a_constant msg ->
                    raise (Internal_error ("LutExe.add_pres",
                                           ("initial value of \""^id^"\" ("^msg^
                                            ")must be a uncontrolable expression")))
                in Value.OfIdent.add inpres (id, v)
              )
          ) in
          let new_pres = List.fold_left addp data.pres ectx in
          let new_data = {data with pres=new_pres} in
          rec_genpath ({br with br_ctrl=te; br_data = new_data })
        )
      (* Parallel: at least one ? *)
      | TE_para ([]) -> assert false
      | TE_para ([e]) -> rec_genpath ({br with br_ctrl = e })
      | TE_para (e::el) -> (
          (* continuation for the head statement *)
          let para_head_cont = 
            mk_cont
              ( fun a ->
                  Verbose.exe ~flag:dbg
                    (fun () -> 
                       Printf.printf  "-- para_head_cont (%s)\n   in context %s\n"
                         (string_of_behavior a) 
                         (string_of_control_state x));
                  match a with
                  (* 1st raises s: whole raises s *)
                  | Raise s -> ( cont.doit (Raise s) )
                  (* 1st vanishes: others continue *)
                  | Vanish -> (
                      rec_genpath ({br with br_ctrl = TE_para(el)})
                    )
                  (* 1st do a trans ... *)
                  | Goto (cl,n) -> (
                      let para_tail_cont = 
                        mk_cont
                          (fun a ->
                             match a with
                             (* others vanish, 1st continue *)
                             | Vanish -> ( cont.doit (Goto (cl,n)) )
                             (* others raise -> forbidden *)
                             | Raise _s ->
                               (raise (Deadlock (MainArg.get_event_nb !it.arg_opt)))
                             | Goto (tcl, tn) -> (
                                 (* N.B. cl IS ALREADY accumulated in tcl *)
                                 cont.doit (Goto (tcl, put_in_para n tn))
                               )
                          ) (Cpara_tail (cl, n)) cont in 
                      (* N.B. cl CONTAINS incoming acc, thus it becomes the whole rec_acc *)
                      let tail_acc = cl in
                      (*** BIG BUG: the other_brs IS NOT THE RIGHT ONE ->
                           SHOULD BE THE ONE REACHED WHEN THE Goto (cl,n) WAS GENERATED !!!
                       ***)
                      rec_genpath ({br with br_ctrl= TE_para(el); br_acc=tail_acc;
                                            br_cont=para_tail_cont})
                    )
              ) (Cpara_head (TE_para el)) cont in
          rec_genpath ({br with br_ctrl=e; br_cont=para_head_cont})
        )


      (* Catch *)
      | TE_catch (i,e,eco) -> (
          let catch_cont = 
            mk_cont
              (fun a ->
                 Verbose.exe ~flag:dbg 
                   (fun () -> 
                      Printf.printf
                        "-- catch_cont (%s)\n   in context %s\n"
                        (string_of_behavior a) (string_of_control_state x));
                 match a with
                 | Goto (cl,n) -> cont.doit (Goto (cl, TE_catch(i, n, eco)))
                 | Raise x -> (
                     if ( x == i) then (
                       match eco with
                       | None -> cont.doit Vanish
                       | Some ec -> (
                           rec_genpath ({br with br_ctrl=ec })
                         )
                     ) else cont.doit (Raise x)
                   )
                 | _ -> cont.doit a
              ) (Ccatch (i,eco)) cont in
          rec_genpath ({br with br_ctrl=e ; br_cont=catch_cont})
        )
      (* Probabilistic choice 
          we use an internal structure for storing
          choices where weights are already evaluated
      *)
      | TE_choice wtel -> (
          let weval (s,l) (te, weo) = (
            let w = match weo with
              | None -> 1
              | Some we -> compute_int !it data we
            in
            if (w <= 0) then
              (* HERE: warns if strictly negative ? *)
              (s,l)
            else (
              (s+w, (w,te)::l)
            )
          ) in
          let (sum, cel) = List.fold_left weval (0, []) wtel in
          try 
            let e' = match cel with
              | [] -> raise (LocalDeadlock !it)
              | [(_,e)] -> e
              | _ -> TE_dyn_choice (sum, cel)
            in rec_genpath ({br with br_ctrl=e'}) 
          with LocalDeadlock nit ->
            it := nit;
            raise (Deadlock (MainArg.get_event_nb !it.arg_opt))
        )
      (* ad hoc node for dynamic simulation: 
         weights are evaluated (int), guaranted to be > 0,
         and whose sum is given.
         Optimize the selection of a head.
         N.B. the order of the remaining choices is flip-floped
         as each call (optimization of the "fold").
      *)
      | TE_dyn_choice (sum, cel) -> (
          let rec select_first sum acc cel= (
            match cel with
            | [] -> assert false
            | [(wc, e)] -> assert (sum = wc); (wc,e)::acc
            | (wc,e)::cel' -> (
                assert (sum > 0);
                if (Random.int sum < wc) then
                  (wc, e)::(acc @ cel')
                else select_first (sum - wc) ((wc,e)::acc) cel'
              )
          ) in
          let e' = match (select_first sum [] cel) with
            | [] -> assert false
            | [(_,e)] -> e
            | (wc,e)::cel' ->
              TE_prio [ e; TE_dyn_choice (sum - wc, cel') ]
          in rec_genpath ({br with br_ctrl=e'}) 
        )
      (* Probabilistic loops
          just like for choice, we use an ad hoc internal structure
          holding the dynamic informations
          (loop counter, goon/stop weight functions) 
      *)
      | TE_dyn_loop (getweights, cpt, te) -> (
          (* equivalent to:
             |goon(cpt): (te \ eps) . dyn_loop(cpt+1, goon, stop, te)
             |stop(cpt): eps
          *)
          let (gw, sw) = getweights cpt in
          try
            let e' = ( match (gw, sw) with 
                | (0,0) -> raise (LocalDeadlock !it)
                | (0,_) -> TE_eps
                | (_,_) -> (
                    let goon_branch = put_in_seq
                        (TE_noeps te)
                        (TE_dyn_loop (getweights, cpt+1, te))
                    in
                    match sw with
                    | 0 -> goon_branch
                    | _ -> TE_dyn_choice (gw + sw, [ (gw, goon_branch) ; (sw, TE_eps) ])
                  )
              ) in
            rec_genpath ({br with br_ctrl=e'}) 
          with LocalDeadlock nit ->
            it := nit;
            raise (Deadlock (MainArg.get_event_nb !it.arg_opt))
        )
      (* N.B. the "cpt" here is a unique identifier used for compilation
          -> not relevant for dynamic simulation
      *)
      | TE_loopi (_,min,max,te,_si) -> (
          (* eval min and max ONCE here *)
          let imin = compute_int !it data min in 
          let imax = compute_int !it data max in 
          if ((imin >= 0) && (imin <= imax)) then (
            (* HERE *)
            let e' = TE_dyn_loop (LoopWeights.interval imin imax, 0, te) in
            rec_genpath ({br with br_ctrl=e'})
          ) else (
            (* HERE: need to have a real notion of run-time error with source ref *)
            let msg = Printf.sprintf
                "Run-time error: bad step interval in loop (%d, %d)" imin imax
            in
            raise (Global_error msg) 
          )
        )
      | TE_loopa (_,av,ecopt,te,_si) -> (
          (* eval min and max ONCE here *)
          let iav = compute_int !it data av in 
          let iec = match ecopt with
            | None -> 1+ ((10 * iav) / 100)
            | Some x -> compute_int !it data x
          in
          if ((iec > 0) && (iav > iec) && (iec <= ((20 * iav) /100))) then (
            (* HERE *)
            let e' = TE_dyn_loop (LoopWeights.average iav iec, 0, te) in
            rec_genpath ({br with br_ctrl=e'})
          ) else (
            (* HERE: need to have a real notion of run-time error with source ref *)
            let msg = Printf.sprintf
                "Run-time error: bad step average in loop (%d, %d)" iav iec
            in
            raise (Global_error msg) 
          )
        )
      (* Run initial:
         - rid is the name of the instance, the expanded code is available
         through "it.expanded_code.runtab"
         - vars is the list of vars computed by the run (LocalIns) 
         - args is the list of input args (uncontrolable expressions)
         - e is the trace to execute in the new scope
         => creates a LutExe.t in its initial state and calls TE_dyn_erun
      *)
      | TE_erun (rid, vars, args, e) -> (
          (* evaluate init vals in vars cf. TE_exist *)
          let addp inpres (id, eo) = ( 
            match eo with
            | None -> inpres
            | Some e -> (
                (* first translate to lucky ... *)
                let v = try compute_exp !it data e
                  with Not_a_constant msg ->
                    raise (Internal_error ("LutExe.add_pres",
                                           ("initial value of \""^id^"\" ("^msg^
                                            ") must be a uncontrolable expression")))
                in Value.OfIdent.add inpres (id, v)
              )
          ) 
          in
          let new_pres = List.fold_left addp data.pres vars in
          let new_data = {data with pres=new_pres} in


          (* get the corresponding expanded code *)
          let (zecode : Expand.t) = Expand.get_run_expanded_code !it.expanded_code rid in

          (* build a slave LutExe *)
          let zeexe = of_expanded_code !it.arg_opt zecode in
          let inits = get_init_internal_state zeexe in

          (* builds the corresponding abstract reactive prg *)
          let zereact = Reactive.DoStep (to_reactive_prg zeexe inits) in 
          let outids = List.map (fun (id,_) -> id) vars in
          (* build the initial TE_dyn_erun *)
          let e' = TE_dyn_erun (rid, zereact, outids, args, e) in
          rec_genpath ({br with br_ctrl=e'; br_data = new_data})
        )
      | TE_dyn_erun_ldbg (_rid, _react, _vars, _args, _e) -> assert false
      | TE_dyn_erun (rid, react, vars, args, e) -> (
          (* Evaluates args in context *)
          let eval_arg x = compute_exp !it data x in 
          let ins = List.map eval_arg args in 
          (* call the reactive prog *)
          try (
            let (outs, react') = Reactive.step react ins in
            (* stores the values in the LocalIns vars *) 
            Verbose.exe ~flag:dbgrun
              (fun () -> 
                 Printf.printf
                   "-- run of %s(%s) gives (%s):=(%s)\n   "
                   rid
                   (Value.list_to_string ins ",")
                   (CoIdent.list_to_string vars ",")
                   (Value.list_to_string outs ","));

            (* let ivars = List.map (id2var it) vars in *)
            assert (List.length outs = List.length vars);
            let new_ins = Value.OfIdent.add_list2 data.curs vars outs in 
            let new_data = { data with curs=new_ins} in
            (* new cont *)
            let run_cont =
              mk_cont
                (fun a ->
                   Verbose.exe
                     ~flag:dbgrun 
                     (fun () -> 
                        Printf.printf "-- run_cont (%s)\n   in context %s\n"
                          (string_of_behavior a) (string_of_control_state x));
                   match a with
                   | Goto (cl,n) ->
                     cont.doit (Goto (cl, TE_dyn_erun(rid,react',vars, args, n)))
                   | _ -> cont.doit a
                ) (Crun (rid)) cont in
            (* recursively execute e in this new data env *)
            rec_genpath ({br with br_ctrl=e; br_data = new_data; br_cont=run_cont})
          ) with
          (* HERE: semantics, decide what to do if run stops ??? *)
          | Stop -> 
            let msg = Printf.sprintf
                "Run-time error: unexpected END while running \"%s\"" rid
            in
            raise (Global_error msg) 
          | Exception x ->  
            let msg =
              Printf.sprintf
                "Run-time error: unexpected EXCEPTION \"%s\" when running \"%s\"" x rid
            in
            raise (Global_error msg) 
        )
      (* run 
         andexp is the post-constraint /\(glob = loc)
         vars : liste des vars internes LocalIn correspondantes
      *)
      | TE_run (rid, andexp, vars, args, e, si) -> (
          (* evaluate init vals in vars:
             by construction, let x be the global name and x' the local one,
             init value x' = pre x
             but if "pre x" has no value, it's not YET an error
          *)
          let addp inpres (id, eo) = ( 
            match eo with
            | None ->
              inpres
            | Some e -> (
                try
                  let v = try_compute_exp !it data e in
                  Verbose.exe ~flag:dbgrun
                    (fun () -> 
                       Printf.printf
                         "TE_run:addp pre %s <- pre %s (%s)\n"
                         id (CoAlgExp.lus_dumps e) (Value.to_string v));
                  Value.OfIdent.add inpres (id, v)
                with _ ->
                  Verbose.exe ~flag:dbgrun
                    (fun () -> 
                       Printf.printf 
                         "TE_run:addp pre %s undefined (as pre %s)\n"
                         id (CoAlgExp.lus_dumps e));
                  inpres
              )
          ) in
          let new_pres = List.fold_left addp data.pres vars in
          let new_data = {data with pres=new_pres} in

          (* get the corresponding expanded code *)
          let (zecode : Expand.t) = Expand.get_run_expanded_code !it.expanded_code rid in

          (* build a slave LutExe *)
          let zeexe = of_expanded_code !it.arg_opt zecode in
          let inits = get_init_internal_state zeexe in

          (* builds the corresponding abstract reactive prg *)
          let zereact = Reactive.DoStep (to_reactive_prg zeexe inits) in 

          (* same as TE_dyn_erun except that we keep trace of the global constraint andexp*)
          (* build the initial TE_dyn_erun *)
          let e' = TE_dyn_run (rid, zereact, andexp, vars, args, e,si) in
          rec_genpath ({br with br_ctrl=e'; br_data = new_data})
        )
      (* similar to dyn_erun except that the continuation
         is completed by andexp, which is a big constraint
         /\ (xi = vi) where xi's are the global variable 
         and the vi's are the corresponding values computed by the call
      *)
      | TE_dyn_run_ldbg (_rid, _react, _andexp, _vars, _args, _e,_si) -> assert false
      | TE_dyn_run (rid, react, andexp, vars, args, e, si) -> (
          (* Evaluates args in context *)
          let eval_arg x = compute_exp !it data x in 
          let ins = List.map eval_arg args in 

          (* call the reactive prog *)
          try (
            (* outs : Value.t list *)
            let (outs, react') = Reactive.step react ins in
            (* stores the values in the LocalIns vars *) 
            let outids = List.map fst vars in
            Verbose.exe ~flag:dbgrun
              (fun () -> 
                 Printf.printf 
                   "-- run of %s(%s) gives (%s):=(%s)\n   "
                   rid
                   (Value.list_to_string ins ",")
                   (CoIdent.list_to_string outids ",")
                   (Value.list_to_string outs ","));

            (* DATA environment for the scoped trace:
               for each local var x' :
               - its cur value is set
               - its pre value is inherited (if any) from pre x
            *)
            let addp inpres (id, eo) = ( 
              match eo with
              | None -> inpres
              | Some e -> (
                  try
                    let v = try_compute_exp !it data e in
                    Verbose.exe ~flag:dbgrun 
                      (fun () -> 
                         Printf.printf 
                           "TE_run:addp pre %s <- pre %s (%s)\n"
                           id (CoAlgExp.lus_dumps e) (Value.to_string v));
                    Value.OfIdent.add inpres (id, v)
                  with _ ->
                    Verbose.exe ~flag:dbgrun
                      (fun () -> 
                         Printf.printf 
                           "TE_run:addp pre %s undefined (as pre %s)\n"
                           id (CoAlgExp.lus_dumps e));
                    inpres
                )
            ) in
            let new_pres = List.fold_left addp data.pres vars in
            assert (List.length outs = List.length outids);
            let new_ins = Value.OfIdent.add_list2 data.curs outids outs in 
            let new_data = {curs=new_ins; pres=new_pres} in
            (* new cont *)
            let run_cont =
              mk_cont
                (fun a ->
                   Verbose.exe ~flag:dbgrun 
                     (fun () -> 
                        Printf.printf 
                          "-- run_cont (%s)\n   in context %s\n"
                          (string_of_behavior a) (string_of_control_state x));
                   match a with
                   | Goto (cl,n) -> (
                       (* add/eval the constraint andexp *)
                       let nit, cl' = add_to_guard !it new_data andexp cl si in
                       it:=nit;
                       cont.doit (
                         Goto (cl', TE_dyn_run(rid,react',andexp,vars, args, n, si)))
                     )
                   | _ -> cont.doit a
                ) (Crun (rid)) cont in
            (* recursively execute e in this new data env *)
            rec_genpath ({br with br_ctrl=e; br_data = new_data; br_cont=run_cont})
          ) with
          (* HERE: semantics, decide what to do if run stops ??? *)
          (* Stop <=> Vanish *)   
          | Stop -> cont.doit Vanish
          (* Exception <=> Raise *)   
          | Exception x -> cont.doit (Raise x)
          (* Deadlock <=? Deadlock *)
        )
    )
    in

    (* Top-level branch *)
    let top_cont : continuation = { doit = (fun a -> a); dbg = [] } in
    let init_branch = {
      br_ctrl = x;
      br_data = data;
      br_supp = (!it.out_vars, !it.loc_vars);
      br_acc = empty_guard;
      br_cont = top_cont;
    } in
    Verbose.exe ~flag:dbg 
      (fun () -> 
         Printf.printf "=== START STEP genpath with:\n%s\n---\n"
           (string_of_branch init_branch));
    let res = rec_genpath init_branch in
    Verbose.exe ~flag:dbg 
      (fun () -> 
         Printf.printf "==> END OF STEP genpath gives:\n   %s\n"
           (string_of_behavior res));
    !it, res
  )

(* performs a reactive step in a way compatible with the interface Reactive, i.e.
   just build a closure DoStep (react_step ze_exe ze_init_prog) to optain
   a suitable Reactive.prg
*) 
and to_reactive_prg (it:t) (curstate:internal_state) (invals: Value.t list) = (
  let (cstate, pres) = curstate in
  let addin acc invar inval = Value.OfIdent.add acc (Var.name invar,inval) in
  let ins = List.fold_left2 addin Value.OfIdent.empty (in_var_list it) invals in
  let data = { curs = ins; pres=pres } in
  let it, b = 
    MainArg.event_incr it.arg_opt; (* call *)
    genpath it data cstate 
  in
  match b with
  | Raise x -> raise (Exception x)
  | Vanish -> raise Stop
  | Goto (zeguard, ctrl') -> (
      (* THIS IS THE NORMAL BEHAVIOR *)
      let it, _zeguard, (outs, locs) = try
          find_one_sol it zeguard
        with Not_found -> assert false
      in
      let pres' = make_pre ins outs locs in
      let state' = (ctrl', pres') in
      let outvals = 
        List.map (fun x -> Value.OfIdent.get outs (Var.name x)) (out_var_list it) 
      in
      MainArg.event_incr it.arg_opt; (* exit *)
      (outvals, Reactive.DoStep (to_reactive_prg it state'))
    )
) 



let rec (genpath_ldbg : t -> store -> t CoTraceExp.t -> ctx ->
         (ctx -> t -> behavior -> e) -> 
         (ctx -> t -> e) -> (ctx -> t -> string -> e) -> e) =
  fun t data x ctx cont fail_cont excn_cont ->  (* data env = inputs + pres *) (
      (*-------------------------------------------*)  
      (* Correspondance id de trace -> trace exp       
         N.B. on traque les récursions ?  *)
      (*-------------------------------------------*)
      let xenv = t.expanded_code in
      let id2trace s =
        (Util.StringMap.find s (Expand.trace_tab xenv)).Expand.ti_def_exp
      in      
      (*       let it = ref t in (* it's too much work to make it a monad. Really? *)  *)
      (*-------------------------------------------*
        * Fonction récursive :
       * --------------------------------------------*)
      let rec (rec_genpath_ldbg :  ctx -> t -> 
               branch_ldbg -> (ctx -> t -> behavior -> e) -> 
               (ctx -> t -> e) -> 
               (ctx -> t -> string -> e) -> 
               e
              ) = 
        fun ctx t br cont fail_cont excn_cont-> (
            let data = br.br_data_ldbg in
            let x = br.br_ctrl_ldbg in
            let acc = br.br_acc_ldbg in
            let br_cont = br.br_cont_ldbg in
            Verbose.exe 
              ~level:3 ~flag:dbg
              (fun _ -> 
                 Printf.fprintf stderr "++REC_GENTRANS:\n" ;
                 Printf.fprintf stderr 
                   "|> CTRL: %s\n" (string_of_control_state br.br_ctrl_ldbg);   
                 Printf.fprintf stderr "   CONT:\n";
                 List.iter (fun c -> Printf.fprintf stderr 
                               "    %s;\n" (string_of_cont_mnemo c)) 
                   br.br_cont_ldbg.dbg_ldbg;
                 Printf.fprintf stderr "--------\n";
              );
            match br.br_ctrl_ldbg with
            (* Aliased trace *)
            | TE_ref s -> (
                rec_genpath_ldbg ctx t ({br with br_ctrl_ldbg = id2trace s}) 
                  cont fail_cont excn_cont)
            (* Leaves: apply cont *)
            | TE_raise s -> br_cont.doit_ldbg ctx t (Raise s) cont fail_cont excn_cont
            | TE_eps -> br_cont.doit_ldbg ctx t Vanish cont fail_cont excn_cont
            | TE_noeps e -> (
                (* No eps: forbids e to vanish (but not to raise !) *)
                let cont2 ctx t noeps_cont =
                  rec_genpath_ldbg
                    ctx t ({br with br_ctrl_ldbg=e; br_cont_ldbg=noeps_cont}) 
                    cont fail_cont excn_cont
                in
                mk_cont_ldbg ctx t
                  (fun ctx t a lcont fail_cont excn_cont->
                     Verbose.exe ~flag:dbg 
                       (fun () -> 
                          Printf.printf 
                            "-- noeps_cont (%s)\n   in context %s\n"
                            (string_of_behavior a) (string_of_control_state x));
                     match a with
                     | Vanish -> fail_cont ctx t
                     | z -> br_cont.doit_ldbg ctx t z lcont fail_cont excn_cont
                  ) 
                  (Cnoeps) 
                  br_cont
                  cont2
              )                
            (* Constraint: ~same but solve the conjunction first *)
            | TE_constraint (ae,si) -> (
                let new_acc = add_to_guard_nc t data ae acc si in
                let cstr = Exp.to_expr new_acc.g_form in
                let si_atoms = 
                  List.map to_src_info 
                    (if snd si = [] then acc.g_src else (snd si)::acc.g_src)
                in
                let try_cont ctx t () =
                  let t, new_acc, is_sat = check_satisfiablity t new_acc in
                  if (is_sat) then 
                    let enb = ctx.RdbgEvent.nb in
                    let ctx = event_incr ctx t.arg_opt in
                    { ctx with 
                      RdbgEvent.kind =  RdbgEvent.MicroStep "sat ";
                      RdbgEvent.nb = enb;
                      RdbgEvent.lang = "lutin";
                      RdbgEvent.next = (fun () -> 
                          (br_cont.doit_ldbg ctx t (Goto (new_acc, TE_eps))
                             cont fail_cont excn_cont));
                      RdbgEvent.sinfo = Some (fun () -> {
                            RdbgEvent.expr = cstr;
                            RdbgEvent.more =  None;
                            RdbgEvent.atoms = si_atoms;
                            RdbgEvent.in_subst = [];
                            RdbgEvent.out_subst = [];
                          });
                      RdbgEvent.depth = ctx.RdbgEvent.depth;
                      RdbgEvent.step = ctx.RdbgEvent.step;
                      RdbgEvent.name    = ctx.RdbgEvent.name;
                      RdbgEvent.inputs  = ctx.RdbgEvent.inputs;
                      RdbgEvent.outputs = ctx.RdbgEvent.outputs;
                      RdbgEvent.locals = []; (* fixme *)
                      RdbgEvent.data   = ctx.RdbgEvent.data;
                      RdbgEvent.terminate = ctx.RdbgEvent.terminate;
                      RdbgEvent.reset = ctx.RdbgEvent.reset;
                    }
                  else (* the constraint is unsat *)
                    let lazy_ci = fun () ->
                      let cc = add_to_guard_nc t data ae empty_guard si in
                      let t, cc,_ = (check_satisfiablity t cc) in
                      let bdd = match new_acc.g_bdd with None -> assert false 
                                                       | Some bdd -> bdd in
                      let bdd_cc = match cc.g_bdd   with None -> assert false 
                                                       | Some bdd -> bdd in
                      let bdd_acc = match acc.g_bdd with None -> assert false 
                                                       | Some bdd -> bdd in
                      let expr_cc = Exp.to_expr cc.g_form in
                      ExprUtil.get_info t.snt bdd bdd_acc (expr_cc, bdd_cc)
                    in
                    let enb = ctx.RdbgEvent.nb in
                    let ctx = event_incr ctx t.arg_opt in
                    let usat_event =
                      { ctx with
                        RdbgEvent.nb = enb;
                        RdbgEvent.kind =  RdbgEvent.MicroStep "usat";
                        RdbgEvent.lang = "lutin";
                        RdbgEvent.next = (* backtrack *) (fun () -> fail_cont ctx t);
                        RdbgEvent.sinfo = Some (fun () -> {
                              RdbgEvent.expr = cstr;
                              RdbgEvent.more = Some lazy_ci;
                              RdbgEvent.atoms = si_atoms;
                              RdbgEvent.in_subst = [];
                              RdbgEvent.out_subst = [];
                            });
                        RdbgEvent.locals = []; (* fixme *)
                      }
                    in
                    usat_event
                in
                let enb = ctx.RdbgEvent.nb in
                let ctx = event_incr ctx t.arg_opt in
                {  ctx with
                   RdbgEvent.nb = enb;
                   RdbgEvent.kind =  RdbgEvent.MicroStep "try ";
                   RdbgEvent.lang = "lutin";
                   RdbgEvent.sinfo = Some (fun () -> {
                         RdbgEvent.expr = cstr;
                         RdbgEvent.more =  None;
                         RdbgEvent.atoms = si_atoms;
                         RdbgEvent.in_subst = [];
                         RdbgEvent.out_subst = [];
                       });
                   RdbgEvent.locals = []; (* fixme *)
                   RdbgEvent.next = try_cont ctx t;
                }
              )
            (* Sequence *)
            | TE_fby (te1, te2) -> (
                let (cont2 : ctx -> t -> continuation_ldbg -> e) = 
                  fun ctx t fby_cont -> 
                    rec_genpath_ldbg ctx t (
                      {br with br_ctrl_ldbg=te1;
                               br_cont_ldbg=fby_cont}) cont fail_cont excn_cont
                in
                mk_cont_ldbg ctx t
                  (fun ctx t a lcont fail_cont excn_cont->
                     Verbose.exe ~flag:dbg 
                       (fun () -> 
                          Printf.printf "-- fby_cont (%s)\n  in context %s\n"
                            (string_of_behavior a)
                            (string_of_control_state x));
                     match a with
                     | Goto (cl,n) -> 
                       br_cont.doit_ldbg ctx t (Goto (cl, put_in_seq n te2)) 
                         lcont fail_cont excn_cont
                     | Vanish -> rec_genpath_ldbg ctx t ({br with br_ctrl_ldbg=te2 }) 
                                   lcont fail_cont excn_cont
                     | Raise _ -> br_cont.doit_ldbg ctx t a lcont fail_cont excn_cont
                  )
                  (Cfby te2)
                  br_cont
                  cont2
              )
            | TE_prio [] -> fail_cont ctx t
            | TE_prio (te::tel) -> (
                (* Priority:  Deadlock is catched HERE *)
                let fail_cont ctx t = 
                  rec_genpath_ldbg ctx t ({br with br_ctrl_ldbg=(TE_prio tel)})
                    cont fail_cont excn_cont
                in
                rec_genpath_ldbg ctx t ({br with br_ctrl_ldbg=te})
                  cont fail_cont excn_cont
              )
            (* Try similar to a recurse priority *)
            | TE_try (e,eco) -> (
                let fail_cont ctx t = 
                  let ec = match eco with
                    | Some e' -> e'
                    | None -> TE_eps
                  in
                  rec_genpath_ldbg ctx t ({br with br_ctrl_ldbg=ec})
                    cont fail_cont excn_cont
                in
                let cont2 ctx t try_cont = 
                  rec_genpath_ldbg ctx t
                    ({br with br_ctrl_ldbg=e; br_cont_ldbg=try_cont})
                    cont fail_cont excn_cont
                in 
                mk_cont_ldbg ctx t
                  (fun ctx t a lcont fail_cont excn_cont ->
                     Verbose.exe ~flag:dbg 
                       (fun () -> 
                          Printf.printf "-- try_cont (%s)\n  in context %s\n"
                            (string_of_behavior a) (string_of_control_state x));
                     match a with
                     | Goto (cl,n) ->
                       br_cont.doit_ldbg ctx t (Goto (cl, TE_try (n,eco)))
                         lcont fail_cont excn_cont
                     | _ -> br_cont.doit_ldbg ctx t a lcont fail_cont excn_cont
                  ) 
                  (Ctry eco) 
                  br_cont 
                  cont2
              )
            (* INFINITE WEAK LOOP *)
            (* must behaves exactly as: (te\eps fby loop te) |> eps *)
            | TE_loop te -> (
                let e' =
                  TE_prio [
                    put_in_seq (TE_noeps te) (TE_loop te);
                    TE_eps
                  ]
                in
                rec_genpath_ldbg ctx t ({br with br_ctrl_ldbg=e'})
                  cont fail_cont excn_cont
              )
            (* INFINITE STRONG LOOP *)
            (* must behaves exactly as: (te\eps fby omega te) *)
            | TE_omega te -> (
                let e' = put_in_seq (TE_noeps te) (TE_omega te)
                in
                rec_genpath_ldbg ctx t ({br with br_ctrl_ldbg=e'})
                  cont fail_cont excn_cont
              )
            (* ASSERT *) 
            (* default assert is WEAK for backward compatibility
               must behave EXACTLY as 
               trap STOP in (te fby raise STOP) &> omega a
            *)
            | TE_assert (a, te, si) -> (
                let stopid = CoIdent.get_fresh (Expand.ident_space xenv) "Stop_loop" in
                let e' = TE_catch (
                    stopid,
                    put_in_para
                      (put_in_seq te (TE_raise stopid))
                      (TE_omega (TE_constraint (a,si)))
                    ,
                    None
                  ) in
                rec_genpath_ldbg ctx t ({br with br_ctrl_ldbg=e'})
                  cont fail_cont excn_cont
              )
            (* STRONG ASSERT *) 
            (* must behave EXACTLY as
               trap STOP in omega a &> (te fby raise STOP)
            *)
            | TE_strong_assert (a, te, si) -> (
                let stopid = CoIdent.get_fresh (Expand.ident_space xenv) "Stop_loop" in
                let e' = TE_catch (
                    stopid,
                    put_in_para
                      (TE_omega (TE_constraint (a,si)))
                      (put_in_seq te (TE_raise stopid))
                    ,
                    None
                  ) in
                rec_genpath_ldbg ctx t ({br with br_ctrl_ldbg=e'})
                  cont fail_cont excn_cont
              )
            (* Exist: problem modifies the data and support, and the cont *)
            | TE_exist (ectx, te) -> (
                let addp inpres (id, eo) = 
                  match eo with
                  | None -> inpres
                  | Some e -> (
                      (* first translate to lucky ... *)
                      let v = try compute_exp t data e
                        with Not_a_constant msg -> 
                          raise (Internal_error ("LutExe.add_pres",
                                                 ("initial value of \""^id^"\" ("^msg^
                                                  ")must be a uncontrolable expression")))
                      in 
                      Value.OfIdent.add inpres (id, v)
                    )
                in
                let new_pres = List.fold_left addp data.pres ectx in
                let new_data = {data with pres=new_pres} in
                rec_genpath_ldbg ctx t (
                  {br with br_ctrl_ldbg=te; br_data_ldbg = new_data })
                  cont fail_cont excn_cont
              )
            (* Parallel: at least one ? *)
            | TE_para ([]) -> assert false
            | TE_para ([e]) -> rec_genpath_ldbg ctx t ({br with br_ctrl_ldbg = e }) 
                                 cont fail_cont excn_cont
            | TE_para (e::el) -> (
                (* continuation for the head statement *)
                let cont2 ctx t para_head_cont =
                  rec_genpath_ldbg ctx t ({
                      br with br_ctrl_ldbg=e; 
                              br_cont_ldbg=para_head_cont}) cont fail_cont excn_cont
                in
                mk_cont_ldbg ctx t
                  ( fun ctx t a lcont fail_cont excn_cont->
                      Verbose.exe 
                        ~flag:dbg 
                        (fun () -> 
                           Printf.printf "-- para_head_cont (%s)\n   in context %s\n"
                             (string_of_behavior a) (string_of_control_state x));
                      match a with
                      (* 1st raises s: whole raises s *)
                      | Raise s ->
                        br_cont.doit_ldbg ctx t (Raise s) lcont fail_cont excn_cont
                      (* 1st vanishes: others continue *)
                      | Vanish -> (
                          rec_genpath_ldbg ctx t ({br with br_ctrl_ldbg = TE_para(el)}) 
                            lcont fail_cont excn_cont
                        )
                      (* 1st do a trans ... *)
                      | Goto (cl,n) -> (
                          let cont3 ctx t para_tail_cont = 
                            (* N.B. cl CONTAINS incoming acc, thus it becomes the 
                                                       whole rec_acc *)
                            let tail_acc = cl in
                            (*** BIG BUG: the other_brs IS NOT THE
                                 RIGHT ONE -> SHOULD BE THE ONE REACHED
                                 WHEN THE Goto (cl,n) WAS GENERATED !!!
                               ***)
                            rec_genpath_ldbg ctx t (
                              {br with 
                               br_ctrl_ldbg= TE_para(el); 
                               br_acc_ldbg=tail_acc; 
                               br_cont_ldbg=para_tail_cont}) lcont fail_cont excn_cont
                          in
                          mk_cont_ldbg ctx t
                            (fun ctx t a lcont fail_cont excn_cont->
                               match a with
                               (* others vanish, 1st continue *)
                               | Vanish -> br_cont.doit_ldbg 
                                             ctx t (Goto (cl,n)) lcont fail_cont excn_cont
                               (* others raise -> forbidden *)
                               | Raise _s -> fail_cont ctx t
                               | Goto (tcl, tn) -> (
                                   (* N.B. cl IS ALREADY accumulated in tcl *)
                                   br_cont.doit_ldbg 
                                     ctx t (Goto (tcl, put_in_para n tn))
                                     lcont fail_cont excn_cont
                                 )
                            ) 
                            (Cpara_tail (cl, n)) 
                            br_cont 
                            cont3
                        )
                  )
                  (Cpara_head (TE_para el)) 
                  br_cont 
                  cont2
              )
            (* Catch *)
            | TE_catch (i,e,eco) -> (
                let cont2 ctx t catch_cont = 
                  rec_genpath_ldbg ctx t (
                    {br with br_ctrl_ldbg=e;
                             br_cont_ldbg=catch_cont}) cont fail_cont excn_cont
                in
                mk_cont_ldbg ctx t
                  (fun ctx t a lcont fail_cont excn_cont ->
                     Verbose.exe
                       ~flag:dbg 
                       (fun () -> 
                          Printf.printf "-- catch_cont (%s)\n   in context %s\n"
                            (string_of_behavior a) (string_of_control_state x));
                     match a with
                     | Goto (cl,n) -> 
                       br_cont.doit_ldbg ctx t (Goto (cl, TE_catch(i, n, eco))) 
                         lcont fail_cont excn_cont
                     | Raise x -> (
                         if ( x == i) then (
                           match eco with
                           | None ->
                             br_cont.doit_ldbg ctx t Vanish lcont fail_cont excn_cont
                           | Some ec -> 
                             rec_genpath_ldbg ctx t ({br with br_ctrl_ldbg=ec })
                               lcont fail_cont excn_cont
                         ) else br_cont.doit_ldbg ctx t (Raise x)
                             lcont fail_cont excn_cont
                       )
                     | _ -> br_cont.doit_ldbg ctx t a lcont fail_cont excn_cont
                  )
                  (Ccatch (i,eco)) 
                  br_cont 
                  cont2
              )
            (* Probabilistic choice 
                we use an internal structure for storing
                choices where weights are already evaluated
            *)
            | TE_choice wtel -> (
                let weval (s,l) (te, weo) = (
                  let w = match weo with
                    | None -> 1
                    | Some we -> compute_int t data we
                  in
                  if (w <= 0) then
                    (* HERE: warns if strictly negative ? *)
                    (s,l)
                  else (
                    (s+w, (w,te)::l)
                  )
                ) in
                try 
                  let (sum, cel) = List.fold_left weval (0, []) wtel in
                  let e' = match cel with
                    | [] -> raise (LocalDeadlock t)
                    | [(_,e)] -> e
                    | _ -> TE_dyn_choice (sum, cel)
                  in
                  rec_genpath_ldbg ctx t ({br with br_ctrl_ldbg=e'})
                    cont fail_cont excn_cont
                with LocalDeadlock t -> fail_cont ctx t
              )
            (* ad hoc node for dynamic simulation: 
               weights are evaluated (int), guaranted to be > 0,
               and whose sum is given.
               Optimize the selection of a head.
               N.B. the order of the remaining choices is flip-floped
               as each call (optimization of the "fold").
            *)
            | TE_dyn_choice (sum, cel) -> (
                let rec select_first sum acc cel=
                  match cel with
                  | [] -> assert false
                  | [(wc, e)] -> assert (sum = wc); (wc,e)::acc
                  | (wc,e)::cel' -> (
                      assert (sum > 0);
                      Verbose.exe ~flag:dbgrun 
                        (fun () -> 
                           Printf.eprintf
                             "*** TE_dyn_choice : PRGS=%i |tbl|=%i\n"
                             (Random.State.bits (Random.State.copy (Random.get_state ())))
                             (Obj.reachable_words (Obj.repr t))
                           (* (tbl_to_string t) *); 
                           flush stderr;
                        );
                      if (Random.int sum < wc) then
                        (wc, e)::(acc @ cel')
                      else select_first (sum - wc) ((wc,e)::acc) cel'
                    )
                in
                let e' = match (select_first sum [] cel) with
                  | [] -> assert false
                  | [(_,e)] -> e
                  | (wc,e)::cel' ->
                    TE_prio [ e; TE_dyn_choice (sum - wc, cel') ]
                in 
                rec_genpath_ldbg ctx t ({br with br_ctrl_ldbg=e'})
                  cont fail_cont excn_cont
              )
            (* Probabilistic loops
                just like for choice, we use an ad hoc internal structure
                holding the dynamic informations
                (loop counter, goon/stop weight functions) 
            *)
            | TE_dyn_loop (getweights, cpt, te) -> (
                (* equivalent to:
                   |goon(cpt): (te \ eps) . dyn_loop(cpt+1, goon, stop, te)
                   |stop(cpt): eps
                *)
                let (gw, sw) = getweights cpt in
                try 
                  let e' =
                    match (gw, sw) with 
                    | (0,0) -> raise (LocalDeadlock t)
                    | (0,_) -> TE_eps
                    | (_,_) -> (
                        let goon_branch = put_in_seq
                            (TE_noeps te)
                            (TE_dyn_loop (getweights, cpt+1, te))
                        in
                        match sw with
                        | 0 -> goon_branch
                        | _ -> TE_dyn_choice (gw+sw, [ (gw, goon_branch);(sw, TE_eps)])
                      ) 
                  in
                  rec_genpath_ldbg ctx t ({br with br_ctrl_ldbg=e'})
                    cont fail_cont excn_cont
                with LocalDeadlock t -> fail_cont ctx t
              )
            (* N.B. the "cpt" here is a unique identifier used for compilation
                -> not relevant for dynamic simulation
            *)
            | TE_loopi (_,min,max,te,_si) -> (
                (* eval min and max ONCE here *)
                let imin = compute_int t data min in 
                let imax = compute_int t data max in 
                if ((imin >= 0) && (imin <= imax)) then (
                  (* HERE *)
                  let e' = TE_dyn_loop (LoopWeights.interval imin imax, 0, te) in
                  rec_genpath_ldbg ctx t ({br with br_ctrl_ldbg=e'})
                    cont fail_cont excn_cont
                ) else (
                  (* HERE: need to have a real notion of run-time error with source ref *)
                  let msg = Printf.sprintf
                      "Run-time error: bad step interval in loop (%d, %d)" imin imax
                  in
                  raise (Global_error msg) 
                )
              )
            | TE_loopa (_,av,ecopt,te,_si) -> (
                (* eval min and max ONCE here *)
                let iav = compute_int t data av in 
                let iec = match ecopt with
                  | None -> 1+ ((10 * iav) / 100)
                  | Some x -> compute_int t data x
                in
                if ((iec > 0) && (iav > iec) && (iec <= ((20 * iav) /100))) then (
                  (* HERE *)
                  let e' = TE_dyn_loop (LoopWeights.average iav iec, 0, te) in
                  rec_genpath_ldbg ctx t ({br with br_ctrl_ldbg=e'})
                    cont fail_cont excn_cont
                ) else (
                  (* HERE: need to have a real notion of run-time error with source ref *)
                  let msg = Printf.sprintf
                      "Run-time error: bad step average in loop (%d, %d)" iav iec
                  in
                  raise (Global_error msg) 
                )
              )
            (* Run initial:
               - rid is the name of the instance, the expanded code is available
               through "it.expanded_code.runtab"
               - vars is the list of vars computed by the run (LocalIns) 
               - args is the list of input args (uncontrolable expressions)
               - e is the trace to execute in the new scope
               => creates a LutExe.t in its initial state and calls TE_dyn_erun
            *)
            | TE_erun (rid, vars, args, e) -> (
                (* evaluate init vals in vars cf. TE_exist *)
                let addp inpres (id, eo) = ( 
                  match eo with
                  | None -> inpres
                  | Some e -> (
                      (* first translate to lucky ... *)
                      let v = try compute_exp t data e
                        with Not_a_constant msg ->
                          raise (
                            Internal_error ("LutExe.add_pres",
                                            ("initial value of \""^id^"\" ("^msg^
                                             ") must be a uncontrolable expression")))
                      in Value.OfIdent.add inpres (id, v)
                    )
                ) in
                let new_pres = List.fold_left addp data.pres vars in
                let new_data = {data with pres=new_pres} in
                (* get the corresponding expanded code *)
                let (zecode : Expand.t) =
                  Expand.get_run_expanded_code t.expanded_code rid 
                in
                (* build a slave LutExe *)
                let zeexe = of_expanded_code t.arg_opt zecode in
                let inits = get_init_internal_state zeexe in
                let (cont2: ctx -> t -> t prg_ldbg -> (ctx -> t -> e)
                     -> (ctx -> t -> string -> e) -> e) = 
                  fun ctx t zereact fail_cont excn_cont-> 
                    let outids = List.map (fun (id,_) -> id) vars in
                    (* build the initial TE_dyn_erun *)
                    let e' = TE_dyn_erun_ldbg (rid, zereact, outids, args, e) in
                    rec_genpath_ldbg ctx t ({
                        br with br_ctrl_ldbg=e'; 
                                br_data_ldbg = new_data}) cont fail_cont excn_cont
                in
                cont2 ctx t (DoStep_ldbg (to_reactive_prg_ldbg rid zeexe inits))
                  fail_cont excn_cont

                (* builds the corresponding abstract reactive prg *)
              )
            | TE_dyn_erun (_rid, _react, _vars, _args, _e) -> assert false
            | TE_dyn_erun_ldbg (rid, react, vars, args, e) -> (
                (* Evaluates args in context *)
                let eval_arg x = compute_exp t data x in 

                let fail_cont _ctx _t = 
                  let msg = Printf.sprintf
                      "Run-time error: unexpected END while running \"%s\"" rid
                  in
                  raise (Global_error msg) 
                in
                let excn_cont _ctx _t x =
                  let msg =
                    Printf.sprintf
                      "Run-time error: unexpected EXCEPTION \"%s\" when running \"%s\"" 
                      x rid
                  in
                  raise (Global_error msg) 
                in
                let ins = List.map eval_arg args in 
                (* call the reactive prog *)
                let (cont3 : ctx -> t -> t prg_ldbg -> Value.t list -> e) = 
                  fun ctx t react' outs -> 
                    (* stores the values in the LocalIns vars *) 
                    Verbose.exe ~flag:dbgrun
                      (fun () -> 
                         Printf.printf 
                           "-- run of %s(%s) gives (%s):=(%s)\n   "
                           rid
                           (Value.list_to_string ins ",")
                           (CoIdent.list_to_string vars ",")
                           (Value.list_to_string outs ","));
                    (* let ivars = List.map (id2var it) vars in *)
                    assert (List.length vars = List.length outs);
                    let new_ins = Value.OfIdent.add_list2 data.curs vars outs in 
                    let new_data = {data with curs=new_ins} in
                    (* new cont *)
                    let cont2 ctx t run_cont = 
                      (* recursively execute e in this new data env *)
                      rec_genpath_ldbg ctx t
                        ({br with 
                          br_ctrl_ldbg=e; 
                          br_data_ldbg = new_data; 
                          br_cont_ldbg = run_cont}) cont fail_cont excn_cont
                    in
                    mk_cont_ldbg ctx t
                      (fun ctx t a lcont fail_cont excn_cont ->
                         Verbose.exe 
                           ~flag:dbgrun
                           (fun () -> 
                              Printf.printf  "-- run_cont (%s)\n   in context %s\n"
                                (string_of_behavior a) (string_of_control_state x));
                         match a with
                         | Goto (cl,n) -> 
                           br_cont.doit_ldbg ctx t 
                             (Goto (cl, TE_dyn_erun_ldbg(rid,react',vars, args, n)))
                             lcont fail_cont excn_cont
                         | _ -> br_cont.doit_ldbg ctx t a lcont fail_cont excn_cont
                      ) 
                      (Crun (rid)) 
                      br_cont 
                      cont2
                in
                (* exiting a run *)
                let enb,_d = ctx.RdbgEvent.nb, ctx.RdbgEvent.depth in
                let ctx = event_incr ctx t.arg_opt in
                let event = 
                  {  ctx with
                     RdbgEvent.step = ctx.RdbgEvent.step;
                     RdbgEvent.nb = enb;
                     RdbgEvent.depth = ctx.RdbgEvent.depth;
                     RdbgEvent.kind = RdbgEvent.MicroStep "quit";
                     RdbgEvent.lang = "lutin";
                     RdbgEvent.name = rid;
                     RdbgEvent.inputs = ctx.RdbgEvent.inputs;
                     RdbgEvent.outputs = ctx.RdbgEvent.outputs;
                     RdbgEvent.locals = []; (* fixme *)
                     RdbgEvent.data = ctx.RdbgEvent.data;
                     RdbgEvent.sinfo = ctx.RdbgEvent.sinfo;
                     RdbgEvent.next =
                       (fun () -> step_ldbg ctx t react ins cont3 fail_cont excn_cont);
                     RdbgEvent.terminate = ctx.RdbgEvent.terminate;
                     RdbgEvent.reset = ctx.RdbgEvent.reset;
                  }
                in 
                event
              )
            (* run 
               andexp is the post-constraint /\(glob = loc)
               vars : liste des vars internes LocalIn correspondantes
            *)
            | TE_run (rid, andexp, vars, args, e, si) -> (
                (* evaluate init vals in vars:
                   by construction, let x be the global name and x' the local one,
                   init value x' = pre x
                   but if "pre x" has no value, it's not YET an error
                *)
                let addp inpres (id, eo) = ( 
                  match eo with
                  | None -> inpres
                  | Some e -> (
                      try
                        let v = try_compute_exp t data e in
                        Verbose.exe ~flag:dbgrun
                          (fun () -> 
                             Printf.printf 
                               "TE_run:addp pre %s <- pre %s (%s)\n"
                               id (CoAlgExp.lus_dumps e) (Value.to_string v));
                        Value.OfIdent.add inpres (id, v)
                      with _ ->
                        Verbose.exe ~flag:dbgrun
                          (fun () -> 
                             Printf.printf 
                               "TE_run:addp pre %s undefined (as pre %s)\n"
                               id (CoAlgExp.lus_dumps e));
                        inpres
                    )
                ) 
                in
                let new_pres = List.fold_left addp data.pres vars in
                let new_data = {data with pres=new_pres} in
                (* get the corresponding expanded code *)
                let (zecode : Expand.t) =
                  Expand.get_run_expanded_code t.expanded_code rid
                in
                (* build a slave LutExe *)
                let zeexe = of_expanded_code t.arg_opt zecode in
                let inits = get_init_internal_state zeexe in
                let (cont2: ctx -> t -> t prg_ldbg -> e) = 
                  fun ctx t zereact -> 
                    let e' = TE_dyn_run_ldbg (rid, zereact, andexp, vars, args, e, si) in
                    rec_genpath_ldbg ctx t (
                      { br with br_ctrl_ldbg=e';
                                br_data_ldbg = new_data}) cont fail_cont excn_cont
                in
                cont2 ctx t (DoStep_ldbg (to_reactive_prg_ldbg rid zeexe inits))
              )
            (* similar to dyn_erun except that the continuation
               is completed by andexp, which is a big constraint
               /\ (xi = vi) where xi's are the global variable 
               and the vi's are the corresponding values computed by the call
            *)
            | TE_dyn_run      (_rid, _react, _andexp, _vars, _args, _e, _si) -> assert false
            | TE_dyn_run_ldbg (rid, react, andexp, vars, args, e, si) -> (
                (* Evaluates args in context *)
                let eval_arg x = compute_exp t data x in 
                let ins = List.map eval_arg args in 
                (* call the reactive prog *)
                let fail_cont ctx t = 
                  (* Emulate the "raise Stop" using the failure continuation.
                     Should'nt i need a stop continuation instead ? Are Stop and
                     Deadlock really different ?
                  *)
                  br_cont.doit_ldbg ctx t Vanish cont fail_cont excn_cont
                in
                let excn_cont ctx t x = 
                  br_cont.doit_ldbg ctx t (Raise x) cont fail_cont excn_cont
                in
                let (cont3 : ctx -> t -> t prg_ldbg -> Value.t list -> e) = 
                  fun ctx t react' outs -> 
                    (* outs : Value.t list *)
                    (* stores the values in the LocalIns vars *) 
                    let outids = List.map fst vars in
                    Verbose.exe ~flag:dbgrun
                      (fun () -> 
                         Printf.printf 
                           "-- run of %s(%s) gives (%s):=(%s)\n   "
                           rid
                           (Value.list_to_string ins ",")
                           (CoIdent.list_to_string outids ",")
                           (Value.list_to_string outs ","));
                    (* DATA environment for the scoped trace:
                       for each local var x' :
                       - its cur value is set
                       - its pre value is inherited (if any) from pre x
                    *)
                    let addp inpres (id, eo) =
                      match eo with
                      | None -> inpres
                      | Some e -> (
                          try
                            let v = try_compute_exp t data e in
                            Verbose.exe ~flag:dbgrun
                              (fun () -> 
                                 Printf.printf 
                                   "TE_run:addp pre %s <- pre %s (%s)\n"
                                   id (CoAlgExp.lus_dumps e) (Value.to_string v));
                            Value.OfIdent.add inpres (id, v)
                          with _ ->
                            Verbose.exe ~flag:dbgrun
                              (fun () -> 
                                 Printf.printf 
                                   "TE_run:addp pre %s undefined (as pre %s)\n"
                                   id (CoAlgExp.lus_dumps e));
                            inpres
                        )
                    in
                    let new_pres = List.fold_left addp data.pres vars in
                    assert (List.length outs = List.length outids);
                    let new_ins = Value.OfIdent.add_list2 data.curs outids outs in 
                    let new_data = {curs=new_ins; pres=new_pres} in
                    (* new cont *)
                    let cont2 ctx t run_cont =
                      (* recursively execute e in this new data env *)
                      rec_genpath_ldbg ctx t ({
                          br with br_ctrl_ldbg=e;
                                  br_data_ldbg = new_data; 
                                  br_cont_ldbg=run_cont}) cont fail_cont excn_cont
                    in
                    mk_cont_ldbg ctx t
                      (fun ctx t a cont fail_cont excn_cont->
                         Verbose.exe
                           ~flag:dbgrun
                           (fun () -> 
                              Printf.printf  "-- run_cont (%s)\n   in context %s\n"
                                (string_of_behavior a) (string_of_control_state x));
                         match a with
                         | Goto (cl,n) -> (
                             (* add/eval the constraint andexp *)
                             let t, cl' = add_to_guard t new_data andexp cl si in
                             br_cont.doit_ldbg ctx t 
                               (Goto (cl', TE_dyn_run_ldbg(rid,react',
                                                           andexp,vars, args, n, si))) 
                               cont fail_cont excn_cont
                           )
                         | _ -> br_cont.doit_ldbg ctx t a cont fail_cont excn_cont
                      ) 
                      (Crun (rid)) 
                      br_cont 
                      cont2
                in
                step_ldbg ctx t react ins cont3 fail_cont excn_cont
              )
          )  (* gen_path_rec *)
      in
      (* Top-level branch *)
      let top_cont : continuation_ldbg = 
        { doit_ldbg = (
              fun ctx t a cont _fail_cont _excn_cont-> cont ctx t a); dbg_ldbg = [] } 
      in
      let init_branch = {
        br_ctrl_ldbg = x;
        br_data_ldbg = data;
        br_supp_ldbg = (t.out_vars, t.loc_vars);
        br_acc_ldbg = empty_guard;
        br_cont_ldbg = top_cont;
      } 
      in
      Verbose.exe ~flag:dbg 
        (fun () -> 
           Printf.printf "=== START STEP genpath with:\n%s\n---\n"
             (string_of_branch_ldbg init_branch));
      rec_genpath_ldbg ctx t init_branch 
        (fun ctx t b ->              
           Verbose.exe
             ~flag:dbg 
             (fun () -> 
                Printf.printf "==> END OF STEP genpath gives:\n   %s\n"
                  (string_of_behavior b)
             );
           cont ctx t b
        )
        fail_cont excn_cont
    ) (* end of genpath_ldbg ? *)

(* performs a reactive step in a way compatible with the interface Reactive, i.e.
   just build a closure DoStep (react_step ze_exe ze_init_prog) to optain
   a suitable Reactive.prg
*) 
and (to_reactive_prg_ldbg :
       string -> t -> internal_state -> ctx -> t -> Value.t list ->  
     (ctx -> t -> t prg_ldbg -> Value.t list -> e) -> 
     (ctx -> t -> e) -> (ctx -> t -> string -> e) -> e) =
  fun rid run_t curstate ctx caller_t invals cont fail_cont excn_cont -> 
    let (cstate, pres) = curstate in
    let addin acc invar inval = Value.OfIdent.add acc (Var.name invar,inval) in
    let ins = List.fold_left2 addin Value.OfIdent.empty (in_var_list run_t) invals in
    let data = { curs = ins; pres = pres } in
    let edata = (Value.OfIdent.content data.curs) in
    let edata = List.map (fun (n,v) -> n, Value.to_data_val v) edata in
    let predata = (Value.OfIdent.content data.pres) in
    let predata = List.map (fun (n,v) -> "pre_"^n, Value.to_data_val v) predata in
    let ctx_save = ctx in
    let ctx = { ctx with 
                RdbgEvent.name = rid;
                RdbgEvent.data = edata@predata;
                RdbgEvent.inputs = List.map to_event_var (in_var_list run_t);
                RdbgEvent.outputs = List.map to_event_var (out_var_list run_t);
              } 
    in
    let ctx = RdbgEvent.incr_event_depth ctx in
    let d = ctx.RdbgEvent.depth in
    let (cont2: ctx -> t -> behavior -> e) = fun ctx2 t b -> 
      match b with
      | Raise x -> excn_cont ctx2 caller_t x
      | Vanish -> fail_cont ctx2 caller_t
      | Goto (zeguard, ctrl') -> 
        (* THIS IS THE NORMAL BEHAVIOR *)
        let t, zeguard, (outs, locs) = 
          try find_one_sol t zeguard with Not_found -> assert false
        in
        let pres' = make_pre ins outs locs in
        let state' = (ctrl', pres') in
        let outvals =
          List.map (fun x -> Value.OfIdent.get outs (Var.name x)) (out_var_list t)
        in
        let edata = 
          List.map 
            (fun x -> Var.name x,
                      Value.to_data_val (Value.OfIdent.get ins (Var.name x))) 
            (in_var_list t)
        in
        let edata =
          edata @ List.map 
            (fun x -> Var.name x,
                      Value.to_data_val (Value.OfIdent.get outs (Var.name x))) 
            (out_var_list t) 
        in
        let edata =
          edata @ List.map 
            (fun x -> Var.name x,
                      Value.to_data_val (Value.OfIdent.get locs (Var.name x))) 
            (loc_var_list t)
        in
        let si_atoms = List.map to_src_info zeguard.g_src in 
        let cstr = Exp.to_expr zeguard.g_form in
        let ctx2 = { ctx_save with
                     (* once we exit, we return back to the previous ctx *)
                     RdbgEvent.nb = ctx2.RdbgEvent.nb;
                     RdbgEvent.data = edata; (* used? *)
                     RdbgEvent.depth = ctx.RdbgEvent.depth -1
                   }
        in
        let ctx2 = event_incr ctx2 t.arg_opt in
        let event = 
          {  ctx with
             RdbgEvent.nb = ctx2.RdbgEvent.nb-1;
             RdbgEvent.kind = RdbgEvent.Exit;
             RdbgEvent.lang = "lutin";
             (* RdbgEvent.port = RdbgEvent.Exit (guard_to_string zeguard, cstr,lazy_si); *)
             RdbgEvent.name = rid;
             RdbgEvent.locals = []; (* fixme *)
             RdbgEvent.data = edata;
             RdbgEvent.sinfo = Some (fun () -> {
                   RdbgEvent.expr = cstr;
                   (*RdbgEvent.str  = guard_to_string zeguard; *)
                   RdbgEvent.more = None;
                   RdbgEvent.atoms = si_atoms;
                   RdbgEvent.in_subst = [];
                   RdbgEvent.out_subst = [];
                 });
             RdbgEvent.next =
               (fun () ->
                  cont ctx2 caller_t
                    (DoStep_ldbg (to_reactive_prg_ldbg rid run_t state')) 
                    outvals );
          }
        in 
        event
    in (* end of cont2 *)
    let enb = ctx.RdbgEvent.nb in
    let ctx = event_incr ctx run_t.arg_opt in
    let ctx = RdbgEvent.incr_event_depth ctx in (* inner events are one step deapper *)
    {  ctx with
       RdbgEvent.nb = enb ;
       RdbgEvent.depth = d;
       RdbgEvent.kind = RdbgEvent.Call;
       RdbgEvent.lang = "lutin"; 
       RdbgEvent.name = rid;
       RdbgEvent.locals = []; (* fixme *)
       RdbgEvent.data = edata @ predata;
       RdbgEvent.next = (fun () -> genpath_ldbg run_t data cstate ctx cont2
                        fail_cont excn_cont);
       RdbgEvent.sinfo = None; 
    }
      
 

(***************************************************************************************)
(* (fake) Lurette-aware version: 
   - returns a behavior_gen, i.e. supposed to allow thick test
   - N.B. but, for the time being, returns at most ONE behaviour ! 
*)
let get_behavior_gen : t -> Var.env_in -> Var.env -> control_state -> 
  (unit -> t * behavior_gen) = 
  fun                  it   ins           pres       cstate        -> (
      (* prépare les params pour l'appel de la fct. rec. de parcours de trace
         on part du principe qu'on a la meme interface que le genpath (?)
      *)
      let and_thats_all () = NoMoreBehavior 0 in
      let data = { curs = ins; pres=pres } in
      fun () -> (
          let it, b = genpath it data cstate in
          it, SomeBehavior (b, and_thats_all)
        )
    )

let get_behavior_gen_ldbg : t -> Var.env_in -> Var.env -> control_state -> ctx -> 
  (ctx -> t -> behavior_gen -> e) -> e = 
  fun it ins pres cstate ctx cont ->
    (* prépare les params pour l'appel de la fct. rec. de parcours de trace
       on part du principe qu'on a la meme interface que le genpath (?)
    *)
      let data = { curs = ins; pres=pres } in
      let cont2 ctx t (b:behavior) =
        let and_thats_all () = NoMoreBehavior 0 in
        cont ctx t (SomeBehavior (b, and_thats_all))
      in
      let fail_cont _ctx _t = failwith ("deadlock") in
      let excn_cont _ctx _t x  = failwith ("Lutin Exception "^x) in
      genpath_ldbg it data cstate ctx cont2 fail_cont excn_cont


(***************************************************************************************)
type data_state =  {
  ins : Value.OfIdent.t;
  outs: Value.OfIdent.t;
  mems : Value.OfIdent.t; 
}

(* version step unique *)
let (step: t -> control_state -> data_state -> t * control_state * data_state) =
  fun prog ctrl data -> 
    let _  = 
      (* clean tabulated results to avoid memory leaks.
           Cleanning at every step may be overkill though...
      *)
      MainArg.event_incr prog.arg_opt; (* call *) 
    in
    let prog = { prog with snt = Bddd.clear prog.snt } in

    let bg = get_behavior_gen prog data.ins data.mems ctrl in
    match bg () with
    | _, NoMoreBehavior i ->
      let msg =
        Printf.sprintf
          "Run-time error: unexpected DEADLOCK at event nb %i" i
      in
      raise (Global_error msg) 
    | prog, SomeBehavior (b, _bg') -> (
        match b with
        | Vanish -> failwith "Normal termination"
        | Raise str -> failwith str
        | Goto (zeguard, ctrl) -> (
            MainArg.event_incr prog.arg_opt; (* exit *)
            (* THIS IS THE NORMAL BEHAVIOR *)
            let prog, _zeguard, (outs, locs) = 
              try find_one_sol prog zeguard
              with Not_found -> assert false
            in
            let data = 
              { 
                ins = data.ins;
                mems = make_pre data.ins outs locs;
                outs = outs
              }
            in
            prog, ctrl, data
          )
      )

(* CPS version of step *)
let (step_rdbg: ctx -> string -> t -> control_state -> data_state ->
     (ctx -> t -> control_state -> data_state -> e) -> e) =
  fun ctx node t ctrl data cont -> 
  let t = { t with snt = Bddd.clear t.snt } in
  let datal =
    (List.map (fun (n,v) -> n, Value.to_data_val v) (Value.OfIdent.content data.ins)) @
      (List.map (fun (n,v) ->
                 "pre_" ^ n, Value.to_data_val v) (Value.OfIdent.content data.mems))
  in
  let ctx_save = ctx in
  let ctx = { ctx with 
              RdbgEvent.name = node;
              RdbgEvent.depth = ctx.RdbgEvent.depth+1;
              RdbgEvent.data = datal;
              RdbgEvent.inputs = List.map to_event_var (in_var_list t);
              RdbgEvent.outputs = List.map to_event_var (out_var_list t);
            } 
  in
    let cont2 = fun ctx2 t bg -> 
    match bg with
    | NoMoreBehavior _i -> assert false
    | SomeBehavior (b, _bg') -> (
      match b with               
      | Vanish -> failwith "Normal termination"
      | Raise str -> failwith str
      | Goto (zeguard, ctrl) -> (
        (* THIS IS THE NORMAL BEHAVIOR *)
        let t, zeguard, (outs, locs) = 
          try find_one_sol t zeguard
          with Not_found -> assert false
        in
        let data = 
          { 
            ins = data.ins;
            mems = make_pre data.ins outs locs;
            outs = outs
          }
        in
        let get_sl vars vals =
          List.map 
            (fun x ->
             Var.name x,
             Value.to_data_val (Value.OfIdent.get vals (Var.name x))
            )
            vars
        in
        let edata = 
          try get_sl (in_var_list t) data.ins
          with Not_found -> [] (* at first step *) 
        in
        let edata = edata @ (get_sl (out_var_list t) outs) in
        let edata = edata @ (get_sl (loc_var_list t) locs) in
        let ctx2 = event_incr ctx2 t.arg_opt in
        let ctx2 = { ctx_save with
            (* once we exit, we return back to the previous ctx *)
          RdbgEvent.nb = ctx2.RdbgEvent.nb;
          RdbgEvent.data = edata; (* used? *)
          RdbgEvent.depth = ctx.RdbgEvent.depth -1
        }
        in
        let si_atoms = List.map to_src_info zeguard.g_src in
        let cstr = Exp.to_expr zeguard.g_form in
        {  ctx with
          RdbgEvent.step = ctx.RdbgEvent.step;
          RdbgEvent.nb = ctx2.RdbgEvent.nb-1;
          RdbgEvent.depth = ctx.RdbgEvent.depth;
          RdbgEvent.kind = RdbgEvent.Exit;
          RdbgEvent.lang = "lutin";
          RdbgEvent.name = node;
          RdbgEvent.inputs  = ctx.RdbgEvent.inputs;
          RdbgEvent.outputs = ctx.RdbgEvent.outputs;
          RdbgEvent.locals = []; (* fixme *)
          RdbgEvent.data = edata;
          
          RdbgEvent.sinfo = Some (fun () -> {
                                RdbgEvent.expr = cstr;
                                (* RdbgEvent.str  = guard_to_string zeguard; *)
                                RdbgEvent.more = None;
                                RdbgEvent.atoms = si_atoms;
                                RdbgEvent.in_subst = [];
                                RdbgEvent.out_subst = [];
                             });
          
          (*                    RdbgEvent.Exit (guard_to_string zeguard, cstr, lazy_si) *)
          RdbgEvent.next = (fun () -> cont ctx2 t ctrl data);
          RdbgEvent.terminate = ctx2.RdbgEvent.terminate;
          RdbgEvent.reset = ctx2.RdbgEvent.reset;
        }
      )
    )
  in
  let enb = ctx.RdbgEvent.nb in
  let ctx = event_incr ctx t.arg_opt in
  let d = ctx.RdbgEvent.depth in
  let ctx = { ctx with depth = ctx.RdbgEvent.depth+1 } in
  { ctx with 
    RdbgEvent.nb = enb;
    RdbgEvent.depth = d;
    RdbgEvent.kind = RdbgEvent.Call;
    RdbgEvent.lang = "lutin";
    RdbgEvent.name = node;
    RdbgEvent.next = (fun () ->
                  get_behavior_gen_ldbg t data.ins data.mems ctrl ctx cont2);
    RdbgEvent.sinfo = None; (* XXX fixme ? *)
  }


(***************************************************************************************)
(*** DEBUG ***)

let dump it = (
  Printf.printf "### DUMP LutExe ########################\n";
  Printf.printf "### EXPANDED CODE ######################\n";
  Expand.dump it.expanded_code;
  let var_details x = Var.to_string_verbose Exp.to_string x in
  let dump_var x = (
    Printf.printf "%s\n" (var_details x)
  ) in
    Printf.printf "### LUCKY INTERFACE ####################\n";
    Printf.printf "--- INPUT VARS -------------------------\n";
    List.iter dump_var it.in_vars ;
    Printf.printf "--- OUTPUT VARS ------------------------\n";
    List.iter dump_var it.out_vars ;
    Printf.printf "--- LOCAL VARS -------------------------\n";
    List.iter dump_var it.loc_vars ;
    Printf.printf "--- INITIAL PRE VALUES -----------------\n";
    Value.OfIdent.print it.init_pres stdout ;
    Printf.printf "--- IDENT 2 LUCKY VARS -----------------\n";
    let dump_id2var id x = (
      Printf.printf "%s -> (%s)\n" id (var_details x)
    ) in
      Util.StringMap.iter dump_id2var it.id2var_tab;
      Printf.printf "--- OUPUT IDENT LIST -------------------\n";
      Printf.printf "{ %s }\n" (String.concat "; " it.out_var_names);
      Printf.printf "--- AND OF CONTROLLABLE BOOL VARS ------\n";
      Printf.printf "%s\n" (Exp.formula_to_string it.bool_vars_to_gen);
      Printf.printf "--- LIST OF CONTROLLABLE NUM VARS ------\n";
      List.iter dump_var it.num_vars_to_gen ;
      Printf.printf "\n### END OF DUMP LutExe #################\n\n";
      flush stdout;
)




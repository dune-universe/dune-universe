(*----------------------------------------------------------
                 TYPE/BINDING CHECK 
------------------------------------------------------------
Table de symboles pour le check
---------------------------------------------------------------
---------------------------------------------------------------

C'est la structure qui permet :
- de réaliser le type/binding check (cf. CheckType)
- de conserver, après ce check, les infos calculées/inférées

Toute référence à un identificateur est identifiée
de manière UNIQUE par son ident (string + lexeme).

Au cours du check, on utilise TROIS tables :
- une table "dynamique" de scoping "string -> ident_info" 
  qui permet de résoudre les référence sous "scope"
- une table GLOBALE de binding "ident -> ident_info"
  remplie au fur et à mesure que les références sont résolue
- une table de typage des "val_exp -> CkTypeEff.t"
  n.b. les val_exp SONT identifiées de manière unique
  par leur lexeme

-------------------------------------------------------------*)

open Lexeme
open LutErrors
open Format
open Syntaxe
(* sous-modules *)
open CkIdentInfo


(*-------------------------------------------*)
(* CREATION DE L'ENVIRONNEMENT               *)
(*-------------------------------------------*)
(* Il contient :
   - une association (fluctuante au cours
   du check) ident -> info
   - une association globale entre les
   val_exp et leur CkTypeEff.t
   N.B. pour eviter tout problème de collision,
   on passe via le lexeme (par définition unique)
   asocié à la val_exp
*)

type lib_desc = {
   ld_ident: string;
   ld_path: string;
   ld_desc: Ezdl.t;
   (* something here *)   
}

type t = {
   ce_scope : (string, CkIdentInfo.t) Hashtbl.t ;
   ce_binding : (Syntaxe.ident, CkIdentInfo.t) Hashtbl.t;
   ce_typing : (Lexeme.t, CkTypeEff.t) Hashtbl.t;
   (* external libs are characterized 
      by an ident and a valid path
      the ident is actually the basename of the path ...
      "option" is important: if None, no verifications are made
      if Some, any externel ref will be checked (even if Some [] !)
   *)
   ce_libs : lib_desc list option;
}

let copy x = {
   ce_scope = Hashtbl.copy x.ce_scope;
   ce_binding = Hashtbl.copy x.ce_binding;
   ce_typing = Hashtbl.copy x.ce_typing;
   ce_libs = x.ce_libs;
}

(* création *)


let create : unit -> t = (
   fun () -> {
      ce_scope = Hashtbl.create 200;
      ce_binding = Hashtbl.create 200;
      ce_typing = Hashtbl.create 200;
      ce_libs = None;
   }
)

let add_libs t ll = (
   let check_lib l = (
      let dd = try (
         Ezdl.dlopen l
      ) with Failure msg -> raise (
         Global_error (
            Printf.sprintf "CheckEnv.add_libs: can't load \"%s\"\n  -> %s" l msg
         )
      ) in 
      {
         ld_ident = Filename.basename l;
         ld_path = l;
         ld_desc = dd;
      }
   ) in
   let cel = List.map check_lib ll in
   let cel' = match t.ce_libs with
   |   None -> cel
   |   Some x -> x@cel
   in
   {
      ce_scope = t.ce_scope;
      ce_binding = t.ce_binding;
      ce_typing = t.ce_typing;
      ce_libs = Some cel';
   }
)

(*********************************************)
(* INTEROGATION DU TYPING                    *)
(*********************************************)
(*
Ne doit être utilisé QU'APRES le type/binding check !
Toute erreur est forcément un BUG ! 
*)

let get_exp_type (env : t) (e : Syntaxe.val_exp)
   (* -> CkTypeEff.t *) 
= (
   try (
      Util.hfind env.ce_typing e.src
   ) with Not_found -> (
      raise (Internal_error
         ("CheckEnv.get_exp_type", "untyped exp"))
   )
)


(*********************************************
AJOUT DU TYPING 
Dans certains cas, une expression peut etre
partagee : cas typique de la valeur d'init
distribuee sur plusieurs vars !
*********************************************)
let set_exp_type (env : t) (ve: Syntaxe.val_exp) (tf: CkTypeEff.t) = (
  (* assert (not (Hashtbl.mem env.ce_typing ve.src)); *)
  try (
    let t1 = Util.hfind  env.ce_typing ve.src in
    if (t1 <> tf) then (
      let msg = Printf.sprintf "can't assign type '%s' to exp '%s', already typed with '%s'"
          (CkTypeEff.to_string tf) 
          (LutErrors.lexeme_details ve.src)
          (CkTypeEff.to_string t1)
      in
      LutErrors.print_internal_error "CheckEnv.set_exp_type" msg;
      exit 1
    )
  ) with Not_found -> 
    Hashtbl.add env.ce_typing ve.src tf
)

(*********************************************)
(* INTEROGATION DU BINDING                   *)
(*********************************************)
(*
Ne doit être utilisé QU'APRES le type/binding check !
Toute erreur est forcément un BUG ! 
*)

let get_binding (env : t) (id : Syntaxe.ident)
   (* -> CkIdentInfo.t *) 
= (
   try (
      Util.hfind env.ce_binding id
   ) with Not_found -> (
      raise (Internal_error
         ("CheckEnv.get_binding",
            "unbounded ident "^(Lexeme.to_string id.src)
         ))
   )
)


(*********************************************)
(* MODIFICATION DU SCOPE                     *)
(*********************************************)

(*
   Toutes modif DOIT passer par la fonction suivante
   qui gère les problèmes de collision : si l'ident
   est déjà "bindé" :
   - erreur si hideable = false
   - rien (ou warning ?) si hideable
*)
let put_in_scope (env: t) (id : Syntaxe.ident) (ii : CkIdentInfo.t) = (
   ( try (
      let ifo = Util.hfind env.ce_scope id.it in
      if (CkIdentInfo.is_hideable ifo) then (
         Hashtbl.add env.ce_scope id.it ii ;
      ) else (
         raise (Compile_error (id.src,
            "declaration conflicts with a global one"
         ))
      )
   ) with Not_found -> (
     Verbose.exe ~level:3
       (fun () -> 
         printf "Add %s to the current scope\n" id.it ;
         flush stdout);
     Hashtbl.add env.ce_scope id.it ii
     )
   )
(*
let bdgs = Util.hfind_all env.ce_scope id.it in
let pii ii = (printf "   %s\n" (CkIdentInfo.to_string ii)) in
printf "CURRENT SCOPE: %s = \n" id.it ;
List.iter pii bdgs
*)
)

(*
   clé pour la modif du scope : juste la liste des idents
*)
type scope_key = string list

let restore (env : t) (k : scope_key) = (
   let rm_scope s = (
      Hashtbl.remove env.ce_scope s
   ) in
   List.iter rm_scope k
)

(*---------------------------------------------------
Ajout d'une cste abstraite dans le scope  
=> global on n'a pas le droit de la redefinir
=> local on a le droit de la redefinir
---------------------------------------------------*)
let add_global_cst
   (env: t)
   (id: Syntaxe.ident)
   (te: CkTypeEff.t) =
(
   let ii = CkIdentInfo.of_global_cst id te in
   put_in_scope env id ii;
   [id.it]
)

let add_local_cst
   (env: t)
   (id: Syntaxe.ident)
   (te: CkTypeEff.t) =
(
(* printf "add_local_cst: src=%s\n" (Lexeme.to_string id.src); *)
   let ii = CkIdentInfo.of_local_cst id te in
   put_in_scope env id ii;
(* printf "add_local_cst: def=%s\n" (CkIdentInfo.to_string ii); *)
   [id.it]
)

(*---------------------------------------------------
Ajout de paramètres formels dans le scope
---------------------------------------------------*)
type typed_ids = (Syntaxe.ident * Syntaxe.type_exp) list
type eff_typed_ids = (Syntaxe.ident * CkTypeEff.t) list

let add_formal_params (env : t) (params : typed_ids option) = (
   match params with
      None -> []
   |   Some palist -> (
      let k = ref [] in
      let treat_param = function (i,t) -> (
         let s = i.it in
         let te = CkTypeEff.of_texp t in 
         let ii = CkIdentInfo.of_param i te in
         (* ICI => warning shadow au cas ou ? *)
         put_in_scope env i ii;
         k := s::!k
      ) in
      List.iter treat_param palist;
      (* retourne la clé (liste des entrées) *)
      !k
   )
)

(*---------------------------------------------------
Ajout de variables support dans le scope

ELLES SONT IMPLICITEMENT MISES EN REF

Le type checking est déjà fait -> t est un CkTypeEff.t 
---------------------------------------------------*)
let add_support_vars (env : t) (valist : eff_typed_ids) = (
   let k = ref [] in
   let treat_param = function (i,t) -> (
      let s = i.it in
      let te = CkTypeEff.ref_of t in
      let ii = CkIdentInfo.of_support i te in
      (* ICI => warning shadow au cas ou ? *)
      put_in_scope env i ii;
      k := s::!k
   ) in
   List.iter treat_param valist;
   (* retourne la clé (liste des entrées) *)
   !k
)
let add_support_profile 
   (env : t)
   (ins : eff_typed_ids)
   (outs : eff_typed_ids) =
(
   let ki = add_support_vars env ins in
   let ko = add_support_vars env outs in
   ki@ko
)

(*---------------------------------------------------
Ajout d'une macro dans le scope.
N.B. on distingue les macros avec 
  liste de param éventuellement vide
  et les alias qui n'ont pas du tout
  d'entrées (important pour l'expansion) 
---------------------------------------------------*)
 
let add_let
    (env: t)
    (li : Syntaxe.let_info)
    (tres : CkTypeEff.t)
    (id : Syntaxe.ident) = (*unit*)
  (
    (* inutile ? let def = li.lti_def in *)
    let ii = match li.lti_inputs with
        None -> CkIdentInfo.of_alias id tres li
      |   Some inlist -> (
          (* le profil ... *)
          let teff_of_param = ( function
                (_, te) -> CkTypeEff.of_texp te   
            ) in
          let tinlist = List.map teff_of_param inlist in
          let prof = CkTypeEff.get_prof tinlist [tres] in
          CkIdentInfo.of_macro id prof li 
        ) in
    (* la clé = le nom dde la macro *)
    let k = li.lti_ident in
    put_in_scope env k ii;
    [k.it] 
  )

let add_node
    (env: t)
    (ni : Syntaxe.node_info)
    (nprof : CkTypeEff.profile)
    (id : Syntaxe.ident) = (*unit*)
  (
    let ii = CkIdentInfo.of_node id nprof ni in
    (* la clé = le nom dde la macro *)
    let k = ni.ndi_ident in
    put_in_scope env k ii;
    [k.it] 
  )

(*---------------------------------------------------
Add extern def in the scope
-> if libs is Some thing, check
---------------------------------------------------*)

let get_in_libs
    (env: t)
    (id: Syntaxe.ident) = (* CkIdentInfo.extern_info option *)
  (
    let rec rec_get_in_libs ll = (
      match ll with
      | [] ->    raise (
          Compile_error (id.src,
                         "can't find any entry for external function in given lib list"
                        )   
        )
      | l::tl -> try (
        let sym = Ezdl.dlsym l.ld_desc id.it in
        (* returns a CkIdentInfo.extern_info *)
        {
          ed_lib_name = l.ld_ident ;
          ed_lib_desc = l.ld_desc ;
          ed_sym = sym;
        }
      ) with Failure _ -> rec_get_in_libs tl
    ) in
    match env.ce_libs with
    |   None -> None
    |   Some ll -> Some (rec_get_in_libs ll)
  )

let add_extern
    (env: t)
    (li : Syntaxe.let_info)
    (tres : CkTypeEff.t)
    (id : Syntaxe.ident) = (*unit*)
  (

    let ii = match li.lti_inputs with
        None -> (
          assert false
        )
      |   Some inlist -> (
          (* le profil ... *)
          let teff_of_param = ( function
                (_, te) -> CkTypeEff.of_texp te   
            ) in
          let tinlist = List.map teff_of_param inlist in
          (* MUST BE PURELY DATA *)
          let prof = (
            let res = CkTypeEff.get_prof tinlist [tres] in
            if (CkTypeEff.is_data_profile res) then res 
            else raise (
                Compile_error (id.src,
                               "invalid profile for external function"
                              )
              )
          ) in
          let de = get_in_libs env id in
          Verbose.exe ~level:3
            (fun () -> Printf.printf "CheckEnv.add_extern: \"%s\", profile \"%s\"   \n"
                (id.it)
                (CkTypeEff.prof_to_string prof))
          ;
          CkIdentInfo.of_extern id prof li de
        ) in
    (* la clé = le nom dde la macro *)
    (* check if k.it exists in some lib *)
    let k = li.lti_ident in
    put_in_scope env k ii;
    [k.it] 
  )

(*---------------------------------------------------
Ajout d'un op predef dans le scope 
---------------------------------------------------*)
let add_predef_op
   (env: t)
   (nme: string)
   (prof: CkTypeEff.profile) =
(
   let ii = CkIdentInfo.of_predef_op nme prof in
   put_in_scope env (Lexeme.flagit nme Lexeme.dummy) ii;
   [nme]
)

(*---------------------------------------------------
Ajout d'une cte predef dans le scope 
---------------------------------------------------*)
let add_predef_cst
   (env: t)
   (nme: string)
   (te: CkTypeEff.t) =
(
   let ii = CkIdentInfo.of_predef_cst nme te in
   put_in_scope env (Lexeme.flagit nme Lexeme.dummy) ii;
   [nme]
)

(*********************************************)
(* INTERROGATION DE L'ENVIRONNEMENT          *)
(* ET AJOUT (eventuel) D'UN BINDING          *)
(*********************************************)

let get_ident_info (env : t) (id : Syntaxe.ident) = (
  (* printf "add_binding: ref=%s\n" (Lexeme.to_string id.src); *)
  let s = id.it in
  let res = try (
    Util.hfind env.ce_scope s
  ) with Not_found -> (
      raise (Compile_error (id.src,"undeclared identifier"))
    ) in
  (* INSERTION/VERIF DU BINDING *)
  (try
     let expected = Util.hfind  env.ce_binding id in
     if (expected != res) then
       let msg = Printf.sprintf "binding error for lexeme %s\n  %s\n  %s"
           (Lexeme.to_string id.src)
           (CkIdentInfo.to_string res)
           (CkIdentInfo.to_string expected)
       in
       raise (Internal_error
                ("CheckEnv.get_ident_info", msg))
     else ()
   with Not_found -> 
     Hashtbl.add env.ce_binding id res
  );
  (*printf "add_binding: def=%s\n" (CkIdentInfo.to_string res);*)
  res
)

let nature_of_ident (env: t) (i: Syntaxe.ident) = (
  let ii = get_ident_info env i in
  CkIdentInfo.get_nature ii
)

let type_of_ident (env : t) (i : Syntaxe.ident) = (
  let ii = get_ident_info env i in
  let res = CkIdentInfo.get_type ii in
  (*DBG
    printf "type_of_ident(%s) = %s\n" i.it (string_of_teff res) ;
  *)
  res
)

(* dump pour debug *)
let dbg_dump (env : t) = (
  let dump_scope k i = (
    printf "scope_key: %s " k;
    printf " => %s\n" (CkIdentInfo.to_string i)
  ) in
  printf "SCOPE TABLE\n";
  Hashtbl.iter dump_scope env.ce_scope;
  let dump_binding id i = (
    printf "ident ref: %s " (Lexeme.to_string id.src);
    printf " => %s\n" (CkIdentInfo.to_string i)
  ) in
  printf "BINDING TABLE\n";
  Hashtbl.iter dump_binding env.ce_binding 
)


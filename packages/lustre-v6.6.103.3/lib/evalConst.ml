(* Time-stamp: <modified the 29/08/2019 (at 15:34) by Erwan Jahier> *)


open Printf 
open Lxm
open Lv6errors
open Lic
open IdSolver
open AstCore 
open LicEvalConst
open LicEvalType

let dbg = (Lv6Verbose.get_flag "eval-const")

(*----------------------------------------------------
EvalArray_error :
        - levée par les fonctions dédiées aux tableaux
----------------------------------------------------*)
exception EvalArray_error of string

(*----------------------------------------------------
EvalConst_error :
  - levée localement dans les sous-fonctions,
  - captée dans EvalConst.f et tranformée en Compile_error.
----------------------------------------------------*)

let finish_me msg = print_string ("\n\tXXX evalConst.ml:"^msg^" ->  finish me!\n")
      
let not_evaluable_construct str =
  raise (EvalConst_error(
           Printf.sprintf "The construct %s is not allowed in static expression" 
             str))

(*----------------------------------------------------
Utilitaire :
extraire une tranche de tableau

N.B. first_ix  last_ix step et width sont supposés 
venir de eva et donc être corrects

N.B. Puisque correct, last_ix est inutile, mais bon ...
-----------------------------------------------------*)
let rec fill init size = if size = 0 then [] else init::(fill init (size-1)) 
let _ = assert (fill 0 5 = [0;0;0;0;0])

let (make_slice_const : 
       Lic.const list -> Lic.type_ -> Lic.slice_info -> Lic.const list) =
  fun clist ctype slice ->
    let sliced_clist,_,_ = 
      List.fold_left 
        (fun (acc, i, j) elt ->
           if i = slice.se_first + j*slice.se_step then
             (elt::acc, i+1, j+1)
           else 
             (acc, i+1, j)
           )
        ([], 0, 0)
        clist
    in
    let sliced_clist = List.rev sliced_clist in
      [Array_const_eff(sliced_clist, ctype)]

(** Utilitaire : fabriquer si possible une constante tableau *)
let (make_array_const : Lic.const list list -> Lic.const) =
  fun ops -> 
    let expected_type = ref None in
    let treat_arg : Lic.const list -> Lic.const =
      fun op -> 
        match op with
          | [x] -> (
            (* non tuple *)
            let xtyp = Lic.type_of_const x in
            match (!expected_type) with
              | None -> expected_type := Some xtyp; x
              | Some t -> (
                if (t = xtyp) then x else 
                  raise (EvalConst_error(
                    "type error in array, "^
                      (Lic.string_of_type xtyp)^
                      " mixed with " ^ (Lic.string_of_type t)
                  ))
              )
          )
          | _  ->  (* tuple *)
            raise (EvalConst_error("array of tuple not allowed"))
    in 
    let res = List.map treat_arg ops in
    match (!expected_type) with
      | None -> raise (EvalConst_error("empty array"))
      | Some t -> Array_const_eff(res, t)

(** Utilitaire : fabriquer si possible une constante structure 

N.B. Par construction on sait que arg_tab n'a pas de doublons
*)
let make_struct_const (teff : Lic.type_) (id_opt : Lv6Id.idref option)
    (arg_tab : (Lv6Id.t, Lxm.t * Lic.const) Hashtbl.t) =
  (* on verifie qu'on a bien un type struct *)
  (match teff with
      Struct_type_eff (tnm, flst) -> (
        let make_eff_field ((fn: Lv6Id.t),((ft:Lic.type_),(fv:Lic.const option))) =
          (* on construit la liste dans le BON ordre *)
          try
            (* on prend en priorité dans arg_tab *)
            let lxm, v = Hashtbl.find arg_tab fn in
            (* effet de bord : on vire la valeur de arg_tab *)
            Hashtbl.remove arg_tab fn ;
            let vt = Lic.type_of_const v in
            if (vt = ft) then (fn, v) (*ok*)
            else raise (Compile_error(
              lxm , 
              sprintf "\n*** type error in struct %s, %s instead of %s"
                (Lv6Id.string_of_long false tnm)
                (Lic.string_of_type vt)
                (Lic.string_of_type ft)  )) 
          with Not_found ->
            (* sinon la valeur par défaut *)
            (match fv,id_opt with
              | Some v,_ -> (fn, v) (* ok : v correcte par construction *)
              | None,Some _ ->
                finish_me " eval const with 'with'";
                assert false
              | None,None -> 
                raise (EvalConst_error(
                  sprintf "bad struct expression, no value given for field %s" 
                    (Lv6Id.to_string fn)))
            )
        in
        (* on mappe flst pour avoir la liste dans le bon ordre *)
        let eff_fields = List.map make_eff_field flst in
        (* si arg_tab n'est pas vide, erreur sur le premier *) 
        let raise_error (id : Lv6Id.t) ((lxm : Lxm.t), (_veff : Lic.const)) 
            = raise(Compile_error(
              lxm, sprintf "\n*** %s is not a field of struct %s" 
                (Lv6Id.to_string id) 
                (Lic.string_of_type(teff))))
        in
        Hashtbl.iter raise_error arg_tab; (* ok : tout s'est bien passé ! *)
        Struct_const_eff (eff_fields, teff)
      )
    | _ -> raise (EvalConst_error(
      sprintf "struct type expected instead of %s" (Lic.string_of_type teff)
    )
    )
  )

(*----------------------------------------------------
        Evaluation récursive des expressions constantes
------------------------------------------------------
f :
        - entrées :  IdSolver.t et val_exp
        - sortie :        Lic.const list
        - Lic.t de bord : Compile_error 
Rôle :
        -> résoud les références aux idents
        -> gère les appels récursifs (évaluation des arguments) 
----------------------------------------------------*)
let rec f
    (env : IdSolver.t) 
    (vexp : val_exp)
    =
(
  (*-----------------------------------
    fonction récursive principale
    -> capte les nv
    -> récupère les EvalConst_error 
    -----------------------------------*)
  let rec rec_eval_const (vexp : AstCore.val_exp) : Lic.const list = (
    match vexp with
      | AstCore.CallByPos ({it=posop; src=lxm}, Oper args) -> (
        try eval_by_pos_const posop lxm args
        with
          | EvalType_error msg -> 
            raise (Compile_error(lxm, "type error: "^msg))
          | EvalConst_error msg ->
            raise (Compile_error(lxm, "can't eval constant: "^msg))
      )
      | AstCore.CallByName ({it=nmop; src=lxm}, nmargs ) -> (
        try eval_by_name_const nmop lxm nmargs
        with EvalConst_error msg ->
          raise (Compile_error(lxm, "can't eval constant: "^msg))
      )
      | Merge_n (_,_) 
      | Merge_bool_n (_,_,_) -> 
        finish_me "merge"; 
        assert false

  )
  (*-----------------------------------
    fonction récursive secondaire
    eval. exp classique (by pos)
    N.B. On distingue les opérations 
    classiques (avec extention tableau
    implicie) des autres. Ici, on traite
    toutes les opérations non classiques. 
    -----------------------------------*)
  and eval_by_pos_const
      (posop : by_pos_op)   (* l'operateur *)
  (lxm : Lxm.t)      (* source de l'opérateur *)
  (args : val_exp list) (* arguments *)
      = (
    match (posop) with 
        (* capte les idents de constantes *)
        IDENT_n  id  -> (
          (* 2007-07 on interdit les externes *)
          match (env.id2const id lxm) with
            | Abstract_const_eff(_,_, const_eff, true) -> [const_eff]
            | Abstract_const_eff(_,_,_,false) -> 
              raise (EvalConst_error(
                sprintf "\n*** cannot access this abstract constant value"))
            | Extern_const_eff(_,_) -> 
              raise (EvalConst_error(
                sprintf "\n*** cannot access this extern constant value"))
            | x -> [ x ]
        )
      (* opérateur lazzy *)
      | WITH_n(a0,a1,a2) -> (
        match (rec_eval_const a0) with
            [ Bool_const_eff true] -> rec_eval_const a1
          | [ Bool_const_eff false] -> rec_eval_const a2
          | x -> type_error_const x "bool"
      )
      (* mettre à plat la liste des args *)
      | TUPLE_n -> ( List.flatten (List.map rec_eval_const args))
      (* les tableaux de tuples sont interdits *)
      | HAT_n -> (
        match args with
          | [cexp; szexp] -> (
            try
              let sz = eval_array_size env szexp in
              match rec_eval_const cexp with
                | [cst] ->
                  let l = fill cst sz in
                  [ Array_const_eff (l, Lic.type_of_const cst) ]
                | _x -> 
                  raise (EvalConst_error("array of tuple not allowed"))
            with 
                EvalArray_error msg -> raise(EvalConst_error msg)
          ) 
          | _ -> raise(EvalConst_error
                         (sprintf "arity error: 2 expected instead of %d" 
                            (List.length args)))
      )
      | CONCAT_n -> (
        let ops = (List.map rec_eval_const args) in
        match ops with
          | [[Array_const_eff (v0, t0)];
             [Array_const_eff (v1, t1)]] -> (
            if(t0 = t1) then 
              [Array_const_eff (List.append v0 v1, t0)]  
            else 
              raise(EvalConst_error(
                sprintf 
                  "\n*** type combination error, can't concat %s with %s"
                  (Lic.string_of_type(t0)) 
                  (Lic.string_of_type(t1)) 
              ))
          )
          | [_;_] -> 
            raise(EvalConst_error(
              "type combination error, array type expected"))
          | _ -> raise(EvalConst_error
                         (sprintf "arity error: 2 expected instead of %d" 
                            (List.length ops)))
      )
      | ARRAY_n -> (
        let ops = (List.map rec_eval_const args) in
        [make_array_const (ops)]
      )
      | ARRAY_ACCES_n ix -> (
        let effargs = List.flatten (List.map rec_eval_const args) in
        match effargs with
          | [Array_const_eff (elts, _typelts)] -> (
            try
              let sz = List.length elts in
              let effix = eval_array_index env ix lxm in
              let _ = if effix > sz then 
                  raise(EvalType_error(
                    sprintf "array index %d out of bounds 0..%d" 
                      effix (sz-1)))
              in
              [List.nth elts effix]
            with EvalArray_error msg -> raise(EvalConst_error msg)
          )
          |  _  -> type_error_const effargs "some array"
      )
      | ARRAY_SLICE_n sl -> (
        let (elts, typelts) =
          match List.flatten (List.map rec_eval_const args) with
            | [Array_const_eff (l, t)] -> (l, t) 
            | x -> type_error_const x "some array"
        in
        (* évalue la slice *)
        try
          let sliceff = eval_array_slice env sl lxm in
          make_slice_const elts typelts sliceff
        with
            EvalArray_error msg -> raise(EvalConst_error msg)
      ) 

      | STRUCT_ACCESS_n fid -> 
        let ceff_list = List.flatten (List.map rec_eval_const args) in
        (match ceff_list with 
          | [Struct_const_eff (flst, typ)] -> (
            try [(List.assoc fid flst)]
            with Not_found -> 
              raise (EvalConst_error
                       (Printf.sprintf "%s is not a field of struct %s" 
                          (Lv6Id.to_string fid) 
                          (Lic.string_of_type(typ))))
          )
          | [x] -> type_error_const [x] "struct type"
          | x -> arity_error_const x "1"
        )

      | CALL_n _ -> not_evaluable_construct "node call"
      | WHEN_n _ -> not_evaluable_construct "when"
      | FBY_n -> not_evaluable_construct "fby"
      | ARROW_n -> not_evaluable_construct "->"
      | CURRENT_n -> not_evaluable_construct "current"
      | PRE_n -> not_evaluable_construct "pre"

      | Predef_n(op) -> 
        let effargs =  (List.map rec_eval_const args) in
        LicEvalConst.f env op.it lxm [] effargs
          
          
  ) (* FIN DE : eval_by_pos_const *)
  (*-------------------------------------*)
  (* Fonction récursive secondaire       *)
  (*-------------------------------------*)
  (* -> Eval. d'une expression spéciale  *)
  (* "par nom"                           *)
  (*-------------------------------------*)
  and eval_by_name_const
      (namop : by_name_op)   (* l'operateur *)
      (lxm : Lxm.t)      (* source de l'opérateur *)
      (namargs : (Lv6Id.t srcflagged * val_exp) list) (* arguments *)
      = (
    let arg_tab = Hashtbl.create 50 in 
    let treat_one_arg opid ((pid:Lv6Id.t srcflagged), (pexp:val_exp)) =
      if Hashtbl.mem arg_tab pid.it
      then
        raise(EvalConst_error(
          sprintf "multiple definition of param %s in %s call"
            (Lv6Id.to_string pid.it) (Lv6Id.string_of_idref false opid)))
      else 
        let v = rec_eval_const pexp in
        match v with
          | [x] -> Hashtbl.add arg_tab pid.it (pid.src, x)
          | _ -> 
            raise(EvalConst_error(
              sprintf "unexpected tuple value for param %s in %s call"
                (Lv6Id.to_string pid.it) (Lv6Id.string_of_idref false opid)))
    in
    match namop with
      | STRUCT_anonymous_n -> finish_me "anonymous struct"; assert false
          
      (* effet de bord : on tabule les parametres effectifs *)
      | STRUCT_n opid -> (
        List.iter (treat_one_arg opid) namargs ;
        (* pour l'instant, on ne traite que les constructions de struct *)
        let teff = env.id2type opid lxm in
        [make_struct_const teff None arg_tab]
      )
      | STRUCT_WITH_n (opid,opid2) -> (
        List.iter (treat_one_arg opid) namargs ;
        let teff = env.id2type opid lxm in
        [make_struct_const teff (Some opid2) arg_tab]
      )
  ) (* FIN DE : eval_by_name_const *)
  (*-------------------------------------*)
  (* Corps de la fonction principale      *)
  (*-------------------------------------*)
  in 

   Lv6Verbose.exe ~flag:dbg (fun () ->
      let lxm = lxm_of_val_exp vexp in
      Dbg.pf "#CALL EvalConst.f '";
      Dbg.p_val_exp vexp;
      Dbg.pf "' %s\n" (Dbg.s_lxm lxm);
   );
   let res = rec_eval_const vexp in
   Lv6Verbose.exe ~flag:dbg (fun () ->
      let _lxm = lxm_of_val_exp vexp in
      Dbg.pf "#RET  EvalConst.f '";
      Dbg.p_val_exp vexp;
      Dbg.pf " = %s\n" (Dbg.s_const_eff_list res)
   );
   res
) (* fin de f *)

(*---------------------------------------------------------------------
  eval_array_size
  -----------------------------------------------------------------------
  Rôle : calcule une taille de tableau 

  Entrées: 

  Sorties :
  int (strictement positif)

  Lic.ts de bord :
  EvalArray_error "bad array size, type int expected but get <t>" si t pas int
  EvalArray_error "bad array size <n>" si n <= 0
  ----------------------------------------------------------------------*)
and (eval_array_size: IdSolver.t -> val_exp -> int) =
  fun id_solver szexp -> 
    match (f id_solver szexp) with
      | [Int_const_eff sz] -> 
        let sz = int_of_string sz in
        if (sz >= 0) then sz else
          raise(EvalArray_error(sprintf "bad array size %d" sz))
      | [x] -> 
        raise(EvalArray_error(sprintf  "bad array size, int expected but get %s"
                                (Lic.string_of_type(Lic.type_of_const x)))) 
      | _ -> 
        raise(EvalArray_error(sprintf "bad array size, int expected, not a tuple"))
          
(*---------------------------------------------------------------------
  eval_array_index
  -----------------------------------------------------------------------
  Rôle :

  Entrées :
  id_solver, val_exp, taille du tableau

  Sorties :
  int (entre 0 et taille du tableau -1
  
  Lic.ts de bord :
  EvalArray_error msg si pas bon
  ----------------------------------------------------------------------*)
and eval_array_index
    (env : IdSolver.t)
    (ixexp : val_exp)
    (lxm : Lxm.t)
    = 
  try 
    (
      match (f env ixexp) with
        | [Int_const_eff i] 
        | [Abstract_const_eff(_,_, (Int_const_eff i), true)] -> int_of_string i 
        | [Abstract_const_eff(id,_,_,false)] ->
          raise(EvalArray_error("The const " ^ (Lv6Id.string_of_long false id) ^ 
                                   " is abstract"))
        | [Extern_const_eff(id,_)]  ->
          raise(EvalArray_error("The const " ^ (Lv6Id.string_of_long false id) ^ 
                                   " is extern"))
        | [x] -> raise(EvalArray_error(sprintf 
                                         "bad array index, int expected but get %s"
                                         (Lic.string_of_type(Lic.type_of_const x)))
        ) 
        | _ -> raise(EvalArray_error(
          sprintf "bad array index, int expected but get a tuple"))

    )
  with 
      EvalArray_error msg -> 
        raise (Compile_error(lxm, "can't eval constant: "^msg))
          
(*  and check_int i sz = *)
(*     if ((i >= 0) && (i < sz)) then i *)
(*     else raise(EvalArray_error( *)
(*                  sprintf "array index %d out of bounds 0..%d" i (sz-1))) *)

(*---------------------------------------------------------------------
  eval_array_slice
  -----------------------------------------------------------------------
  Rôle :

  Entrées :
  IdSolver.t, slice_info, size du tableau,
  lxm (source de l'opération slice pour warning)
  Lic.Sor :
  slice_info_eff, i.e.
  (fisrt,last,step,width) tels que step <> 0 et
  - si step > 0 alors 0<=first<=last<=sz
  - si step < 0 alors 0<=last<=first<=sz
  - 1<=width<=sz 
  Lic.ts de bord :
  EvalArray_error msg si pas bon
  ----------------------------------------------------------------------*)
and eval_array_slice (env : IdSolver.t) (sl : slice_info) (lxm : Lxm.t) =
  try
    let first_ix = eval_array_index env sl.si_first lxm in 
    let last_ix = eval_array_index env sl.si_last lxm in
    let step =
      match sl.si_step with
        | Some stepexp -> (
          match (f env stepexp) with
            | [Int_const_eff s] ->  int_of_string s (* ok *)                    
            | [x] -> raise(EvalArray_error(
              sprintf  "bad array step, int expected but get %s"
                (Lic.string_of_type (Lic.type_of_const x)))) 
            | _ -> raise(EvalArray_error(
              sprintf "bad array step, int expected but get a tuple"))
        )
        | None -> if (first_ix <= last_ix) then 1 else -1
    in
    if
      (step = 0) 
      || ((step > 0) && (first_ix > last_ix))
      || ((step < 0) && (first_ix < last_ix))
    then
      let msg = sprintf "bad array slice [%d..%d] step %d" first_ix last_ix step in
      raise (EvalArray_error msg)
    else
      (* index relatif du dernier *)
      let last_rel = abs (last_ix-first_ix) in
      let abs_step = abs step in
      (* le dernier est-il pris dans la tranche ? *)
      if ((last_rel mod abs_step) <> 0) then
        warning lxm (sprintf "last index out of slice [%d..%d step %d]" 
                       first_ix last_ix step);
      let width = 1 + last_rel/abs_step in
      (* on force le dernier a être dans la tranche *)
      let real_last_ix = first_ix + (width-1) * step in
      (* (first_ix,last_ix,step,width) *)
      {
        se_first = first_ix;
        se_last = real_last_ix;
        se_step = step;
        se_width = width
      }
  with 
      EvalArray_error msg -> 
        raise (Compile_error(lxm, "can't eval constant: "^msg))

(* Time-stamp: <modified the 29/08/2019 (at 16:16) by Erwan Jahier> *)

open AstPredef
open Lic

(* open Lxm *)
(* open Lv6errors *)

type const_evaluator = Lic.const evaluator
   
exception EvalConst_error of string

let eval_real_error () =
  raise (EvalConst_error ("expression involving reals are not evaluated\n***"^
           "                      to avoid semantics issues, sorry."))

(* exported *)
let (type_error_const : Lic.const list -> string -> 'a) =
  fun _v expect -> 
    raise (EvalConst_error(
             "type mismatch "^(if expect = "" then "" else (expect^" expected"))))
      
let soi = string_of_int
let ios = int_of_string

let (arity_error_const : Lic.const list -> string -> 'a) =
  fun v expect -> 
    raise (EvalConst_error(
      Printf.sprintf "\n*** arity error : %d argument%s, whereas %s were expected"
        (List.length v) (if List.length v>1 then "s" else "") expect))
    

let (bbb_evaluator:(bool -> bool -> bool) -> const_evaluator) =
  fun op -> fun ll -> match List.flatten ll with
    | [Bool_const_eff v0; Bool_const_eff v1] -> [Bool_const_eff (op v0 v1)]
    | _ -> assert false (* should not occur because eval_type is called before *)
          
let (ooo_evaluator:(int -> int -> int) -> (float -> float -> float) -> 
     const_evaluator) =
  fun opi _opr -> fun ll -> match List.flatten ll with
    | [Int_const_eff v0; Int_const_eff v1] -> [Int_const_eff (soi (opi (ios v0) (ios v1)))]
    | [Real_const_eff _v0; Real_const_eff _v1] -> eval_real_error ()
    (* [Real_const_eff (opr v0 v1)] *)
    | _ -> assert false (* should not occur because eval_type is called before *)

let (iii_evaluator:(int -> int -> int) -> const_evaluator) =
  fun op -> fun ll -> match List.flatten ll with
    | [Int_const_eff v0; Int_const_eff v1] -> [Int_const_eff (soi (op (ios v0) (ios v1)))]
    | _ -> assert false (* should not occur because eval_type is called before *)

let (aab_evaluator:('a -> 'a -> bool) -> const_evaluator) =
  fun op -> fun ll -> match List.flatten ll with
    | [v0; v1] -> [Bool_const_eff (op v0 v1)]
    | _ -> assert false (* should not occur because eval_type is called before *)

let (fff_evaluator:(float -> float -> float) -> const_evaluator) =
  fun _op -> fun ll -> match List.flatten ll with
    | [Real_const_eff _v0; Real_const_eff _v1] -> eval_real_error ()
(*         [Real_const_eff (op v0 v1)] *)
    | _ -> assert false (* should not occur because eval_type is called before *)
        
let (bb_evaluator:(bool -> bool) -> const_evaluator) =
  fun op -> fun ll -> match List.flatten ll with
    | [Bool_const_eff v0] -> [Bool_const_eff (op v0)]
    | _ -> assert false (* should not occur because eval_type is called before *)
        
let (ii_evaluator:(int -> int) -> const_evaluator) =
  fun op -> fun ll -> match List.flatten ll with
    | [Int_const_eff v0] -> [Int_const_eff (soi (op (ios v0)))]
    | _ -> assert false (* should not occur because eval_type is called before *)
        

let (_uminus_evaluator:const_evaluator) =
  fun ll -> match List.flatten ll with
    | [Real_const_eff v0] -> [Real_const_eff ("-"^v0)] (* touch it a less as possible *)
    | _ -> assert false (* should not occur because eval_type is called before *)
        
let (uminus_evaluator:const_evaluator) =
  fun ll -> match List.flatten ll with
    | [Int_const_eff  v0] -> [Int_const_eff  ("-" ^ v0)]
    | [Real_const_eff v0] -> [Real_const_eff ("-" ^ v0)]
    | _ -> assert false (* should not occur because eval_type is called before *)
        
let (sf_evaluator: Lv6Id.t -> const_evaluator) =
  fun id _ceff_ll ->  [Real_const_eff (Lv6Id.to_string id)]

let (si_evaluator: Lv6Id.t -> const_evaluator) =
  fun id _ceff_ll -> 
    try let v = (Lv6Id.to_string id) in
        [Int_const_eff v]
    with Failure _ -> 
      raise (EvalConst_error(
        Printf.sprintf "\n*** fail to convert the string \"%s\" into an int"
          (Lv6Id.to_string id)))

let (sb_evaluator: bool -> const_evaluator) =
  fun v _ceff_ll -> 
      [Bool_const_eff v]

let (fi_evaluator:(string -> int) -> const_evaluator) =
  fun op -> fun ll -> match List.flatten ll with
     | [Real_const_eff v0] ->  [Int_const_eff (soi (op v0))] 
    | _ -> assert false (* should not occur because [eval_type] is called before *)

let (if_evaluator: (int -> string) -> const_evaluator) =
  fun op -> fun ll -> match List.flatten ll with
    | [Int_const_eff v0] -> [Real_const_eff (op (ios v0))]
    | _ -> assert false (* should not occur because [eval_type] is called before *)
        
let (ite_evaluator : const_evaluator) =
  function
    | [[Bool_const_eff c]; t; e] -> if c then t else e
    | _ -> assert false (* should not occur because [eval_type] is called before *)

let (boolred_evaluator : int -> int -> const_evaluator) =
  fun min max ceff_ll ->
    let nb = List.fold_left
      (fun acc -> function
         | (Bool_const_eff b) -> if b then acc+1 else acc | _ -> assert false)
      0
      (List.flatten ceff_ll)
    in
      [Bool_const_eff (min <= nb && nb <= max )]
      
         
(* exported *)
let f
   (id_solver: IdSolver.t)
   (op: op)
   (lxm: Lxm.t)
   (_sargs: Lic.static_arg list)
: const_evaluator = fun ll -> 
    (* we first check the type so that we do not need to check it during the const
       evaluation *)
    ignore (LicEvalType.f id_solver op lxm (List.map (List.map Lic.type_of_const) ll));
    match op with
      | TRUE_n  -> sb_evaluator true ll
      | FALSE_n -> sb_evaluator false ll
      | ICONST_n id -> si_evaluator id ll
      | RCONST_n id -> sf_evaluator id ll
      | NOT_n -> bb_evaluator (not) ll
      | REAL2INT_n -> fi_evaluator int_of_string ll
      | INT2REAL_n -> if_evaluator string_of_int ll
      | AND_n  -> bbb_evaluator (&&) ll
      | OR_n   -> bbb_evaluator (||) ll
      | IMPL_n -> bbb_evaluator (fun a b -> (not a) || b) ll
      | EQ_n   -> aab_evaluator (=) ll
      | NEQ_n  -> aab_evaluator (<>) ll 
      | LT_n  | ILT_n  | RLT_n   -> aab_evaluator (<) ll
      | LTE_n | ILTE_n | RLTE_n  -> aab_evaluator (<=) ll
      | GT_n  | IGT_n  | RGT_n   -> aab_evaluator (>) ll
      | GTE_n | IGTE_n | RGTE_n  -> aab_evaluator (>=) ll
      | DIV_n  -> iii_evaluator (/) ll
      | MOD_n  -> iii_evaluator (mod) ll
      | IF_n -> ite_evaluator ll
      | UMINUS_n -> uminus_evaluator ll
      | MINUS_n -> ooo_evaluator (-) (-.) ll
      | PLUS_n  -> ooo_evaluator (+) (+.) ll
      | SLASH_n -> ooo_evaluator (/) (/.) ll
      | TIMES_n -> ooo_evaluator ( * ) ( *.) ll
      | IUMINUS_n  -> ii_evaluator (fun x -> -x) ll
      | IMINUS_n -> iii_evaluator (-) ll
      | IPLUS_n  -> iii_evaluator (+) ll
      | ISLASH_n -> iii_evaluator (/) ll
      | ITIMES_n -> iii_evaluator ( * ) ll
      | RUMINUS_n -> uminus_evaluator ll
      | RMINUS_n -> fff_evaluator (-.) ll
      | RPLUS_n  -> fff_evaluator (+.) ll
      | RSLASH_n -> fff_evaluator (/.) ll
      | RTIMES_n -> fff_evaluator ( *.) ll
      | NOR_n   -> boolred_evaluator 0 0 ll
      | DIESE_n -> boolred_evaluator 0 1 ll
      | XOR_n  ->  boolred_evaluator 1 1 ll
(*
      | CondAct -> assert false
      | Map -> assert false
      | Fill -> assert false
      | Red -> assert false
      | FillRed -> assert false
      | BoolRed -> 
          match sargs with 
            | [ConstStaticArgLic(_,Int_const_eff i);
               ConstStaticArgLic(_,Int_const_eff j);
               ConstStaticArgLic(_,Int_const_eff n)
              ] -> 
                boolred_evaluator i j ll

*)


(*********************************************************************************)
(*********************************************************************************)
(* 
pour evaluer l'égalité, Pascal faisait comme ca (j'ai été plus (trop ?) brutal) :
      (*----------------------------
        Calcul de l'égalité
        N.B. Sur les constantes abstraites
        on est très méfiant
        N.B. Sur les types structure,
        on fait des appels récursifs
        ----------------------------*)
    let rec compute_eq 
        (args : const_eff list)
        = (
          let rec fields_eq f0 f1 = (
            match (f0, f1) with
              | ([], []) -> 
                  [Bool_const_eff true]
                 
              | ((f0,h0)::t0, (f1,h1)::t1) -> (
                  assert (f0 = f1);
                  match (compute_eq [h0;h1]) with
                      [Bool_const_eff false] -> [Bool_const_eff false]
                    | [Bool_const_eff true] -> (fields_eq t0 t1) 
                    | _ -> assert false
                )
              | _ -> assert false
          ) 
          in
            match args with
                [Bool_const_eff v0; Bool_const_eff v1] -> [Bool_const_eff (v0 = v1)]
              | [Int_const_eff v0; Int_const_eff v1] -> [Bool_const_eff (v0 = v1)]
              | [Real_const_eff v0; Real_const_eff v1] -> (
                  let res = (v0 = v1) in
                    warning src 
                      (sprintf "float in static exp: %f=%f evaluated as %b" v0 v1 res);
                    [Bool_const_eff res]
                )
                  (*
                    2007-07 obsolete

                    |  [Extern_const_eff (v0, t0); Extern_const_eff (v1, t1)] -> (
                    if (t0 <> t1) then (
                    type_error args "t*t for some type t"
                    ) else if (v0 <> v1) then (
                    uneval_error args (
                    sprintf "%s=%s (external constants)"
                    (string_of_fullid v0)
                    (string_of_fullid v1)
                    )
                    ) else (
                    [Bool_const_eff true]
                    )
                    )
                  *)
              | [Enum_const_eff (v0, t0); Enum_const_eff (v1, t1)] -> (
                  if (t0 = t1) then [Bool_const_eff (v0 =  v1)]
                  else type_error args "t*t for some type t"
                )
              | [Struct_const_eff (f0, t0); Struct_const_eff (f1, t1)] -> (
                  if (t0 = t1) then (fields_eq f0 f1)
                  else type_error args "t*t for some type t"
                )
              | [x;y] -> type_error args "t*t for some type t"
              | x -> arity_error args "2"
        ) 
    in
 *)

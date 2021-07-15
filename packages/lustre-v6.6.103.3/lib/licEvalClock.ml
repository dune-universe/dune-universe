(* Time-stamp: <modified the 29/08/2019 (at 16:07) by Erwan Jahier> *)

open AstPredef


(* exported *)
exception EvalClock_error of string


type clocker = UnifyClock.subst -> Lic.id_clock list list -> 
  Lic.id_clock list * UnifyClock.subst


(** A few useful clock profiles *)

let (constant_profile: string -> clocker) = 
  fun str s _ -> 
    let s, clk = UnifyClock.new_clock_var s in
      [Lv6Id.of_string str, clk], s

let (op_profile: clocker) =
  fun s cl -> 
    match cl with
      | clk::_ -> clk, s
      | [] -> assert false

      
let if_clock_profile _lxm s = 
  function
    | [_clk1; clk2; _clk3] -> clk2, s
    | _ -> assert false


(* This table contains the clock profile of predefined operators *)
let f
   (_id_solver: IdSolver.t)
   (op: op)
   (lxm: Lxm.t)
: clocker = fun s ->  
    match op with
      | TRUE_n | FALSE_n | ICONST_n _ | RCONST_n _ -> 
          constant_profile (AstPredef.op2string op) s

      | NOT_n | REAL2INT_n | INT2REAL_n | UMINUS_n | IUMINUS_n | RUMINUS_n
      | IMPL_n | AND_n | OR_n | XOR_n 
      | NEQ_n | EQ_n | LT_n | LTE_n | GT_n | GTE_n
      | ILT_n|ILTE_n|IGT_n|IGTE_n|RLT_n|RLTE_n|RGT_n|RGTE_n
      | MINUS_n  |  PLUS_n |  TIMES_n |  SLASH_n  
      | RMINUS_n | RPLUS_n | RTIMES_n | RSLASH_n  
      | DIV_n | MOD_n | IMINUS_n | IPLUS_n | ISLASH_n | ITIMES_n 
      | NOR_n | DIESE_n  
          -> op_profile s

      | IF_n                 -> if_clock_profile lxm s

    

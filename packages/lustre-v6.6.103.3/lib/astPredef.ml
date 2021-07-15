(* Time-stamp: <modified the 21/06/2017 (at 15:24) by Erwan Jahier> *)

(** Predefined operators Type definition *)

(* XXX shoud not type int, real, and bool be handled there ? *)

type op =   

(* zero-ary *)
  | TRUE_n

  | FALSE_n
  | RCONST_n of Lv6Id.t (* we don't want to touch reals! *)
  | ICONST_n of Lv6Id.t (* so we don't touch int either...*)
(* unary *)
  | NOT_n
  | REAL2INT_n
  | INT2REAL_n
(* binary *)
  | AND_n
  | OR_n
  | XOR_n
  | IMPL_n
  | EQ_n
  | NEQ_n
  | LT_n | LTE_n | GT_n | GTE_n
  | ILT_n | ILTE_n| IGT_n| IGTE_n 
  | RLT_n| RLTE_n| RGT_n| RGTE_n 
  | DIV_n
  | MOD_n
(* ternary *)
  | IF_n
(* n-ary *)
  | NOR_n
  | DIESE_n

(* overloaded operator *)
  | UMINUS_n
  | MINUS_n
  | PLUS_n
  | SLASH_n
  | TIMES_n

(* un-overloaded operator *)
  | IUMINUS_n
  | IMINUS_n
  | IPLUS_n
  | ISLASH_n
  | ITIMES_n

  | RUMINUS_n
  | RMINUS_n
  | RPLUS_n
  | RSLASH_n
  | RTIMES_n

let all_op = [ 
  NOT_n;  REAL2INT_n;  INT2REAL_n;  AND_n;  OR_n;  XOR_n;  IMPL_n;  
  EQ_n;  NEQ_n;  LT_n;  LTE_n;  GT_n;  GTE_n;  
  LT_n; LTE_n; GT_n; GTE_n ;
  ILT_n; ILTE_n; IGT_n; IGTE_n ;
  DIV_n;  MOD_n;  IF_n;  
  NOR_n;  DIESE_n;  UMINUS_n;  MINUS_n;  PLUS_n;  SLASH_n;  TIMES_n; 
  IUMINUS_n;  IMINUS_n;  IPLUS_n;  ISLASH_n;  ITIMES_n;  RUMINUS_n;  
  RMINUS_n;  RPLUS_n;  RSLASH_n;  RTIMES_n
]


(* can occur into an array iterator *)
(* GESTION DES OP PREDEF LAISSE A DESIRER *)
let iterable_op =  [ 
  NOT_n; REAL2INT_n; INT2REAL_n; AND_n; OR_n; XOR_n; IMPL_n;
  DIV_n; MOD_n; IUMINUS_n; IMINUS_n; IPLUS_n; ISLASH_n; ITIMES_n;
  RUMINUS_n; RMINUS_n; RPLUS_n; RSLASH_n; RTIMES_n ;
  UMINUS_n; MINUS_n; PLUS_n; SLASH_n; TIMES_n ; 
  EQ_n; NEQ_n; 
  LT_n; LTE_n; GT_n; GTE_n ;
  ILT_n; ILTE_n; IGT_n; IGTE_n ;
  RLT_n; RLTE_n; RGT_n; RGTE_n ;
  IF_n; 
 ]

(* iterators? *)

let op2string = function
  | TRUE_n -> "true"
  | FALSE_n -> "false"
  | ICONST_n id -> Lv6Id.to_string id
  | RCONST_n id -> Lv6Id.to_string id
  | NOT_n -> "not"
  | REAL2INT_n -> "real2int"
  | INT2REAL_n ->  "int2real"
  | AND_n -> "and"
  | OR_n -> "or"
  | XOR_n -> "xor"
  | IMPL_n -> if Lv6MainArgs.global_opt.Lv6MainArgs.kcg then assert false else "=>"
  | EQ_n -> "="
  | NEQ_n -> "<>"
  | LT_n | ILT_n  | RLT_n -> "<"
  | LTE_n | ILTE_n | RLTE_n -> "<="
  | GT_n  | IGT_n  | RGT_n -> ">"
  | GTE_n | IGTE_n | RGTE_n -> ">="
  | DIV_n -> "div"
  | MOD_n -> "mod"
  | IF_n -> "if"
  | NOR_n -> "nor"
  | DIESE_n -> "#"
  | UMINUS_n -> "-"
  | MINUS_n -> "-"
  | PLUS_n -> "+"
  | SLASH_n -> if Lv6MainArgs.global_opt.Lv6MainArgs.kcg then "div" else "/"
  | TIMES_n -> "*"
  | IUMINUS_n -> "-"
  | IMINUS_n -> "-"
  | IPLUS_n -> "+"
  | ISLASH_n -> "/"
  | ITIMES_n -> "*"
  | RUMINUS_n -> "-"
  | RMINUS_n -> "-"
  | RPLUS_n -> "+"
  | RSLASH_n -> "/"
  | RTIMES_n -> "*"

let op2string_long = function
  | EQ_n -> "eq"
  | NEQ_n -> "neq"
  | IMPL_n -> "impl"
  | LT_n -> "lt"
  | ILT_n -> "ilt"
  | RLT_n -> "rlt"
  | LTE_n -> "lte"
  | ILTE_n -> "ilte"
  | RLTE_n -> "rlte"
  | GT_n -> "gt"
  | IGT_n -> "igt"
  | RGT_n -> "rgt"
  | GTE_n -> "gte"
  | IGTE_n -> "igte"
  | RGTE_n -> "rgte"
  | DIESE_n -> "diese"
  | UMINUS_n -> "uminus"
  | MINUS_n -> "minus"
  | PLUS_n -> "plus"
  | SLASH_n -> "slash"
  | TIMES_n -> "times"
  | IUMINUS_n -> "iuminus"
  | IMINUS_n -> "iminus"
  | IPLUS_n -> "iplus"
  | ISLASH_n -> "islash"
  | ITIMES_n -> "itimes"
  | RUMINUS_n -> "ruminus"
  | RMINUS_n -> "rminus"
  | RPLUS_n -> "rplus"
  | RSLASH_n -> "rslash"
  | RTIMES_n -> "rtimes"
  | op -> op2string op

let is_infix = function
  | AND_n | OR_n | XOR_n | IMPL_n | EQ_n | NEQ_n | LT_n | LTE_n | GT_n | GTE_n 
  | ILT_n | ILTE_n| IGT_n| IGTE_n 
  | RLT_n| RLTE_n| RGT_n| RGTE_n 
  | DIV_n
  | MOD_n | IF_n | MINUS_n | PLUS_n | SLASH_n | TIMES_n | IMINUS_n | IPLUS_n
  | ISLASH_n | ITIMES_n | RMINUS_n | RPLUS_n | RSLASH_n | RTIMES_n
      -> true
  | ICONST_n _ | RCONST_n _

  | RUMINUS_n 
  | IUMINUS_n | UMINUS_n | DIESE_n | NOR_n | INT2REAL_n | REAL2INT_n | NOT_n 
  | FALSE_n | TRUE_n
      -> false


(*********************************************************************************)
let (string_to_op : string -> op) = 
  function
      (* zero-ary *)
    | "true" -> TRUE_n
    | "false" -> FALSE_n
        (* unary *)
    | "not" -> NOT_n
    | "real2int" -> REAL2INT_n
    | "int2real" -> INT2REAL_n
        (* binary *)
    | "and" -> AND_n
    | "or" -> OR_n
    | "xor" -> XOR_n
    | "impl" -> IMPL_n
    | "eq" -> EQ_n
    | "neq" -> NEQ_n
    | "lt" -> LT_n
    | "ilt" -> ILT_n
    | "rlt" -> RLT_n
    | "lte" -> LTE_n
    | "ilte" -> ILTE_n
    | "rlte" -> RLTE_n
    | "gt" -> GT_n
    | "igt" -> IGT_n
    | "rgt" -> RGT_n
    | "gte" -> GTE_n
    | "igte" -> IGTE_n
    | "rgte" -> RGTE_n
    | "div" -> DIV_n
    | "mod" -> MOD_n
        (* ternary *)
    | "if" -> IF_n
        (* n-ary *)
    | "nor" -> NOR_n
    | "#" -> DIESE_n 
    | "diese" -> DIESE_n

    (* overloaded operator *)
    | "uminus" -> UMINUS_n
    | "minus" -> MINUS_n
    | "plus" -> PLUS_n
    | "slash" -> SLASH_n
    | "times" -> TIMES_n

    (* un-overloaded operator *)
    | "iuminus" -> IUMINUS_n
    | "iminus" -> IMINUS_n
    | "iplus" -> IPLUS_n
    | "islash" -> ISLASH_n
    | "itimes" -> ITIMES_n

    | "ruminus" -> RUMINUS_n
    | "rminus" -> RMINUS_n
    | "rplus" -> RPLUS_n
    | "rslash" -> RSLASH_n
    | "rtimes" -> RTIMES_n

(* Condact = predefined macro-op *)
    (* | "condact" -> CondAct *)


    (* array iterator *)
(*
    | "map"  -> Map
    | "fill" -> Fill
    | "red"  -> Red
    | "fillred"  -> FillRed
    | "boolred" -> BoolRed
*)

    | _ -> raise Not_found

let (is_a_predef_op : string -> bool) =
  fun str -> 
    try ignore (string_to_op str); true
    with Not_found -> false

(** An evaluator  returns a list because Lustre calls returns tuples. 
    
    SE: migth raise some check error!
*)
type 'a evaluator = 'a list list -> 'a list 


let (op_to_long : op -> Lv6Id.long) =
  fun op -> 
    Lv6Id.make_long 
      (Lv6Id.pack_name_of_string "Lustre") 
      (Lv6Id.of_string (op2string_long op))

let (op_to_idref : op -> Lv6Id.idref) =
   fun op ->
      Lv6Id.make_idref
      (Lv6Id.pack_name_of_string "Lustre") 
      (Lv6Id.of_string (op2string_long op))
    
(*********************************************************************************)
(* Automatically generate the latex documentation associated to predefined
   entities *)
	
(* let (gen_tex_doc : string -> unit) = *)
(*   fun file ->  *)
(*   let oc = open_out file in  *)
(*   let p = output_string oc in *)
(*     p " Lustre V6 predefined operators \n\n"; *)
(*     List.iter (fun (n,def) -> p ("\t" ^ n ^ "\n")) list; *)
(*     close_out oc *)

(*********************************************************************************)



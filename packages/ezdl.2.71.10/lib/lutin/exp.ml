(*-----------------------------------------------------------------------
** Copyright (C) - Verimag.
** This file may only be copied under the terms of the CeCill
** Public License
**-----------------------------------------------------------------------
**
** File: exp.ml
** Author: erwan.jahier@univ-grenoble-alpes.fr
*)

open Value

(****************************************************************************)



type ext_func_name = string
type ext_func_type = Type.t list
type ext_lib_name = string
type ext_func_tbl = (ext_func_type * (ext_lib_name * Ezdl.t)) Util.StringMap.t

type t =
    Formu of formula
  | Numer of num
  | Liste of simple_tbl
      (** struct and arrays are flattened in such a list *)

and simple =
    Fo of formula
  | Nu of num

and func_call_arg = string * Ezdl.cfunc * ext_func_type * ext_lib_name * t list

and num =
  | Sum  of num * num
  | Diff of num * num
  | Prod of num * num
  | Quot of num * num
  | Mod  of num * num  (** modulo *)
  | Div  of num * num  (** euclidian division *)
  | Uminus of num

  | Inf_int
  | Ival of Num.num (* to take advantage of big_int (ratio not used) *)
  | Fval of float
  | Ivar of var
  | Fvar of var

  | FFC of func_call_arg
  | IFC of func_call_arg
  | Gcont of num * num * num
  | Gstop of num * num * num
  | Icont of num * num * num
  | Istop of num * num * num

  | Ite of formula * num * num

and formula =
  | And  of formula * formula
  | Or   of formula * formula
  | Xor  of formula * formula
  | NXor of formula list
  | Nor  of formula list
  | Diese of formula list
  | Impl of formula * formula
  | IteB of formula * formula * formula
  | Not  of formula
  | EqB  of formula * formula

  | True
  | False
  | Bvar of var

  | Eq    of num * num (* =  *)
  | Sup   of num * num (* >  *)
  | SupEq of num * num (* >= *)
  | Inf   of num * num (* <  *)
  | InfEq of num * num (* <= *)
and
  var = t Var.t
and simple_tbl
  = simple Util.StringMap.t

(*******************************************************************************)
let (ext_func_type_to_string : ext_func_type -> string) = 
  fun tl -> 
    List.fold_left 
    (fun acc t -> (acc ^ " -> " ^ (Type.to_string t)))
    (Type.to_string  (List.hd tl))
    (List.tl tl)

let (ext_func_tbl_to_string : ext_func_tbl -> string) =
  fun tbl -> 
    Util.StringMap.fold
    (fun k (eft,(efn,_efl)) acc -> 
       (acc ^ "\t" ^ k ^ " : " ^ (ext_func_type_to_string eft) ^
	" \t(defined in " ^ efn ^ ")\n"))
    tbl
    "External functions:\n"

type var_tbl = var Util.StringMap.t


let (add_value : string -> simple -> simple_tbl -> simple_tbl) =
  fun str exp tbl -> 
    Util.StringMap.add str exp tbl

let (add_var : string -> var -> var_tbl -> var_tbl) =
  fun str var tbl -> 
(*     if Util.StringMap.mem str tbl then *)
(*       ( *)
(* 	print_string ("\n*** Error: The variable " ^ str ^ " is defined twice.\n"); *)
(* 	flush stdout; *)
(* 	assert false *)
(*       ); *)
    Util.StringMap.add str var tbl


let (empty_var_tbl : var Util.StringMap.t) = 
  Util.StringMap.empty

let (empty_simple_tbl : simple Util.StringMap.t) = 
  Util.StringMap.empty




let (remove_prefix : string -> string -> string -> string) =
  fun lv_prefix prefix vn0 ->
    let l_lv_prefix = (String.length lv_prefix) in
    let vn1 =
      try
	if 
	  (String.sub vn0 0 l_lv_prefix) = lv_prefix
	then
	  (String.sub vn0 l_lv_prefix ((String.length vn0) - l_lv_prefix) )
	else
	  vn0
      with _ -> vn0
    in
    let l = String.length prefix in
    let l2 = String.length vn1 in
      assert (String.sub vn1 0 l = prefix);
      String.sub vn1 l (l2-l) 


let _ = assert (
  (remove_prefix "toto__" "s" "toto__s.field1[1]" = ".field1[1]"))

let _ = assert (
  (remove_prefix "" "s.field1" "s.field1[1]" = "[1]"))

let _ = assert (
  (remove_prefix "" "s" "s[23].field[1]" = "[23].field[1]"))


let (remove_var_name : string -> string) =
  fun vn0 ->
    
    try
      let index = 
	Str.search_forward (Str.regexp "\\.\\|\\[") vn0 0
      in
	String.sub vn0 index ((String.length vn0) - index)
    with _ -> vn0

let _ = assert (
  (remove_var_name "s.field[1]" = ".field[1]"))

let _ = assert (
  (remove_var_name "s[23].field[1]" = "[23].field[1]"))

 
(****************************************************************************)

let type_num_tbl = Hashtbl.create 100

(* exported *)
let rec (num_is_an_int : num -> bool) =
  fun e ->
    try
      Util.hfind type_num_tbl e
    with Not_found ->
      let res =
	     match e with
	       Sum(e1, _e2) -> num_is_an_int e1
	     | Diff(e1, _e2) -> num_is_an_int e1
	     | Prod(e1, _e2) -> num_is_an_int e1
	     | Quot(e1, _e2) -> num_is_an_int e1
	     | Mod(e1, _e2) ->  num_is_an_int e1
	     | Uminus(e) -> num_is_an_int e
	     | Div(e1, _e2) -> num_is_an_int e1
	     | Ivar(_var) -> true
	     | Fvar(_var) -> false
	     | Ival(_i) -> true
	     | Fval(_f) -> false
	     | Ite(_f,e1,_e2) -> num_is_an_int e1
	     | Inf_int -> true
	     | FFC _  -> false
	     | IFC _  -> true
	     | Gcont(_,_,_) -> true
	     | Gstop(_,_,_) -> true 
	     | Icont(_,_,_) -> true
	     | Istop(_,_,_) -> true
      in
	   Hashtbl.add type_num_tbl e res;
	   res


(* exported *)
let (formula_to_var_value: formula -> Value.t) = function
    True -> B(true)
  | False -> B(false)
  | _ -> assert false

(* exported *)
let (num_to_var_value: num -> Value.t) = function
    Ival(i) -> N(I(i))
  | Fval(f) -> N(F(f))
  | _ -> assert false


let (to_value: t -> Value.t) = function
| Numer n -> num_to_var_value n
| Formu f -> formula_to_var_value f
| Liste _  -> assert false

let rec (simplifie_a_little : formula -> formula) =
  fun f -> 
    (* But not too much ... *)
    match f with
      | And(True, f) -> simplifie_a_little f
      | And(f, True) -> simplifie_a_little f

      | Or(False, f) -> simplifie_a_little f
      | Or(f, False) -> simplifie_a_little f 
      | Not(True) -> False
      | Not(False) -> True

      | And(f1, f2) -> 
	       let f1' = simplifie_a_little f1
	       and f2' = simplifie_a_little f2 
	       in
	         if f1 <> f1' || f2 <> f2' then
	           simplifie_a_little (And(f1', f2'))
	         else
	           And(f1', f2')
      | Or(f1, f2) -> 
	       let f1' = simplifie_a_little f1
	       and f2' = simplifie_a_little f2 in
	       let f12' = Or(f1', f2') in
	         if f1 <> f1' || f2 <> f2' then
	           simplifie_a_little f12'
	         else
	           f12'
      | Xor(f1, f2) -> Xor(simplifie_a_little f1, simplifie_a_little f2)
      | NXor fl -> NXor(List.map simplifie_a_little fl)
      | Nor fl -> Nor(List.map simplifie_a_little fl)
      | Diese fl -> Diese(List.map simplifie_a_little fl)
      | Impl(f1, f2) -> Impl(simplifie_a_little f1, simplifie_a_little f2)
      | IteB(f1, f2, f3) -> IteB(simplifie_a_little f1, simplifie_a_little f2,
                                 simplifie_a_little f3)
      | Not(f1) -> Not(simplifie_a_little f1)
      | EqB(f1, f2) -> EqB(simplifie_a_little f1, simplifie_a_little f2)

      | True -> f
      | False -> f
      | Bvar(_var) -> f
      | Eq(_e1, _e2) -> f
      | SupEq(_e1, _e2) -> f
      | Sup(_e1, _e2) -> f
      | InfEq(_e1, _e2) -> f
      | Inf(_e1, _e2) -> f


let rec i2tab i = if i <= 0 then "" else ("\t" ^ (i2tab (i-1)))
open Util

let rec
    (formula_to_string: formula -> string) = 
  fun f -> 
    (f2s 1 f)
      (* "\n"^ (f2s 1 f) *)
      
and (f2s : int -> formula -> string) =
  fun i f -> 
    let i' = i+1 in
      match f with 
        | And(f1, f2) -> (f2s i f1) ^  " and " ^ (f2s i f2) 
        | Or(f1, f2)  -> 
	         (i2tab i)  ^ "(\n" ^ 
	           (f2s i f1) ^ "\n" ^
	           (i2tab i)  ^ "or \n" ^ 
	           (f2s i f2) ^ "\n" ^ 
	           (i2tab i)  ^ ")"

        | Xor(f1, f2) -> 
	         (i2tab i)  ^ "(\n" ^ 
	           (f2s i f1) ^ "\n" ^
	           (i2tab i)  ^ "xor \n" ^ 
	           (f2s i f2) ^ "\n" ^ 
	           (i2tab i)  ^ ")"
        | NXor fl -> (i2tab i) ^ "xor(" ^ (String.concat "," (List.map (f2s i) fl)) ^ ")" 
        | Nor fl -> (i2tab i) ^ "nor(" ^ (String.concat "," (List.map (f2s i) fl)) ^ ")" 
        | Diese fl -> (i2tab i) ^ "#(" ^ (String.concat "," (List.map (f2s i) fl)) ^ ")" 
        | Impl(f1, f2) ->
	         (i2tab i)  ^ "(\n" ^ 
	           (f2s i f1) ^ "\n" ^
	           (i2tab i)  ^ "=> \n" ^ 
	           (f2s i f2) ^ "\n" ^ 
	           (i2tab i)  ^ ")"

        | IteB(f1, f2, f3) -> 
	         (i2tab i) ^ "(if " ^ (f2s i' f1) ^"\n"^ (i2tab i) ^
              "then " ^ (f2s i' f2) ^ 
	           "\n" ^ (i2tab i) ^ "else " ^ (f2s i' f3) ^ ")"
        | Not(f1) ->  "not(" ^ (f2s i' f1) ^ ")"
        | EqB(f1, f2) -> ((f2s i f1) ^" = " ^ (f2s i f2))
        | True -> "true"
        | False -> "false"
        | Bvar(var) -> (
	         match Var.alias var with 
	           | None -> (Prevar.format (Var.name var))
	           | Some exp -> to_string exp
          )
        | Eq(e1, e2)    -> ("("^(n2s i e1) ^ " = "  ^ (n2s i e2)^")")
        | SupEq(e1, e2) -> ((n2s i e1) ^ " >= " ^ (n2s i e2))
        | Sup(e1, e2)   -> ((n2s i e1) ^ " > "  ^ (n2s i e2))
        | InfEq(e1, e2) -> ((n2s i e1) ^ " <= " ^ (n2s i e2))
        | Inf(e1, e2)   -> ((n2s i e1) ^ " < "  ^ (n2s i e2))
and
    (num_to_string: num -> string) =
  fun e -> n2s 0 e
and n2s i e = 
  let i' = i+1 in
    match e with
        Sum(e1, e2) ->  ("(" ^ (n2s i e1) ^ " + " ^ (n2s i e2)  ^ ")")
      | Diff(e1, e2) -> ("(" ^ (n2s i e1) ^ " - " ^ (n2s i e2)  ^ ")")
      | Prod(e1, e2) -> ("(" ^ (n2s i e1) ^ " * " ^ (n2s i e2)  ^ ")")
      | Quot(e1, e2) -> ("(" ^ (n2s i e1) ^ " / " ^ (n2s i e2)  ^ ")")
      | Mod(e1, e2) ->  ("(" ^ (n2s i e1) ^ " mod " ^ (n2s i e2)  ^ ")")
      | Uminus(e) -> ("-" ^ (n2s i e))
      | Div(e1, e2) -> ((n2s i e1) ^ " div " ^ (n2s i e2))
      | Ivar(var) -> (Prevar.format (Var.name var))
      | Fvar(var) -> (Prevar.format (Var.name var))
      | Ival(i) -> Num.string_of_num i
      | Fval(f) -> Util.my_string_of_float f
      | Ite(f,e1,e2) -> "(\n" ^
	       (i2tab i) ^ "if \n" ^ (f2s i' f) ^ "\n" ^ 
	         (i2tab i) ^ "then \n" ^ (i2tab i') ^ (n2s i' e1) ^ "\n" ^ 
	         (i2tab i) ^ "else \n" ^ (i2tab i') ^ (n2s i' e2) ^ "\n" ^ (i2tab i) ^ ") "
      | Inf_int -> (i2tab i) ^ "infinity"
      | FFC _ -> (i2tab i) ^ "<ext func call returning a float>"
      | IFC _ -> (i2tab i) ^ "<ext func call returning a int>"
      | Gcont(a1,a2,a3) -> 
	       ((i2tab i) ^ "gauss_continue" ^ (n2s i a1) ^ " "  ^ (n2s i a2) ^ " "  ^ (n2s i a3))
      | Gstop(a1,a2,a3) ->  
	       ((i2tab i) ^ "gauss_stop" ^ (n2s i a1) ^ " "  ^  (n2s i a2) ^ " "  ^ (n2s i a3))
      | Icont(a1,a2,a3) -> 
	       ((i2tab i) ^ "interval_continue" ^ (n2s i a1) ^ " "  ^ (n2s i a2) ^ " "  ^ (n2s i a3))
      | Istop(a1,a2,a3) -> 
	       ((i2tab i) ^ "interval_stop" ^ (n2s i a1) ^ " "  ^ (n2s i a2) ^ " "  ^ (n2s i a3))
            (* exported *)
and (to_string : t -> string) =
  fun e ->
    match e with
	     Formu f -> formula_to_string f
      | Numer e -> num_to_string e
      | Liste l ->
	       (StringMap.fold
	          (fun _ e acc -> acc ^ " " ^ simple_to_string e)
	          l
	          ""
	       )
and (simple_to_string : simple -> string) =
  fun e ->
    match e with
	     Fo f -> formula_to_string f
      | Nu e -> num_to_string e

(****************************************************************************)

(* exported *)
let (to_simple : t -> simple) =
  fun e ->
    match e with
	Formu f -> Fo f
      | Numer e -> Nu e
      | Liste _ -> assert false
	
let var_to_string var =
  ((Var.name var) ^ ":" ^ (Type.to_string (Var.typ var)) ^ ":" ^
   (Var.mode_to_string (Var.mode var))
  )

let (print_var : var -> unit) =
  fun var ->
    Format.print_string (var_to_string var)


(* exported *)
let rec (support : formula -> Var.name list) =
  fun f ->
    match f with
	And(Bvar(vn), ff) -> ((Var.name vn)::(support ff))
      | Bvar(vn) ->  [(Var.name vn)]
      | _ -> []

(*******************************************************************************)
(* exported *)
type weight =
    Wint of int
  | Wexpr of num
  | Infinity

(* exported *)
let (weight_to_string : weight -> string) =
  fun w ->
    match w with
	     Wint(w) -> string_of_int w
      | Wexpr(e) -> num_to_string e
      | Infinity -> ("Infinity")
	
(****************************************************************************)

let rec(flat_the_top_and : Expr.t -> Expr.t list) =
 fun e ->
   match e with
     | Expr.Op(Expr.And, l) -> List.flatten (List.map flat_the_top_and l)
     | _ -> [e]

let (to_expr : formula -> Expr.t) =
  fun f ->
    let op oper args = Expr.Op(oper,args) in
    let rec aux f = match f with
      | (NXor _|Nor _|Diese _) -> assert false
      | And (f1, f2) -> op Expr.And [aux f1; aux f2] 
      | Or  (f1, f2) -> op Expr.Or  [aux f1; aux f2]
      | Xor (f1, f2) -> op Expr.Xor [aux f1; aux f2]
      | Impl(f1, f2) -> op Expr.Impl [aux f1; aux f2]
      | IteB(c, f1, f2) ->  op Expr.Ite [aux c; aux f1; aux f2]
      | Not (f) -> op Expr.Not [aux f]
      | EqB (f1, f2) -> op Expr.Eq [aux f1; aux f2]

      | True  -> Expr.True
      | False -> Expr.False
      | Bvar var -> Expr.Var (Var.name var)

      | Eq   (n1, n2) -> op Expr.Eq    [auxe n1; auxe n2]
      | Sup  (n1, n2) -> op Expr.Sup   [auxe n1; auxe n2]
      | SupEq(n1, n2) -> op Expr.SupEq [auxe n1; auxe n2]
      | Inf  (n1, n2) -> op Expr.Inf   [auxe n1; auxe n2]
      | InfEq(n1, n2) -> op Expr.InfEq [auxe n1; auxe n2]
    and auxe e = match e with 
      | Sum (n1, n2) -> op Expr.Sum  [auxe n1; auxe n2]
      | Diff(n1, n2) -> op Expr.Diff [auxe n1; auxe n2]
      | Prod(n1, n2) -> op Expr.Prod [auxe n1; auxe n2]
      | Quot(n1, n2) -> op Expr.Quot [auxe n1; auxe n2]
      | Mod (n1, n2) -> op Expr.Mod  [auxe n1; auxe n2]
      | Div (n1, n2) -> op Expr.Div  [auxe n1; auxe n2]

      | Uminus(num) -> op Expr.Uminus [auxe num]

      | Ival(i) -> Expr.Ival i
      | Fval(f) -> Expr.Fval f 
      | Ivar(var) -> Expr.Var (Var.name var)
      | Fvar(var) -> Expr.Var (Var.name var)
      | Ite(f, n1, n2) -> op Expr.Ite [aux f; auxe n1; auxe n2]

      | Inf_int -> assert false
      | FFC(_func_call_arg) -> assert false
      | IFC(_func_call_arg) -> assert false
      | Gcont(_n1, _n2, _num) -> assert false
      | Gstop(_n1, _n2, _num) -> assert false
      | Icont(_n1, _n2, _num) -> assert false
      | Istop(_n1, _n2, _num) -> assert false
    in 
    Expr.Op(Expr.And, flat_the_top_and (aux f))



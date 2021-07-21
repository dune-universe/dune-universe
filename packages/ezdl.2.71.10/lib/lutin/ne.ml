(*-----------------------------------------------------------------------
** Copyright (C) - Verimag.
** This file may only be copied under the terms of the CeCill
** Public License
**-----------------------------------------------------------------------
**
** File: ne.ml
** Main author: erwan.jahier@univ-grenoble-alpes.fr
*)


open Value

module StringMap = struct
  include Map.Make(
    struct
      type t = string
      let compare = compare
    end
  )
end

let mfind = StringMap.find

(* exported *)
(** Normal expressions.

  Keys are var names, and the content is the coefficient of the
  monomial. By convention, "" maps the constant value. For instance,
  [("a" -> I(3) ; "b" -> I(-2) ; "" -> I(11))] represents the
  expression [3*a - 2*b + 11].
*)
type t = Value.num StringMap.t

type subst = (string * Value.num) * t
(****************************************************************************)
(* exported *)

let (to_expr: t -> Expr.t) =
  fun ne ->
    let l =
    StringMap.fold
      (fun var c acc ->
        let c = match c with I i -> Expr.Ival i | F f -> Expr.Fval f in
        if var = "" then c::acc else (Expr.Op (Expr.Prod, [c; Expr.Var var]))::acc
      )
      ne
      []
    in
    Expr.Op (Expr.Sum, l)

exception Choose_elt of Value.num
let (mapchoose:t -> Value.num) = 
  fun ne -> 
    (* only available since ocaml 3.12.0... *)
    (*   StringMap.choose ne *)
  try ignore (StringMap.fold (fun _k a _acc -> raise (Choose_elt a)) ne 0); (F 0.0) (* dummy *)
  with Choose_elt x -> x
    

let (is_int: t -> bool) =
  fun ne ->
    assert(ne <> StringMap.empty);
    match (mapchoose ne) with
      | I _ -> true
      | F _ -> false

(****************************************************************************)
(* exported *)
let (to_string_gen : (Value.num -> string) -> string -> t -> string) =
  fun nv_to_string plus ne ->
    let str =
      (StringMap.fold
	      (fun vn v acc ->
	         if vn = "" then
	           let v_str = nv_to_string v in
		          match v_str with
		              ""  -> "+1" ^ acc
		            | "-" -> "-1" ^ acc
		            | _ -> (v_str ^ acc)
	         else
	           (acc^plus ^ (nv_to_string v ) ^ "" ^ ((Prevar.format vn)))
	      )
	      ne
	      ""
      )
    in
    let str = Str.global_replace (Str.regexp "+ -") "-" str in
    let str = Str.global_replace (Str.regexp "+-") "-" str in
    let str = Str.global_replace (Str.regexp "+ +") "+" str in
    let str = Str.global_replace (Str.regexp "++") "+" str in
      str

(* exported *)
let (to_string : t -> string) =
  fun ne ->
    to_string_gen (num_value_to_string) " + " ne

(* exported *)
let (print : t -> unit) =
  fun ne ->
    print_string (to_string ne)

(* exported *)
let (substl_to_string : subst list -> string) =
  fun sl ->
    (List.fold_left
       (fun acc ((vn, a), ne) ->
	       (acc ^ "   " ^ (Value.num_value_to_string a) ^ "." ^ vn ^
	        " -> " ^ to_string ne ^ "\n")
       )
       ""
       sl
    )


(****************************************************************************)
(* exported *)
let (is_a_constant : t -> bool) =
  fun ne ->
    (StringMap.remove "" ne) = StringMap.empty



(****************************************************************************)

(* exported *)
let (opposite: t -> t) =
  fun ne ->
    StringMap.map (fun num -> match num with I(i) -> I(Num.minus_num i) | F(f) -> F(-.f)) ne


(****************************************************************************)

(* exported *)
let (add: t -> t -> t) =
  fun ne1 ne2 ->
    ( StringMap.fold
	(fun vn1 val1 acc ->
	   try
	     let val2 = mfind vn1 acc in
	     let valr = (add_num val1 val2) in
	       if ((not (num_eq_zero valr)) || (vn1 = "") )
	       then StringMap.add vn1 valr acc
	       else StringMap.remove vn1 acc
	   with Not_found ->
	     StringMap.add vn1 val1 acc
	)
	ne1
	ne2
    )

let _ = assert (
  let ne1 = StringMap.add "" (I(Num.num_of_int 1)) 
    (StringMap.add "toto" (I(Num.num_of_int 2))  StringMap.empty)
  and ne2 = StringMap.add "" (I(Num.num_of_int 2)) 
    (StringMap.add "toto" (I(Num.num_of_int (-3))) StringMap.empty)
  and ne_res = StringMap.add "" (I(Num.num_of_int 3)) 
    (StringMap.add "toto" (I(Num.num_of_int (-1))) StringMap.empty)
  in
  let ne_cal = add ne1 ne2 in

    ((mfind "toto" ne_res) = (mfind "toto" ne_cal))
    &&
    ((mfind "" ne_res) = (mfind "" ne_cal))
)

(****************************************************************************)
let (diff: t -> t -> t) =
  fun ne1 ne2 ->
    ( StringMap.fold
	(fun vn2 val2 acc ->
	   try
	     let val1 = mfind vn2 acc in
	     let valr = (diff_num val1 val2) in
	       if (not (num_eq_zero valr) || vn2 = "" )
	       then StringMap.add vn2 valr acc
	       else StringMap.remove vn2 acc
	   with Not_found ->
	     let minus_val2 =
	       match val2 with
		   I(i) -> I(Num.minus_num i)
		 | F(f) -> F(-. f)
	     in
	       StringMap.add vn2 minus_val2 acc
	)
	ne2
	ne1
    )

let _ = assert (
  let ne1 = StringMap.add "" (I(Num.num_of_int 1)) 
    (StringMap.add "toto" (I(Num.num_of_int 2)) StringMap.empty)
  and ne2 = StringMap.add "" (I(Num.num_of_int 2)) 
    (StringMap.add "titi" (I(Num.num_of_int 3))
				   (StringMap.add "toto" (I(Num.num_of_int 3)) StringMap.empty))
  and ne_res = StringMap.add "" (I(Num.num_of_int (-1))) 
    (StringMap.add "toto" (I(Num.num_of_int (-1)))
				       (StringMap.add "titi" (I(Num.num_of_int 3)) StringMap.empty))
  in
  let ne_cal = diff ne1 ne2
  in
    ((mfind "toto" ne_res) = (mfind "toto" ne_cal))
    &&
    ((mfind "" ne_res) = (mfind "" ne_cal))
)

(****************************************************************************)
let (mult: t -> t -> t) =
  fun ne1 ne2 ->
    if is_a_constant ne1
    then
      let coeff = mfind "" ne1 in
	   if num_eq_zero coeff
	   then ne1
	   else
	     ( StringMap.fold
	         (fun vn value acc ->
		        StringMap.add vn (mult_num coeff value) acc
	         )
	         ne2
	         StringMap.empty
	     )
    else if is_a_constant ne2
    then
      let coeff = mfind "" ne2 in
	   if num_eq_zero coeff
	   then ne2
	   else
	     ( StringMap.fold
	         (fun vn value acc ->
		        StringMap.add vn (mult_num coeff value) acc
	         )
	         ne1
	         StringMap.empty
	     )
    else
      let ne_str = "("^(to_string ne1) ^ ") x (" ^ (to_string ne2) ^ ")" in
      print_string ("\n*** Cannot solve non-linear constraints: "^ne_str^"\n");
      flush stdout;
      exit 2

let _ = assert (
  let ne1 = StringMap.add "" (I(Num.num_of_int 1)) 
    (StringMap.add "toto" (I(Num.num_of_int 2)) StringMap.empty) in
  let ne2 = StringMap.add "" (I(Num.num_of_int 2)) StringMap.empty in

  let ne_res = 
    StringMap.add "" (I(Num.num_of_int 2))
      (StringMap.add "toto" (I(Num.num_of_int 4)) StringMap.empty) 
  in
  let ne_cal = mult ne1 ne2 in

    ((mfind "toto" ne_res) = (mfind "toto" ne_cal))
    &&
    ((mfind "" ne_res) = (mfind "" ne_cal))
)

(****************************************************************************)
let (modulo: t -> t -> t) =
  fun ne1 ne2 ->
    if
      is_a_constant ne1 && is_a_constant ne2 
    then
      (* dead code ? (simplified before) *)
      let c1 = mfind "" ne1 in
      let c2 = mfind "" ne2 in
      StringMap.add "" (Value.modulo_num c1 c2) StringMap.empty
    else
      failwith "*** arguments of 'mod' should be known. \n"
	(* indeed, x mod y <=> 0 <= x < y and x = k * y  where is a 
	   (uncontrollable) local variable*)


(****************************************************************************)
let (_div_hide: t -> t -> t) =
  fun ne1 ne2 ->
    if
      is_a_constant ne1 && is_a_constant ne2
    then
      (* dead code ? (simplified before) *)
      let c1 = mfind "" ne1 in
      let c2 = mfind "" ne2 in
      StringMap.add "" (Value.div_num c1 c2) StringMap.empty
    else
      failwith ("*** arguments of 'div' should be known \n")

let (div: t -> t -> t) =
  fun ne1 ne2 ->
    if is_a_constant ne2
    then
      let coeff = mfind "" ne2 in
	   if num_eq_zero coeff
	   then (
        let ne_str = "("^(to_string ne1) ^ ") / (" ^ (to_string ne2) ^ ")" in
        print_string ("\n*** Cannot divide by zero: "^ne_str^"\n");
        flush stdout;
        exit 2
      )
	   else
	     ( StringMap.fold
	         (fun vn value acc ->
		        StringMap.add vn (div_num value coeff) acc
	         )
	         ne1
	         StringMap.empty
	     )
    else
      let ne_str = "("^(to_string ne1) ^ ") / (" ^ (to_string ne2) ^ ")" in
      print_string ("\n*** Cannot solve non-linear constraints: "^ne_str^"\n");
      flush stdout;
      exit 2

               
let (quot: t -> t -> t) = div
      
(****************************************************************************)

(* exported *)
let (fold : (string -> Value.num -> 'acc -> 'acc) -> t -> 'acc -> 'acc) =
  fun f ne acc0 ->
    StringMap.fold f ne acc0

(* exported *)
let (make : string -> Value.num -> t) =
  fun vn nval ->
    (StringMap.add vn nval StringMap.empty)

(* exported *)	
let (find : string -> t -> Value.num option) =
  fun vn ne ->
    try Some(mfind vn ne)
    with Not_found -> None

let (find_constant : t -> Value.num option) =
  find ""

(****************************************************************************)


(* exported *)
let (neg_nexpr : t -> t) =
  fun ne ->
    StringMap.map (fun x -> match x with I(i) -> I(Num.minus_num i) | F(f) -> F(-.f)) ne

type split_res =
  | Split of string * Value.num * t
      (* i.e., for expressions over floats, or for simple integer expressions *)
  | No_solution (* e.g., "2.x + 3 = 0" have no solution for integers *)
  | Dont_know   (* e.g., "a.x + b.y = 0", for which it is difficult to 
                   find a substition for integers *)

(* exported *)
let (split : t -> split_res) =
  fun ne ->
    let list_to_ne = 
      List.fold_left (fun acc (c,v) -> StringMap.add v c acc) StringMap.empty 
    in
    let divide c1 c2 = 
      match (c1,c2) with
        | I i1, I i2 -> Num.eq_num (Num.mod_num i2 i1) (Num.Int 0)
        | _, _ -> true
    in
    let cl =  (StringMap.fold (fun v c acc -> (c,v)::acc) ne []) in
    let res = 
      match cl with
        | []
        | [_,_] -> 
            print_string "The impossible occured!\n"; flush stdout;
            assert false (* dim > 0 *)
        | (F f_cst,"")::(F f, v)::rest 
        | (F f, v)::(F f_cst,"")::rest 
          -> (* for floats, no soucis... *)
            (* a.x+b*)
            let ne_rest = list_to_ne ((F f_cst, "")::rest) in
              Split(v, F f, ne_rest)

        (* For integers, let's be careful *)
        | (c,v)::(c0, "")::[] 
        | (c0, "")::(c,v)::[] -> 
            (* c.v = 0  =>  c = 0 *)
            let ne_rest = list_to_ne [(c0, "")] in
            if divide c c0 then Split (v, c, ne_rest) else No_solution
              
        | _ -> (
            try (* we search if one of the coef divide all the others *)
              let (c,v) = 
                List.find 
                  (fun (c,v) ->
                     v<>"" && List.for_all (fun (c',_v') -> divide c c') cl
                  ) 
                  cl
              in
              let _, rest = List.partition (fun x -> x=(c,v)) cl in
              let ne_rest = list_to_ne rest in
                Split(v,c, ne_rest)
            with 
                (* if no coef divide all the other, we dont know what to do.
                   That will be handled later by polka.
                *) 
                Not_found -> Dont_know
          )
    in
        res

(* exported *)
let (dimension : t -> int) =
  fun ne ->
    StringMap.fold (fun vn _ cpt -> if vn = "" then cpt else cpt+1) ne 0


(* exported *)
let (nexpr_add : (Value.num * string) -> t -> t) =
  fun (nval, vn) ne2 ->
    StringMap.add vn nval ne2


(* exported *)
let (apply_subst : t -> subst -> t) =
  fun ne2 ((vn, b), ne1) ->
    if
      not (StringMap.mem vn ne2)
    then
      ne2
    else
      let _ = assert (not (Value.num_eq_zero b)) in
      let a = mfind vn ne2 in
      let a_on_b = Value.quot_num a b in
      let new_ne1 =
	StringMap.map
	  (fun x -> Value.mult_num x a_on_b)
	  ne1
      in
      let rest_ne2 = (StringMap.remove vn ne2) in
	add new_ne1 rest_ne2


(* exported *)
let (apply_substl : subst list -> t -> t) =
  fun sl ne ->
    (*
       Beware that operating rigth-to-left matters.
       Typically, sl is of the form :
       [
          N -> 1;
          N' -> N+1
       ]
       Therefore, if we apply the substitution left-to-rigth,
       a N would remain in the expression.
    *)
    List.fold_right (fun x y -> apply_subst y x) sl ne



(* exported *)
let (apply_simple_subst : t -> string * Value.num -> t) =
  fun  ne (vn, v) ->
    try
      let a = mfind vn ne in
      let rest = StringMap.remove vn ne in
	(
	  try
	    let b = mfind "" ne in
	      StringMap.add "" (Value.add_num b (Value.mult_num v a)) rest
	  with
	      Not_found ->
		StringMap.add "" (Value.mult_num v a) rest
	)
    with
	Not_found -> ne




(* exported *)
let (get_vars : t -> string list) =
  fun ne ->
    (StringMap.fold
       (fun vn _ acc -> vn::acc)
       ne
       []
    )


(* exported *)
let (eval : t -> Var.num_subst list -> Value.num) =
  fun ne s ->
    let acc0 = match (snd (List.hd s)) with I _ -> I (Num.num_of_int 0) | F _ ->  F 0.0 in
      (StringMap.fold
	 (fun vn v acc ->
	    if
	      vn = ""
	    then
	      Value.add_num acc v
	    else
	        Value.add_num acc (Value.mult_num v (List.assoc vn s))
         )
	 ne
	 acc0
      )


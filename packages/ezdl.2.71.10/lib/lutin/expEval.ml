
(* NON RECURSIVE partial eval of Exp.t 

	Tries to simplify the TOP-LEVEL operation ONLY

*)

open Exp

let bval b = if b then True else False

let exp_to_ezdl_arg e = (
	match e with
	| Numer (Ival i) -> Ezdl.Int_carg (Util.int_of_num i)
	| Numer (Fval f) -> Ezdl.Double_carg f
	| Formu (True) -> Ezdl.Int_carg 1
	| Formu (False) -> Ezdl.Int_carg 0
	| _ -> raise Not_found 
)

open Num
let rec simp_num it = (
  match it with
	 | Sum (Ival i1,  Ival i2) -> Ival (i1 +/ i2)
	 | Sum (Fval r1,  Fval r2) -> Fval (r1 +. r2)
	 | Diff ( Ival i1,  Ival i2) -> Ival (i1 -/ i2)
	 | Diff ( Fval r1,  Fval r2) -> Fval (r1 -. r2)
	 | Prod ( Ival i1,  Ival i2) -> Ival (i1 */ i2)
	 | Prod ( Fval r1,  Fval r2) -> Fval (r1 *. r2)
	 | Quot ( Ival i1,  Ival i2) -> Ival (quo_num i1  i2)
	 | Quot ( Fval r1,  Fval r2) -> Fval (r1 /. r2)
	 | Mod  ( Ival i1,  Ival i2) -> Ival (mod_num i1 i2)
	 | Div ( Ival i1,  Ival i2) -> Ival (Num.quo_num i1 i2)
	 | Uminus (Ival i1) -> Ival (minus_num i1)
	 | Uminus (Fval r1) -> Fval (-. r1)
	 | FFC (_s,cfunc,_ftype,_flib,al) -> (
		  try (
			 let dlargs = List.map exp_to_ezdl_arg al in
			 let res_call = Ezdl.cargs2f cfunc dlargs in
			   Fval (res_call)
		  ) with Not_found -> it 
	   )
	 | IFC (_s,cfunc,_ftype,_flib,al) -> (
		  try (
			 let dlargs = List.map exp_to_ezdl_arg al in
			 let res_call = Ezdl.cargs2i cfunc dlargs in
			   Ival (Num.num_of_int res_call)
		  ) with Not_found -> it 
	   )
	 | Gcont (Ival i1, Ival i2, Ival i3) -> (
        let i1,i2,i3=(Util.int_of_num i1, Util.int_of_num i2,Util.int_of_num i3) in
		  let i = LutinUtils.gauss_continue i1 i2 i3 in
		    Ival (Num.num_of_int i)
	   )
	 | Gstop (Ival i1, Ival i2, Ival i3) -> (
        let i1,i2,i3=(Util.int_of_num i1, Util.int_of_num i2,Util.int_of_num i3) in
		  let i = LutinUtils.gauss_stop i1 i2 i3 in
		    Ival (Num.num_of_int i)
	   )
	 | Icont (Ival i1, Ival i2, Ival i3) -> (
        let i1,i2,i3=(Util.int_of_num i1, Util.int_of_num i2,Util.int_of_num i3) in
		  let i = LutinUtils.interval_continue i1 i2 i3 in
		    Ival (Num.num_of_int i)
	   )
	 | Istop (Ival i1, Ival i2, Ival i3) -> (
        let i1,i2,i3=(Util.int_of_num i1, Util.int_of_num i2,Util.int_of_num i3) in
		  let i = LutinUtils.interval_stop i1 i2 i3 in
		    Ival (Num.num_of_int i)
	   )
	 | Ite (True, a2, _) -> a2
	 | Ite (False, _, a3) -> a3
	 | _ -> it
) and simp_formula it = (
  match it with
	 | And  (False, _) | And (_, False) -> False
	 | And  (True, a) | And (a, True) -> a 
	 | Or  (False, a) | Or (a, False) -> a
	 | Or  (True, _) | Or (_, True) -> True 
	 | Xor (False, a) | Xor (a, False) -> a
	 | Xor (True , a) | Xor (a, True ) -> simp_formula (Not a)
	 | Impl (False, _) | Impl ( _, True ) -> True
	 | Impl (True, a)  -> a
	 | Impl (a, False ) -> simp_formula (Not a)
	 | IteB (True ,    a    ,    _    )
	 | IteB (False,    _    ,    a    ) -> a 
	 | IteB ( a   ,  True   ,    b    ) -> simp_formula (Or (a , b))
	 | IteB ( a   ,  False  ,    b    ) -> simp_formula (And (simp_formula (Not a),  b))
	 | IteB ( a   ,    b    ,  True   ) -> simp_formula (Or (simp_formula (Not a),  b))
	 | IteB ( a   ,    b    ,  False  ) -> simp_formula (And (a,b))
	 | Not True -> False
	 | Not False -> True
	 | EqB  (True, a)  | EqB (a, True) -> a
	 | EqB  (False, a) | EqB (a, False) -> simp_formula (Not a)
	 | Eq ( Ival i1,  Ival i2) -> bval (i1 = i2)
	 | Eq ( Fval r1,  Fval r2) -> bval (r1 = r2)
	 | Sup ( Ival i1,  Ival i2) -> bval (i1 > i2)
	 | Sup ( Fval r1,  Fval r2) -> bval (r1 > r2)
	 | SupEq ( Ival i1,  Ival i2) -> bval (i1 >= i2)
	 | SupEq ( Fval r1,  Fval r2) -> bval (r1 >= r2)
	 | Inf ( Ival i1,  Ival i2) -> bval (i1 < i2)
	 | Inf ( Fval r1,  Fval r2) -> bval (r1 < r2)
	 | InfEq ( Ival i1,  Ival i2) -> bval (i1 <= i2)
	 | InfEq ( Fval r1,  Fval r2) -> bval (r1 <= r2)
	 | _ -> it
) and simp_exp e = (
  match e with
	 | Formu f -> Formu (simp_formula f)
	 | Numer n -> Numer (simp_num n)
	 |	_ -> assert false
)

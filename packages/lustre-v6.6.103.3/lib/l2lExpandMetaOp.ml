(** Time-stamp: <modified the 29/08/2019 (at 15:42) by Erwan Jahier> *)

open Lxm
open Lic

let dbg = (Lv6Verbose.get_flag "ei")

(* pack useful info into a single struct *)
type local_ctx = { 
  node : Lic.node_exp;
  prg : LicPrg.t;
}

(********************************************************************************)
(* stuff to create fresh var names. *)
let new_var str _lctx type_eff clock_eff = 
  let id = Lv6Id.of_string (FreshName.local_var (str)) in 
  let var =
    { 
      var_name_eff   = id;
      var_nature_eff = AstCore.VarLocal;
      var_number_eff = -1; (* this field is used only for i/o. 
                              Should i rather put something sensible there ? *)
      var_type_eff   = type_eff;
      var_clock_eff  = id, clock_eff;
    }
  in
    var

(********************************************************************************)
(* A small util function followed by a quick unit test. *)
let rec fill i size = if i >= size then [] else (i)::(fill (i+1) size) 
let _ = assert (fill 0 5 = [0;1;2;3;4])
      
let rec (list_map3: 
           ('a -> 'b -> 'c -> 'd) -> 'a list -> 'b list -> 'c list -> 'd list) =
  fun f l1 l2 l3 ->
    match (l1, l2, l3) with
      |	([], [], []) -> []
      | (e1::t1, e2::t2, e3::t3) -> (f e1 e2 e3)::(list_map3 f t1 t2 t3)
      | _ -> (* should not occur *) 
          print_string "*** list_map3 called with lists of different size.\n";
          flush stdout;
          assert false
(********************************************************************************)
(* Some utililities to build Lic expressions *)

(* We generate code that does not correspond to any source *)
let lxm = Lxm.dummy "no_source"

let (val_exp_of_var_info : Lxm.t -> Lic.var_info -> Lic.val_exp) =
  fun lxm vi -> 
    { 
      ve_core = CallByPosLic({src=lxm;it=Lic.VAR_REF vi.var_name_eff}, []); 
      ve_typ  = [vi.var_type_eff];
      ve_clk  = [snd vi.var_clock_eff];
      ve_src  =  lxm
    }

let (val_exp_of_ident : Lxm.t -> string -> Lic.type_ -> Lic.clock -> Lic.val_exp) =
  fun lxm id t clk -> 
    { 
      ve_clk = [clk]; 
      ve_typ = [t]; 
      ve_core = CallByPosLic({it=Lic.VAR_REF id;src=lxm},[]);
      ve_src = lxm
    }

let (val_exp_of_int : Lxm.t -> string -> Lic.clock -> Lic.val_exp) =
  fun lxm i clk -> 
    { 
      ve_clk = [clk]; 
      ve_typ = [Int_type_eff]; 
      ve_core = CallByPosLic({it= CONST(Int_const_eff i);src=lxm},[]);
      ve_src = lxm
    }


let (val_exp_of_const : Lic.const -> Lic.val_exp) =
  fun c -> 
    let _,ve = UnifyClock.const_to_val_eff lxm true UnifyClock.empty_subst c in
    ve

let (add_when : Lic.clock -> Lic.val_exp -> Lic.val_exp) =
  fun clk ve -> 
    { ve with 
      ve_clk = List.map (fun _ -> clk) ve.ve_clk; 
      ve_core = CallByPosLic({it= WHEN clk;src=lxm},[ve]);
    }

(* [add_current ve clk out_cl] return the expression current(ve when clk)  *)
let (add_current : Lic.val_exp -> Lic.clock -> Lic.clock list -> Lic.val_exp) =
  fun ve clk out_cl -> 
    match clk with
    | On((cc,cv,_),sclk) ->
      let lxm = ve.ve_src in
        let clk_exp = val_exp_of_ident lxm cv Bool_type_eff sclk in
        { ve with
          ve_clk = out_cl;  
          ve_core = CallByPosLic({it= CURRENT(Some cc);src=lxm},[clk_exp;ve])
        }
      | _  -> assert false (* SNO *)
   


let rec (elt_type_of_array : Lic.type_ -> Lic.type_) =
  function
    | Array_type_eff(t, _) -> t
    | Abstract_type_eff(_,t) -> elt_type_of_array t
    | _  -> assert false 

let (array_var_to_val_exp : Lxm.t -> int -> var_info -> val_exp) =
  fun lxm i vi -> 
    (* vi holds x of type array and returns  x.[i] *)
    let t_elt = elt_type_of_array vi.var_type_eff in
    let op_flg = {src = lxm ; it = ARRAY_ACCES(i)} in
    { 
      ve_core = CallByPosLic(op_flg, [val_exp_of_var_info lxm vi]); 
      ve_typ  = [t_elt];
      ve_clk  = [snd vi.var_clock_eff];
      ve_src  = lxm
    }

let (node_to_val_exp : Lxm.t -> Lic.node_key -> Lic.type_ list -> Lic.clock list -> 
     val_exp list -> val_exp) =
  fun lxm nk tl cl vel -> 
    let nk = { src = lxm ; it = nk } in
    let core = CallByPosLic( { src = lxm ; it = CALL nk }, vel)  in
    {
      ve_clk = cl;
      ve_typ = tl;
      ve_core = core;
      ve_src  = lxm
    } 
let (binop_to_val_exp : Lxm.t -> Lv6Id.t -> val_exp -> val_exp -> val_exp) =
  fun lxm op ve1 ve2 ->
    let op = { it = PREDEF_CALL({src=lxm;it=("Lustre",op),[]}) ; src = lxm } in
    {
      ve_clk = ve1.ve_clk;
      ve_typ = ve1.ve_typ;
      ve_core = CallByPosLic(op, [ve1; ve2]);
      ve_src  = lxm
    }
let (binop_to_val_exp_bool : Lxm.t -> Lv6Id.t -> val_exp -> val_exp -> val_exp) =
  fun lxm op ve1 ve2 ->
    let op = { it = PREDEF_CALL({src=lxm;it=("Lustre",op),[]}) ; src = lxm } in
    {
      ve_clk = ve1.ve_clk;
      ve_typ = [Bool_type_eff];
      ve_core = CallByPosLic(op, [ve1; ve2]);
      ve_src = lxm
        
    }
let (fby_to_val_exp : Lxm.t -> val_exp -> val_exp -> val_exp) =
  fun lxm ve1 ve2 ->
    let op = { it = FBY ; src = lxm } in
    {
      ve_clk = ve1.ve_clk;
      ve_typ = ve1.ve_typ;
      ve_core = CallByPosLic(op, [ve1; ve2]);
      ve_src = lxm
    }
let (tuple_to_val_exp : Lxm.t -> val_exp list -> val_exp) =
  fun lxm vel ->
    let op = { it = TUPLE ; src = lxm } in
    {
      ve_clk = List.flatten (List.map (fun ve -> ve.ve_clk) vel);
      ve_typ = List.flatten (List.map (fun ve -> ve.ve_typ) vel);
      ve_core = CallByPosLic(op, vel);
      ve_src = lxm
    }
let (ite_to_val_exp : Lxm.t -> val_exp -> val_exp -> val_exp -> val_exp) =
  fun lxm ve1 ve2 ve3 ->
    let ite_op  = { it = PREDEF_CALL({src=lxm;it=("Lustre","if"),[]}); src = lxm } in
    {
      ve_clk = ve2.ve_clk; 
      ve_typ = ve2.ve_typ;
      ve_core = CallByPosLic(ite_op, [ve1; ve2; ve3]) ;
      ve_src = lxm
    } 

let (array_var_to_left : int -> var_info -> Lic.left) =
  fun i vi -> 
    let lp = LeftVarLic(vi,lxm) in
    let t_elt = elt_type_of_array vi.var_type_eff in
    LeftArrayLic(lp,i,t_elt)


let (create_fillred_body: local_ctx -> Lic.static_arg list ->
     Lic.node_body * var_info list) =
  fun lctx sargs ->
    (* Given 
       - a node n of type : tau * tau_1 * ... * tau_n -> tau * teta_1 * ... * teta_l
       - a integer c
       
       the fillred expression has the profile: 
       tau * tau_1^c * ... * tau_n^c  -> tau * teta_1^c * ... * teta_l^c
    *)
    let iter_node,c = match List.sort compare sargs with
      | [ConstStaticArgLic(_,Int_const_eff(c));NodeStaticArgLic(_,_node_key)]
      | [ConstStaticArgLic(_,Int_const_eff(c));TypeStaticArgLic(_);
         NodeStaticArgLic(_,_node_key)]
        -> 
        _node_key,c
      | _ -> assert false
    in
    let lxm = lctx.node.Lic.lxm in
    let iter_node = Lxm.flagit iter_node lxm in
        (*
          Hence:
          node(acc_in:tau; X1:tau_1^c ; ... ; Xn:tau_n^c) 
          returns (acc_out:tau; Y1:teta_1^c; ... ; Yl:teta_l^c) = fillred<<n,c>>;
         *)
    assert (lctx.node.Lic.inlist_eff <> []);
    assert (lctx.node.Lic.outlist_eff <> []);
    let (acc_in : var_info)      = List.hd lctx.node.Lic.inlist_eff  in
    let (y1_yl  : var_info list) = List.tl lctx.node.Lic.inlist_eff  in
    let (acc_out: var_info)      = List.hd lctx.node.Lic.outlist_eff in
    let (x1_xn  : var_info list) = List.tl lctx.node.Lic.outlist_eff in
        (*
          can be defined like this:
          node(acc_in:tau; X1:tau_1^c ; ... ; Xn:tau_n^c) 
          returns (acc_out:tau; Y1 : teta1^c; ... ; Yl: teta_l^c) =
          var
          acc_1, ..., acc_c-2 : tau;
          let 
          
          acc_1, Y1[0], ... ,Yl[0] = n(acc_in,X1[0], ... ,Xk[0]);
          acc_2, Y1[1], ... ,Yl[1] = n(acc_1, X1[1], ... ,Xk[1]);
          ... 
          acc_i+1, Y1[i], ... ,Yl[i] = n(acc_i,X1[i], ... ,Xk[i]);
          ...
          acc_out, Y1[c-1], ... ,Yl[c-1] = n(acc_c-1,X1[c-1], ... ,Xk[c-1]);
          
	       « for all i = 0, ..., c-1 » 
          acc_i+1, Y1[i], ... ,Yl[i] = n(acc_i,X1[i], ... ,Xk[i])
          tel
        *)
    let c = int_of_string c in
    let index_list = fill 0 c in        
        (* Building this list "acc_left_list" as [acc_1, ..., acc_c-2, acc_out] *)
    let type_exp,clock_exp = acc_in.var_type_eff, snd acc_in.var_clock_eff in
    let (acc_vars : var_info list) =
      let rec f i acc = if i = 0 then acc else
          f (i-1) ((new_var "acc" lctx type_exp clock_exp)::acc)
      in
      List.rev(f (c-1) [])
    in
    let (acc_left_list : left list) =
      (List.map (fun vi -> LeftVarLic(vi,lxm)) (acc_vars@[acc_out]))
    in
        (* Ditto for rigth part :  [acc_in, acc_1, ..., acc_c-1]*)
    let (acc_rigth_list : val_exp list) =
      List.map (val_exp_of_var_info lxm) (acc_in::acc_vars)
    in
    let neqs =
          (*
            So now we build those equations ;
            acc_1, Y1[0], ... ,Yl[0] = n(acc_in,X1[0], ... ,Xk[0]);
            acc_2, Y1[1], ... ,Yl[1] = n(acc_1, X1[1], ... ,Xk[1]);
            ... 
            acc_i+1, Y1[i], ... ,Yl[i] = n(acc_i,X1[i], ... ,Xk[i]);
            ...
            acc_out, Y1[c-1], ... ,Yl[c-1] = n(acc_c-1,X1[c-1], ... ,Xk[c-1]);
          *)
      list_map3
        (fun i acc_left acc_rigth ->
          let (xi_j:val_exp list) = (* X1[i], ... ,Xn[i] *)
            List.map (array_var_to_val_exp lxm i) y1_yl
          in
          let args = acc_rigth::xi_j in
          let (yi_k : left list) =  (* Y1[i], ... ,Yl[i] *)
            List.map (array_var_to_left i) x1_xn
          in
          let lhs = acc_left::yi_k in
		    let cl = 
            List.map (fun l -> snd (Lic.var_info_of_left l).var_clock_eff) lhs
          in
          let rhs = {
            ve_typ = List.map Lic.type_of_left lhs;
            ve_clk = cl;
            ve_core = 
              if AstPredef.is_a_predef_op (snd(fst iter_node.it)) then
                CallByPosLic({src=lxm;it=(Lic.PREDEF_CALL iter_node)}, args)
              else
                CallByPosLic({src=lxm;it=(CALL iter_node)}, args);
            ve_src = lxm
          }
          in
		    let eq = { src = lxm ; it = (lhs, rhs) } in
          eq 
        )
        index_list
        acc_left_list
        acc_rigth_list
    in
    { asserts_eff = []; eqs_eff = List.rev neqs }, acc_vars

let (create_map_body: local_ctx -> Lic.static_arg list -> Lic.node_body * var_info list) =
  fun lctx sargs ->
	 (* Given
	    - a node n of type: tau_1 * ... * tau_n -> teta_1 * ... * teta_l
	    - and an integer c
       
	    The profile of map<<node,c>> is:
  	    tau_1^c * ... * tau_n^c -> teta_1^c * ... * teta_l^c
       and
       
	    Y1, ... ,Yl = map<<node; c>>(X1,...,Xk)
       <=>
	    for all i = 0, ..., c-1; (Y1[i], ... ,Yl[i]) = N(X_1[i], ... ,X_k[i])
    *)
    let iter_node,c = match List.sort compare sargs with
      | [ConstStaticArgLic(_,Int_const_eff(c)) ; NodeStaticArgLic(_,_node_key)]
      | [ConstStaticArgLic(_,Int_const_eff(c));TypeStaticArgLic(_); NodeStaticArgLic(_,_node_key)]
        -> 
        _node_key,c
      | _ -> assert false
    in
    let iter_node = Lxm.flagit iter_node lxm in
    let (y1_yl  : var_info list) = lctx.node.Lic.inlist_eff  in
    let (x1_xn  : var_info list) = lctx.node.Lic.outlist_eff in
    let c = int_of_string c in
    let index_list = fill 0 c in        
    let neqs =
      List.map
        (fun  i ->
          let (xi_j:val_exp list) = (* X1[i], ... ,Xn[i] *)
            List.map (array_var_to_val_exp lxm i) y1_yl
          in
          let (lhs : left list) =  (* Y1[i], ... ,Yl[i] *)
            List.map (array_var_to_left i) x1_xn
          in
		    let cl = 
            List.map (fun l -> snd (Lic.var_info_of_left l).var_clock_eff) lhs
          in
          let p,n = fst iter_node.it in
          let rhs = {
            ve_typ = List.map Lic.type_of_left lhs;
            ve_clk = cl;
            ve_core =
              if p="Lustre" && AstPredef.is_a_predef_op n then
                CallByPosLic({src=lxm;it=(Lic.PREDEF_CALL iter_node)}, xi_j)
              else
                CallByPosLic({src=lxm;it=(CALL iter_node)}, xi_j);
            ve_src = lxm
          }
          in
		    let eq = { src = lxm ; it = (lhs, rhs) } in
          eq 
        )
        index_list
    in
    { asserts_eff = []; eqs_eff = List.rev neqs }, []

let (create_boolred_body: local_ctx -> int -> int -> int -> Lic.node_body * var_info list) =
  fun lctx i j k ->
    (* Given - 3 integers i, j, k boolred<<i,j,k>> has the profile: bool^n -> bool
       and is defined by
       node toto = boolred<<i,j,k>>(tab); 
       <=>
       node toto(tab:bool^n) returns (res:bool);
       var 
         cpt:int;
         let 
           cpt = (if tab[0] then 1 else 0) + ... + (if tab[k-1] then 1 else 0);
           res = i <= cpt && cpt <= j;
         tel
    *)
    assert(0 <= i && i <= j && j <= k && k>0);
    let lxm = lctx.node.Lic.lxm in
    let (tab_vi : var_info) = match lctx.node.Lic.inlist_eff with
      | [vi] -> vi 
      | _ -> assert false
    in
    let (res_vi : var_info) = match lctx.node.Lic.outlist_eff with
      | [vi] -> vi 
      | _ -> assert false
    in
    let (cpt_vi : var_info) = new_var "cpt" lctx Int_type_eff BaseLic in
    let cpt_left = LeftVarLic (cpt_vi,lxm) in
    let zero = val_exp_of_int lxm "0" BaseLic
    and one = val_exp_of_int lxm  "1" BaseLic in
    let index_list = fill 0 k in (* [0;1; ...;k-1]*)
    let (ite_list:Lic.val_exp list) = List.map
      (fun i -> (* returns [if A[i] then 1 else 0]_i=0,k-1 *)
        let tab_ve_i = array_var_to_val_exp lxm i tab_vi in
        ite_to_val_exp lxm tab_ve_i one zero
      )
      index_list
    in
    let cpt_rigth =
      assert(ite_list<>[]);
      List.fold_left (binop_to_val_exp lxm "plus")
                     (List.hd ite_list) (List.tl ite_list) in
    let res_left = LeftVarLic (res_vi,lxm) in
    let res_rigth = (* i <= cpt && cpt <= j; *)
      let i_eff   = val_exp_of_int lxm (string_of_int i) BaseLic in
      let j_eff   = val_exp_of_int lxm (string_of_int j) BaseLic in
      let cpt_eff = val_exp_of_var_info lxm cpt_vi in
      let i_inf_cpt = binop_to_val_exp_bool lxm "lte" i_eff cpt_eff in
      let cpt_inf_j = binop_to_val_exp_bool lxm  "lte" cpt_eff j_eff in
      binop_to_val_exp lxm  "and" i_inf_cpt cpt_inf_j
    in 
    let cpt_eq = { src = lxm ; it = ([cpt_left], cpt_rigth) } in
    let res_eq = { src = lxm ; it = ([res_left], res_rigth) } in
    { 
      asserts_eff = []; 
      eqs_eff = [cpt_eq; res_eq] 
    }, [cpt_vi]

let rec (replace_base : Lic.clock -> Lic.clock -> Lic.clock) =
  fun ck1 ck2  -> 
    match ck2 with
      | BaseLic -> ck1
      | On(x,sck)  -> On(x,replace_base sck ck2)
      | ClockVar _ -> assert false

let (create_condact_body: local_ctx -> Lic.static_arg list -> Lic.node_body * var_info list) =
  fun lctx sargs ->
    (*
      node condact_plus_0(i0:bool; i1:int; i2:int) returns (o0:int) = Lustre::condact<<toto, 0>>;

      <=>

      o0 = if i0 then current(toto(i1 when i0, i2 when i0) ) else (0 fby o0);

      Lustre::condact<<Lustre::plus, 0>>;
    *)
    let lxm = lctx.node.Lic.lxm in
    let node,c = match List.sort compare sargs with
      | [ConstStaticArgLic(_, c);TypeStaticArgLic(_);NodeStaticArgLic(_, node_key)]
      | [ConstStaticArgLic(_, c) ; NodeStaticArgLic(_, node_key)]
        -> node_key,c
      | _ -> assert false
    in
    let cond_exp, inputs = 
      match List.map (val_exp_of_var_info lxm) lctx.node.Lic.inlist_eff with
          [] -> assert false
        | x::t -> x,t
    in 
    let outputs_clk = List.map (fun var -> snd var.var_clock_eff) lctx.node.Lic.outlist_eff in
    let left = List.map (fun x -> LeftVarLic (x,lxm)) lctx.node.Lic.outlist_eff in
    let node_out_type_list = 
      List.map (fun x -> x.Lic.var_type_eff) lctx.node.Lic.outlist_eff 
    in
    let true_cc = ("Lustre","true") in
    let i0 = List.hd lctx.node.Lic.inlist_eff in
    let i0_clk = Lic.On((true_cc, i0.var_name_eff,Bool_type_eff), snd i0.var_clock_eff) in
    let inputs_when_i0 = List.map (add_when i0_clk) inputs in    
    let outputs_clk_when_i0 = (* replace baseLic by i0_clk *)
      List.map (replace_base i0_clk) outputs_clk
    in
    let then_exp =
      node_to_val_exp lxm node node_out_type_list outputs_clk_when_i0 inputs_when_i0
    in
    let then_exp = add_current then_exp i0_clk outputs_clk in
    let const_exp = val_exp_of_const c in
    let out_exp = match lctx.node.Lic.outlist_eff with 
        [o] -> val_exp_of_var_info lxm o 
      | _ -> tuple_to_val_exp lxm
        (List.map (val_exp_of_var_info lxm) lctx.node.Lic.outlist_eff)
    in
    let else_exp = fby_to_val_exp lxm const_exp out_exp in
    let rigth = ite_to_val_exp lxm cond_exp then_exp else_exp in
    let eq = { src = lxm ; it = (left, rigth) } in
    { asserts_eff = []; eqs_eff = [eq] }, []

let ios = int_of_string

let (create_merge_body: local_ctx -> Lic.static_arg list -> Lic.node_body * var_info list) =
  fun _lctx _sargs ->
    assert false (* XXX finish me! *)

let (create_meta_op_body:  local_ctx -> Lic.node_key -> Lic.node_body * var_info list) =
  fun lctx (nk,sargs) ->
    match nk with
      | "Lustre", "fill" 
      | "Lustre", "red" 
      | "Lustre", "fillred" -> create_fillred_body lctx sargs
      | "Lustre", "map"     -> create_map_body lctx sargs
      | "Lustre", "boolred" -> (
        let (i,j,k) =
          match sargs with 
            | [ConstStaticArgLic(_, Int_const_eff i);
               ConstStaticArgLic(_, Int_const_eff j);
               ConstStaticArgLic(_, Int_const_eff k)
              ] -> 
              (ios i,ios j,ios k)
            | _ -> assert false
        in
        create_boolred_body lctx i j k  
      )
      | "Lustre", "diese" -> (
        (* a diese is a particular kind of boolred:
           #(A,...,an) = boolred(0,1,n)([a1,...,an])
        *)
        let n = List.length lctx.node.Lic.inlist_eff in
        create_boolred_body lctx 0 1 n 
      )
      | "Lustre", "nor" -> (
        (* a nor is a particular kind of boolred too:
           nor(A,...,an) = boolred(0,0,n)([a1,...,an])
        *)
        let n = List.length lctx.node.Lic.inlist_eff in
        create_boolred_body lctx 0 0 n 
      )
      | "Lustre", "condact" -> create_condact_body lctx sargs
      | "Lustre", "merge"   -> create_merge_body lctx sargs
      | _,_  -> assert false


let rec (node : local_ctx -> Lic.node_exp -> bool -> Lic.node_exp) =
  fun lctx n only_boolred ->
    let sonk = Lic.string_of_node_key in
    Lv6Verbose.exe ~flag:dbg (fun () ->
      Printf.printf "#DBG: L2lInlineMetaOp %s\n" (sonk n.node_key_eff));
    match n.def_eff with
    | MetaOpLic ->
      if only_boolred && (fst n.node_key_eff) <> ("Lustre", "boolred") then n else
        let nk = n.node_key_eff in
        let nbody, nlocs = create_meta_op_body lctx nk in
        { n with 
          def_eff = BodyLic nbody;
          loclist_eff = Some nlocs;
        }
    | ExternLic 
    | AbstractLic None -> n
    | AbstractLic (Some pn) ->
      { n with def_eff = AbstractLic (Some (node lctx pn only_boolred)) }
    | BodyLic _b -> n
      
(* exported *)
let (doit :  LicPrg.t -> LicPrg.t) =
  fun inprg -> 
    let outprg = LicPrg.empty in
    (* types and constants do not change *)
    let outprg = LicPrg.fold_types  LicPrg.add_type  inprg outprg in
    let outprg = LicPrg.fold_consts LicPrg.add_const inprg outprg in
    (* transform nodes *)
    let  (do_node : Lic.node_key -> Lic.node_exp -> LicPrg.t -> LicPrg.t) = 
      fun nk ne outprg -> 
        let lctx = {
          node = ne;
          prg = inprg;
        }
        in
        let ne = node lctx ne false in
        LicPrg.add_node nk ne outprg
    in
    let outprg = LicPrg.fold_nodes do_node inprg outprg in
    outprg
(* exported *)

let (doit_boolred :  LicPrg.t -> LicPrg.t) =
  fun inprg -> 
    let outprg = LicPrg.empty in
    (* types and constants do not change *)
    let outprg = LicPrg.fold_types  LicPrg.add_type  inprg outprg in
    let outprg = LicPrg.fold_consts LicPrg.add_const inprg outprg in
    (* transform nodes *)
    let  (do_node : Lic.node_key -> Lic.node_exp -> LicPrg.t -> LicPrg.t) = 
      fun nk ne outprg -> 
        let lctx = {
          node = ne;
          prg = inprg;
        }
        in
        let ne = node lctx ne true in
        LicPrg.add_node nk ne outprg
    in
    let outprg = LicPrg.fold_nodes do_node inprg outprg in
    outprg

(* Time-stamp: <modified the 19/06/2020 (at 16:09) by Erwan Jahier> *)
 
  
open LicEvalConst
open Lic
open Lxm
open Lv6errors
open UnifyClock
(* DEBUG FLAG *)
let dbg = (Lv6Verbose.get_flag "clock-check")

(** 
    ** Foreword.

    The clock mechanism of Lustre is a simple kind of type dependant
    system.  Indeed, clocks depend on program variables. More
    precisely, and according to Pouzet&Colaco [emsoft03], the clock
    type system of Lustre is equivalent to a "firt class abstract
    type system".

    Note however that here, things are simpler than in emsoft03,
    since we are not interested in type reconstruction (or
    inference), but only in checking types are provided in node
    headers.


   ** clock checking of expressions. 

   nb:
   In the following, we speak about input/output arguments/parameters of an 
   expression because:
   - the difficult case wrt clock checking concerns nodes
   - other kind of expressions can be seen as nodes, as they «take» some input
   arguments, and return some outputs.
   
   The clock checking consists in verifying that expression arguments clocks 
   are compatible with the expression parameters clocks. 

   For instance, for a node of profile:

      node toto(a; b when a) returns (x when a; y when x);

   when it is called, e.g., like that:

      (v1,v2) = toto(v3,v4);

   we need to check that:
     - the argument clocks are ok, i.e., v4 is on v3  (1)
       cf the [check_arg] function below
     - the result clocks are ok, i.e., v2 is on v1 + v1 is on v3 (2)
       cf the [check_res] function below

   ** Checking expression arguments 
   --------------------------------

   ok. So now, let us detail how we clock check expression arguments.  
   e.g., in the equation

      (x,y)=toto(a,b);

   we want to make sure that the clocks of the argument "a" and "b" are compatible
   with the input clock profile of "toto".

   To do that, we suppose we have already computed:

     - "cil_par" the expe  cted input clock profile (via "get_clock_profile")
     - "cil_arg" the list of clocks of arguments  (via a rec call to f)

   In order to check that this call is correct, we check that both
   terms match.

    It also modifies the substitution s (acculumated all along the
    clock checking of the node) such that:
     - the clock var in the callee parameters migth be be substituted
     - ??? what else
*)


let rec fold_left3 f accu l1 l2 l3 =
  match (l1, l2, l3) with
    ([], [], []) -> accu
  | (a1::l1, a2::l2, a3::l3) -> fold_left3 f (f accu a1 a2 a3) l1 l2 l3
  | (_, _, _) -> invalid_arg "in EvalClock.fold_left3"

let (check_args : Lxm.t -> Lxm.t list -> subst -> Lic.id_clock list ->
                  Lic.id_clock list -> subst) =
  fun lxm lxms (s1,s2) cil_par cil_arg ->
  Lv6Verbose.exe
    ~flag:dbg (
      fun () ->
      let pos = Lxm.position lxm in
      let id_clock2str (id,c) = Printf.sprintf "[%s,%s]" id
                                               (LicDump.string_of_clock2 c) in
      let par = String.concat "," (List.map id_clock2str cil_par) in
      let arg = String.concat "," (List.map id_clock2str cil_arg) in
 	   Printf.printf 
	     "# EvalClock.check_args (%s) \n#\t clock params=%s\n#\t clock args = %s\n"
        pos par arg;
      flush stdout
    );
  assert (List.length cil_par = List.length cil_arg);
  let idl_par,cil_par = List.split cil_par
  and idl_arg,cil_arg = List.split cil_arg
  in
  let ns1 = (List.map2 (fun x y -> x,y) idl_arg idl_par)@s1 in 
  let ns = 
    assert (List.length cil_arg = List.length cil_par);
    fold_left3 UnifyClock.f (ns1,s2) lxms cil_arg cil_par
  in
  let res = (s1,snd ns) in (* should UnifyClock.f modify the current bindings *)
  Lv6Verbose.exe
    ~flag:dbg (
      fun () ->
      let s1 = UnifyClock.subst_to_string (s1,s2) in
      let s2 = UnifyClock.subst_to_string ns in
      let s3 = UnifyClock.subst_to_string res in
      Printf.printf "# EvalClock.check_args result:\n#\t s=%s\n#\t ns=%s \n#\t res=%s\n"
                    s1 s2 s3;
      flush stdout
    );
    res 
(*     ns *)

(** Checking expression result
    --------------------------

    It is the same thing, except that we have Lic.left list instead 
    of Lic.id_clock list.

    e.g., in the equation:

         (x,y)=toto(a,b);

 
   we want to check that the clocks of "x" and "y" are compatible with
    the output clock profile of node "toto".

    To do that, we suppose we have:
    
    - "left" the list of Lic.left
    - "rigth" the list of result clock. (via "get_clock_profile" again)

    and we just need to check that both side match.
*)

let rec (var_info_eff_of_left_eff: Lic.left -> Lic.var_info) =
  function
    | LeftVarLic   (v, _) -> v
    | LeftFieldLic (l, id,_) -> 
      let v = var_info_eff_of_left_eff l in
      let new_name = (Lv6Id.to_string v.var_name_eff) ^ "." ^ (Lv6Id.to_string id) in
      { v with  var_name_eff = Lv6Id.of_string new_name }

    | LeftArrayLic (l,i,_) -> 
      let v = var_info_eff_of_left_eff l in
      let new_name = (Lv6Id.to_string v.var_name_eff) ^ "[" ^ 
        (string_of_int i) ^ "]" 
      in
      { v with  var_name_eff = Lv6Id.of_string new_name }

    | LeftSliceLic (l,si,_) -> 
      let v = var_info_eff_of_left_eff l in
      let new_name = (Lv6Id.to_string v.var_name_eff) ^
                       (LicDump.string_of_slice_info_eff si)
      in
      { v with  var_name_eff = Lv6Id.of_string new_name }


let var_info_eff_to_clock_eff v = v.var_clock_eff

(* exported *)
let (check_res : Lxm.t list -> subst -> Lic.left list -> Lic.id_clock list -> unit) =
  fun lxms (s1,s2) left rigth ->
  let left_vi = List.map var_info_eff_of_left_eff left in
  let left_ci = List.map var_info_eff_to_clock_eff left_vi in
  if (List.length left_ci <> List.length rigth) then assert false;
  let idl_rigth,rigth = List.split rigth
  and idl_left, left_ci = List.split left_ci in
  let s = (List.combine idl_rigth idl_left)@s1, s2 in
  assert (List.length left_ci = List.length rigth);
  Lv6Verbose.exe
    ~flag:dbg (
      fun () ->
      let s1 = UnifyClock.subst_to_string (s1,s2) in
      let s2 = UnifyClock.subst_to_string s in
      let pos = Lxm.position (List.hd lxms) in
      Printf.printf "# EvalClock.check_res (%s):\n#\t s=%s\n#\t s=%s\n"
                    pos s1 s2;
      flush stdout
    );
  
  let ns = fold_left3 UnifyClock.f s lxms left_ci rigth in
  Lv6Verbose.exe
    ~flag:dbg (
      fun () ->
      let s2 = UnifyClock.subst_to_string s in
      let s3 = UnifyClock.subst_to_string ns in
      let pos = Lxm.position (List.hd lxms) in
      Printf.printf "# EvalClock.check_res (%s):\n#\t s=%s\n#\t ns=%s \n"
                    pos s2 s3;
      flush stdout
    )
  
  

  (******************************************************************************)

  (** In order to clock check "when" statements, we need to know if a clock [sc]
    is a sub_clock of another one c (i.e., for all instants i, c(i) => sc(i))

    If it is a sub_clock, we return the accumulator (the current substitution)
   *)
let (is_a_sub_clock : 
           Lxm.t -> subst -> Lic.id_clock -> Lic.id_clock -> subst option) =
  fun lxm s (_id_sc,sc) (_id_c,c) -> 
  (* Check if [sc] is a sub-clock of [c] (in the large sense). 
       Returns Some updated substitution if it is the case, and None otherwise *)
  let rec aux sc c =
    match sc, c with 
    (* the base clock is a sub-clock of all clocks *)
    | BaseLic, (BaseLic|On(_,_)|ClockVar _) -> Some s            
    | On(_,_), BaseLic -> None

    | sc, On(_,c_clk) -> (
      try Some(UnifyClock.f s lxm sc c)
      with Compile_error _ -> aux sc c_clk 
    )
    | _,_ -> 
       try Some(UnifyClock.f s lxm sc c)
       with Compile_error _ -> None
  in
  aux sc c
          
type clock_profile = Lic.id_clock list * Lic.id_clock list

let (get_clock_profile : Lic.node_exp -> clock_profile) = 
  fun n -> 
      (List.map var_info_eff_to_clock_eff n.inlist_eff, 
       List.map var_info_eff_to_clock_eff n.outlist_eff)

let ci2str = LicDump.string_of_clock2


let list_fill x n = 
  let rec aux x n acc =
    if n <= 0 then acc else aux x (n-1) (x::acc)
  in
  aux x n []
let rec (get_lxm_list : Lic.val_exp list -> int -> Lxm.t list) =
  fun args n -> 
    
    if n <> List.length args then
      (* occur if args is a tuple (only ?). 
         get the list from the tuple in that case *)
      (match args with
        | [{ve_core=CallByPosLic({it=Lic.TUPLE;_}, args);_}] -> get_lxm_list args n
        | _ -> (* occurs ? *)
          let lxm = lxm_of_val_exp (List.hd args) in
          list_fill lxm n
      )
    else 
      List.map lxm_of_val_exp args 
      

(******************************************************************************)
(** Now we can go on and define [f].  *)

let rec (f : IdSolver.t -> subst -> Lic.val_exp -> Lxm.t list -> Lic.clock list -> 
             Lic.val_exp * Lic.id_clock list * subst) =
  fun id_solver s ve lxms exp_clks ->
  (* we split f so that we can reinit the fresh clock var generator *)
  let ve, inf_clks, s = f_aux id_solver s ve in
  let s =  
    if exp_clks = [] then s else (
      if (List.length  exp_clks <> List.length inf_clks) then
        let msg = Printf.sprintf "Bad arity: %i expected, %i found"
                                 (List.length exp_clks) (List.length inf_clks)
        in
        raise (Compile_error(lxm_of_val_exp ve, msg))
      else
        fold_left3
          (fun s lxm eclk iclk -> UnifyClock.f s lxm eclk iclk) 
          s 
          lxms
          exp_clks
          (List.map (fun (_,clk) -> clk) inf_clks)

    )
  in 
  let inf_clks = List.map (fun (id,clk) -> id, apply_subst2 s clk) inf_clks in
  let clks = snd (List.split inf_clks) in
  let ve = apply_subst_val_exp s ve in
  if Lv6Verbose.level() > 2 then
    print_string  (
        "Clocking the expression '" ^ (LicDump.string_of_val_exp_eff false ve) ^"': "^ 
          (LicDump.string_of_clock2 (List.hd clks)) ^"\n");
  assert(ve.ve_clk <> []);
  ve, inf_clks, s

and (f_aux : IdSolver.t -> subst -> Lic.val_exp
             -> Lic.val_exp * Lic.id_clock list * subst) =
  fun id_solver s ve -> 
  let ve, cel, s, lxm = 
    match ve.ve_core with    
    | CallByPosLic ({it=posop; src=lxm},  args) -> (
      let args, cel, s = eval_by_pos_clock id_solver posop lxm args s in
      if
        (match posop with CURRENT _ -> true | _ -> false) &&
          (List.length args = 1) && Lic.val_exp_is_constant (List.hd args) 
      then
      (* current of a constant: we can ignore the current *)
        let ve,cel,s = f_aux id_solver s (List.hd args) in
        ve, cel, s, lxm  
      else
        let ve = 
          match posop,args with
          | CURRENT None, { ve_clk = (On((cc,cv,ct),cv_clk))::clks ;_ }::_ -> 
             (* We attach the clock constructor to CURRENT and the
                   clock var to the list of args. Indeed, the user does not need
                   to specify the clock when it uses current ; hence we add this
                   information as soon as it is computed, i.e., here.
              *)
             let cv_val_exp = flagit (Lic.VAR_REF cv) lxm in
             let cv_val_exp = Lic.CallByPosLic(cv_val_exp,[]) in
             let cv_val_exp = { ve_core = cv_val_exp ; ve_typ = [ct] ;
                                ve_clk = [cv_clk];ve_src = lxm } in
             let posop,args = CURRENT (Some cc), cv_val_exp::args in
             List.iter
               (* all clks should be the same *) 
               (fun clk -> assert(clk = On((cc,cv,ct),cv_clk))) clks;
             let ve = { ve with 
                        ve_core = CallByPosLic ({it=posop; src=lxm}, args) ;
                        ve_clk  = cv_clk::(List.map (fun _ -> cv_clk) clks)
                      }
             in
             ve
          | _ -> { ve with ve_core = CallByPosLic ({it=posop; src=lxm},  args)}
        in
        (*         List.iter (fun arg -> assert (arg.ve_clk <> [])) args; *)
        ve, cel, s, lxm
    )
    | CallByNameLic ({it=nmop; src=lxm}, nmargs) -> (
      try
        let nmargs, cel, s = eval_by_name_clock id_solver nmop lxm nmargs s in
        let ve = { ve with ve_core = CallByNameLic ({it=nmop; src=lxm}, nmargs) } in
        List.iter (fun (_,arg) -> assert (arg.ve_clk <> [])) nmargs;
        ve, cel, s, lxm
      with EvalConst_error msg ->
        raise (Compile_error(lxm, "\n*** can't eval constant: "^msg))
    )
    | Merge(ce, cl) -> 
       let ce, _cel, s = f_aux id_solver s ce in
       let (merge_clk : Lic.clock) = List.hd ce.ve_clk in
       let ce_id,lxm = match ce with
         | { ve_core= CallByPosLic({it = VAR_REF id ;_},[]) ;_} -> id, ve.ve_src
         | _ -> assert false
       in 
       let check_case (s,acc) (c,ve) =
         (* Check that ve is on c(ce) on merge_clk *)
         let id_clk =
           match c.it with
           | Bool_const_eff true  -> "Lustre", "true"
           | Bool_const_eff false -> "Lustre", "false"
           | Enum_const_eff (s,_) ->  s
           | _ -> assert false
         in
         let id_clk = (id_clk, ce_id, Lic.type_of_const c.it) in
         let exp_clk = On(id_clk, merge_clk) in
         (* [ve] can be a tuple! nb: tuple with different clocks won't work here *)
         let exp_clk = List.map (fun _ -> exp_clk) ve.ve_typ in
         let lxms =  List.map (fun _ -> c.src) ve.ve_typ in
         let ve,_cel,s = f id_solver s ve lxms exp_clk in
         s, (c,ve)::acc
       in
       let s,cl = List.fold_left check_case (s,[]) cl in
       let ve = { ve with ve_core =  Merge(ce, List.rev cl) } in
       let merge_clk = List.map (fun _ -> ce_id, merge_clk) ve.ve_typ in
       ve, merge_clk, s, lxm
  in
  let new_clk = snd (List.split cel) in
  let s, ve = 
    if ve.ve_clk = [] then (s, { ve with ve_clk = new_clk }) else
      let s = 
        assert(List.length ve.ve_clk = List.length new_clk);
        List.fold_left2 (fun s -> UnifyClock.f s lxm) s ve.ve_clk new_clk in
      s, ve
  in
  let ve = apply_subst_val_exp s ve in
  ve, cel, s

(* iterate f on a list of expressions *)
and (f_list : IdSolver.t -> subst -> Lic.val_exp list -> 
              Lic.val_exp list * Lic.id_clock list list * subst) =
  fun id_solver s args ->
  let aux (args,acc,s) arg =
    let arg, cil, s = f_aux id_solver s arg in 
    (arg::args, cil::acc, s)
  in
  let (args, cil, s) = List.fold_left aux ([],[],s) args in
  let args = List.rev args in
  let cil = List.rev cil in
  let cil = List.map (List.map(fun (id,clk) -> id, apply_subst2 s clk)) cil in
  (*   List.iter (fun arg -> if arg.ve_clk <> [] then assert false else ()) args; *)
  args, cil, s

and (eval_by_pos_clock : IdSolver.t -> Lic.by_pos_op -> Lxm.t -> Lic.val_exp list ->
                         subst -> Lic.val_exp list * Lic.id_clock list * subst) =
  fun id_solver posop lxm args s ->
  let apply_subst s (id,clk) = id, UnifyClock.apply_subst s clk in
  let args, (cl,s) =
    match posop,args with
    | Lic.CURRENT (Some _), _clk_arg::_ -> assert false
    | Lic.CURRENT None, [arg] -> ( (* we return the clock of the argument *)
      let args, clocks_of_args, s = f_list id_solver s [arg] in
      let current_clock = function
        | (id, BaseLic) -> (id, BaseLic)
        | (id, On(_,clk)) -> (id, clk)
        | (id, ClockVar i) -> (id, ClockVar i)
      in
      args, (List.map current_clock (List.flatten clocks_of_args), s)
    )
    | Lic.CURRENT None, _     -> assert false (* SNO *)
    | Lic.CURRENT (Some _), _ -> assert false (* SNO *)
    | Lic.WHEN when_exp_clk,args -> (
      let c_clk =
        match when_exp_clk with
        | BaseLic -> BaseLic
        | ClockVar _ -> assert false
        | On((_cc,_c,_), c_clk) -> 
           (* | NamedClock { it = (cc,c) ; src = lxm } -> *)
           (* let id, c_clk = (id_solver.id2var c lxm).var_clock_eff in *)
           c_clk
      in
	   let aux_when exp_clk s =
        (* 
               In order to clock check an expression such as

               exp when C(c)
               
               we first need to check that the clock of c (c_clk)
               is a sub-clock of the clock of exp (exp_clk).
         *)
        match is_a_sub_clock lxm s exp_clk (fst exp_clk,c_clk) with
        | None -> 
           let msg = "\n*** clock error: '" ^ (ci2str (snd exp_clk)) ^ 
                       "' is not a sub-clock of '" ^ (ci2str c_clk) ^ "'"
           in
           raise (Compile_error(lxm, msg))
        | Some s -> 
           let id_when_exp_clk, s =
             match exp_clk with
             | id, On(_var,_) -> (id, when_exp_clk), s
             | id, BaseLic -> (id, when_exp_clk), s
             | id, ClockVar i ->
                let id_when_exp_clk = id, when_exp_clk in
                let (s1,s2) = s in
                id_when_exp_clk, 
                (s1, UnifyClock.add_subst2 i when_exp_clk s2)
           in
           id_when_exp_clk, s
	   in
      let res = 
        (match f_list id_solver s args with
         (* XXX ce cas ne sert à rien !!!
               le traitement du cas ou la liste ne contient qu'un element
               n'a aucun raison d'etre particulier, si ???
          *)
         | args,[[exp_clk]], s -> 
		      let (exp_clk,s) = aux_when exp_clk s in
		      args,([exp_clk], s)

         | args, [exp_clk_list], s ->
            (* when on tuples *)
		      let exp_clk_list, s = 
		        List.fold_left 
		          (fun (acc,s) exp_clk -> 
			        let (exp_clk,s) = aux_when exp_clk s in
			        exp_clk::acc, s
		          ) 
		          ([],s) 
		          exp_clk_list
		      in
		      args,(List.rev exp_clk_list, s)
         |  _ -> assert false (* "(x1,x2) when node (x,y)" *)
        )
      in
      Lv6Verbose.exe
        ~flag:dbg (
          fun () ->
          let _,(clk_list,_) = res in
          let id_clock2str (id,c) = Printf.sprintf "[%s,%s]"
                                                   id (LicDump.string_of_clock2 c)
          in
          let str = List.map id_clock2str clk_list in
          let str = String.concat "," str in
          let pos = Lxm.position lxm in
          Printf.printf
            "# EvalClock.eval_by_pos_clock @When (%s) %s\n" pos str;
          flush stdout            
        );
      res
    )
    | Lic.HAT(_i), [] ->  assert false
    | Lic.HAT(_i), ve::_ -> 
       let (ve,clk,s) = f_aux id_solver s ve in
       [ve],(clk,s)

    | Lic.VAR_REF id,args -> 
       let vi = IdSolver.var_info_of_ident id_solver id lxm in
       args,([var_info_eff_to_clock_eff vi], s)

    | Lic.CONST c, args -> (
      let s, clk = UnifyClock.new_clock_var s in
      args,([Lic.string_of_const c, clk], s)
    )
    | Lic.ARRAY,[] -> ( (* empty arrays are a kind of constant actually *)
      let s, clk = UnifyClock.new_clock_var s in
      [],(["[]", clk],s)
    )
    | Lic.CONST_REF idl,args -> 
       let _const = IdSolver.const_eff_of_item_key id_solver idl lxm in
       let s, clk = UnifyClock.new_clock_var s in
       args,([Lv6Id.of_long idl, clk], s)
    | Lic.CALL nkf,args -> 
       let node_key = nkf.it in
       let lxm = nkf.src in
       let node_exp_eff = IdSolver.node_exp_of_node_key id_solver node_key lxm in
       let (cil_arg, cil_res) = get_clock_profile node_exp_eff in
       let s, rel_base = UnifyClock.new_clock_var s in
       (*  the value of the base clock of a node is actually relative
              to the context into which the node is called.
              
              Hence we create a fresh var clock, that will be instanciated
              by the caller.
        *)
       let (replace_base : Lic.clock -> Lic.id_clock -> Lic.id_clock) =
         fun rel_base (id,ci) -> 
         (* [replace_base rel_base ci ] replaces in [ci] any occurences of the
                 base clock by [rel_base] *)
         let rec aux ci =
           match ci with
           | BaseLic -> rel_base
           | On(v,clk) -> On(v, aux clk)
           | ClockVar _i -> ci
         in
         id, aux ci
       in
       let cil_arg = List.map (replace_base rel_base) cil_arg in 
       let cil_res = List.map (replace_base rel_base) cil_res in 
       let args, clk_args, s = f_list id_solver s args in
       let lxms = get_lxm_list args (List.length cil_arg) in
       let s = check_args lxm lxms s cil_arg (List.flatten clk_args) in
       let cil_res = List.map (apply_subst s) cil_res in
       args, (cil_res, s)

    | Lic.PREDEF_CALL (nkf),args  -> 
       let op = AstPredef.string_to_op (snd(fst nkf.it)) in
       let args, clk_args, s =  f_list id_solver s args in
       let flat_clk_args = List.flatten clk_args in (* all predef nodes are mono-clock! *)
       let _,flat_clk_args = List.split flat_clk_args in
       let lxms = get_lxm_list args (List.length flat_clk_args) in
       let clk_list, s =       
         if args = [] then [],s else
           let s = UnifyClock.list lxms flat_clk_args s in
           List.map (List.map (apply_subst s)) clk_args, s
       in
       args, LicEvalClock.f id_solver op lxm s clk_list

    (* One argument. *)
    | Lic.PRE,args
    | Lic.STRUCT_ACCESS _,args
    | Lic.ARRAY_ACCES  (_),args
    | Lic.ARRAY_SLICE (_),args  -> 
       assert(List.length args = 1);
       let (arg,clk,s) = f_aux id_solver s (List.hd args) in
       [arg], (clk,s)
    (* may have tuples as arguments *)
    | Lic.TUPLE,args
    | Lic.ARROW,args
    | Lic.FBY   ,args
    | Lic.CONCAT,args
    | Lic.ARRAY,args -> (
      (* Check that all args are of the same (unifiable) clocks.

             XXX : we suppose that all those operators are
             mono-clocks (i.e., when they return tuples, all elements
             are on the same clock).  It would be sensible to have,
             e.g., arrows on multiple clocks. We'll refine later.  *)
      let args, clk_args, s =  f_list id_solver s args in
      let flat_clk_args = List.flatten clk_args in (* => mono-clock! *)
      let idl,flat_clk_args = List.split flat_clk_args in
      let lxms = get_lxm_list args (List.length flat_clk_args) in
      let s = UnifyClock.list lxms flat_clk_args s in
      let clk_list =
        match posop, clk_args with
        | Lic.TUPLE,_ -> 
           let clk_l = List.map (UnifyClock.apply_subst s) flat_clk_args in
           List.combine idl clk_l
        | _, [] -> assert false (* cf above *)
        | _, _ -> List.map (apply_subst s) (List.hd clk_args)
      in
      args, (clk_list, s)
    )          
  in
  args, cl, s

and (eval_by_name_clock : IdSolver.t -> Lic.by_name_op -> Lxm.t -> 
                          (Lv6Id.t Lxm.srcflagged * Lic.val_exp) list -> subst -> 
                          (Lv6Id.t Lxm.srcflagged * Lic.val_exp) list * Lic.id_clock list * subst) =
  fun id_solver namop _lxm namargs s -> 
  let apply_subst s (id,clk) = id, UnifyClock.apply_subst s clk in
  let args = List.map (fun (_id,ve) -> ve) namargs in
  (* XXX The 3 following lines duplicates the code of TUPLE_eff and co *)
  let args, clk_args, s =  f_list id_solver s args in
  let namargs = List.map2 (fun (id,_ve) ve -> id,ve) namargs args in
  let flat_clk_args = List.flatten clk_args in (* => mono-clock! *)
  let _,flat_clk_args = List.split flat_clk_args in
  let lxms = get_lxm_list args (List.length flat_clk_args) in
  let s = UnifyClock.list lxms flat_clk_args s in
  let clk_list = List.map (apply_subst s) (List.hd clk_args) in
  match namop with
  | Lic.STRUCT_anonymous -> assert false (* cf EvalType.E *)
  | Lic.STRUCT(_) -> namargs, clk_list, s
  | Lic.STRUCT_with(_, _dft) ->
     (* XXX should i do something here ??? *)
     namargs,clk_list, s

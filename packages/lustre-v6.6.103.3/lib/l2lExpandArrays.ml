(** Time-stamp: <modified the 11/03/2021 (at 21:07) by Erwan Jahier> *)

(* Replace structures and arrays by as many variables as necessary.
   Since structures can be nested, it might be a lot of new variables...
   
   For instance, a variable
   v : Toto { f1 : int ; f2 : int ^ 3 ; f3 : t^2 }

   where
   type t = T { x:int ; y:int } 

   will be expanded into 

   _v_f1 : int;
   _v_f2_0 : int;
   _v_f2_1 : int;
   _v_f2_2 : int;
   _v_f3_1_x : int;
   _v_f3_1_y : int;
   _v_f3_2_x : int;
   _v_f3_3_y : int;

   nb : if 't' was a type that does not contain any struct type, we would just 
   have 3 variables.
  *)

open Lxm
open Lic
    
type acc =     
    Lic.val_exp srcflagged list      (* assertions *)
    * (Lic.eq_info srcflagged) list  (* equations *)
    * Lic.var_info list              (* new local vars *)

let dbg =  (Lv6Verbose.get_flag "esa")

(********************************************************************************)
(* pack useful info (while expanding nodes) into a single struct *)
type local_ctx = { 
  node : Lic.node_exp;
  prg : LicPrg.t;
}
        
(* stuff to create fresh var names. 
   XXX code dupl. with Split.new_var 
*)
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

(* for local use: polymorphic predef operators should not transformed; hence,
   whenever we reach a Any/AnyNum type, we raise that exception and skip the 
   transformation of the current node.
*)
exception Polymorphic


(* returns a new var based on [vi] with type [type_eff]. *)
let clone_var _node_env vi str type_eff = 
  let str = (Lv6Id.to_string vi.var_name_eff) ^ str in
  let id = Lv6Id.of_string (str) in
  let clk_id = Lv6Id.of_string str in
  let type_eff = match type_eff with 
      TypeVar Any | TypeVar AnyNum -> raise Polymorphic
    | _ -> type_eff
  in
  let var =
    { 
      var_name_eff   = id;
      var_nature_eff = vi.var_nature_eff;
      var_number_eff = vi.var_number_eff; (* this field is useless: to be removed. *)
      var_type_eff   = type_eff;
      var_clock_eff  = clk_id, snd vi.var_clock_eff;
    }
  in
  (*     Hashtbl.add node_env.lenv_vars id var; *)
  var


let rec (is_a_basic_type : Lic.type_ -> bool) =
  function
    | Array_type_eff _ | Struct_type_eff _ -> false
    | TypeVar Any | TypeVar AnyNum -> raise Polymorphic
    | Abstract_type_eff(_, teff) -> is_a_basic_type teff 
    | External_type_eff(_) 
    | Enum_type_eff (_, _) 
    | Real_type_eff 
    | Int_type_eff 
    | Bool_type_eff  -> true

let soi = string_of_int


let (index_list_of_slice_info : Lic.slice_info -> int list) =
  fun si -> 
    let rec aux acc cpt =
      if ((si.se_step > 0 && cpt > si.se_last) || (si.se_step < 0 && cpt < si.se_last))
      then acc else aux (cpt::acc) (cpt + si.se_step)
    in
      List.rev (aux [] si.se_first)


(** left expr expansion.

  The objective is to generate the set of vars defined by a left expr.

  First step: var_trees_of_left recursively traverse the left structure 
  to compute the left expr variable. E.g., in the left expr "X.f2[4]" we 
  want to find "X" (and its type). 
  
  Second step: Using the type of X, we compute the set of variables defined 
  by "X" (gen_var_trees). The set is actually structured into a tree-like
  data struture var_tree (to be able to deal with slices).
  
  Third step (var_trees_of_left again): cut off some branches of the tree using
  the left filter ("f2[4]") to keep only the  variable effectivily defined 
  by the left expr (exercise for the reader: try to do the same with a flat data 
  type ; it's just a nigthmare because of slices).
  
  In other words: 
  - when we find a left leave, we generate all the possible names
  corresponding to that var, in a data structure (a tree) that reflect
  the lustre data structure (w.r.t. array and struct)
  - Then, struct or array accesses remove some branches of that tree
  
*)
(* var_trees are used to represent  left var_tree, and val_exp var_tree *)
type 'a var_tree = 
    A of 'a var_tree list  (* should i use an array there? *)
  | S of (Lv6Id.t * 'a var_tree) list (* A Map.t ? *)
  | L of 'a
(* Quite similar to L2lCheckOutputs.var_def_state, which is logic. *)

let rec (flatten_var_tree : 'a var_tree -> 'a list) =
  function       
    | A array -> List.flatten (List.map flatten_var_tree array)
    | S fl -> List.flatten (List.map (fun (_id,vt) -> flatten_var_tree vt) fl)
    | L str -> [str] 

let rec (gen_var_trees :
        (string -> Lic.type_ -> 'a) -> string -> Lic.type_ -> 'a var_tree) =
  fun make_leave prefix teff ->
    let loop = gen_var_trees make_leave in
    match teff with
      | TypeVar Any | TypeVar AnyNum -> raise Polymorphic
      | Bool_type_eff | Int_type_eff | Real_type_eff 
      | Enum_type_eff(_) | External_type_eff(_)
        -> 
        L (make_leave prefix teff)

      | Abstract_type_eff(_,teff) ->  loop prefix teff

      | Array_type_eff(teff_elt,i) ->
        let rec unfold acc cpt =
       if cpt < 0 then acc else
            let prefix = prefix ^ "_" ^ (soi cpt) in
            let vt = loop prefix teff_elt in
            unfold (vt::acc) (cpt-1)
        in
     A (unfold [] (i-1))
      | Struct_type_eff(_, fl) ->
        S (List.map 
             (fun (fn, (steff, _const_opt)) ->
               let prefix = prefix^"_"^(Lv6Id.to_string fn) in
         (fn, loop prefix steff )
             ) 
             fl)

let (expand_left : local_ctx -> left -> left list) = 
  fun lctx left -> 
  let rec (var_trees_of_left : left -> left var_tree) =
    fun left -> 
      match left with
      | LeftVarLic (vi,lxm)   -> 
        let make_left lctx lxm vi prefix teff =
          LeftVarLic (clone_var lctx vi prefix teff, lxm)
        in
	gen_var_trees (make_left lctx lxm vi) "" vi.var_type_eff
      | LeftFieldLic (l,id,_t) -> 
        (match var_trees_of_left l with
         | S fl -> List.assoc id fl 
         | A _ | L _  -> assert false
        )
      | LeftArrayLic (l,i,_t)  ->
        (match var_trees_of_left l with
         | A array -> List.nth array i
         | S _ | L _ -> assert false
        )
      | LeftSliceLic (l,si,_t) ->
        (match var_trees_of_left l with
         | A array -> 
           let index_list = index_list_of_slice_info si in
           let l = List.map (fun i -> List.nth array i) index_list in
           A l
         | S _ | L _ -> assert false
        )
  in
  let vt = try var_trees_of_left left 
    with 
    | Polymorphic -> assert false
    | Not_found -> assert false
    | Failure _ ->  assert false
    (* should not occur: just a defense against nth and assoc *)
  in
  flatten_var_tree vt

let rec unfold i x = if i <= 0 then [] else x::(unfold (i-1) x)

let rec (expand_array_types : Lic.type_ list -> Lic.type_ list) =
  fun tl ->
  (* arrays are transformed into tuples *)
  List.flatten (List.map aux tl)
and
  (aux :Lic.type_  -> Lic.type_ list) = function
  | Array_type_eff(st,i) -> unfold i st
  | Struct_type_eff(_, l) ->
    let tl = List.map (fun (_, (t,_)) -> t) l in
    let tll = List.map aux tl in
    List.flatten tll
  | t -> [t]
        (* arrays within abstract and struct won't be translated.  

           XXX should i raise an error saying that -esa is not
           compatible with structure of arrays (instead of silently
           returns arrays) ?  To handle them, i would need to modify
           Lic.type_ and to replace 'type_' by 'type_ list' in all
           the recursive cases. It would be quite a lot of work and
           -esa is not a useful option anymore...  *)

(********************************************************************************)
(** build a new loc that will alias ve, and add its definition in the 
    set of equations (cf acc) *)
let rec (make_new_loc : local_ctx -> Lxm.t -> acc -> Lic.val_exp 
         -> acc * var_info) =
  fun lctx lxm acc ve -> 
  let teff = List.hd ve.ve_typ in
  let ceff = List.hd ve.ve_clk in
  let nv = new_var "v" lctx teff ceff in
  let neq = [LeftVarLic(nv,lxm)], ve in
  let neq = flagit neq lxm in
  let nvl, (asserts,eqs,locs) = expand_var_info lctx ([],acc) nv in
  let acc = (asserts,eqs, List.rev_append nvl locs) in
  expand_eq lctx acc neq, nv

and (var_trees_of_val_exp : 
       local_ctx -> acc -> Lic.val_exp -> acc * Lic.val_exp var_tree) =
  fun lctx acc ve -> 
  let make_val_exp lxm vi prefix teff =
    let prefix = (Lv6Id.to_string vi.var_name_eff) ^ prefix in
    let id = prefix in 
    {
      ve_core = CallByPosLic({src=lxm;it=(VAR_REF id)}, []);
      ve_typ  = [teff] ;
      ve_clk = [snd vi.var_clock_eff];
      ve_src = lxm
    }
  in
  let loop = var_trees_of_val_exp lctx acc in
  match ve.ve_core with
  | Merge(_ce,_cl) -> assert false (* todo *)
  | CallByPosLic (by_pos_op, vel) -> (
      let lxm = by_pos_op.src in
      let by_pos_op = by_pos_op.it in
      match by_pos_op with
      | STRUCT_ACCESS (id) -> (
          let ve = try List.hd vel with _ -> assert false in
          match loop ve with
          | acc, S fl -> acc, List.assoc id fl 
          | _, (A _ | L _)  -> assert false
	)
      | ARRAY_ACCES (i) -> (
          let ve = try List.hd vel with _ -> assert false in
          match loop ve with
          | acc, A array -> acc, List.nth array i
          | _, (S _ | L _)  -> assert false

        )
      | ARRAY_SLICE (si) -> (
          let ve = try List.hd vel with _ -> assert false in
          match loop ve with
          | acc, A array -> 
            let index_list = index_list_of_slice_info si in
            let l = List.map (fun i -> List.nth array i) index_list in
            acc, A l			         
          | _, (S _ | L _)  -> assert false
	)
      | VAR_REF id -> (
          match LicPrg.find_var id lctx.node with
          | Some vi -> 
            (acc, gen_var_trees (make_val_exp lxm vi) "" vi.var_type_eff)
          | None -> 
            let msg = 
              "\n*** during Array expansion: '"^
              (id)^
	      "': Unknown variable.\n"^
              "*** Current variables are: "^
	      (List.fold_left
                 (fun acc v -> acc^(Printf.sprintf "\n\t%s"
                                      (Lic.string_of_var_info v)))
                 ""
                 (match lctx.node.Lic.loclist_eff with None  -> [] | Some v -> v))
            in
	    raise (Lv6errors.Compile_error(lxm, msg))
        )
      | CONST const -> do_const acc lctx lxm const
      | CONST_REF idl -> (
          try
            let const = 
              match LicPrg.find_const lctx.prg idl with 
              | Some c -> c 
              | None -> assert false 
            in 
            do_const acc lctx lxm const
          with _ ->  
            let msg = 
              "\n*** during Array expansion: '"^ (Lv6Id.string_of_long false idl)^
              "': Unknown constant.\n*** Current constants are: "^
	      (LicPrg.fold_consts
                 (fun _k c acc ->
                    acc^(Printf.sprintf "\n\t%s" (Lic.string_of_const c)))
                 lctx.prg
                 "")
            in
            raise (Lv6errors.Compile_error(lxm, msg))
        ) 
      | HAT(_) | CONCAT | ARRAY
      | PREDEF_CALL _ | CALL _ 
      | PRE | ARROW | FBY | CURRENT _ | WHEN _ | TUPLE -> (
          (* Create a new loc var to alias such expressions *)
          let acc, nloc = make_new_loc lctx lxm acc ve in
          acc, gen_var_trees (make_val_exp lxm nloc) "" nloc.var_type_eff
        )
    )
  | CallByNameLic(by_name_op, _fl) ->
    let lxm = by_name_op.src in
    let acc, nloc = make_new_loc lctx lxm acc ve in
    acc, gen_var_trees (make_val_exp lxm nloc) "" nloc.var_type_eff

and do_const acc lctx lxm const =
  let _s, ve_const = 
    UnifyClock.const_to_val_eff lxm true UnifyClock.empty_subst const
  in
  let ve_const,acc =
    match ve_const.ve_core with
    | CallByPosLic ({it=CONST_REF _;_},_) ->  
      (* in order to avoid a potential infinite loop *)
      (ve_const, acc)

    | _ -> expand_val_exp lctx acc ve_const 
  in
  (acc, L (ve_const))

and (break_tuple : Lxm.t -> left list -> val_exp -> Lic.eq_info srcflagged list) =
  (* break
          x1, x2 = ve1, ve2;
     into
          x1 = ve1;
          x2 = ve2;

     Note that this work only if the node expansion has already been
     done!  (otherwise, we would not have the same number of items in the
     left and in the rigth part) *)
  fun lxm left_list ve ->
  let rec aux ve = (* flatten val exp*)
    match ve.ve_core with 
    | CallByPosLic ({it= TUPLE;_}, vel)
    | CallByPosLic ({it= CONCAT;_}, vel)
    | CallByPosLic ({it= ARRAY;_}, vel) -> List.flatten (List.map aux vel)
    | CallByPosLic ({src=lxm;it= CONST (Array_const_eff(cl,t))}, []) ->
      List.map (fun c ->
          { ve_core = CallByPosLic ({src=lxm;it= CONST c}, []);
            ve_typ = [t];
            ve_clk = [List.hd ve.ve_clk];
            ve_src = ve.ve_src
          }) cl
    | CallByPosLic ({src=lxm;it= HAT(i)}, vel) ->
      let ve1 = List.hd vel in
      let ve1l = aux ve1 in
      List.map
	(fun ve1 -> { ve1 with ve_core = CallByPosLic ({src=lxm;it= HAT(i)}, [ve1])}) 
	ve1l 
    | CallByPosLic (unop, [ve1]) ->
      let ve1l = aux ve1 in
      List.map
	(fun ve1 -> { ve1 with ve_core = CallByPosLic (unop, [ve1])} ) 
	ve1l 
    | CallByPosLic ({ it=CURRENT c ; src=lxm}, [clk;ve]) -> (
	let vel = aux ve in
	List.map
	  (fun ve -> { ve with ve_core = CallByPosLic
                                   ({it=CURRENT c;src=lxm}, [clk;ve])}) 
	  vel 
      )
    | CallByPosLic (binop, [ve1;ve2]) ->
      let ve1l, ve2l = aux ve1, aux ve2 in
      if (List.length ve1l <> List.length ve2l) then
        [ve] 
      else
	List.map2 
	  (fun ve1 ve2 -> { ve with ve_core = CallByPosLic (binop, [ve1;ve2])})
	  ve1l 
	  ve2l		          
    | CallByPosLic ({it= PREDEF_CALL(
        {src=if_lxm ; it = ("Lustre","if"),[]}); src=lxm}, [cond; ve1; ve2]) -> (
	let ve1l = aux ve1 in
	let ve2l = aux ve2 in
        let l1,l2= List.length ve1l, List.length ve2l in
	if (l1 <> l2) then
	  let vel2str vel = 
	    (String.concat ", " (List.map (LicDump.string_of_val_exp_eff false) vel))
	  in
	  let msg = Printf.sprintf 
              "error: expression \n %s\n cannot be broken \n %s (%d)
  should have the same arity as\n%s(%d)"
              (LicDump.string_of_val_exp_eff false ve)
              (vel2str ve1l) l1  (vel2str ve2l) l2
          in
	  raise (Lv6errors.Compile_error(lxm, msg)) 
	else
	  List.map2 
	    (fun ve1 ve2 -> 
               { ve with
                 ve_core = 
                   CallByPosLic ({it= PREDEF_CALL({src=if_lxm ; 
                                                   it = ("Lustre","if"),[]}); src=lxm}, 
                                 [cond;ve1;ve2])}
            ) 
	    ve1l 
	    ve2l
      )
    |  _ -> [ve]
  in
  let lll = List.length left_list in
  if lll = 1 then (* nothing to break *)
    [{ src = lxm ; it = (left_list, ve) }] 
  else
    let vel = aux ve in
    if (List.length vel <> lll) then
      (* migth occur for generic nodes, that needs to be compiled,
         but that will not be dumped. *)
      [{ src = lxm ; it = (left_list, ve) }] 
    else
      List.map2
	(fun l ve -> 
           let clk = [snd (Lic.var_info_of_left l).var_clock_eff] in
	   { src = lxm ; 
             it = ([l], { ve with ve_typ = [Lic.type_of_left l] ; ve_clk = clk}) }
	)
	left_list
	vel

and (expand_eq :
       local_ctx -> acc -> Lic.eq_info srcflagged -> acc) =
  fun lctx acc eqf -> 
  let { src = lxm_eq ; it = (left_list, ve) } = eqf in
  let left_list = List.flatten (List.map (expand_left lctx) left_list) in
  let ve,acc = expand_val_exp lctx acc ve in
  let eq_list = break_tuple lxm_eq left_list ve in    
  let (asserts, eqs, locs) = acc in
  (asserts,  eq_list@eqs, locs)

and expand_val_exp_list lctx acc vel = 
  List.fold_left 
    (fun (vel,acc) ve -> 
       let ve,acc = expand_val_exp lctx acc ve in
       ve::vel, acc
    ) 
    ([],acc) (List.rev vel)


and (build_and_eq: Lic.node_key srcflagged -> val_exp list -> val_exp list -> val_exp) =
  fun op vel1 vel2 ->
  (* transform "[(x1;x2] = [y1;y2]" into "x1=y1 and x2=y2" *)
  assert (op.it = (("Lustre","eq"),[]) || op.it = (("Lustre","neq"),[]));
  let conj_op = if op.it = (("Lustre","eq"),[]) then
      {src = op.src; it=(("Lustre","and"),[]) }
    else
      (* t<>t1 iff t[0]<>t'[0] OR t[1]<>t'[2] OR ... *)
      {src = op.src; it=(("Lustre","or"),[]) }
  in
  let make_eq ve1 ve2 =
    let lxm = op.src in
    {ve_core = CallByPosLic({src=lxm;it=PREDEF_CALL(op)},[ve1;ve2]);
     ve_typ = [Bool_type_eff];
     ve_clk = ve1.ve_clk;
     ve_src = lxm}
  in
  let make_and acc ve1 ve2 =
    let eq = make_eq ve1 ve2 in
    let lxm = op.src in
    {ve_core = CallByPosLic({src=lxm;it=PREDEF_CALL(conj_op)},[acc;eq]);
     ve_typ = [Bool_type_eff];
     ve_clk = ve1.ve_clk;
     ve_src = lxm}
  in
  match vel1,vel2 with
  | ve1::vel1, ve2::vel2 -> List.fold_left2 make_and (make_eq ve1 ve2) vel1 vel2
  | _,_ -> assert false (* sno *)

and (expand_val_exp: local_ctx -> acc -> val_exp -> val_exp * acc) =
  fun lctx acc ve ->
  match ve.ve_core with
  | Merge(ce,cl) -> 
    let left,vel = List.split cl in
    let vel,acc = expand_val_exp_list lctx acc vel in
    let cl = List.combine left vel in
    let newve =  { ve with ve_core = Merge(ce,cl) } in
    newve, acc
  | CallByPosLic (by_pos_op, vel) -> 
    let lxm = by_pos_op.src in
    let by_pos_op = by_pos_op.it in
    let by_pos_op, acc, vel = 
      match by_pos_op with
      | HAT(i) -> (
          let ve, acc = expand_val_exp lctx acc (List.hd vel) in
          let rec unfold (cpt, ve_acc) =
            if cpt = 0 then ve_acc else (unfold (cpt-1, ve::ve_acc))
          in
          let ve = unfold (i,[]) in 
          TUPLE, acc, ve
        )
      | PREDEF_CALL ({ src = _lxm; it = (("Lustre",("eq"|"neq")),[]) } as op) -> (
          let vel,acc = expand_val_exp_list lctx acc vel in
          match vel with
          | [{ve_core = CallByPosLic ({it = TUPLE;_}, ve1::ve12::vel1) ;_};
             {ve_core = CallByPosLic ({it = TUPLE;_}, ve2::ve22::vel2) ;_}
            ] ->
            let and_ve = build_and_eq op (ve1::ve12::vel1) (ve2::ve22::vel2) in
            let and_op, and_vel =
              match and_ve.ve_core with
              | CallByPosLic(op,vel) -> op.it, vel
              | _ -> assert false (* sno *)
            in
            and_op, acc, and_vel 

          | [_ve1; _ve2] -> by_pos_op, acc, vel
          | _  -> assert false (* sno *)
        )
      | CONCAT | PREDEF_CALL _ | CALL _  
      | PRE | ARROW | FBY | CURRENT _ | WHEN _ | TUPLE | CONST _
        -> 
        let vel,acc = expand_val_exp_list lctx acc vel in
        by_pos_op, acc, vel
      | ARRAY -> 
        let vel,acc = expand_val_exp_list lctx acc vel in
        TUPLE, acc,vel
      | STRUCT_ACCESS (_)
      | ARRAY_ACCES (_)
      | ARRAY_SLICE (_) 
      | VAR_REF _ 
      | CONST_REF _ ->
	let acc, vt = try var_trees_of_val_exp lctx acc ve 
          with (Not_found | Failure _) -> 
            assert false (* SNO: a defense against nth and assoc *)
	in
        TUPLE, acc, flatten_var_tree vt
    in
    let newve = CallByPosLic(Lxm.flagit by_pos_op lxm, vel) in
    let new_typ = expand_array_types ve.ve_typ in
    let newve =  { ve with 
                   ve_core = newve ;
                   ve_typ = new_typ;
                   ve_clk = List.map (fun _ -> List.hd ve.ve_clk) new_typ
                 } 
    in
    (*     if newve.core <> ve.core then ( *)
    (*               EvalClock.copy newve ve *)
    (*             ); *)
    newve, acc

  | CallByNameLic(by_name_op, fl_val) ->
    (* we want to print fields in the order of the type.
       Moreover, we have to deal with default value.
    *)
    let teff = ve.ve_typ in 
    match teff with 
    | [Struct_type_eff(_,fl)] -> 
      let lxm = by_name_op.src in
      let vel,acc = 
        List.fold_left
          (fun (vel,acc) (id,(_,const_opt)) ->
             try
               let _,ve = List.find (fun (id2,_) -> id2.it = id) fl_val in
               let ve,acc = expand_val_exp lctx acc ve in
               ve::vel, acc
             with Not_found -> 
             match const_opt with
             | None -> assert false
             (* ougth to have been checked before *)
             | Some const ->
               let _s, ve_const =  (* XXX *)
                 UnifyClock.const_to_val_eff lxm true
                   UnifyClock.empty_subst const
               in
               let ve_const,acc=  
                 expand_val_exp lctx acc ve_const 
               in
               ve_const::vel,acc
          )
          ([],acc)
          fl 
      in
      let newve = { ve with
                    ve_core= CallByPosLic({ src=lxm ; it=TUPLE }, (List.rev vel)) 
                  }
      in
      (*             if newve.core <> ve.core then ( *)
      (* EvalClock.copy newve ve *)
      (*                     ); *)
      newve, acc
    | _ -> assert false

and (expand_val_exp_flag: local_ctx -> acc -> 
     val_exp srcflagged -> val_exp srcflagged * acc) =
  fun lctx acc { src = lxm ; it = ve } -> 
  let ve,acc = expand_val_exp lctx acc ve in
  { src = lxm ; it = ve }, acc

and (expand_assert: local_ctx -> acc -> val_exp srcflagged -> acc) =
  fun lctx acc ve -> 
  let (ve, (asserts, eqs, locs)) = expand_val_exp_flag lctx acc ve in
  (ve::asserts, eqs, locs)

and (expand_var_info: local_ctx -> var_info list * acc ->
     var_info -> var_info list * acc) =
  fun lctx (vil, acc) vi -> 
  let rec aux teff =
    match teff with
    | Abstract_type_eff (_, teff) -> aux teff
    | TypeVar Any | TypeVar AnyNum -> raise Polymorphic
    | Struct_type_eff (_name, fl) -> 
      List.fold_left
        (fun (vil,acc) (fn, (ft,_const_opt)) ->
           let new_var = clone_var lctx vi ("_" ^ Lv6Id.to_string fn) ft in
           let new_vil, new_acc = expand_var_info lctx (vil,acc) new_var in
           new_vil, new_acc
        )
        (vil, acc)
        fl
    | Array_type_eff(at,size) -> (
        let rec local_aux i (vil,acc) =
          if i=size then  (vil,acc) else
            let new_var = clone_var lctx vi ("_" ^ soi i) at in
            let new_vil, new_acc = expand_var_info lctx (vil,acc) new_var in
            if new_vil = new_var::vil then (
              (* [new_var] type is not made of structure *)
              assert (is_a_basic_type at);
              (* XXX
                 Hashtbl.add nenv.lenv_vars new_var.var_name_eff new_var *)
            );
            local_aux (i+1) (new_vil, new_acc)
        in
        local_aux 0 (vil,acc)
      )
    | External_type_eff(_)
    | Enum_type_eff (_, _)
    | Real_type_eff
    | Int_type_eff
    | Bool_type_eff -> 
      vi::vil, acc
  in
  let vil,acc = aux vi.var_type_eff in
  vil,acc

let rec (node : local_ctx -> Lic.node_exp -> Lic.node_exp) =
  fun lctx n -> 
    try
      let inlist = n.inlist_eff in
      let outlist = n.outlist_eff in            
      let acc = ([],[],[]) in
      let inlist, acc = List.fold_left (expand_var_info lctx) ([],acc)  inlist in
      let outlist, acc = List.fold_left (expand_var_info lctx) ([],acc) outlist in
      let n =
        match n.def_eff with
          | ExternLic 
          | MetaOpLic
          | AbstractLic None -> n
          | AbstractLic (Some pn) -> 
            { n with def_eff = AbstractLic (Some (node lctx pn)) }
          | BodyLic b ->
            let loclist = match n.loclist_eff with None -> [] | Some l -> l in
            let loclist, acc = List.fold_left (expand_var_info lctx) ([],acc) loclist in
            let acc = List.fold_left (expand_eq lctx) acc (List.rev b.eqs_eff) in
            let acc = List.fold_left (expand_assert lctx) acc b.asserts_eff in
            let (asserts, neqs, nv) = acc in
            let nb = { 
              eqs_eff = neqs ; 
              asserts_eff = asserts
            } 
            in
            { n with 
              loclist_eff = Some (List.rev_append loclist nv);
              def_eff = BodyLic nb
            }
      in
      { n with 
        inlist_eff  = List.rev inlist;
        outlist_eff = List.rev outlist;
      }
    with Polymorphic -> n

(* exported *)
let (doit : LicPrg.t -> LicPrg.t) =
  fun inprg -> 
    let outprg = LicPrg.empty in
  (* types and constants do not change *)
    let outprg = LicPrg.fold_types  LicPrg.add_type  inprg outprg in
    let outprg = LicPrg.fold_consts LicPrg.add_const inprg outprg in
  (* transform nodes *)
    let (do_node : Lic.node_key -> Lic.node_exp -> LicPrg.t -> LicPrg.t) = 
      fun nk ne outprg -> 
        Lv6Verbose.exe ~flag:dbg (fun() ->
                                  Printf.printf "#DBG: L2lExpandArrays expands '%s'\n"
            (Lic.string_of_node_key nk));
        let lctx = {
          node = ne;
          prg = inprg;
        }
        in
        let ne = node lctx ne in
        LicPrg.add_node nk ne outprg
    in
    let outprg = LicPrg.fold_nodes do_node inprg outprg in
    outprg

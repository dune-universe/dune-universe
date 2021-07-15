(* Time-stamp: <modified the 29/08/2019 (at 16:47) by Erwan Jahier> *)

(* XXX a revoir et faire comme expliqué ici :
http://www.cs.cornell.edu/courses/cs3110/2011sp/lectures/lec26-type-inference/type-inference.htm

   C'est peu ou prou, ce que je fais, mais bon, j'ai plus trop confiance
   dans ce module, et il mériterait un bon nettoyage.

   Un truc louche, c'est que je pretend utiliser de l'unification
   pour vérifier les appels de noeud, en cherchant à unifier les
   parametres et les arguments. Mais, me semble-t'il, cette
   vérification est orientée (ca ne marche que si on fait "f par arg"
   (ou le contraire). Et cette dissymétrie n'apparait ni dans le mli,
   ni dans le terme unify.

   Par ailleur, subst devrait s'appeler env (en reminiscence des
   environnements de types).

*)
open Lxm
open Lv6errors
open Lic

(** DEBUG FLAG *)
let dbg = (Lv6Verbose.get_flag "clock-check2")


let ci2str = LicDump.string_of_clock2

(* exported *)
type subst1 = (Lv6Id.t * Lv6Id.t) list

(**
   A dedicated kind of substitution tailored to deal with clock variables 
   (Lic.ClockVar). A few fact motivating why a general unification algorithm
   is not necessary.
   
   - ClockVar can only be created at val_exp leaves.

   - each leave returns a unique ClockVar

   - By construction, clocks of the form "On(id,clk)" do not
   contain ClockVar (I should probably craft a new Lic.clock type
   that materialize this fact).
   
   - When unifying two Lic.clocks, as far as ClockVar are concerned, there is 
   therefore only two cases:

   (1) c1,c2= ClockVar i, ClockVar j
   (2) c1,c2= ClockVar i, clk (where clk does not contain ClockVar _ )
   
   
   We do that by constructing subst2 only via the function [add_link2] (case 1) and 
   [add_subst2] (case 2).

   And since [add_subst2] is linear (cf the code), this unification algorithm 
   is also linear.
*)


module IntMap = Map.Make(struct type t = int let compare = compare end)
module IntSet = Set.Make(struct type t = int let compare = compare end)

(* Clock variable information associated to a clock var cv *)
type cv_info = 
  | Equiv of IntSet.t (* the list of CV that are equals to cv *)
  | Clk of clock (* a ground clock *)
      (* The idea of this DS is the following. We maintain, for free CVs,
         the list of CVs that are equals to them. Hence, once a CV is
         becomes bounded, it is easy to bound all the CVs that were equal to
         it.
      *)

type cv_tbl = cv_info IntMap.t
type subst2 = { cpt : int; cv_tbl : cv_tbl }

type subst = subst1 * subst2

(******************************************************************************)
let (subst2_to_string : subst2 -> string) = 
  fun s2 -> 
    (String.concat " ; " 
       (IntMap.fold 
          (fun i cvi acc ->
            (match cvi with
              | Clk c -> "CV"^(string_of_int i)^"->"^(ci2str c)
              | Equiv is -> "CV" ^ (string_of_int i) ^ " in  {" ^ 
                (String.concat "," 
                   (IntSet.fold (fun i acc -> ("CV"^(string_of_int i))::acc) is [])) ^
                "}"
            )::acc
          )
          s2.cv_tbl
          []
       )) 

let (subst_to_string : subst -> string) = 
  fun (s1,s2) -> 
    let v2s = Lv6Id.to_string in
    let s2str = subst2_to_string s2 in
    let s1str = 
      (String.concat ", " (List.map (fun (v1,v2) -> (v2s v1) ^ "->" ^ (v2s v2)) s1))
    in
    if s2str = "" then s2str else if s1str = "" then s2str else
       (s1str ^" ; " ^ s2str)
        
(******************************************************************************)

(* exported *)
let (empty_subst2:subst2) = { cpt = 0 ; cv_tbl = IntMap.empty }
let (empty_subst:subst) = [], empty_subst2

let (_add_subst1 : Lv6Id.t -> Lv6Id.t -> subst1 -> subst1) =
  fun id1 id2 s ->
    if List.mem_assoc id1 s then s else (id1,id2)::s


let rec (add_link2 : int -> int -> subst2 -> subst2) =
  fun i j s2 ->
    if j > i then add_link2 j i s2 else
      let tbl = s2.cv_tbl in
      let cvi_i = IntMap.find i tbl
      and cvi_j = IntMap.find j tbl in
      let tbl =
        match cvi_i, cvi_j with
          | Equiv li, Equiv lj ->
            let l = IntSet.union li lj in
            let tbl = IntSet.fold (fun i tbl -> IntMap.add i (Equiv l) tbl) l tbl in
            tbl
          | Equiv l, Clk c 
          | Clk c, Equiv l ->
            IntSet.fold (fun i tbl -> IntMap.add i (Clk c) tbl) l tbl
          | Clk c1, Clk c2 -> 
            assert (c1=c2); tbl
      in
      { s2 with cv_tbl = tbl }


let (add_subst2 : int -> Lic.clock -> subst2 -> subst2) =
  fun i c s2 ->
    let tbl =
      match IntMap.find i s2.cv_tbl with
        | Equiv l -> 
          IntSet.fold (fun i tbl -> IntMap.add i (Clk c) tbl) l s2.cv_tbl
        | Clk c2 -> 
          IntMap.add i (Clk c2) s2.cv_tbl
    in
    { s2 with cv_tbl = tbl }



let (find_subst1 : Lv6Id.t -> subst1 -> Lv6Id.t option) =
  fun id s ->
    try Some (List.assoc id s) with Not_found -> None

let (find_subst2 : int -> subst2 -> Lic.clock option) =
  fun i s2 ->
    try
      match IntMap.find i s2.cv_tbl with
        | Equiv _l -> None
        | Clk c -> Some c
    with Not_found -> 
      print_string (" *** Don't know anything about CV" ^ (string_of_int i) ^ "\n");
      print_string (" in the table : " ^ subst2_to_string s2);
      flush stdout;
      assert false


(******************************************************************************)
(* Stuff to generate fresh clock vars *)

(* exported *)
let (new_clock_var : subst -> subst * Lic.clock) =
  fun (s1, s2) -> 
    let clk = ClockVar s2.cpt in
    let tbl = IntMap.add s2.cpt (Equiv (IntSet.singleton s2.cpt)) s2.cv_tbl in
    let s2 = { cpt = s2.cpt+1; cv_tbl = tbl } in
(*       print_string (" >>> Creating CV" ^ (string_of_int (s2.cpt-1)) ^ "\n"); *)
(*       flush stdout; *)
      (s1, s2), clk


(* exported *)
let rec (apply_subst:subst -> Lic.clock -> Lic.clock) =
  fun (s1,s2) c ->
    match c with
      | BaseLic -> BaseLic
      | On((cc,cv,ct),clk) ->
	     let cv = match find_subst1 cv s1 with Some cv2 -> cv2 | None -> cv in
        let clk = apply_subst (s1,s2) clk in
	     On((cc,cv,ct),  clk)
      | ClockVar i ->
        match find_subst2 i s2 with
          | Some clk -> apply_subst (s1,s2) clk
          | None -> c


(* exported *)
(* apply only the second part of the subst *)
let rec (apply_subst2:subst -> Lic.clock -> Lic.clock) =
  fun (s1,s2) c ->
    match c with
      | BaseLic -> BaseLic 
      | On(v,clk) -> 
        let clk = apply_subst2 (s1,s2) clk in
        On(v, clk)
      | ClockVar i -> 
        match find_subst2 i s2 with
          | Some clk -> apply_subst2 (s1,s2) clk
          | None -> c
	    
let rec (apply_subst_val_exp : subst -> Lic.val_exp -> Lic.val_exp) =
  fun s ve ->
    let ve_core = 
      match ve.ve_core with
        | CallByPosLic (by_pos_op, vel) -> 
          let vel = List.map (apply_subst_val_exp s) vel in
          CallByPosLic (by_pos_op, vel)
        | CallByNameLic(by_name_op, fl) -> 
          let fl = List.map
            (fun (fn,ve) -> (fn, apply_subst_val_exp s ve)) fl
          in
          CallByNameLic(by_name_op, fl)
        | Merge(ce,cl) -> 
          let cl = List.map
            (fun (fn,ve) -> (fn, apply_subst_val_exp s ve)) cl
          in
          Merge(ce, cl) 
    in
    let new_clk = List.map (apply_subst s) ve.ve_clk in
    let ve = { ve with ve_core = ve_core ; ve_clk = new_clk } in
    ve


let is_clock_var = function 
  | On(_) | BaseLic -> false | ClockVar _ -> true
  
(* exported 

It's not really an unification algo, but rather a « compatibility checking ».
By compatibility, I mean that, 
- c1 = c2
- c1 or c2 are clock var
- if c1=On(id1,cc1) and c2=On(id2,cc2) then
   cc1 and cc1[id1/id2] are compatible
*)

let (f : subst -> Lxm.t -> Lic.clock -> Lic.clock -> subst) =
  fun (s1,s2) lxm  arg par -> 
    let arg = apply_subst (s1,s2) arg in
    let par = apply_subst (s1,s2) par in
    let rec aux (s1,s2) (c1,c2) = 
      match (c1,c2) with
	     | On(_v,_clk), BaseLic
	     | BaseLic, On(_v,_clk) -> (s1,s2)
	     (* not unifiable: we stop trying to find a substitution and return s;
	        the error message is issued outside [aux] (just below).
	     *)
	     | BaseLic, BaseLic -> (s1,s2) (* ok *)
	     | On((cc1,cv1,_), clk1), On((cc2,cv2,_), clk2) ->
           if (cc1,cv1) = (cc2,cv2) then aux (s1,s2) (clk1, clk2) else (s1,s2)
	     | ClockVar i, ClockVar j ->
          if i=j then s1,s2 else s1,(add_link2 i j s2) 
	     | ClockVar i, ci
	     | ci, ClockVar i -> s1, add_subst2 i ci s2
    in
    let s = aux (s1,s2) (arg,par) in
    let compatible =
      (* Two clocks are compatible if, once s have been applied, they are
         equal, or one of them is a clock variable *)
      let npar = apply_subst s par
      and narg = apply_subst s arg in
      narg = npar || is_clock_var narg || is_clock_var npar
    in
    if s <> (s1,s2) then
      Lv6Verbose.exe
        ~flag:dbg
        (fun () ->
 	      print_string (
	          "# UnifyClock.f ("^(Lxm.position lxm)^"), unifying \n#\t -" ^ (ci2str arg) ^
	            " \n#\t -" ^ (ci2str par) ^ "\n# produces s = " ^ (subst_to_string s)^
                 "\n# out of   s = " ^ (subst_to_string (s1,s2))^
                   "\n"
           );
         flush stdout;
        );
    if compatible then s else
	   let msg =  
	     ("\n*** clock error: The two following clocks are not compatible:\n***\t" ^ 
	         (ci2str arg) ^ " (provided clock)\n***\t" ^ 
	         (ci2str par) ^ " (expected clock)\n")
	   in
	   raise(Compile_error(lxm, msg))


(* exported *)
let (list : Lxm.t list -> Lic.clock list -> subst -> subst) =
  fun lxms cl s ->
  if cl = [] then s 
  else (
    assert(lxms<>[]);
    snd(
    List.fold_left2
      (fun (clk,s) lxm clk2 ->
	     let s = f s lxm clk clk2 in
	     let clk = apply_subst s clk in (* necessary? *)
	     (clk,s)
      )
      (List.hd cl, s)
      (List.tl lxms)
      (List.tl cl)
  ))


(******************************************************************************)
(* exported *)
(* This function is in this module to avoid to export new_clock_var. A good idea? *)
let rec (const_to_val_eff: Lxm.t -> bool -> subst -> const -> subst * val_exp) = 
  fun lxm expand_const s const -> 
    let mk_by_pos_op_arg by_pos_op_eff arg =
      let _s,clk = match const with
	     | Tuple_const_eff cl ->
          let f (s,clks) _c =  
            let s, clk = new_clock_var s in
            (s,clk::clks)
          in
          List.fold_left f (s,[]) cl

	     | _c ->
          let s, clk = new_clock_var s in
          s, [clk]
      in
      { ve_core = CallByPosLic(flagit by_pos_op_eff lxm, arg) ; 
        ve_typ = types_of_const const ;
        ve_clk = clk;
        ve_src = lxm
      }
    in
    let mk_by_pos_op by_pos_op_eff = mk_by_pos_op_arg by_pos_op_eff [] in
    match const with
    | Bool_const_eff _ 
    | Int_const_eff  _
    | Real_const_eff _ -> s, mk_by_pos_op (CONST const)
    | Enum_const_eff   (l, _)
    | Extern_const_eff (l, _) -> s, mk_by_pos_op (CONST_REF l)

    | Abstract_const_eff (l, _teff, c, _is_exported) -> 
      if expand_const 
      then const_to_val_eff lxm expand_const s c
      else s, mk_by_pos_op (CONST_REF l)
    | Array_const_eff  (ct, _) -> 
      let s, vel = 
        List.fold_left 
          (fun (s,vel) c -> 
             let s,ve = const_to_val_eff lxm expand_const s c in
             (s,ve::vel)
          ) 
          (s,[])
          ct
      in
      let vel = List.rev vel in
      s, mk_by_pos_op_arg ARRAY vel
    | Tuple_const_eff cl ->
      let s, vel = 
        List.fold_left 
          (fun (s,vel) c ->  
             let s, ve = const_to_val_eff lxm expand_const s c in
             (s, ve::vel)
          )
          (s,[])
          cl
      in
      let vel = List.rev vel in
      s, mk_by_pos_op_arg TUPLE vel
    | Struct_const_eff (fl, stype) ->
      let sname = match stype with 
        | Struct_type_eff(sname, _) -> sname
        | _ -> assert false
      in
      let name_op_flg = flagit (STRUCT(sname)) lxm in
      let s, fl = 
        List.fold_left
          (fun (s,fl) (id,const) -> 
             let s, ve = const_to_val_eff lxm expand_const s const in
             s, (flagit id lxm, ve)::fl
          )
          (s,[])
          fl
      in
      let fl = List.rev fl in
      s, { ve_core = (CallByNameLic(name_op_flg, fl));
           ve_typ = [stype] ;
           ve_clk = [BaseLic];
           ve_src = lxm
         }

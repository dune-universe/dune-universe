(*-----------------------------------------------------------------------
** Copyright (C) - Verimag.
** This file may only be copied under the terms of the CeCill
** Public License
**-----------------------------------------------------------------------
**
** File: gne.ml
** Main author: erwan.jahier@univ-grenoble-alpes.fr
*)

(* ZZZ :

   Never use NeMap.add, but add_ne instead, otherwise the gne.t
   migth be incoherent !!!

   Indeed, I use a map to ease the search of common expression.
   Hence when adding a branch to a gne, I need to merge it 
   if the same expression exists !!
*)

module NeMap = struct
  include Map.Make(struct type t = Ne.t let compare = compare end)
end


(* exported *)	
type t = (Bdd.t * bool) NeMap.t
(* The flag is true iff the [Ne.t] depends on inputs or pre. *)

let (dump : t -> unit) =
  fun gne ->
    ignore (NeMap.fold
       (fun ne (bdd, _dep) acc ->
	       print_string ("\n\t" ^ (Ne.to_string ne) ^ "  -> ");
          flush stdout;
          Bdd.print_mons bdd;
          flush stdout;
          acc
       )
       gne
       ""
    );
    print_string "\n"

(* This one does not manipulate gne that are partitions !!  It just
   adds an element to the partition, merging common expressions if
   necessary.
*)
let (add_ne : Bdd.t -> bool -> Ne.t -> t -> t) =
  fun bdd dep ne gne -> 
	 try 
      let (bdd_gne, dep_gne) = NeMap.find ne gne in
        NeMap.add ne ((Bdd.dor bdd bdd_gne), dep || dep_gne) gne
	 with Not_found ->
		NeMap.add ne (bdd, dep) gne


(* Check that all bbds in gne forms a partition, which should always
   be the case !! *)
let (dyn_check_gne : t  -> bool) =
  fun gne -> 
    let bddl = NeMap.fold
	   (fun _ne (c,_dep) acc -> c::acc)
	   gne
	   []
    in
    let union_ok =
      let union = 
        List.fold_left
          (fun bdd acc -> Bdd.dor acc bdd)
          (Bdd.dfalse())
          bddl
      in
        Bdd.is_true union
    in
    let inter_ok =
      let inter_i_ok = Util.cartesian_product bddl bddl
        (fun x y -> x=y || (Bdd.is_false(Bdd.dand x y)))
      in
        List.fold_left
          (fun x acc -> x && acc)
          true
          inter_i_ok 
    in
      if NeMap.empty <> gne && not union_ok then (
        print_string "BAD GNE: the unions of guards is not equal to true!\n";
        dump gne;
        flush stdout
      );
      if not inter_ok then (
        print_string "BAD GNE: some guards have non-null intersection!\n";
        dump gne;
        flush stdout
      );
      (NeMap.empty = gne) || 
        (union_ok && inter_ok)

(****************************************************************************)


let (apply_op_gn_expr : (Ne.t -> Ne.t) -> t -> t) =
  fun op gne ->
    let res = 
    ( NeMap.fold
	     (fun ne (c,dep) acc -> 
           add_ne c dep (op ne) acc)
	     gne
	     NeMap.empty
    )
    in
      assert(dyn_check_gne res); 
      res


(* exported *)	
let (opposite : t -> t) =
  fun gne -> 
    let res = 
      apply_op_gn_expr (Ne.opposite) gne
    in
      assert(dyn_check_gne res); 
      res



(****************************************************************************)
let (apply_binop_gn_expr : (Ne.t -> Ne.t -> Ne.t) -> t -> t -> t) =
  fun op gne1 gne2 ->
    let res = 
      ( NeMap.fold
	       (fun ne1 (c1, dep1) acc1 ->
	          ( NeMap.fold
	              (fun ne2 (c2, dep2) acc2 ->
		              let c = (Bdd.dand c1 c2)
		              and dep = dep1 || dep2
		              in
		                if Bdd.is_false c
		                then acc2
	                   else
		                  let ne = op ne1 ne2 in
                          add_ne c dep ne acc2
	              )
	              gne2
	              acc1
	          )
	       )
	       gne1
	       NeMap.empty
      )
    in
      assert(dyn_check_gne res); 
      res



    (* exported *)	
let (add : t -> t -> t) =
  fun gne1 gne2 -> apply_binop_gn_expr (Ne.add) gne1 gne2

(* exported *)	
let (diff : t -> t -> t) =
  fun gne1 gne2 -> apply_binop_gn_expr (Ne.diff) gne1 gne2

(* exported *)	
let (mult : t -> t -> t) =
  fun gne1 gne2 -> apply_binop_gn_expr (Ne.mult) gne1 gne2

(* exported *)	
let (quot : t -> t -> t) =
  fun gne1 gne2 -> apply_binop_gn_expr (Ne.quot) gne1 gne2

(* exported *)	
let (modulo : t -> t -> t) =
  fun gne1 gne2 -> apply_binop_gn_expr (Ne.modulo) gne1 gne2

(* exported *)	
let (div : t -> t -> t) =
  fun gne1 gne2 -> apply_binop_gn_expr (Ne.div) gne1 gne2

(****************************************************************************)

(* exported *)	
let (empty : unit -> t) =
  fun _ ->
    NeMap.empty

(* exported *)	
let (make : Ne.t -> bool -> t) =
  fun  ne b ->
    let res = 
      (* it is ok to us NeMap.add instead of add_ne on an empty Gne.t *)
      NeMap.add ne (Bdd.dtrue (), b) NeMap.empty
    in
      res

(* exported *)	
let (fold : (Ne.t -> Bdd.t * bool -> 'acc -> 'acc) -> t -> 'acc -> 'acc) =
  fun f gne acc0 ->
    NeMap.fold f gne acc0

let (_find : Ne.t -> t -> Bdd.t * bool) =
  fun ne gne ->
    NeMap.find ne gne


(* exported *)	
let (of_ite : Bdd.t -> bool -> t -> t -> t) =
  fun bdd b gne_t gne_e ->
    let bdd_not = Bdd.dnot bdd in
    let gne = 
      if Bdd.is_true  bdd then gne_t else
      if Bdd.is_false bdd then gne_e else
        let gne =
          NeMap.fold
            (fun ne (g, dep) acc -> 
               let bdd = Bdd.dand g bdd in
                 if not (Bdd.is_false bdd) then 
                   add_ne bdd (dep||b) ne acc
                 else
                   acc
            )
            gne_t
            NeMap.empty
        in         
        let gne = 
          NeMap.fold
            (fun ne (g, dep) acc -> 
               let bdd = Bdd.dand g bdd_not in
                 if not (Bdd.is_false bdd) then 
                   add_ne bdd (dep||b) ne acc
                 else
                   acc
            )
            gne_e
            gne
        in
          gne
    in
      assert(dyn_check_gne gne); 
      gne
      
(* exported *)	
let (get_constant : t -> Value.num option) =
  fun gne ->
    match
      (NeMap.fold
	      (fun ne (bdd, _) acc -> (bdd, ne)::acc)
	      gne
	      []
      )
    with
	     [(bdd, ne)] ->
	       if
	         (Bdd.is_true bdd) && (Ne.is_a_constant ne)
	       then
	         Ne.find "" ne
	       else
	         None
      | _ -> None


(****************************************************************************)
(****************************************************************************)

(* exported *)	
let (to_string : t -> string) =
  fun gne ->
    (NeMap.fold
       (fun ne (_bdd, _dep) acc ->
	       ("\n\t" ^ (Ne.to_string ne) ^ "  -> << a bdd >>  ;" ^ acc ))
       gne
       "\n"
    )





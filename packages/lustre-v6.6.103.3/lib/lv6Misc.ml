(* Time-stamp: <modified the 29/08/2019 (at 15:35) by Erwan Jahier> *)
open Lic
open Lxm

(** [left_eff] is a kind of list, but which is in the « reverse » order for
    easy checking; [filtered_left] contains just the same information, but
    the list is made explicit and the information (struct or array
    accesses) is ordered in the « good » way.
*)

type filtered_left = Lic.var_info * Lxm.t * filter list
and filter = 
  | Slice of int * int * int * Lic.type_
  | Faccess of Lv6Id.t * Lic.type_
  | Aaccess of int * Lic.type_

let (left_eff_to_filtered_left: Lic.left Lxm.srcflagged -> filtered_left) =
  fun le ->
    let rec (aux : Lic.type_ -> filter list -> Lic.left -> filtered_left) =
      fun _te_top acc le -> match le with
	     | LeftVarLic  (v,lxm) -> v, lxm, acc
	     | LeftFieldLic(le,id,te) -> aux te (Faccess(id,te)::acc) le
	     | LeftArrayLic(le,i,te)  -> aux te (Aaccess(i,te)::acc) le
	     | LeftSliceLic(le,si,te) ->
	       aux te (Slice(si.se_first,si.se_last,si.se_step,te)::acc) le
    in
    let te_top = (Lic.var_info_of_left le.it).var_type_eff in
    let (v,lxm,f) = aux te_top [] le.it in
    let (_,f) =
      (* It's more useful want to associate to each accessors the
	      type of the « accessed elements », but its own type. E.g.,
	      if "t" is an array of bool, we want to associate 't[0]' and
	      an array of bool, and not to a bool.  *)
      List.fold_left 
	     (fun (te_top,acc) el -> 
	       match el with
	         | Slice(i,j,k,te) -> te, (Slice(i,j,k,te_top))::acc
	         | Faccess(id,te)  -> te, (Faccess(id,te_top))::acc
	         | Aaccess(i,te)   -> te, (Aaccess(i,te_top))::acc
	     )
	     (te_top,[])
	     f
    in
    (v,lxm, List.rev f) 


(* Time-stamp: <modified the 29/08/2019 (at 15:42) by Erwan Jahier> *)

open Lxm
open Lv6errors
open Lic
open Lv6Misc

(*********************************************************************************)
(** Used to represent how much « defined » a variable is. *)
type var_def_state =
  | VDS_def    (* completly defined   *)
  | VDS_undef  (* completly undefined *)
  | VDS_struct of (Lv6Id.t * var_def_state) list
  | VDS_array of var_def_state array

let id2str = Lv6Id.to_string
let int2str = string_of_int

(* Returns the list of undefined variables *)
let (vds_to_undefined_vars : string -> var_def_state -> string list) =
  fun v vds -> 
    let rec aux vds v acc = match vds with
      | VDS_def -> acc
      | VDS_undef -> v::acc
      | VDS_struct(fl) -> 
	     List.fold_left (fun acc (id,vds) -> aux vds (v^"."^(id2str id)) acc) acc fl
      | VDS_array(a) -> fst
	     (Array.fold_left
	        (fun (acc,i) vds -> aux vds (v^"["^(int2str i) ^"]") acc, i+1) (acc,0) a)
    in 
    aux vds v []
	
(* Returns the list of defined variables *)
let (vds_to_defined_vars : string -> var_def_state -> string list) =
  fun v vds -> 
    let rec aux vds v acc = match vds with
      | VDS_def -> v::acc
      | VDS_undef -> acc
      | VDS_struct(fl) -> 
	     List.fold_left (fun acc (id,vds) -> aux vds (v^"."^(id2str id)) acc) acc fl
      | VDS_array(a) -> fst
	     (Array.fold_left
	        (fun (acc,i) vds -> aux vds (v^"["^(int2str i) ^"]") acc, i+1) (acc,0) a)
    in 
    aux vds v []
	
(*********************************************************************************)
(** This is the main function: it computes a new [var_def_state] from an
    [var_def_state] and a [filtered_left].  *)
let (update_var_def_state : var_def_state -> filtered_left -> var_def_state) = 
  fun vds (v, lxm, filters) ->
    let rec (update_vds : string -> var_def_state -> filter list -> var_def_state) =
      fun v vds filters -> 
	     if vds = VDS_def then 
	       let msg = "\n*** Variable " ^ v ^ " is already defined." in 
	       raise (Compile_error(lxm, msg))
	     else
	       match filters with
	         | [] -> 
		        let already_def_vars = vds_to_defined_vars v vds in
		        if already_def_vars  <> [] then 
		          let msg = 
		            "\n*** Variable " ^ v ^ " is already partly defined (" ^
		              (String.concat "," (already_def_vars)) ^ ")."
		          in 
		          raise (Compile_error(lxm, msg))
		        else
		          VDS_def
	         | Slice(i,j,k,te)::tail -> update_slice v i j k tail 
		        (match vds with
		          | VDS_array(a) -> a
		          | VDS_undef -> undef_array_of_type te
		          | _ -> assert false (* sno, by type checking *)
		        )
	         | Aaccess(i,te)::tail -> update_array_access v i  tail
		        (match vds with
		          | VDS_array(a) -> a
		          | VDS_undef -> undef_array_of_type te
		          | _ -> assert false (* sno, by type checking *)
		        )
	         | Faccess(id,te)::tail -> update_field_access v id tail 
		        (match vds with 
		          | VDS_struct(fl) -> fl
		          | VDS_undef -> undef_struct_of_type te
		          | _ -> assert false (* sno, by type checking *)
		        )

    and undef_array_of_type = function
      | Abstract_type_eff(_, Array_type_eff(_,size))
      | Array_type_eff(_,size) -> Array.make size VDS_undef
      | t -> 
        prerr_string ((Lic.string_of_type t) ^ " outh to be an array\n");
        flush stderr;
        assert false
	       
    and undef_struct_of_type = function
      | Struct_type_eff(_,fl) -> List.map (fun (id,_) -> id, VDS_undef) fl
      | _ -> assert false
	     
    and array_for_all pred a = Array.fold_left (fun acc x -> acc && (pred x)) true a 
    and update_slice v i j k filters a =
      let v = v^"["^(int2str i)^ ".."^(int2str j)^
	     (if k=1 then "]" else " step "^(int2str k)^"]") 
      in
      let sub_size = ((j-i)/k+1) in
      let sub_a = Array.make sub_size VDS_undef in
      let vds = 
	     for l=0 to sub_size-1 do sub_a.(l) <- a.(i+l*k) done;
	     update_vds v (VDS_array sub_a) filters 
      in
	   (match vds with
	     | VDS_undef
	     | VDS_struct(_) -> assert false
	     | VDS_def      -> for l=0 to sub_size-1 do a.(i+l*k) <- VDS_def   done
	     | VDS_array _sa -> for l=0 to sub_size-1 do a.(i+l*k) <- sub_a.(l) done
	   );
	   if array_for_all (fun elt -> elt = VDS_def) a then VDS_def else VDS_array(a)

    and update_array_access v i filters a =
      let v = v ^ "["  ^ (int2str i) ^ "]" in
      let vds_i = update_vds v a.(i) filters in
	   a.(i) <- vds_i;
	   if array_for_all (fun elt -> elt = VDS_def) a then VDS_def else VDS_array(a)

    and update_field_access v id filters fl =
      let vds_id = List.assoc id fl in
      let v = v ^ "." ^ (id2str id) in
      let vds = update_vds v vds_id filters in
      let fl = replace_field id vds fl in
	   if List.for_all (fun (_,vds) -> vds = VDS_def) fl
	   then VDS_def
	   else VDS_struct(fl)

    and replace_field fn new_fv l =
      match l with
	     | (id,fv)::tail ->
	       if id = fn then (id,new_fv)::tail
	       else (id,fv)::(replace_field fn new_fv tail)
	     | [] -> assert false (* fn is necessarily in l *)
    in
    let res = update_vds (id2str v.var_name_eff) vds filters in
    res

(*********************************************************************************)
(** Sort out the Lic.left according to the variable they define. *)

module VarMap = Map.Make(
  struct 
    type t = Lic.var_info 
    let compare = Lic.compare_var_info
  end
)
  
let (partition_var : Lic.left srcflagged list -> (Lic.left srcflagged list) VarMap.t) = 
  fun l ->
    let f tab le =  
      let v = Lic.var_info_of_left le.it in
	   try VarMap.add v (le::(VarMap.find v tab)) tab
	   with Not_found -> VarMap.add v [le] tab
    in
    List.fold_left f VarMap.empty l
    
(*********************************************************************************)
(* exported *)
let (check_node : Lic.node_exp -> unit) =
  fun node -> 
    let vars_to_check  = match node.loclist_eff with
      | None -> node.outlist_eff 
      | Some l -> node.outlist_eff @ l
    in
    let (check_one_var : Lic.var_info -> Lic.left srcflagged list -> unit) =
      fun v lel ->
	     let ell = List.map left_eff_to_filtered_left lel in
	     match List.fold_left update_var_def_state VDS_undef (List.rev ell) with
	       | VDS_def -> ()
	       | vds -> 
		      let msg = "\n*** Undefined variable(s): " ^ 
		        (String.concat ", " (vds_to_undefined_vars (id2str v.var_name_eff) vds)) 
		      in
            let lxm = (List.hd lel).src in
		      raise (Compile_error(lxm, msg))
    in
    match node.def_eff with  
	   | ExternLic 
	   | MetaOpLic
	   | AbstractLic _ -> ()
	   | BodyLic{ eqs_eff = eql ;_} ->
	     let lel = (* list of left *)
          List.flatten 
            (List.map 
               (fun {it=(left,_);src=lxm} -> 
                 List.map (fun l -> Lxm.flagit l lxm) left)
               eql)
        in
	     let lel_map = partition_var lel in
	     (* Check that one does not define an input *) 
	     VarMap.iter 
		    (fun v lel -> 
		      if v.var_nature_eff = AstCore.VarInput then 
		        let msg = "\n*** Error; " ^(id2str v.var_name_eff) ^ 
		          " is an input, and thus cannot be defined."
		        in
              let lxm = (List.hd lel).src in
		        raise (Compile_error(lxm, msg))
		    )
		    lel_map;
	     List.iter
		    (fun v ->
		      try check_one_var v (VarMap.find v lel_map)
		      with Not_found ->
              let msg = "\n*** \"" ^  
                (id2str v.var_name_eff) ^ "\" (" ^
                (Lic.string_of_var_info v) ^ 
                ") is not defined. \n*** Defined variables are: \n  - "^
		          (String.concat "\n  - " 
                   (List.map (fun (v,_v) -> 
                     "\""^(id2str v.var_name_eff) ^ "\" ("^Lic.string_of_var_info v ^ ")"
(*                      id2str v.var_name_eff *)
                    ) (VarMap.bindings lel_map))) ^ "\n"
		        in
		        raise (Compile_error(node.lxm, msg))
		    )
		    vars_to_check

(* XXX put that check into a new dedicated L2lCheckKeyword module *)
let reserved_node_names = ["map";"fold";"boolred";"red";"fill";"fillred";"condact"]

(* exported *)
let (doit :  LicPrg.t -> unit) =
  fun inprg -> 
  let (do_node : Lic.node_key -> Lic.node_exp -> unit) = 
    fun nk ne -> 
    let pack_name, node_name = fst nk in
    if List.mem node_name reserved_node_names && pack_name <> "Lustre" then
	   let msg = Printf.sprintf "'%s' is a reserved node name, sorry" node_name in
      raise (Lv6errors.Global_error msg)    
    else
      check_node ne
  in
  LicPrg.iter_nodes do_node inprg 

open Exenum
open Exenum_internals.Convenience

(* Test injectivity:
 * Two differents indices lead to two different values. 
 * Quite easy to check, using ocaml polymorphic comparison. *)
let check_injective exen start length =
   
  Printf.printf "Starting injectivity test...\n%!" ;

  let rec check set index tested =
    if tested = length then ()
    else
      begin
	if tested mod 100000 = 0 then
	  Printf.printf "%d values tested.\n%!" tested ;

	let value = get exen index in
	assert (not (Setp.mem value set)) ;
	check (Setp.add value set) (succ index) (tested + 1)
      end
  in
  check (Setp.empty compare) start 0 ;

  Printf.printf "Injectivity test passed successfully.\n%!" ;
  ()


(* Test surjectivity 
 * See category.mli *)
module C = Categorize

(* Records stats for a given category. *)
type cat_stats = {
    (* We just count values. We do not remember values to check they are different since we assume injectivity. *)
    values_seen : Z.t ;

    size : Z.t ;
  }

let get_stat map category =
  try Hashtbl.find map (C.id category)
  with Not_found ->
    { values_seen = bigzero ;
      size = C.size category }

let check_surjective catfun exen start length =
  
  let map = Hashtbl.create 2000 in

  Printf.printf "Starting surjectivity test...\n%!" ;
  
  let rec check index tested =
    if tested = length then ()
    else
      begin
	if tested mod 10000 = 0 then
	  Printf.printf "%d values tested.\n%!" tested ;

	let value = get exen index in
	let category = catfun value in	

	let cat_stat = get_stat map category in
	let new_stat = { cat_stat with values_seen = succ cat_stat.values_seen } in

	(* Printf.printf "Category %s : %s / %s\n%!" (C.name category) (sob new_stat.values_seen) (sob new_stat.size) ; *)

	if 0 = big_compare new_stat.values_seen new_stat.size then
	  Printf.printf "Category %s has been fully enumerated.\n%!" (C.name category) ;

	assert (new_stat.values_seen <= new_stat.size) ;

	Hashtbl.replace map (C.id category) new_stat ;

	check (succ index) (tested + 1)
      end    
  in

  check start 0 ;

  Printf.printf "Surjectivity test finished.\n%!" ;  
  ()


let charlist = ['A' ; 'B' ; 'C' ; 'E' ; 'F' ; 'G']
let e_mystring = e_rstring charlist
let cat_mystring = C.cat_rstring charlist

let ex1 = pair (e_list (pair e_bool e_mystring)) (e_list (pair (e_list e_bool) e_int))

let cat_ex1 = C.pair (C.list (C.pair C.cat_bool cat_mystring)) (C.list (C.pair (C.list C.cat_bool) C.cat_int))

let ex2 = e_list (pair e_bool e_int)
let cat_ex2 = C.list (C.pair C.cat_bool C.cat_int)

let shortcharlist = ['A' ; 'B' ; 'C']
let e_mystring2 = e_rstring shortcharlist
let cat_mystring2 = C.cat_rstring shortcharlist

let ex3 = triple e_mystring2 e_mystring2 e_mystring2
let cat_ex3 = C.triple cat_mystring2 cat_mystring2 cat_mystring2

let () =
(*  check_injective ex1 bigzero 10000000 ;
  check_surjective cat_ex1 ex1 bigzero 10000000 ;

  check_injective ex2 bigzero 10000000 ;
  check_surjective cat_ex2 ex2 bigzero 10000000 ;

  check_injective e_mystring bigzero 10000000 ;
  check_surjective cat_mystring e_mystring bigzero 10000000 ;

  check_injective (e_list e_bool) bigzero 10000000 ;
  check_surjective (C.list C.cat_bool) (e_list e_bool) bigzero 10000000 ;
*)

  (* check_injective ex3 bigzero 10000000 ; *)
  check_surjective cat_ex3 ex3 bigzero 10000000 ;

  ()

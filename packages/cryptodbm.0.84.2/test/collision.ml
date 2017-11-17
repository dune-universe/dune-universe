open Bitarray
open Big_int_Z
    
(* n arrays of size 2^k *)
type detector = 
    { bits: bitarray array ;
      hashsize: int ;
      k: int ;
      mask: big_int ; 
    }

let create ~card ~fail =
  let k = card + 1 in
  let bitsize = 1 lsl k in

  { bits = Array.init fail (fun _ -> create bitsize) ;
    hashsize = fail * k ;
    k ;
    (* 2^k - 1 *)
    mask = pred_big_int (shift_left_big_int unit_big_int k) }
    
let hashsize d = d.hashsize

let insert ~hashcode d =
  let (result, _) =
    Array.fold_left 
      begin fun (flag, hash) ar ->
	
	let pos = int_of_big_int (and_big_int hash d.mask)
	and newhash = shift_right_big_int hash d.k 
	in
	
	let old = get ar pos in
	set ar pos true ;
	
	(flag && old, newhash)
      end
      (true, hashcode) d.bits
  in
  not result


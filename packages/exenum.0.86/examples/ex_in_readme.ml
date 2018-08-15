open Exenum

(* Build a new enumeration of type int list. *)
let enum_intlist = e_list e_int

(* Pretty-printing *)
let string_of_intlist l = "[" ^ String.concat ", " (List.map string_of_int l) ^ "]"

(* Show values from index 0 to 10. *)
let () = show enum_intlist string_of_intlist 0 11

(* Show values at a large index. *)
let index = Z.pow (Z.of_int 10) 200 (* Indeed, this is 10^200. *)
let () = bigshow enum_intlist string_of_intlist index 2


(* The function we wish to test on several lists. *)
let mysum l = List.fold_left (+) 0 l

let test_mysum l =
  let sum1 = mysum l
  and sum2 = List.fold_right (+) l 0 in

  assert (sum1 = sum2)

let () = tester enum_intlist ~tos:string_of_intlist ~len:1000 ~verbose_period:10000 test_mysum

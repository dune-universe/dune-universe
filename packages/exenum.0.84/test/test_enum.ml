open Exenum
open Exenum_internals.Convenience


(* Show a value. *)
let show index showf value =
  Printf.printf "=== Value at position %s ===> " (Z.to_string index) ;
  Printf.printf "%s\n%!" (showf value) ;
  Printf.printf "\n%!" ;
  ()
    
(* Show length values numbered from start. *)
let test_enum start length enum showf =

  for i = 0 to length-1 do
    let index = start +++ i in
    let value = get enum index in
    show index showf value
  done ;
  ()

let show_bool b = if b then "True" else "False"


let charlist = ['A' ; 'B' ; 'C' ; 'D' ; 'E' ; 'F' ; 'G']
let e_mystring = e_rstring charlist

(*
(* TEST 1 : enumerate integers *)
let () = test_enum (boi 0) 100 e_int string_of_int 
*)

(* TEST 2 : enumerate pairs of bool x integers *)
(*
let e_test = pair e_bool e_int
let () = test_enum (boi 0) 100 e_test (fun (x,y) -> Printf.sprintf "%b,%d%!" x y) 
*)

let huge_index = bos "9994444444444433333333333333399999999999999999800000000000"

let idstring x = "\"" ^ x ^ "\""

(* TEST 3 : enumerate Strings *)
(* let () = test_enum huge_index 400 e_string idstring *)

(* TEST 4 : enumerate boolean lists *)
(*
let blist = e_list e_bool
let () = test_enum huge_index 100 blist (sep show_bool ", ")
*)

(*
(* TEST 5 : enumerate int lists *)
let ilist = e_list e_int
let () = test_enum huge_index 100 ilist (sep string_of_int ", ")
*)

(* TEST 6 : enumerate pairs of strings *)
(* let () = test_enum bigzero 1000 (pair e_mystring e_mystring) (fun (x,y) -> (idstring x) ^ "," ^ (idstring y)) *)

(* TEST 7 : enumerate triples of strings *)
let e_mystring2 = e_rstring ['A' ; 'B' ; 'C']
let () = test_enum bigzero 1000 (triple e_mystring2 e_mystring2 e_mystring2) (fun (x,y,z) -> (idstring x) ^ "," ^ (idstring y) ^ "," ^ (idstring z))




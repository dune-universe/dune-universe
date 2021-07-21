(* Time-stamp: <modified the 20/11/2018 (at 09:41) by Erwan Jahier> *)

(** A simple front-end to the lutin Random toss machinary *)

val verbose : int ref


(** [draw bvl nvl f] draw values according to constraints contained in [f]
    - [bvl] contains Bool vars to gen
    - [nvl] contains  numeric vars to gen
    - [bvl] and [nvl] should contains at least all the variables appearing in [f].
    - The optional argument [number] controls the number of draw to be done
      (1 by default) 
 *)
val draw : ?number:int ->
  Exp.var list -> Exp.var list -> Exp.formula -> (string * bool) list list


(** [draw_in_dimacs_file dimacs_file]

   Do the same job a [draw], by looking for constraints in a dimacs file

*)
val draw_in_dimacs_file : ?number:int -> string -> (string * bool) list list


(* type num = I of Num.num | F of float *)
(* type value = B of bool | N of num *)
(* val num_draw : t -> (string * value) list *)


(*  
#require "rdbg-plugin";;
#require "lutin";;
#require "lutin.bddrand";;
open BddRandom;;
open Dimacs;;

let res_67 = draw_in_dimacs_file "tutorial1.sk_1_1.cnf";;


let res_313 = draw_in_dimacs_file "polynomial.sk_7_25.cnf";;
let res_795 = draw_in_dimacs_file "scenarios_tree_delete3.sb.pl.sk_2_32.cnf";;

let res_1026 = draw_in_dimacs_file "tableBasedAddition.sk_240_1024.cnf"

let res = draw_in_dimacs_file "10.sk_1_46.cnf";;

*)

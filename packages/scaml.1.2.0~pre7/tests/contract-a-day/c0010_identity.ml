open SCaml
let main (p:string) (s:string) = [], s

(*
parameter string ;
storage string ;
code { { /* defs */ } ;
       { /* entry point init */ DUP ; CDR ; DIP { CAR } } ;
       { /* entry point */
         { /* entry main_1004 */
           { /* = __s_1163, = s_1006, = __v_1164, = __s_1161 */ { /* var storage_1158 */ DUP } } ;
           NIL operation ;
           PAIR } } ;
       { /* final clean up */ DIP { DROP 2 } } } ;

* parameter is restored from the pair, but never used.
* We do not use UNPAIR but another variant.
* storage is DUPed but the usage is linear.
   
*)


     

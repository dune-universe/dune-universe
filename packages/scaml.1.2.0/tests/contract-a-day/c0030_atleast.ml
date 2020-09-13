open SCaml

let main () threshold =
  if Global.get_amount () < threshold then failwith "send more";
  [], threshold

(*
parameter unit;
return unit;
storage tez;                    # How much you have to send me
code {CDR; DUP;                 # Get the amount required (once for comparison, once to save back in storage)
      AMOUNT; CMPLT;            # Check to make sure no one is wasting my time
      IF {FAIL} {UNIT; PAIR}}   # Finish the transaction or reject the person
*)

(*
parameter unit ;
storage mutez ;
code { { /* defs */ } ;
       { /* entry point init */ DUP ; CDR ; DIP { CAR } } ;
       { /* entry point */
         { /* entry main_1004 */
           { /* = __threshold_1163, = threshold_1005, = __v_1164, = __threshold_1161 */
             { /* var storage_1158 */ DUP } } ;
           PUSH unit Unit ;
           DROP ;
           AMOUNT ;
           COMPARE ;
           LT ;
           IF { PUSH string "send more" ; FAILWITH } { UNIT } ;
           DROP ;
           { /* = __threshold_1163, = threshold_1005, = __v_1164, = __threshold_1161 */
             { /* var storage_1158 */ DUP } } ;
           NIL operation ;
           PAIR } } ;
       { /* final clean up */ DIP { DROP 2 } } } ;
   
* get_amount () produced  PUSH UNIT ; DROP ; AMOUNT  The argument is required for the simulation
  in OCaml side, but it produces meaningless PUSH-then-DROP.  We can clean it in Michelson level.
   
* if e1 then e2 ; e3   e2 must always end with FAILWITH.      
  The code is now compiled to   IF { ... FAILWITH } { UNIT } ; DROP ; e3 but compiled to
  IF { ... FAILWITH } { e3 }
  
*)

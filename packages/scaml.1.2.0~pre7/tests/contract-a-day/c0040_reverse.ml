open SCaml

let main (ss : int list) _storage =
  [], 
  Loop.left (fun (ss,rev) ->
      match ss with
      | [] -> Right rev
      | s::ss -> Left (ss, s::rev)) (ss, [])

(*
Here's a contract that takes a list of strings and reverses it. This contract gets into the list instructions and the LOOP instruction.

parameter (list string);
return (list string);
storage unit;
code { CAR; NIL string; SWAP; PUSH bool True; # Set up loop
       LOOP { IF_CONS {SWAP; DIP{CONS}; PUSH bool True} # Cons onto accumulator if non-empty
                      {NIL string; PUSH bool False}};   # Handle empty list
       DROP; UNIT; SWAP; PAIR}
   
Wow, the old Michelson can return a value!   
*)

(*
parameter (list int) ;
storage (list int) ;
code { { /* defs */ } ;
       { /* entry point init */ DUP ; CDR ; DIP { CAR } } ;
       { /* entry point */
         { /* entry main_1004 */
           PUSH (list int) {} ;
           { /* = __ss_1186, = ss_1005, = __v_1187, = __ss_1165 */
             { /* var global_param_1164 */ DIG 2 ; DUP ; DUG 3 } } ;
           PAIR ;
           PUSH (lambda (pair (list int) (list int)) (or (pair (list int) (list int)) (list int)))
                { { /* __r_1180 */ { /* = __v_1179 */ { /* var __arg_1167 */ DUP } } ; CDR } ;
                  { /* = __v_1173, = __ss_1177, = ss_1007, = __l_1181 */
                    { /* = __v_1179 */ { /* var __arg_1167 */ DIG 1 ; DUP ; DUG 2 } } ;
                    CAR } ;
                  IF_CONS
                    { { /* = __rev_1178, = rev_1008 */ { /* var __r_1180 */ DIG 2 ; DUP ; DUG 3 } } ;
                      { /* = __s_1171, = s_1009 */ { /* var __hd_1175 */ DIG 1 ; DUP ; DUG 2 } } ;
                      CONS ;
                      { /* = __ss_1172, = ss_1010 */ { /* var __tl_1174 */ DIG 2 ; DUP ; DUG 3 } } ;
                      PAIR ;
                      LEFT (list int) ;
                      DIP { DROP 2 } }
                    { { /* = __rev_1178, = rev_1008 */ { /* var __r_1180 */ DUP } } ;
                      RIGHT (pair (list int) (list int)) } ;
                  { /* clean __r_1180 */ DIP { DROP } } ;
                  { /* lambda clean up */ DIP { DROP } } } ;
           SWAP ;
           LEFT (list int) ;
           LOOP_LEFT { DIP { DUP } ; EXEC } ;
           DIP { DROP } ;
           NIL operation ;
           PAIR } } ;
       { /* final clean up */ DIP { DROP 2 } } } ;

   * PUSH lambda  can be converted to   LAMBDA
   * LOOP_LEFT { DIP { DUP } ; EXEC } ; DIP { DROP }
     => LOOP_LEFT { code }
*)

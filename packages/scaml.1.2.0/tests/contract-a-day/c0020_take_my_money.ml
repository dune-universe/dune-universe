open SCaml

let main kh () =
  let c = Contract.implicit_account kh in
  [ Operation.transfer_tokens () (Tz 1.0) c ], ()

(*
parameter key_hash;
return unit;
storage unit;
code { CAR; DEFAULT_ACCOUNT; # Create an account for the recipient of the funds
       DIP{UNIT};             # Push a value of the storage type below the contract
       PUSH tez "1.00";       # The person can have a ꜩ
       UNIT;                 # Push the contract's argument type
       TRANSFER_TOKENS;      # Run the transfer
       PAIR };                # Cleanup and put the return values
*)

(*
parameter key_hash ;
storage unit ;
code { { /* defs */ } ;
       { /* entry point init */ DUP ; CDR ; DIP { CAR } } ;
       { /* entry point */
         { /* entry main_1004 */
           PUSH unit Unit ;
           NIL operation ;
           { /* = __c_1165, = c_1006, = __v_1166, = __x_1163 */
             { /* = __kh_1171, = kh_1005, = __v_1172, = __kh_1161 */
               { /* var global_param_1160 */ DIG 3 ; DUP ; DUG 4 } } ;
             IMPLICIT_ACCOUNT } ;
           PUSH mutez 1000000 ;
           PUSH unit Unit ;
           TRANSFER_TOKENS ;
           CONS ;
           PAIR } } ;
       { /* final clean up */ DIP { DROP 2 } } } ;

* Apart from the initial parameter handling, the code is very optimal.

*)

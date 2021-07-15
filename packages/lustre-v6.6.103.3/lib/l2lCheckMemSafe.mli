(** Time-stamp: <modified the 21/01/2015 (at 15:43) by Erwan Jahier> *)

(** Check node declaration wrt safety and memory.

    More precisely, a node that has memory ougth to be declared using
    "node", and using "function" otherwise.
    
    Moreover, a node that performs side-effects (i.e., if it calls an
    extern node that performs side effects) ougth to be declared as
    "unsafe".

    Safe/unsafe mismatches raise an error. 

    Memory mismatches raise an error in one way (a "function" that
    uses memory), and a warning in the other way (a "node" that uses
    no memory).

*)
val doit : LicPrg.t -> unit


val is_safe_val_exp : LicPrg.t -> Lic.val_exp -> bool
val is_memoryless_val_exp : LicPrg.t -> Lic.val_exp  -> bool

(* Time-stamp: <modified the 04/07/2021 (at 09:18) by Erwan Jahier> *)
exception OracleError of string

val start : unit -> RdbgEvent.t
val lurette_start : unit -> unit

(* Terminate all lurette child processes cleanly *)
val clean_terminate : int -> unit


(** In order to use an rdbg ml program to provide missing variables values:
- rdbg_mv holds the missing  input and output var names and types 
- rdbg_mv_hook holds a function that is used (instead Luciole or the keyboard) 
  to provide missing values if:
    - Some variables are missing (to the SUT or its env), and
    - [!rdbg_mv_hook <> None] 
*)
val rdbg_mv :  ((string * Data.t) list * (string * Data.t) list) ref
val rdbg_mv_hook : (Data.subst list -> Data.subst list option) option ref

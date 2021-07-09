(* Time-stamp: <modified the 30/06/2021 (at 10:21) by Erwan Jahier> *)
(* To deal with the rdbg wrapper command line arguments *)

val rdbg_tuning_file : string
val rdbg_cmds_file : string

val mkoptab : unit -> unit
val help : unit -> unit
                     
val verbose : bool ref 
val lurette : bool ref
val ocamldebug: bool ref
val suts : string list ref
val envs : string list ref
val oracles : string list ref
val suts_nd : string list ref
val envs_nd : string list ref
val oracles_nd : string list ref
val test_length : int ref
val missing_vars_at_the_end : bool ref
val output_file : string ref
val input_file : string option ref
val no_rif : bool ref
val drdbg : bool ref
val go : bool ref
val options : (string * string list) list  ref 
val arg_options : (Arg.key * Arg.spec * Arg.doc) list ref
val emacs_mode : bool ref 
val display_gnuplot : bool ref
val display_sim2chro : bool ref
val dont_stop_on_oracle_error : bool ref
val overwrite_output : bool ref
val cov_file : string ref
val reset_cov_file : bool ref
val luciole_mode : bool ref
val sasa_mode : bool ref
val salut_mode : bool ref
val cmd : string ref

val log : bool ref
val margin : int ref
val usage : string ref

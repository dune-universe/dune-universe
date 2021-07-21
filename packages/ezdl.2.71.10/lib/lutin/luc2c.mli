(*-----------------------------------------------------------------------
** Copyright (C) - Verimag.
** This file may only be copied under the terms of the CeCill
** Public License
**-----------------------------------------------------------------------
**
** File: luc2c.mli
** Author: erwan.jahier@univ-grenoble-alpes.fr
**
** functions to generate C files to call Lucky or Lutin from C.
** 

*)


(* To specify the different C backends (not all the tools have the
   same convention for interfacing via C). *)
type gen_mode = Lustre | Scade | Alices | Luciole | Nop

type step_mode = Inside | Edges | Vertices

type optionT = {
  mutable env : string list;       (* lutin/lucky files *)
  mutable main_node : string;   (* Main node *)
  mutable boot : bool;
  mutable load_mem : bool;
  mutable pp : string option;     (* Pre-processor *)
  mutable output : string option; (* A string used in generated files name *)
  mutable rif : string option;    (* Name of the rif file used to put generated data *)
  mutable calling_module_name : string; (* also used in generated files name *)
  mutable gen_mode : gen_mode;    
  mutable step_mode : step_mode;
  mutable seed : int option; 
  mutable precision : int option;
  mutable use_sockets : bool; (* For the C stubs file generation: use the lutin interpreter via sockets 
                                 instead of the interpreter embedded into the C libraries. *)
  mutable sock_addr : string;
  mutable sock_port : int;
  mutable output_dir : string;
  mutable oracle_ec : string option;
}
val option : optionT

val main : Exp.var list -> Exp.var list -> Exp.var list -> unit

(* Time-stamp: <modified the 24/03/2020 (at 17:47) by Erwan Jahier> *)

(**  
    As a side effect, in rdbg mode, this mode generates the following files:
      + my-rdbg-tuning.ml (if not existing) 
      + rdbg-cmds.ml 
*)

val supported_plugins : string list

(** true if no sut/env/oracle is set in the command line.
   Must be called after [file] has been called once 
 *)
val empty_session : unit -> bool

(** [file outc] generates a fresh session file. [outs] is used to output messages.

nb: most of its inputs comes from the command line via RdbgWrapperArgs 

    It returns 
     - the name of the generated session file
     - the list of necessary plugins that is built out of CL args. 
    
    As a side effect, it also generates the following files:

    - in sasa mode 
      + read_dot.ml
 *)
val file :  out_channel -> string * string list

(** [lurette outs] 
 - calls [file outc] 
 - compiles the resulting lurette session file
 - executes it

and then exits 
 *)                             
val lurette :  out_channel -> unit

(* Time-stamp: <modified the 11/02/2019 (at 16:18) by Erwan Jahier> *)


(* In order to generate a xxx.cmxs or a xxx.cmo loadable from
    rdbg, one need to define a .ml program that registries various
    info using the functions below. E.g.,

    let dyn_file = (Dynlink.adapt_filename "xxx.cmo")
    let p = {
      inputs = inputs;
      outputs= outputs;
      kill=kill;
      init_inputs=imems;
      init_outputs=omems;
      step=step;     
      step_dbg=step_dbg;
    }
    
    let _ = 
      OcamlRM.reg_plugin dyn_file p
    
*)

val reg_plugin: string -> RdbgPlugin.t -> unit
val get_plugin: string -> RdbgPlugin.t

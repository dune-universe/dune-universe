(* Demonstrate the use of Lutin from Ocaml. *)

let oc = open_out "call_foo.rif"


(* node main(a:int; b:bool; c:real) returns ( x:int; y:bool; z:real) = *)

(* open Rdbg *)
open Data
open RdbgPlugin 

let _ = 
  let inputs, outputs, kill, step, step_dbg, mems_i,mems_o = 
    let args = Array.of_list ["lutin";"foo.lut";"-n";"main";"-seed";"1"] in
    let plugin = LutinRun.make args in
    plugin.inputs,plugin.outputs,plugin.kill,plugin.step,plugin.step_dbg,
    plugin.init_inputs,plugin.init_outputs
  in


  let rec main_loop  (a, b, c) cpt cpt_max = 

    let foo_inputs = ["a",I a ; "b", B b; "c", F c] in
    let foo_outputs = step  foo_inputs in 
    
    let x = match List.assoc "x" foo_outputs with I x -> x | _ -> assert false
    and y = match List.assoc "y" foo_outputs  with B x -> x | _ -> assert false
    and z = match List.assoc "z" foo_outputs  with F x -> x | _ -> assert false
    in
    if cpt >= cpt_max then () else (
      RifIO.write oc (Printf.sprintf "\n#step %i\n" cpt);
      RifIO.write_outputs oc string_of_float inputs foo_inputs;
      RifIO.write oc " #outs ";
      RifIO.write_outputs oc string_of_float outputs foo_outputs;
      
      main_loop  (x, not y, z) (cpt+1) cpt_max)
  in
  

  RifIO.write_interface oc inputs outputs None None;
  main_loop (0, true, 0.0) 1 100


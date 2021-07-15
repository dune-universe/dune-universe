(* Time-stamp: <modified the 06/02/2015 (at 10:52) by Erwan Jahier> *)

open Lv6MainArgs

let gen_assign_var_expr x =  
  match global_opt.io_transmit_mode with 
    | Heap -> Soc2cHeap.gen_assign_var_expr x
    | HeapStack -> Soc2cHeap.gen_assign_var_expr x
    | Stack ->  Soc2cStack.gen_assign_var_expr x 

let step_name x =  
  match global_opt.io_transmit_mode with 
    | Heap -> Soc2cHeap.step_name x
    | HeapStack -> Soc2cHeap.step_name x 
    | Stack ->  Soc2cStack.step_name x  

let get_step_prototype x y =
  match global_opt.io_transmit_mode with 
    | Heap -> Soc2cHeap.get_step_prototype x y
    | HeapStack -> Soc2cHeap.get_step_prototype x y
    | Stack ->  Soc2cStack.get_step_prototype x  y

let string_of_var_expr x =  
  match global_opt.io_transmit_mode with 
    | Heap -> Soc2cHeap.string_of_var_expr x
    | HeapStack -> Soc2cHeap.string_of_var_expr x 
    | Stack ->  Soc2cStack.string_of_var_expr x  

let ctx_var x =  
  match global_opt.io_transmit_mode with 
    | Heap -> Soc2cHeap.ctx_var x
    | HeapStack -> Soc2cHeap.ctx_var x 
    | Stack ->  Soc2cStack.ctx_var x  


let gen_step_call x =  
  match global_opt.io_transmit_mode with 
    | Heap -> Soc2cHeap.gen_step_call x
    | HeapStack -> Soc2cHeap.gen_step_call x 
    | Stack ->  Soc2cStack.gen_step_call x  

let inlined_soc x =  
  match global_opt.io_transmit_mode with 
    | Heap -> Soc2cHeap.inlined_soc x
    | HeapStack -> Soc2cHeap.inlined_soc x 
    | Stack ->  Soc2cStack.inlined_soc x  

let typedef_of_soc x =  
  match global_opt.io_transmit_mode with 
    | Heap -> Soc2cHeap.typedef_of_soc x
    | HeapStack -> Soc2cHeap.typedef_of_soc x 
    | Stack -> Soc2cStack.typedef_of_soc x  


let get_predef_op x =  
  match global_opt.io_transmit_mode with 
    | Heap -> SocPredef2cHeap.get_predef_op x
    | HeapStack -> SocPredef2cHeap.get_predef_op x 
    | Stack -> SocPredef2cStack.get_predef_op x  
let get_iterator x =  
  match global_opt.io_transmit_mode with 
    | Heap -> SocPredef2cHeap.get_iterator x
    | HeapStack -> SocPredef2cHeap.get_iterator x 
    | Stack -> SocPredef2cStack.get_iterator x  
let get_condact x =  
  match global_opt.io_transmit_mode with 
    | Heap -> SocPredef2cHeap.get_condact x
    | HeapStack -> SocPredef2cHeap.get_condact x 
    | Stack -> SocPredef2cStack.get_condact x  
let get_boolred x =  
  match global_opt.io_transmit_mode with 
    | Heap -> SocPredef2cHeap.get_boolred x
    | HeapStack -> SocPredef2cHeap.get_boolred x 
    | Stack -> SocPredef2cStack.get_boolred x  




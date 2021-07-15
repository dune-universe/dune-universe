(* Time-stamp: <modified the 29/08/2019 (at 17:10) by Erwan Jahier> *)

open Data
open Soc
open Soc2cIdent

(* A boring but simple module... *)

let (lustre_binop : Soc.key -> string -> string) =
  fun sk op  -> 
  let ctx = get_ctx_name sk in
  let cast = "(const void *)" in
  let ctype = match sk with
    | _,t::_,_ -> Printf.sprintf "sizeof(%s)" (Soc2cUtil.data_type_to_c t "")
    | _, [],_ ->  assert false
  in
  (match sk with
   | (("Lustre::eq"|"Lustre::equal"),(Array _)::_,_) -> 
      Printf.sprintf "  %s.out = memcmp(%s %s.i1, %s %s.i2, %s)==0;\n"
                     ctx ctx cast ctx cast ctype
   | (("Lustre::eq"|"Lustre::equal"),(Struct _)::_,_) -> 
      Printf.sprintf "  %s.out = memcmp(%s &%s.i1, %s &%s.i2, %s)==0;\n"
                     ctx ctx cast ctx cast ctype
   | (("Lustre::neq"|"Lustre::diff"),(Array _)::_,_)  ->
      Printf.sprintf "  %s.out = !memcmp(%s %s.i1, %s %s.i2, %s)==0;\n"
                     ctx ctx cast ctx cast ctype
   | (("Lustre::neq"|"Lustre::diff"),(Struct _)::_,_)  ->
      Printf.sprintf "  %s.out = !memcmp(%s &%s.i1, %s &%s.i2, %s)==0;\n"
                     ctx ctx cast ctx cast ctype
   | _ -> 
      Printf.sprintf "  %s.out = (%s.i1 %s %s.i2);\n" ctx ctx op ctx
  )

let (lustre_unop : Soc.key -> string -> string) =
  fun sk op  -> 
    let ctx = get_ctx_name sk in
    (* use gen_assign? *)
    Printf.sprintf"  %s.out = %s %s.i1;\n" ctx op ctx

let (lustre_ite : Soc.key -> string) =
  fun sk -> 
    let ctx = get_ctx_name sk in
    let t = match sk with  (_,_::t::_,_) -> t | _ ->  assert false in
(*     Printf.sprintf"  %s.z = (%s.c)? %s.xt : %s.xe;\n" ctx ctx ctx ctx *)
    Soc2cHeap.gen_assign t (Printf.sprintf "%s.out" ctx)
       (Printf.sprintf "(%s.cond)? %s.xthen : %s.xelse" ctx ctx ctx)
    
let (lustre_impl : Soc.key -> string) =
  fun sk -> 
    let ctx = get_ctx_name sk in
    (* use gen_assign? *)
    Printf.sprintf"  %s.out = (!%s.i1 || %s.i2);\n" ctx ctx ctx 

    
let (lustre_arrow : Soc.key -> string) =
  fun sk -> 
    let x,y,z = "ctx->i1", "ctx->i2", "ctx->out" in
    let t = match sk with  (_,_::t::_,_) -> t | _ ->  assert false in
    let vo = Printf.sprintf"((ctx->_memory)? %s : %s)" x y in
    (Soc2cHeap.gen_assign t z vo) ^ ("  ctx->_memory = _false;\n")
      
let (lustre_hat : Soc.key -> string) =
  fun (n,tl,si_opt) -> 
    let ctx = get_ctx_name (n,tl,si_opt) in
    let i,t = match tl with
      | [_;Data.Array(t,i)] -> i,t
      | _ -> assert false
    in
    let buff = ref "" in
    for j=0 to i-1 do
      buff := !buff^(Soc2cHeap.gen_assign t (Printf.sprintf "%s.out[%d]" ctx j)
                       (Printf.sprintf "%s.i1" ctx)); 
    done;
    !buff

let (lustre_array: Soc.key -> string) =
  fun (n,tl,si_opt) -> 
    let ctx = get_ctx_name (n,tl,si_opt) in
    let t,i = match List.hd (List.rev tl) with
      | Data.Array(t,i) -> t,i
      | _ -> assert false
    in
    let buff = ref "" in
    for j=0 to i-1 do
      buff := !buff^(Soc2cHeap.gen_assign t (Printf.sprintf "%s.out[%d]" ctx j)
                       (Printf.sprintf "%s.i%d" ctx (j+1))); 
    done;
    !buff
 
let (lustre_concat: Soc.key -> string) =
  fun (n,tl,si_opt) -> 
  let ctx = get_ctx_name (n,tl,si_opt) in
  let t,s1,s2 = match tl with
    | [Data.Array(t,s1); Data.Array(_,s2); _] -> t,s1,s2
    | _ -> assert false
  in
  let t1 = Printf.sprintf "%s.i1" ctx
  and t2 = Printf.sprintf "%s.i2" ctx  
  and t12 = Printf.sprintf "%s.out" ctx 
  and t1_type = Soc2cUtil.data_type_to_c (Data.Array(t,s1)) ""
  and t2_type = Soc2cUtil.data_type_to_c (Data.Array(t,s2)) "" 
  in  
  if not Lv6MainArgs.global_opt.Lv6MainArgs.gen_wcet then (
    (Printf.sprintf "  memcpy(&%s, &%s, sizeof(%s));\n" t12 t1 t1_type)^ 
      (Printf.sprintf " // memcpy(&%s+%d, &%s, sizeof(%s));\n" t12 s1 t2 t2_type) ^
        (Printf.sprintf "  memcpy(&%s[%d], &%s, sizeof(%s));\n" t12 s1 t2 t2_type)
  )
  else
    (* Both seems to work *)
    let buff = ref "" in
    for j=0 to s1-1 do
      buff := !buff^(Soc2cHeap.gen_assign t
                                          (Printf.sprintf "%s[%d]" t12 j)
                                          (Printf.sprintf "%s[%d]" t1 j)
                    );
    done;
    for j=s1 to s1+s2-1 do
      buff := !buff^(Soc2cHeap.gen_assign t
                                          (Printf.sprintf "%s[%d]" t12 j)
                                          (Printf.sprintf "%s[%d]" t2 (j-s1))
                    );
    done;
    !buff

let (lustre_slice: Soc.key -> string) =
  fun (n,tl,si_opt) -> 
    let ctx = get_ctx_name (n,tl,si_opt) in
    let t, _size = match List.hd (List.rev tl) with
      | Data.Array(t,i) -> t,i
      | _ -> assert false
    in
    match si_opt with
      | Slic(b,e,step) -> 
        let buff = ref "" in  
        let j=ref 0 in
        for i = b to e do
          if (i-b) mod step = 0 then (
            buff := !buff^(Soc2cHeap.gen_assign t (Printf.sprintf "%s.out[%d]" ctx !j)  
                             (Printf.sprintf "%s.i1[%d]" ctx i)
            );   
            incr j);
          done; 
          !buff
      | _ -> assert false



(* exported *)
let (get_predef_op: Soc.key -> string) =
  fun sk -> 
    let (n,_tl,_si_opt) = sk in
    match n with
      | "Lustre::rplus" 
      | "Lustre::plus"
      | "Lustre::iplus"  -> lustre_binop sk "+" 
      | "Lustre::itimes" 
      | "Lustre::times"
      | "Lustre::rtimes" -> lustre_binop sk "*" 
      | "Lustre::idiv"
      | "Lustre::div"  
      | "Lustre::rdiv"   -> lustre_binop sk "/" 
      | "Lustre::islash" 
      | "Lustre::slash" 
      | "Lustre::rslash" -> lustre_binop sk "/"
      | "Lustre::iminus"
      | "Lustre::minus"
      | "Lustre::rminus" -> lustre_binop sk "-" 

      | "Lustre::mod" -> lustre_binop sk "%"
      | "Lustre::iuminus"
      | "Lustre::uminus" 
      | "Lustre::ruminus"-> lustre_unop sk "-" 

      | "Lustre::eq" -> lustre_binop sk "==" 
      | "Lustre::equal" -> lustre_binop sk "==" 

      | "Lustre::and" -> lustre_binop sk "&&" 
      | "Lustre::neq" -> lustre_binop sk "!="  
      | "Lustre::diff" -> lustre_binop sk "!="  
      | "Lustre::or"  -> lustre_binop sk "||"  

      | "Lustre::xor" -> lustre_binop sk "!=" 
      | "Lustre::not" -> lustre_unop  sk "!"
      | "Lustre::real2int" -> lustre_unop sk "(_integer)"
      | "Lustre::int2real" -> lustre_unop sk "(_real)"

      | "Lustre::lt"
      | "Lustre::rlt"
      | "Lustre::ilt" -> lustre_binop sk "<"
      | "Lustre::gt"
      | "Lustre::rgt"
      | "Lustre::igt" -> lustre_binop sk ">"
      | "Lustre::lte" 
      | "Lustre::rlte"
      | "Lustre::ilte" -> lustre_binop sk "<="
      | "Lustre::gte"
      | "Lustre::rgte"
      | "Lustre::igte" -> lustre_binop sk ">="
      | "Lustre::impl" -> lustre_impl sk

      | "Lustre::if"
      | "Lustre::rif"
      | "Lustre::iif" -> lustre_ite sk

      | "Lustre::current" -> assert false
      | "Lustre::arrow" -> lustre_arrow sk

      | "Lustre::hat" -> lustre_hat sk
      | "Lustre::array" -> lustre_array sk
      | "Lustre::concat" -> lustre_concat sk
      | "Lustre::array_slice" -> lustre_slice sk


      | "Lustre::nor" -> assert false (* ougth to be translated into boolred *)
      | "Lustre::diese" -> assert false (* ditto *)

      | _ -> assert false


let rec type_elt_of_array = function 
  | Data.Array(t,_) -> t
  | Data.Alias(_,t) -> type_elt_of_array t
  | _ -> assert false


(* exported *)
let (get_iterator : Soc.t -> string -> Soc.t -> int -> string) = 
  fun soc iterator it_soc n -> 
    let iter_inputs,iter_outputs = soc.profile in
    let node_step = match soc.step with [step] -> step.name  | _ ->  assert false in
    let step_args, ctx, array_index,ctx_access = 
      match soc.instances with
      | [] -> (
          let ctx_access = Printf.sprintf "%s." (get_ctx_name soc.key) in
          let (array_index : int -> var -> Soc.var_expr) =
            fun i (vn,vt) -> Var(Printf.sprintf "%s%s[%d]" ctx_access vn i, type_elt_of_array vt)
          in          
          Array.make n "", 
          Array.make n (get_ctx_name it_soc.key),
          array_index,ctx_access
        )
      | _  -> 
        let il, _ = Soc2cInstances.to_array soc.instances in
        let step_args = List.flatten(List.map(
            fun (sk,i) -> 
              let l = Lv6util.gen_N i in
              let id = get_ctx_name sk in
              let step_args = List.map (fun n -> Printf.sprintf "ctx->%s_tab[%d]" id n) l in
              step_args
          ) il)
        in
        let ctx = step_args in
        let step_args = List.map (fun x -> "&"^x) step_args in
        let ctx_access =  "ctx->"  in
        let (array_index : int -> var -> Soc.var_expr) =
          fun i (vn,vt) -> Var(Printf.sprintf "ctx->%s[%d]" vn i,vt) 
        in
        Array.of_list step_args,
        Array.of_list ctx,
        array_index,ctx_access
    in
    let buff = ref "" in
    for i=0 to n-1 do
      let vel_in, vel_out =
        match iterator with
        | "map" -> 
          (List.map (array_index i) iter_inputs,
           List.map (array_index i) iter_outputs)
        | "fold" | "red" | "fill" | "fillred" ->
          let name, telt = List.hd iter_inputs in
          let a_in = ctx_access ^ name in
          let a_in = Var(a_in, telt) in
          (a_in::(List.map (array_index i) (List.tl iter_inputs)),
           a_in::(List.map (array_index i) (List.tl iter_outputs)))
        | _ -> assert false (* should not occur *)
      in
      buff := !buff^(
          Soc2cHeap.gen_step_call
            soc it_soc vel_out vel_in ctx.(i) node_step step_args.(i))
    done;

    if iterator <> "map" then (
      let type_in = (snd (List.hd iter_inputs)) in
      let a_in  = ctx_access ^ (fst (List.hd iter_inputs)) in
      let a_out = ctx_access ^ (fst (List.hd iter_outputs)) in
      buff := !buff^(Soc2cHeap.gen_assign type_in a_out a_in)  (* a_out=a_n *)
    );
    !buff
 

(* exported *)
let (get_condact : Soc.t -> Soc.t -> var_expr list -> string ) = 
  fun soc condact_soc el -> 
    let buff = ref "" in
    let add str = buff:=!buff^(str^"\n") in
    let add_ncr str = buff:=!buff^(str) in

    let clk = Printf.sprintf "ctx->activate" in
    let vel_in,vel_out = soc.profile in
    let vel_in = List.tl vel_in in
    let vel_in  = List.map (fun var -> Var var) vel_in  in
    let vel_out = List.map (fun var -> Var var) vel_out in
    add (Printf.sprintf "\n  if (%s == _true) { " clk); 
    if SocUtils.is_memory_less condact_soc then
      let condact_ctx = get_ctx_name condact_soc.key in
      add (Soc2cHeap.gen_step_call soc condact_soc vel_out vel_in condact_ctx "step" "")
    else (
      let condact_ctx = 
        match soc.instances with
          | [inst] ->             
            let _, get_index = Soc2cInstances.to_array soc.instances in
            let index = get_index (inst) in
            (Printf.sprintf "ctx->%s_tab[%d]" (get_ctx_name condact_soc.key) index)
          | _ -> assert false
      in
      add_ncr (Soc2cHeap.gen_step_call soc condact_soc vel_out vel_in 
             condact_ctx "step" ("&"^condact_ctx))
    );
    add "    ctx->_memory = _false;";
    add "   } else if (ctx->_memory == _true) {";
    List.iter2 (fun var ve -> 
      add (Printf.sprintf "    %s = %s;" (Soc2cHeap.string_of_var_expr soc var)
             (Soc2cHeap.string_of_var_expr soc ve) )
    ) vel_out el ;
    add "    ctx->_memory = _false;";
    add "  }";
    !buff


(* exported *)
let (get_boolred : Soc.t -> int -> int -> int -> string )= 
  fun soc i j k -> 
    let ctx = get_ctx_name soc.key in
    let buff = ref "" in
    let add str = buff:=!buff^(str^"\n") in
    add "  int cpt,i;";
    add "  cpt=0;";
    add (Printf.sprintf
           "  for (i = 0 ; i < %d ; i += 1) if (%s.i1[i] == _true) cpt++;" k ctx);    
    add (Printf.sprintf "  %s.out = (%d <= cpt && cpt <= %d) ? _true : _false;" ctx i j );
    !buff

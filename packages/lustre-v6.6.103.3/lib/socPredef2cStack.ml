(* Time-stamp: <modified the 15/03/2021 (at 18:06) by Erwan Jahier> *)

open Data
open Soc
open Soc2cIdent

(* A boring but simple module... *)

let (lustre_binop : Soc.key -> string -> string) =
  fun sk op  ->
  let cast = "(const void *)" in
  let ctype = match sk with
    | _,t::_,_ -> Printf.sprintf "sizeof(%s)" (Soc2cUtil.data_type_to_c t "")
    | _, [],_ ->  assert false
  in
  (match sk with
   | (("Lustre::eq"|"Lustre::equal"),(Array _)::_,_) -> 
      Printf.sprintf "  *out = memcmp(%s i1, %s i2, %s)==0;\n" cast cast ctype
   | (("Lustre::eq"|"Lustre::equal"),(Struct _)::_,_) -> 
      Printf.sprintf "  *out = memcmp(%s &i1, %s &i2, %s)==0;\n" cast cast ctype
   | (("Lustre::neq"|"Lustre::diff"),(Array _)::_,_)  ->
      Printf.sprintf "  *out = !memcmp(%s i1, %s i2, %s)==0;\n" cast cast ctype
   | (("Lustre::neq"|"Lustre::diff"),(Struct _)::_,_)  ->
      Printf.sprintf "  *out = !memcmp(%s &i1, %s &i2, %s)==0;\n" cast cast ctype

   | _ -> 
      Printf.sprintf "  *out = (i1 %s i2);\n" op
  )

let (lustre_unop : Soc.key -> string -> string) =
  fun _sk op  -> 
    (* use gen_assign? *)
    Printf.sprintf"  *out = %s i1;\n" op 

let (lustre_ite : Soc.key -> string) =
  fun sk -> 
    let t = match sk with  (_,_::t::_,_) -> t | _ ->  assert false in
(*     Printf.sprintf"  %s.z = (%s.c)? %s.xt : %s.xe;\n" ctx ctx ctx ctx *)
    Soc2cStack.gen_assign t (Printf.sprintf "*out")
       (Printf.sprintf "(cond)? xthen : xelse")

let (lustre_impl : Soc.key -> string) =
  fun _sk -> 
    (* use gen_assign? *)
    Printf.sprintf"  *out = (!i1 || i2);\n"

    
let (lustre_arrow : Soc.key -> string) =
  fun sk -> 
    
    let t = match sk with  (_,_::t::_,_) -> t | _ ->  assert false in
    let x,y = "i1", "i2" in
    let z = match t with
      | Data.Array(_,_) -> "out"
      | _ ->  "*out" 
    in 
    let vo = Printf.sprintf"((ctx->_memory)? %s : %s)" x y in
    (Soc2cStack.gen_assign t z vo) ^ 
      ("  ctx->_memory = _false;\n")
      
let (lustre_hat : Soc.key -> string) =
  fun (_n,tl,_si_opt) -> 
    let i,t = match tl with
      | [_;Data.Array(t,i)] -> i,t
      | _ -> assert false
    in
    let buff = ref "" in
    for j=0 to i-1 do
      buff := !buff^(Soc2cStack.gen_assign t (Printf.sprintf "out[%d]" j)
                       (Printf.sprintf "i1")); 
    done;
    !buff

let (lustre_array: Soc.key -> string) =
  fun (_n,tl,_si_opt) -> 
    let t,i = match List.hd (List.rev tl) with
      | Data.Array(t,i) -> t,i
      | _ -> assert false
    in
    let buff = ref "" in
    for j=0 to i-1 do
      buff := !buff^(Soc2cStack.gen_assign t (Printf.sprintf "out[%d]" j)
                       (Printf.sprintf "i%d" (j+1))); 
    done;
    !buff
 
let (lustre_concat: Soc.key -> string) =
  fun (_n,tl,_si_opt) -> 
  let t,s1,s2 = match tl with
    | [Data.Array(t,s1); Data.Array(_,s2); _] -> t,s1,s2
    | _ -> assert false
  in
  let t1 = Printf.sprintf "i1"
  and t2 = Printf.sprintf "i2" 
  and t12 = Printf.sprintf "out" 
  and t1_type = Soc2cUtil.data_type_to_c (Data.Array(t,s1)) ""
  and t2_type = Soc2cUtil.data_type_to_c (Data.Array(t,s2)) "" 
  in  
  if not Lv6MainArgs.global_opt.Lv6MainArgs.gen_wcet then (
    (Printf.sprintf "  memcpy(%s, %s, sizeof(%s));\n" t12 t1 t1_type)^ 
      (Printf.sprintf "  memcpy(%s+%d, %s, sizeof(%s));\n" t12 s1 t2 t2_type)
  ) else (
    (* Both seems to work *)
    let buff = ref "" in
    for j=0 to s1-1 do
      buff := !buff^(Soc2cStack.gen_assign t
                                          (Printf.sprintf "%s[%d]" t12 j)
                                          (Printf.sprintf "%s[%d]" t1 j)
                    );
    done;
    for j=s1 to s1+s2-1 do
      buff := !buff^(Soc2cStack.gen_assign t
                                          (Printf.sprintf "%s[%d]" t12 j)
                                          (Printf.sprintf "%s[%d]" t2 (j-s1))
                    );
    done;
    !buff
  )
(*      let buff = ref "" in  *)
(*      for j=0 to s1-1 do  *)
(*        buff := !buff^(Soc2cStack.gen_assign t (Printf.sprintf "%s.z[%d]" ctx j));   *)
(*      done;  *)
(*      for j=s1 to s1+s2-1 do  *)
(*        buff := !buff^(Soc2cStack.gen_assign t (Printf.sprintf "%s.z[%d]" ctx j));   *)
(*      done;  *)
(*      !buff  *)

let (lustre_slice: Soc.key -> string) =
  fun (_n,tl,si_opt) -> 
    let t,_size = match List.hd (List.rev tl) with
      | Data.Array(t,i) -> t,i
      | _ -> assert false
    in
    match si_opt with
      | Slic(b,e,step) -> 
        let buff = ref "" in  
        let j=ref 0 in
        for i = b to e do
          if (i-b) mod step = 0 then (
            buff := !buff^(Soc2cStack.gen_assign t (Printf.sprintf "out[%d]" !j)  
                             (Printf.sprintf "i1[%d]" i)
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


let rec _type_elt_of_array = function 
  | Data.Array(t,_) -> t
  | Data.Alias(_,t) -> _type_elt_of_array t
  | _ -> assert false
let (not_an_array : Data.t -> bool) = function 
  | Data.Array(_,_) -> false | _ -> true



(* exported *)
let (get_iterator : Soc.t -> string -> Soc.t -> int -> string) = 
  fun soc iterator it_soc n -> 
    let iter_inputs,iter_outputs = soc.profile in
    let node_step = match soc.step with [step] -> step.name  | _ ->  assert false in
    let step_args, ctx, array_index = 
      match soc.instances with
      | [] -> (
          let (array_index : int -> Soc.var -> Soc.var_expr) =
            fun i (vn,vt) -> (* Var(Printf.sprintf "%s[%d]" vn i, type_elt_of_array vt) *)
              let vt_elt = match vt with
                | Data.Array(vt,_) -> vt 
                | _ -> assert false
              in 
              Index(Var (vn,vt),i,vt_elt)
          in          
          Array.make n "", 
          Array.make n (get_ctx_name it_soc.key),
          array_index
        )
      | _  -> 
        let il, _ = Soc2cInstances.to_array soc.instances in
        let step_args = List.flatten(List.map(
            fun (sk,i) -> 
              let l = Lv6util.gen_N i in
              let id = get_ctx_name sk in
              let step_args = List.map (fun n -> Printf.sprintf "&ctx->%s_tab[%d]" id n) l in
              (*              let ctx = List.map (fun n -> Printf.sprintf "&ctx->%s" id n) l in *)
              step_args
          ) il)
        in
        let ctx = step_args (*List.map (fun sn -> ("ctx->"^(id2s sn))) inst_names *) in
        let (array_index : int -> var -> Soc.var_expr) =
          fun i (vn,vt) ->
            let vt_elt = match vt with
              | Data.Array(vt,_) -> vt
              | _ -> assert false
            in
            Index(Var (vn, Data.Array(vt,i)),i,vt_elt)
            (* Var(Printf.sprintf "%s[%d]" vn i,vt) *)
        in
        Array.of_list step_args,
        Array.of_list ctx,
        array_index
    in
    let buff = ref "" in
    let name, telt = List.hd iter_inputs in
    let a_in = name in
    (* if the accumulator is passed by adress (eg, if it is an array), we need
       an extra variable for fillred (not for map)  *)
    let a_in_bis = "_local_"^name in (* can it clash? *)
    let acc_is_passed_by_value = 
      match telt with
      | Alias(_, (Enum _ | Bool | Int | Real))
      | Struct _ (* ZZZ if we pass struct by adress one day, I should move that line *)
      | Enum _ | Bool | Int | Real -> 
        (* arg passed by value are copied into the stack, so it's safe
           to use the same variable *)
        true
        | String | Alias _ | Alpha _ -> assert false 
      (* ougth to be deadcode; true is the safe value anyway *)
      | Extern _
      | Array _ -> false
    in
    let a_in, a_in_bis = if acc_is_passed_by_value then a_in, a_in else a_in, a_in_bis in
    let a_in_v     = Var(a_in,  telt) in
    let a_in_bis_v = Var(a_in_bis, telt) in
    if not acc_is_passed_by_value && iterator <> "map" then (
      buff := !buff^(Printf.sprintf "  %s;\n" 
                       (Soc2cUtil.data_type_to_c telt a_in_bis));
    );
    (match Lv6MainArgs.global_opt.Lv6MainArgs.soc2c_inline_loops, iterator with
     | true, "map" -> (
         for i=0 to n-1 do
           let vel_in, vel_out =
             (List.map (array_index i) iter_inputs,
              List.map (array_index i) iter_outputs)
           in
           buff := !buff^(
               Soc2cStack.gen_step_call
                 soc it_soc vel_out vel_in ctx.(i) node_step step_args.(i));
         done
       )
     | false, "map" -> (
         let vel_in, vel_out = 
           (* xxx <ugly>... Soc.var_expr cannot hold index variable
              and I don't want to rewrite (nor duplicate)
              gen_step_call for now. Hence this ugly 666 turn-around *)
           (List.map (array_index (-666)) iter_inputs,
            List.map (array_index (-666)) iter_outputs)
         in
         let for_loop = Printf.sprintf "for (_i=0 ; _i<%i ; _i+=1) {" n  in
         let body = Soc2cStack.gen_step_call
             soc it_soc vel_out vel_in ctx.(0) node_step step_args.(0);
         in
         let body =
           Str.global_replace (Str.regexp "\\[-666\\]") "[_i]" body
         in (* </ugly> *)
         let str = Printf.sprintf "  int _i;\n  %s\n  %s  }" for_loop body in
         buff := !buff ^ str
       )
     | true, ("fold" | "red" | "fill" | "fillred") -> (
         let a_out = 
           let o,t = List.hd iter_outputs in
           if Soc2cStack.is_soc_output (Var(o,t)) soc && not_an_array t  
           then "*"^o else o
         in
         for i=0 to n-1 do
           let vel_in, vel_out =
             let a_in_v, a_in_bis_v = 
               if i mod 2 = 0 then a_in_v, a_in_bis_v else a_in_bis_v, a_in_v
             in
             a_in_v    ::(List.map (array_index i) (List.tl iter_inputs)),
             a_in_bis_v::(List.map (array_index i) (List.tl iter_outputs))
           in
           buff := !buff^(
               Soc2cStack.gen_step_call
                 soc it_soc vel_out vel_in ctx.(i) node_step step_args.(i)) ;
         done;
         if n mod 2 = 0 then         
           buff := !buff^(Soc2cStack.gen_assign telt a_out a_in)  (* a_out=a_n *)
         else
           buff := !buff^(Soc2cStack.gen_assign telt a_out a_in_bis) 
       )
     | false, ("fold" | "red" | "fill" | "fillred") -> (
         let a_out = 
           let o,t = List.hd iter_outputs in
           if Soc2cStack.is_soc_output (Var(o,t)) soc && not_an_array t  
           then "*"^o else o
         in
         let for_loop = Printf.sprintf "for (_i=0 ; _i<%i ; _i+=2){" (n-1) in
         let vel_in, vel_out =
           a_in_v    ::(List.map (array_index (-666)) (List.tl iter_inputs)),
           a_in_bis_v::(List.map (array_index (-666)) (List.tl iter_outputs))
         in
         let vel_in2, vel_out2 =
           List.hd vel_out ::( List.tl vel_in),
           List.hd vel_in ::( List.tl vel_out)
         in
         let step_arg  =
           Str.global_replace (Str.regexp "\\[0\\]") "[_i]"  step_args.(0) in
         let step_arg2 =
           Str.global_replace (Str.regexp "\\[0\\]") "[_i+1]" step_args.(0) in
         let body = Soc2cStack.gen_step_call
             soc it_soc vel_out vel_in ctx.(0) node_step step_arg;
         in
         let body2 = Soc2cStack.gen_step_call
             soc it_soc vel_out2 vel_in2 ctx.(0) node_step step_arg2;
         in
         let call1 = Str.global_replace (Str.regexp "\\[-666\\]") "[_i]" body in
         let call2 = Str.global_replace (Str.regexp "\\[-666\\]") "[_i+1]" body2 in (* </ugly> *) 
         let call3 = if (n mod 2 = 1) then 
             Str.global_replace (Str.regexp "\\[-666\\]")
               ("["^string_of_int (n-1)^"]") body
           else ""
         in
         let str = Printf.sprintf 
             "  int _i;\n  %s\n  %s  %s  }\n%s " for_loop call1 call2 call3 
         in
         buff := !buff ^ str;
         if n mod 2 = 0 then         
           buff := !buff^(Soc2cStack.gen_assign telt a_out a_in)  (* a_out=a_n *)
         else
           buff := !buff^(Soc2cStack.gen_assign telt a_out a_in_bis) 
       )
     | _,_ -> assert false (* SNO *)
    );
    !buff
 

(* exported *)
let (get_condact : Soc.t -> Soc.t -> var_expr list -> string ) = 
  fun soc condact_soc el -> 
    let buff = ref "" in
    let add str = buff:=!buff^(str^"\n") in
    let add_ncr str = buff:=!buff^(str) in

    let clk = Printf.sprintf "activate" in
    let vel_in,vel_out = soc.profile in
    let vel_in = List.tl vel_in in
    let vel_in  = List.map (fun var -> Var var) vel_in  in
    let vel_out = List.map (fun var -> Var var) vel_out in
    add (Printf.sprintf "\n  if (%s == _true) { " clk); 
    (if SocUtils.is_memory_less condact_soc then
        let condact_ctx = get_ctx_name condact_soc.key in
        add (Soc2cStack.gen_step_call soc condact_soc vel_out vel_in condact_ctx "step" "")
     else
        let condact_ctx = 
          match soc.instances with
            | [inst] -> 
              let _, get_index = Soc2cInstances.to_array soc.instances in
              let index = get_index (inst) in
              (Printf.sprintf "ctx->%s_tab[%d]" (get_ctx_name condact_soc.key) index)
            | _ -> assert false
        in
        add_ncr (Soc2cStack.gen_step_call soc condact_soc vel_out vel_in 
               condact_ctx "step"  ("&"^condact_ctx))
    );
    add "    ctx->_memory = _false;";
    add "   } else if (ctx->_memory == _true) {";
    List.iter2 (fun var ve -> 
      add (Printf.sprintf "    *%s = %s;" (Soc2cStack.string_of_var_expr soc var)
             (Soc2cStack.string_of_var_expr soc ve) )
    ) vel_out el ;
    add "    ctx->_memory = _false;";
    add "  }";
    !buff


(* exported *)
let (get_boolred : Soc.t -> int -> int -> int -> string )= 
  fun _soc i j k -> 
    let buff = ref "" in
    let add str = buff:=!buff^(str^"\n") in
    add "  int cpt,i;";
    add "  cpt=0;";
    add (Printf.sprintf "  for (i = 0 ; i < %d ; i += 1) if (i1[i] == _true) cpt++;" k);    
    add (Printf.sprintf "  *out = (%d <= cpt && cpt <= %d) ? _true : _false;" i j);
    !buff

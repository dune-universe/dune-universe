(* Time-stamp: <modified the 06/03/2020 (at 13:36) by Erwan Jahier> *)

open Soc2cUtil
open Soc2cIdent
open Soc



let (mem_interface_out : Soc.t -> string -> bool) =
  fun soc id -> 
    let _,outs = soc.profile in
    List.mem_assoc id outs

let (not_an_array : Data.t -> bool) = function 
  | Data.Array(_,_) -> false | _ -> true


let (ve_not_an_array : Soc.var_expr -> bool) =
  fun v -> 
    match v with
      | Var(_,t) 
      | Const(_,t) 
      | Field(_,_,t) 
      | Index(_,_,t) -> not_an_array t 
      | Slice(_,_,_,_,_,_) -> false

let (ve_not_a_field : Soc.var_expr -> bool) =
  fun v -> 
    match v with
      | Var(_,_) 
      | Const(_,_) 
      | Index(_,_,_) 
      | Slice(_,_,_,_,_,_) -> true
      | Field(_,_,_t) -> false

(* exported : returns true if v is an output of soc *)
let rec (is_soc_output : Soc.var_expr -> Soc.t -> bool) =
  fun v soc -> 
    match v with
      | Var(n,t) -> List.mem (n,t) (snd soc.profile)
      | Const(_) -> false
      | Index(ve,_,_t) 
      | Field(ve,_,_t)
      | Slice(ve,_,_,_,_,_t) ->  is_soc_output ve soc
                                              
let (is_soc_output_and_not_a_struct : Soc.var_expr -> Soc.t -> bool) =
  fun v soc -> 
    match v with
      | Var(n,t) -> List.mem (n,t) (snd soc.profile)
      | Const(_) -> false
      | Index(_ve,_,_t) 
      | Field(_ve,_,_t)
      | Slice(_ve,_,_,_,_,_t) -> false (* is_soc_output_and_not_a_struct ve soc *)

let rec (string_of_var_expr: Soc.t -> Soc.var_expr -> string) = 
  fun soc var -> match var with
    | Const("true", _) -> "_true"
    | Const("false", _) -> "_false"
    | Const(id, _) -> id2s id
    | Var ("_memory",_)   -> (* Clutch! it's not an interface var... *) "ctx->_memory" 
    | Var (id,_t)   -> id2s id
    | Field(f, id,_) -> 
      if is_soc_output_and_not_a_struct f soc
      then Printf.sprintf "%s->%s" (string_of_var_expr soc f) (id2s id) 
      else Printf.sprintf "%s.%s"  (string_of_var_expr soc f) (id2s id) 
    | Index(f, index,_) -> Printf.sprintf "%s[%i]" (string_of_var_expr soc f) index
    | Slice(_f,_fi,_la,_st,_wi,_vt) -> assert false (* should not occur *)


(* exported *) 
let rec (gen_assign : Data.t  -> string -> string -> string) =
  fun t vi vo -> 
    let t_str = Soc2cUtil.data_type_to_c t "" in
    match t with
      | Data.Alias(_,t) -> gen_assign t vi vo
      | Data.Enum _  
      | Data.Struct(_) (* should I rather use memcpy for struct? *)
      | Data.Bool | Data.Int | Data.Real -> Printf.sprintf "  %s = %s;\n" vi vo
      | Data.Alpha(_) (* dead code ? *) 
      | Data.String 
      | Data.Array(_) -> 
        let t_str_short = Soc2cIdent.type_to_short_string t in
        Printf.sprintf "  _assign_%s(%s, %s, sizeof(%s));\n" t_str_short vi vo t_str

      | Data.Extern (id) -> 
        Printf.sprintf "  _assign_%s(%s, %s, sizeof(%s));\n" (id2s id) vi vo t_str
      

let (gen_assign_var_expr : Soc.t -> Soc.var_expr -> Soc.var_expr -> string) =
fun soc vo vi -> 
  match vo,vi  with
    | Slice _, _  | _, Slice _ ->  assert false
    | _,_ -> 
      let left = string_of_var_expr soc vo in
      let left = if is_soc_output_and_not_a_struct vo soc && ve_not_an_array vo && ve_not_a_field vo
        then "*"^left else left  
      in
      let vi_str = string_of_var_expr soc vi in
      let vi_str = 
        if is_soc_output_and_not_a_struct vi soc && ve_not_an_array vi then "*"^vi_str else vi_str
      in
      gen_assign (Soc.data_type_of_var_expr vo) left vi_str 


let (step_name : Soc.key -> string -> string) =
  fun sk sm -> 
    let str = Printf.sprintf "%s_%s" (Soc2cIdent.get_soc_name sk) sm in
    (* Printf.printf " XXX step_name(%s)=%s\n" (SocUtils.string_of_soc_key sk) str; *)
    (* flush stdout; *)
    (id2s str)

let (ctx_var : var_kind -> Soc.t -> Lv6Id.t -> string) =
  fun _opt soc id -> 
    if mem_interface_out soc id then 
      Printf.sprintf "*%s" (id2s id)
    else 
      Printf.sprintf "%s" (id2s id)

let (list_split : 'a list -> int -> 'a list * 'a list) =
  fun l s ->
    let rec aux s l acc =
      match s,l with
        | 0, _ -> List.rev acc,l
        | _, x::l -> aux (s-1) l (x::acc)
        | _, [] -> assert false
    in 
    aux s l []

let _ = assert (list_split [1;2;3;4;5;6] 3 = ([1;2;3],[4;5;6]))

let (inline_soc: Soc.t -> Soc.t -> Soc.var_expr list -> Soc.var_expr list ->
     string option) =
  fun soc called_soc vel_out vel_in -> 
    let called_soc_name,_,_ = called_soc.key in 
    match called_soc_name with
      (* those soc are inlined. Currently we only inline ite because
         of its polymorphism. Simple arith operators (+,-,*,/,etc.)
         should be inlined too. *)
      | "Lustre::if" ->
        let c,vel_in= match vel_in with [] -> assert false | c::l -> c,l in
        let s = (List.length vel_out) in
        let vel_in_t, vel_in_e = list_split vel_in s in
        let lt = List.map2 (gen_assign_var_expr soc) vel_out vel_in_t in
        let le = List.map2 (gen_assign_var_expr soc) vel_out vel_in_e in
        let ptr_indir = if is_soc_output_and_not_a_struct c soc then "*" else "" in
        let str = "   if ("^ ptr_indir^string_of_var_expr soc c ^" == _true) {\n   "^
          (String.concat "   " lt)^ "   } else {\n   "^
          (String.concat "   " le)^ "   }\n"
        in
        Some str

      | _  ->
        try
          if 
            Lv6MainArgs.global_opt.Lv6MainArgs.gen_c_inline_predef 
            && Soc2cPredef.is_call_supported called_soc.key 
          then
            let vel_in_str = List.map (string_of_var_expr soc) vel_in in
            let vel_in = List.map2
              (fun v s -> if is_soc_output_and_not_a_struct v soc  && ve_not_an_array v 
                then "*"^s else s) vel_in vel_in_str
            in

            let vel_out_str = List.map (string_of_var_expr soc) vel_out in
            let vel_out = List.map2
              (fun v s -> if is_soc_output_and_not_a_struct v soc && ve_not_an_array v 
                then "*"^s else s) vel_out vel_out_str
            in
            Some (Soc2cPredef.gen_call called_soc.key soc vel_out vel_in)
          else 
            None
        with Not_found -> 
(*           Printf.eprintf "won't inline %s\n" called_soc_name; *)
          None
(* exported *) 
let (inlined_soc : Soc.key -> bool) =
  fun key ->
    let soc_name,_,_ = key in
    soc_name = "Lustre::if" || Soc2cPredef.is_call_supported key 


(* exported *)
let (gen_step_call : Soc.t -> Soc.t -> Soc.var_expr list -> Soc.var_expr list ->
     string -> string -> string -> string) =
  fun soc called_soc vel_out vel_in _ctx sname step_arg ->
    match inline_soc soc called_soc vel_out vel_in with
    | Some str -> str
    | None ->
      let vel_in_str = List.map (string_of_var_expr soc) vel_in in
      let vel_in =
        List.map2 (fun v s -> if is_soc_output_and_not_a_struct v soc && ve_not_an_array v 
                    then "*"^s else s) vel_in vel_in_str
      in
      let vel_out_str = List.map (string_of_var_expr soc) vel_out in
      let vel_out =
        List.map2 
          (fun v s -> 
             if (not (is_soc_output_and_not_a_struct v soc) && ve_not_an_array v )
             then "&"^s 
             else s) 
          vel_out vel_out_str
      in
      let step_arg = if step_arg = "" then [] else [step_arg] in
      let step_arg = String.concat "," (vel_in@vel_out@step_arg) in
      let str = Printf.sprintf "  %s(%s); \n" (step_name called_soc.key sname) step_arg in
      str

(* exported *) 
let (typedef_of_soc : Soc.t -> string) =
  fun soc -> 
    if inlined_soc soc.key then ""  (* don't generate code if inlined *) else
      if SocUtils.is_memory_less soc then "" else
    let ctx_name = get_ctx_name soc.key in
    let ctx_name_type = ctx_name^"_type" in    
    let str = Printf.sprintf  "/* %s */\ntypedef struct {\n" ctx_name in
    let str = str ^
      (match soc.memory with
        | No_mem -> ""
        | Mem t ->  Printf.sprintf "   /*Memory cell*/\n   %s ;\n" 
          (id2s (Soc2cUtil.data_type_to_c t "_memory"))
        | Mem_hidden -> ""
      )
    in
    let str =  str ^ (if soc.instances <> [] then  "   /*INSTANCES*/\n" else "") in
    let il, _get_index = Soc2cInstances.to_array soc.instances in
    let string_of_instance (sk,i) = 
      let n = get_ctx_name sk in
      Printf.sprintf "   %s_type %s_tab[%d];\n" n n i
    in
    let str = List.fold_left (fun acc inst -> acc^(string_of_instance inst)) str il in
    let str = Printf.sprintf  "%s} %s;\n\n" str ctx_name_type in
    str

(* exported *)
(* for soc of type (int * int -> int), it generates something like 

"void step(int, int, int*, soc_ctx_type* );",
"void step(int x, int y, int* res, soc_ctx_type* ctx){"

*) 
let (get_step_prototype : Soc.step_method -> Soc.t -> string * string * string) =
  fun sm soc ->
    let sname = step_name soc.key sm.name in
    let inputs, outputs = soc.Soc.profile in
    let inputs  = SocUtils.filter_step_params sm.Soc.idx_ins  inputs in
    let outputs = SocUtils.filter_step_params sm.Soc.idx_outs outputs in
    let to_param_decl is_an_output (_id,dt) = 
      match is_an_output, dt with
        | true, Data.Array(_,_) -> Soc2cUtil.data_type_to_c dt "" ^"/*out*/" 
        (* arrays are already pointers... *)
        | false, _ -> Soc2cUtil.data_type_to_c dt ""
        | true,  _ -> Soc2cUtil.data_type_to_c dt "*"
    in
    let to_param out (id,dt) = 
      match out, dt with
        | true, Data.Array(_,_) -> Soc2cUtil.data_type_to_c dt id ^"/*out*/"
        | false, _ -> Soc2cUtil.data_type_to_c dt id
        | true,  _ -> Soc2cUtil.data_type_to_c dt ("*"^id)
    in
    let in_params = List.map (to_param false) inputs in
    let out_params = List.map (to_param true) outputs in
    let in_params_decl = List.map (to_param_decl false) inputs in
    let out_params_decl = List.map (to_param_decl true) outputs in
    let params = String.concat "," (in_params@out_params) in
    let params_decl = String.concat "," (in_params_decl@out_params_decl) in
    let ctype = match inputs with
      | (_,t)::_ -> 
         Printf.sprintf "sizeof(%s)" (Soc2cUtil.data_type_to_c t "")
      | [] ->  "" (* soc without intputs won't need this output *)
    in
    if SocUtils.is_memory_less soc then
      Printf.sprintf "void %s(%s);\n" sname params_decl,
      Printf.sprintf "void %s(%s){\n" sname params,
      ctype
    else 
      let ctx = Printf.sprintf "%s_type* ctx" (get_ctx_name soc.key)  in
      let ctx_decl = Printf.sprintf "%s_type*" (get_ctx_name soc.key) in
      Printf.sprintf "void %s(%s,%s);\n" sname params_decl ctx_decl,
      Printf.sprintf "void %s(%s,%s){" sname params ctx,
      ctype

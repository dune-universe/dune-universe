(* Time-stamp: <modified the 06/03/2020 (at 13:34) by Erwan Jahier> *)

open Soc2cUtil
open Soc2cIdent
open Soc


let (mem_interface : Soc.t -> string -> bool) =
  fun soc id -> 
    let ins,outs = soc.profile in
    List.mem_assoc id ins || List.mem_assoc id outs

let rec (string_of_var_expr: Soc.t -> Soc.var_expr -> string) = 
  fun soc -> function
    | Const("true", _) -> "_true"
    | Const("false", _) -> "_false"
    | Const(id, _) -> id2s id
    | Var ("_memory",_)   -> (* Clutch! it's not an interface var... *) "ctx->_memory" 
    | Var (id,_)   -> 
      if not (mem_interface soc id) then id2s id 
      else if SocUtils.is_memory_less soc then
          Printf.sprintf "%s.%s" (get_ctx_name soc.key) (id2s id)
        else 
          Printf.sprintf "ctx->%s" (id2s id)
    | Field(f, id,_) -> Printf.sprintf "%s.%s" (string_of_var_expr soc f) (id2s id) 
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
      | Data.Bool | Data.Int | Data.Real -> 
        Printf.sprintf "  %s = %s;\n" vi vo
      | Data.Alpha(_) (* dead code ? *)
      | Data.String 
      | Data.Array(_) -> 
        let t_str_short = Soc2cIdent.type_to_short_string t in
        Printf.sprintf "  _assign_%s(%s, %s, sizeof(%s));\n" t_str_short vi vo t_str

      | Data.Extern (id) -> 
        Printf.sprintf "  _assign_%s(%s, %s, sizeof(%s));\n" (id2s id) vi vo t_str

let (gen_assign_var_expr : Soc.t -> Soc.var_expr -> Soc.var_expr -> string) =
fun soc vi vo -> 
  match vi,vo with
    | Slice _, _ -> assert false
    | _, Slice _ ->  assert false
    | _,_ -> 
      gen_assign (Soc.data_type_of_var_expr vi)
        (string_of_var_expr soc vi) (string_of_var_expr soc vo)


let (step_name : Soc.key -> string -> string) =
  fun sk sm ->
  let str = Printf.sprintf "%s_%s" (Soc2cIdent.get_soc_name sk) sm in
  id2s str

let (ctx_var : var_kind -> Soc.t -> Lv6Id.t -> string) =
  fun opt _soc id -> 
    match opt with
      | ML_IO sk -> Printf.sprintf "%s_ctx.%s" (Soc2cIdent.get_soc_name sk) (id2s id)
      | M_IO  ->  Printf.sprintf "ctx->%s" (id2s id)
      | Local -> Printf.sprintf "%s" (id2s id)


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

let (inline_soc :
       Soc.t -> Soc.t -> Soc.var_expr list -> Soc.var_expr list -> string option) =
  fun soc called_soc vel_out vel_in -> 
    let called_soc_name,_,_ = called_soc.key in 
    match called_soc_name with
      (* those soc are inlined. Currently we only inline ite because
         of its polymorphism. Maybe simple arith operators
         (+-,*,/,etc.) should be inlined too. *)
      | "Lustre::if" -> 
        let c,vel_in= match vel_in with [] -> assert false | c::l -> c,l in 
        let s = (List.length vel_out) in
        let vel_in_t, vel_in_e = list_split vel_in s in
        let lt = List.map2 (gen_assign_var_expr soc) vel_out vel_in_t in
        let le = List.map2 (gen_assign_var_expr soc) vel_out vel_in_e in
        let str = "   if ("^(string_of_var_expr soc c)  ^ " == _true) {\n   "^
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
            let vel_in = List.map (string_of_var_expr soc) vel_in in
            let vel_out = List.map (string_of_var_expr soc) vel_out in
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
  fun soc called_soc vel_out vel_in ctx sname step_arg -> 
    match inline_soc soc called_soc vel_out vel_in with
      | Some str -> str
      | None ->
        let vel_in  = List.map (string_of_var_expr soc) vel_in in
        let vel_out = List.map (string_of_var_expr soc) vel_out in
        let si_str =
          if vel_in = [] then "" (* occurs for pre *) else 
            let inputs = fst called_soc.profile in
            let l = try (
              List.map2 (fun (name, t) ve -> 
                gen_assign t (Printf.sprintf "%s.%s" ctx name) ve) 
                inputs vel_in
            ) with _ -> assert false (* are all parameters necessarily used? *)
            in
            (String.concat "" l) 
        in
        let so_str =
          if vel_out = [] then "" (* occurs for pre *) else 
            let outputs = snd called_soc.profile in
            let l = try (
              List.map2
                (fun  (name,t) ve -> 
                  let ve2 = Printf.sprintf "%s.%s" ctx name in
                  gen_assign t ve ve2) 
                outputs vel_out
            ) with _ -> assert false
            in
            (String.concat "" l) ^"\n"
        in
        let str = Printf.sprintf "  %s(%s); \n"
                                 (step_name called_soc.key sname) step_arg in
        (si_str ^ str ^ so_str)

(* exported *) 
let (typedef_of_soc : Soc.t -> string) =
  fun soc -> 
    if inlined_soc soc.key then ""  (* don't generate code if inlined *) else
    let ctx_name = get_ctx_name soc.key in
    let ctx_name_type = ctx_name^"_type" in    
    let il,ol = soc.profile in
    let str = Printf.sprintf  "/* %s */\ntypedef struct {\n   /*INPUTS*/\n" ctx_name in
    let str = List.fold_left (fun acc v -> acc^ (string_of_flow_decl v)) str il in
    let str = str ^ "   /*OUTPUTS*/\n" in
    let str = List.fold_left (fun acc v -> acc^ (string_of_flow_decl v)) str ol in
    let str = str ^
      (match soc.memory with
        | No_mem -> ""
        | Mem t -> 
           Printf.sprintf "   /*Memory cell*/\n   %s ;\n"
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

let (get_step_prototype : Soc.step_method -> Soc.t -> string * string * string) =
  fun sm soc ->
    let sname = step_name soc.key sm.name in
    let ctx = if SocUtils.is_memory_less soc then "" else
        Printf.sprintf "%s_type* ctx" (get_ctx_name soc.key)
    in
    let ctx_decl = if SocUtils.is_memory_less soc then "" else
        Printf.sprintf "%s_type*" (get_ctx_name soc.key)
    in
    let inputs, _ = soc.Soc.profile in
    let ctype = match inputs with
      | (_,t)::_ -> 
         Printf.sprintf "sizeof(%s)" (Soc2cUtil.data_type_to_c t "")
      | [] ->  "" (* soc without intputs won't need this output *)
    in
    Printf.sprintf "void %s(%s);\n" sname ctx_decl,
    Printf.sprintf "void %s(%s){\n" sname ctx,
    ctype

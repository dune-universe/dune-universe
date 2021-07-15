(* Time-stamp: <modified the 05/07/2021 (at 17:30) by Erwan Jahier> *)


(* let put (os: out_channel) (fmt:('a, unit, string, unit) format4) : 'a = *)
(* 	Printf.kprintf (fun t -> output_string os t) fmt *)

open Printf
open Soc2cIdent
open Data


let rec (type_to_string_rif : Data.t -> string) = 
  fun v -> 
    let str =
      match v with
        | String -> "string"
        | Bool -> "bool"
        | Int  -> "int"
        | Real -> "real"
        | Extern _s ->  (* view extern type as strings *) "string" (*id2s s*)
        | Enum  (s, _sl) -> id2s s
        | Struct (sid,_) -> (id2s sid)
        | Array (ty, sz) -> Printf.sprintf "%s_%d" (type_to_string_rif ty) sz 
        | Alpha nb -> "alpha_"^(string_of_int nb) 
        | Alias(n,_) -> n
    in
    str

let inlined_soc = Soc2cDep.inlined_soc

(****************************************************************************)

(* Soc printer *)
type 'a soc_pp = {
  hfmt:  ('a, unit, string, unit) format4 -> 'a;
  cfmt:  ('a, unit, string, unit) format4 -> 'a;
  cput : string -> unit;
  hput : string -> unit;
  soc: Soc.t
}


let (string_of_soc_key : Soc.key -> string) = Soc2cIdent.get_soc_name


let string_of_var_expr = Soc2cDep.string_of_var_expr
open Soc

(* when an error occur, remove the generated c file (for the nonreg tests) *)
exception Delete_C_files

let var_expr_is_not_a_slice = function Slice _ -> false | _ -> true

let (gao2c : Soc.tbl -> 'a soc_pp -> Soc.gao -> unit) =
  fun stbl sp gao -> 
    let rec gao2str gao = 
      match gao with
        | Case(id, id_gao_l,_) -> ( 
          let to_case_str (v,gaol) =
            let gaol_str = (List.map gao2str gaol) in
            let gaol_block = String.concat "" gaol_str in
            (id2s v), gaol_block
          in
          let cases = List.map to_case_str id_gao_l in
          let ctx_opt = 
            let il,ol = sp.soc.profile in
            if List.mem_assoc id il || List.mem_assoc id ol then
              (if SocUtils.ctx_is_global sp.soc 
               then Soc2cUtil.ML_IO sp.soc.key 
               else Soc2cUtil.M_IO)
            else Soc2cUtil.Local 
          in
          let str = Soc2cUtil.gen_c_switch (Soc2cDep.ctx_var ctx_opt sp.soc id) cases in
          str
        )
        | Call(vel_out, Assign, vel_in,_) -> (
          let l = List.map2 (Soc2cDep.gen_assign_var_expr sp.soc) vel_out vel_in in
          String.concat "" l 
        )
        | Call(vel_out, Method((inst_name,sk),sname), vel_in,lxm) -> ( 
          let called_soc = SocUtils.find lxm sk stbl in
          let _, get_index = Soc2cInstances.to_array (sp.soc).instances in
          let index = get_index (inst_name,sk) in
          let step_arg = Printf.sprintf "ctx->%s_tab[%d]" (get_ctx_name sk) index in
          let ctx = step_arg in
          let step_arg = "&"^step_arg in
          List.iter (fun ve  -> assert(var_expr_is_not_a_slice ve)) vel_in;
          List.iter (fun ve  -> assert(var_expr_is_not_a_slice ve)) vel_out;
          Soc2cDep.gen_step_call sp.soc called_soc vel_out vel_in ctx sname step_arg
        )
        | Call(vel_out, Procedure sk, vel_in, lxm) -> (
          let called_soc = SocUtils.find lxm sk stbl in
          let ctx = get_ctx_name called_soc.key in
          (try
            List.iter (fun ve  -> assert(var_expr_is_not_a_slice ve)) vel_in;
            List.iter (fun ve  -> assert(var_expr_is_not_a_slice ve)) vel_out;
          with _ -> 
            print_string
              "*** Error. Slices in left part not yet supported in the C code generator, sorry\n";
            flush stdout;
            raise Delete_C_files
          ); 
          Soc2cDep.gen_step_call sp.soc called_soc vel_out vel_in ctx "step" ""
        )
    in
    sp.cput (gao2str gao)

let (step2c : Soc.tbl -> 'a soc_pp -> Soc.step_method -> unit) =
  fun stbl sp sm -> 
  if inlined_soc sp.soc.key then () (* don't generate code if inlined *) else
    (*     let sname = Soc2cDep.step_name sp.soc.key sm.name in *)
    let sname = Soc2cDep.step_name sp.soc.key sm.name in
    if sm.impl<>Extern then (
      let decl, def, ctype = Soc2cDep.get_step_prototype sm sp.soc in
      sp.hput (Printf.sprintf "%s\n" decl);
      sp.cput (Printf.sprintf "%s" def);
      (match sm.impl with
       | Extern -> ()
       | Predef ->
         (match sp.soc.key with
          | ("Lustre::eq",(Array _)::_,_) ->  
            let str = Printf.sprintf
                "  *out = memcmp((const void *) i1, (const void *) i2, %s)==0;\n"
                ctype in
            sp.cput str  
          | ("Lustre::eq",(Struct _)::_,_) -> 
            let str = Printf.sprintf
                "  *out = memcmp((const void *) &i1, (const void *) &i2, %s)==0;\n"
                ctype in
            sp.cput str  
          | ("Lustre::neq",(Array _)::_,_)  ->
            let str = Printf.sprintf
                "  *out = !memcmp((const void *) i1, (const void *) i2, %s)==0;\n"
                ctype in
            sp.cput str  
          | ("Lustre::neq",(Struct _)::_,_)  ->
            let str = Printf.sprintf
                "  *out = !memcmp((const void *) &i1, (const void *) &i2, %s)==0;\n"
                ctype in
            sp.cput str  
          | n -> sp.cput (Soc2cDep.get_predef_op n)
         )
       | Gaol(vl, gaol) -> (
           if Lv6MainArgs.global_opt.Lv6MainArgs.gen_wcet then 
             List.iter
               (fun v -> sp.cput (Soc2cUtil.string_of_flow_decl_w7annot gaol v))
               vl
           else
             List.iter (fun v -> sp.cput (Soc2cUtil.string_of_flow_decl v)) vl;
           sp.cput "\n"; 
           List.iter (gao2c stbl sp) gaol
         )
       | Iterator(it,it_soc_key,s) -> (
           (* iterating on Lustre::eq or neq currently does not work
              Refuse such programs explicitly  *)
           match sp.soc.key with
           |  _, (Array (Array _,_))::_, _
           |  _, (Array (Struct _,_))::_, _  -> (
               match it_soc_key with
               | _,(Alpha _::_),_  -> 
                 let msg = Printf.sprintf
                     "%s\nIterating on polymorphic nodes over arrays of structures %s"
                     (Lxm.details sm.lxm)
                     "is currently unsupported"
                 in
                 failwith msg
               | _ -> ()
             )
           | _,_,_ -> ()
         );   
         let it_soc = SocUtils.find sm.lxm it_soc_key stbl in
         sp.cput (Soc2cDep.get_iterator sp.soc it it_soc s)
       | Boolred(i,j,k) -> 
         sp.cput (Soc2cDep.get_boolred sp.soc i j k)
       | Condact(k,el) -> 
         sp.cput (Soc2cDep.get_condact sp.soc (SocUtils.find sm.lxm k stbl) el)
      );
      sp.cput (sprintf "\n} // End of %s\n\n" sname)
    )
let (gen_instance_init_call : 'a soc_pp -> Soc.key * int -> unit) =
  fun sp (key,i) -> 
    let ctx_name = get_ctx_name key in
    if Lv6MainArgs.global_opt.Lv6MainArgs.soc2c_inline_loops || i<4 then
      for k=0 to i-1 do
        sp.cfmt "\n    %s_reset(&ctx->%s_tab[%d]);" ctx_name ctx_name k
      done
    else (
      sp.cput (Printf.sprintf "  for (_i=0 ; _i<%d ; _i+=1){" i);
      sp.cput (Printf.sprintf "\n    %s_reset(&ctx->%s_tab[_i]);" ctx_name ctx_name);
      sp.cput "\n }"
    )

module KeySet = Set.Make(struct type t = Soc.key let compare = compare end)

(****************************************************************************)
let (type_to_format_string : Data.t -> string) =
  function
    | String -> "%s"
    | Bool -> "%d"
    | Int -> "%d"
    | Real-> "%f"
    | Extern _s -> "%s"
    | Enum  (_s, _sl) -> "%d"
    | Struct (_sid,_) -> "%s"
    | Array (_ty, _sz) -> "%s"
    | Alpha _nb -> assert false
    | Alias _ -> assert false

let (get_used_soc : Soc.t -> KeySet.t) =
  fun soc -> (* dig into the soc for the list of socs it uses *)
  let rec get_soc_of_gao acc = function
    | Case(_,l,_) ->
       List.fold_left
         (fun acc (_,gaol) -> List.fold_left get_soc_of_gao acc gaol) acc l         
    | Call(_,Assign,_,_) -> acc
    | Call(_,Method((_,sk),_),_,_) 
    | Call(_,Procedure sk,_,_) -> KeySet.add sk acc
  in
  let get_soc_of_step acc sm =
    match sm.impl with
    | Gaol(_, gaol) -> List.fold_left get_soc_of_gao acc gaol
    | Iterator(_,sk,_)
    | Condact(sk,_) -> KeySet.add sk acc
    | _ -> acc
  in
  List.fold_left get_soc_of_step KeySet.empty soc.step
           
let one_file() = Lv6MainArgs.global_opt.Lv6MainArgs.soc2c_one_file

(* soc2c
- creates c and h file(s) 
- updates/returns the list of created C files *)
let (soc2c : int -> out_channel -> out_channel -> Soc.tbl -> Soc.key ->
             string list -> Soc.t -> string list) = 
  fun pass hfile cfile stbl msoc_key cfiles_acc soc -> 
  if inlined_soc soc.key then cfiles_acc (* don't generate code if inlined *) else
    let ctx_name = get_ctx_name soc.key in
    let ctx_name_type = ctx_name^"_type" in
    if pass=1 then (
      (* Only for ctx of memoryless nodes + main node *)
      if SocUtils.ctx_is_global soc then
        Printf.kprintf (fun t -> output_string cfile t) "static %s %s;\n" ctx_name_type ctx_name;
        cfiles_acc
    ) else (
      let dir = Lv6MainArgs.global_opt.Lv6MainArgs.dir in
      let base0 = (string_of_soc_key soc.key) in
      let base = Filename.concat dir base0 in
      let cfile,hfile,cfiles_acc =
        if one_file() || msoc_key = soc.key then cfile, hfile, cfiles_acc else
          let _cfile0 =  (base0^".c") in
          let hfile0 =  (base0^".h") in
          let cfile =  (base^".c") in
          let hfile =  (base^".h") in
          let cfile_oc = open_out cfile in
          let hfile_oc = open_out hfile in
          (*open_out (base^".h"), *)
          Lv6util.entete cfile_oc "/*" "*/" ;
          Lv6util.entete hfile_oc "/*" "*/" ;
          Printf.fprintf cfile_oc "#include \"%s\"\n" hfile0;
          Printf.fprintf hfile_oc "#include \"lustre_types.h\"\n";
          Printf.fprintf hfile_oc "#include \"lustre_consts.h\"\n";
          Printf.fprintf hfile_oc "#ifndef _%s_H_FILE \n" base0;
          Printf.fprintf hfile_oc "#define _%s_H_FILE \n" base0;
          flush cfile_oc;
          flush hfile_oc;
          cfile_oc, hfile_oc,
          (if List.mem cfile cfiles_acc then cfiles_acc else cfile::cfiles_acc)
      in
      let hfmt fmt = Printf.kprintf (fun t -> output_string hfile t) fmt in
      let cfmt fmt = Printf.kprintf (fun t -> output_string cfile t) fmt in
      let hput str = output_string hfile str in
      let cput str = output_string cfile str in
      let sp = { hfmt = hfmt; cfmt=cfmt; hput = hput; cput = cput; soc = soc } in
      (* include the header files that define the step functions used by the soc *)
      if (one_file()) then () else (
        let (used_soc:Soc.key list) = KeySet.elements (get_used_soc soc) in
        List.iter
          (fun sk -> if inlined_soc sk then () else
                       hfmt "#include \"%s.h\"\n" (string_of_soc_key sk)
          )
          used_soc;
        if msoc_key <> soc.key then hfmt "%s\n" (Soc2cDep.typedef_of_soc soc);
      );
      
      if SocUtils.is_memory_less soc then () else (
        cfmt "// Memory initialisation for %s\n" ctx_name;
        hfmt "void %s_reset(%s_type* ctx);\n" ctx_name ctx_name;
        cfmt "void %s_reset(%s_type* ctx){" ctx_name ctx_name;
        (* Call the reset_ctx functions of the soc instances *)
        if Lv6MainArgs.global_opt.Lv6MainArgs.soc2c_inline_loops
        then () else if soc.instances <> [] then sp.cput "\n  int _i;\n";
        List.iter (gen_instance_init_call sp)
          (fst (Soc2cInstances.to_array soc.instances));
        (match soc.key with
         (* set the parameter fields that have a default value (arrow,fby) *)
         | (_,_,MemInit (ve)) ->
            assert(var_expr_is_not_a_slice ve);
            cfmt "  ctx->_memory = %s;" (string_of_var_expr soc ve)
         | _ -> ()
        );
        cfmt "\n}\n";
        if
          SocUtils.is_memory_less soc
        then () (*no ctx at all in this case ! *) 
        else if
          Lv6MainArgs.global_opt.Lv6MainArgs.soc2c_global_ctx
        then
          (
            cfmt "\n// Initialisation of the  internal structure of %s\n" ctx_name;
            hfmt "void %s_init(%s_type* ctx);\n" ctx_name ctx_name;
            cfmt "void %s_init(%s_type* ctx){" ctx_name ctx_name;
            cfmt "
  // ctx->client_data = cdata;
  %s_reset(ctx);
 }
" ctx_name 
          )   
        else
          (
            cfmt "// Memory allocation for %s\n" ctx_name;
            hfmt "%s_type* %s_new_ctx();\n" ctx_name ctx_name;
            cfmt "%s_type* %s_new_ctx(){" ctx_name ctx_name;
            cfmt "

   %s_type* ctx = (%s_type*)calloc(1, sizeof(%s_type));
   // ctx->client_data = cdata;
   %s_reset(ctx);
  return ctx;
}
" ctx_name ctx_name ctx_name ctx_name) 
      );
      cfmt "// Step function(s) for %s\n" ctx_name;
      List.iter (step2c stbl sp) soc.step;
      if not (one_file() || msoc_key = soc.key) then (
        hfmt "#endif /* _%s_H_FILE */" base;
        close_out hfile;
        close_out cfile;
        Printf.eprintf "W: %s.h has been generated.\n" base;
        Printf.eprintf "W: %s.c has been generated.\n" base;
        flush stderr
      );
      cfiles_acc
      )


(****************************************************************************)


module ItemKeySet = Set.Make(struct type t = Lic.item_key let compare = compare end)

(* To perform the topological sort of typedef. nf stands for "no
   fixpoint", that should be done by the caller. it is recursive just
   to deal with array of arrays *)
let  (find_typedep_nf : Lic.type_ -> Lic.item_key list) =
  fun t -> 
    let rec aux top = function
      | Lic.Bool_type_eff | Lic.Int_type_eff | Lic.Real_type_eff
      | Lic.External_type_eff _   | Lic.TypeVar _  
        ->  []
      | Lic.Abstract_type_eff(name,_) 
      | Lic.Enum_type_eff(name,_) -> if top then [] (* avoid self dep *) else [name] 
      | Lic.Array_type_eff(t,_) -> aux false t
      | Lic.Struct_type_eff(name, fl) -> 
        if not top then [name] else List.flatten(List.map (fun (_,(t,_)) -> aux false t) fl)
    in
    aux true t

let (is_extern_type: Lic.type_ -> bool) =
  function 
    | Lic.External_type_eff _ -> true
    | _  -> false


(* returns the typedef *)
let user_typedef licprg = 
  let to_c k t =
    Printf.sprintf "typedef %s;\n" (Soc2cUtil.lic_type_to_c t (long2s k))
  in
  let rec (typedef_to_string : Lic.item_key -> Lic.type_ -> string * ItemKeySet.t -> 
                               string * ItemKeySet.t) =
    fun k t acc ->
    (* topological sort according to type dep *)
    if is_extern_type t then acc else
      if ItemKeySet.mem k (snd acc) then acc else
        let type_list = find_typedep_nf t in
        let acc = List.fold_left 
                    (fun acc k -> 
                     match LicPrg.find_type licprg k with
                     | Some t -> typedef_to_string k t acc
                     | None -> acc (* occurs ? *)
                    ) 
                    acc type_list 
        in
        ((fst acc)^(to_c k t), ItemKeySet.add k (snd acc))
  in
  fst (LicPrg.fold_types typedef_to_string licprg ("",ItemKeySet.empty))
    
let (typedef_all : LicPrg.t -> Soc.tbl -> Soc.t -> string) =
  fun _licprg soc_tbl main_soc ->
  (* We need to print the ctx typedef in a good order
       (w.r.t. typedef dependencies).  To do that, we traverse
       the tree of soc instances which root is the main soc. 
   *)

  (* Soc with memory can be used several times; hence we mark via this
       set the ones that have already been visited. *)
  let visited = KeySet.empty in
  let rec (soc_with_mem : string * KeySet.t -> Soc.t -> string * KeySet.t) =
    (* recursively traverse the soc dependancies to define the typedef
       in the good order (i.e., define before use) *)
    fun (acc,visited) soc ->
    if KeySet.mem soc.key visited then (acc,visited) else
      let visited = KeySet.add soc.key visited in
      let acc,visited =
        List.fold_left
          (fun (acc,visited) (_iname, sk) ->
           let soc = SocUtils.find_no_exc sk soc_tbl in
           soc_with_mem (acc,visited) soc
          )
          (acc,visited) soc.instances
      in
      let acc = acc ^ (
          if one_file() || soc.key = main_soc.key then
            Soc2cDep.typedef_of_soc soc
          else
            (Printf.sprintf "#include \"%s.h\"\n" (string_of_soc_key soc.key))
        )
      in
      acc,visited
  in
  let soc_ctx_typedef_with_mem = 
    if SocUtils.ctx_is_global main_soc then "" else
      fst (soc_with_mem ("",visited) main_soc)
  in
  (* Then we still have to print memoryless soc that can not appear
       as a soc instance *)
  let soc_ctx_typedef_without_mem =
    let socs = Soc.SocMap.bindings soc_tbl in
    let socs = snd (List.split socs) in
    let memless_soc_to_string acc soc =
      if SocUtils.is_memory_less soc then acc^(Soc2cDep.typedef_of_soc soc) else acc
    in 
    List.fold_left  memless_soc_to_string "" socs
  in 
"// Memoryless soc ctx typedef \n"^soc_ctx_typedef_without_mem
  ^"// Memoryfull soc ctx typedef \n"^soc_ctx_typedef_with_mem
    



(****************************************************************************)
let rec (const_to_c: Lic.const -> string) =
  function
    | Lic.Bool_const_eff true -> "1"
    | Lic.Bool_const_eff false -> "0"
    | Lic.Int_const_eff i -> (sprintf "%s" i)
    | Lic.Real_const_eff r -> r
    | Lic.Extern_const_eff (s,_t) -> (long2s s)
    | Lic.Abstract_const_eff (_s,_t,v,_) -> const_to_c v
    | Lic.Enum_const_eff   (s,Lic.Enum_type_eff(_,ll)) -> Lic.enum_to_string s ll
    | Lic.Enum_const_eff   _ -> assert false (* SNO *)
    | Lic.Struct_const_eff (fl, _t) -> (
      let string_of_field = 
        function (id, veff) -> 
          (Lv6Id.to_string id)^" = "^ (const_to_c veff) 
      in
      let flst = List.map string_of_field fl in
(*       (string_of_type_eff t)^ *)
        "{"^(String.concat "; " flst)^"}"
    )
    | Lic.Array_const_eff (ctab, _t) -> (
      let vl = List.map const_to_c ctab in
      "{"^(String.concat ", " vl)^"}"
    )
    | Lic.Tuple_const_eff _cl -> assert false

(* returns a pair: the lhs for the .h, the rhs for the .c
Indeed, arrays constant need to be defined in a .c
*)
let (constdef : LicPrg.t -> string*string) = 
  fun licprg ->
  let rec (array_type_to_c: string -> string -> int -> Lic.type_ -> string) =
    fun name acc s t -> match t with
      | Lic.Array_type_eff (t2, s2) ->
        array_type_to_c name (Printf.sprintf "%s [%i]" acc s) s2 t2
      | Lic.Bool_type_eff->  Printf.sprintf "_boolean %s %s[%i]" name acc s
      | Lic.Int_type_eff ->  Printf.sprintf "_integer %s %s[%i]" name acc s
      | Lic.Real_type_eff -> Printf.sprintf "_real %s %s[%i]" name acc s
      | Lic.External_type_eff _ -> assert false
      | Lic.Abstract_type_eff _ -> assert false
      | Lic.Enum_type_eff _ -> assert false
      | Lic.Struct_type_eff  _ -> assert false 
      | Lic.TypeVar  _ -> assert false
  in
  let to_c k = function
    | Lic.Extern_const_eff _ -> "",""
    (* | Lic.Array_const_eff (ctab, Array_type_eff(_t,s)) -> ( *)
    | Lic.Array_const_eff (ctab, t) -> (
        let vl = List.map const_to_c ctab in
        let s = List.length vl in
        let tab_exp = "{"^(String.concat ", " vl)^"}" in
        Printf.sprintf "const %s;\n"  (array_type_to_c (long2s k) "" s t), 
        Printf.sprintf "const %s = %s;\n" (array_type_to_c (long2s k) "" s t) tab_exp 
      )       
    | c  -> 
      Printf.sprintf "#define %s %s\n"
        (long2s k)
        (const_to_c c),""
  in
  let strh,strc = LicPrg.fold_consts
      (fun k t (acc_h,acc_c) ->
         let h,c = to_c k t in
         (acc_h^h,acc_c^c)) licprg  ("","")
  in
  (if strh = "" then "" else
     "\n// Constant definitions \n" ^ strh),
  (if strc = "" then "" else
     "\n// Constant definitions \n" ^ strc)

(****************************************************************************)

let (gen_memoryless_ctx : Soc.tbl -> string) =
  fun stbl -> 
    let do_soc _sk soc acc =
      if (SocUtils.ctx_is_global soc) && not (inlined_soc soc.key) then 
        let ctx_name = get_ctx_name soc.key in
        let ctx_name_type = ctx_name^"_type" in    
        Printf.sprintf "%sextern %s %s;\n" acc ctx_name_type ctx_name 
      else
        acc
    in
    let acc = Soc.SocMap.fold do_soc stbl "" in
    if acc = "" then "" else 
      Printf.sprintf "\n// Allocation of memoryless ctx\n%s" acc

(* a shortcut *)
let io_transmit_mode () = Lv6MainArgs.global_opt.Lv6MainArgs.io_transmit_mode

(****************************************************************************)
let gen_main_loop_body inputs outputs soc ctx =
  if not Lv6MainArgs.global_opt.Lv6MainArgs.soc2c_global_ctx then
    (match io_transmit_mode () with
     | Lv6MainArgs.Stack ->
        let to_c_decl (n,t) = ((Soc2cUtil.data_type_to_c t n)^ ";\n  ") in
        let inputs_t  = List.map to_c_decl inputs in
        let outputs_t = List.map to_c_decl outputs in
        let inputs_decl = Printf.sprintf "\n  %s" (String.concat "" inputs_t) in
        let outputs_decl = Printf.sprintf "%s" (String.concat "" outputs_t) in
        let ctx_decl = if SocUtils.is_memory_less soc then "" else 
                         "  "^ctx^"_type* ctx = "^ ctx^"_new_ctx(NULL);\n"
        in
        inputs_decl ^ outputs_decl ^ ctx_decl
     | Lv6MainArgs.Heap -> ("
/* Context allocation */
" ^ (if SocUtils.is_memory_less soc then ctx^"_type* ctx = &"^ctx^";\n"
     else ctx^"_type* ctx = "^ ctx^"_new_ctx(NULL);")
                              )
        | Lv6MainArgs.HeapStack -> ("
/* Context allocation */
" ^ (if SocUtils.is_memory_less soc then ctx^""
     else ctx^"_type* ctx ;\n"^ ctx^"_reset(ctx);")
                                   )
    ) 
  else
    (match io_transmit_mode () with
     | Lv6MainArgs.Stack ->
        let to_c_decl (n,t) = ((Soc2cUtil.data_type_to_c t n)^ ";\n  ") in
        let inputs_t  = List.map to_c_decl inputs in
        let outputs_t = List.map to_c_decl outputs in
        let inputs_decl = Printf.sprintf "\n  %s" (String.concat "" inputs_t) in
        let outputs_decl = Printf.sprintf "%s" (String.concat "" outputs_t) in
        let ctx_decl = if SocUtils.is_memory_less soc then "" else 
                         ctx^"_type ctx_struct;\n  "^
                           ctx^"_type* ctx = &ctx_struct;\n  "^
                             ctx^"_init(ctx);"
        in
        inputs_decl ^ outputs_decl ^ ctx_decl
     | Lv6MainArgs.Heap -> ("
/* Context allocation */
" ^ (if SocUtils.is_memory_less soc then 
       ctx^"_type* ctx = &"^ctx^";\n"
     else (
       ctx^"_type ctx_struct;
    "^ctx^"_type* ctx = & ctx_struct;
    "^ctx^"_init(ctx);    
")
    )
                           )
     | Lv6MainArgs.HeapStack -> ("
/* Context allocation */
" ^ (if SocUtils.is_memory_less soc then ctx^""
     else ctx^"_type* ctx ;\n"^ ctx^"_reset(ctx);")
                                )
    )
      
(****************************************************************************)
let (gen_main_wcet_file : Soc.t -> string -> Soc.tbl -> unit) =
  fun soc base _stbl ->
    let base0 = Filename.basename base in
    let mainfile = base^"_main.c" in
    let oc = open_out mainfile in

    let putc s = output_string oc s in
    let ctx = get_ctx_name soc.key in
    let step = Soc2cDep.step_name soc.key "step" in
    let inputs,outputs = soc.profile in
    Lv6util.entete oc "/*" "*/";
    putc  ("
#include <stdlib.h>
#include \""^base0 ^".h\" 
int main(){" ^ (gen_main_loop_body inputs outputs soc ctx));
    (match io_transmit_mode () with
      | Lv6MainArgs.Stack ->
        let i =  fst (List.split inputs) in
        let o = List.map (fun (n,t) -> match t with Data.Array(_,_) -> n | _ ->"&"^n) outputs in
        let io = String.concat "," (i@o) in
        let io = if SocUtils.is_memory_less soc then io else if io = "" then "ctx" else io^",ctx" in
        putc ("  " ^ step^"("^io^");
  return 0; 
}
"
        );
      | Lv6MainArgs.HeapStack -> assert false
      | Lv6MainArgs.Heap ->
        let io = if SocUtils.is_memory_less soc then  "" else "ctx"  in
        putc ("  " ^ step^"("^io^");
  return 0; 
}
"
        );
    );
    Printf.eprintf "W: %s has been generated.\n" mainfile; flush stderr;
    close_out oc


let (gen_loop_file : string -> LicPrg.t -> Soc.t -> string -> out_channel -> Soc.tbl
     -> unit) =
  fun fn licprg soc base oc stbl ->
    let base0 = Filename.basename base in
    let putc s = output_string oc s in
    let ctx = get_ctx_name soc.key in
    let step = Soc2cDep.step_name soc.key "step" in
    let inputs,outputs = soc.profile in
    let inputs_io  = SocVar.expand_profile true false inputs  in
    let outputs_io = SocVar.expand_profile true false outputs in
    let inputs_exp = SocVar.expand_profile true true  inputs  in
    let outputs_exp= SocVar.expand_profile true true  outputs in

    Lv6util.entete oc "/*" "*/";
    putc  ("
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include \""^base0 ^".h\" 
/* Print a promt ? ************************/
static int ISATTY;
/* MACROS DEFINITIONS ****************/
#ifndef TT
#define TT \"1\"
#endif
#ifndef FF
#define FF \"0\"
#endif
#ifndef BB
#define BB \"bottom\"
#endif
#ifdef CKCHECK
/* set this macro for testing output clocks */
#endif

void _read_pragma("^ (
             if SocUtils.is_memory_less soc then "" else
               ctx^"_type* ctx,"
           ) ^ "char b[]) {
   int s = 1;

   if (!strcmp(b,\"#quit\")) exit(0);
   if (!strcmp(b,\"#q\")) exit(0);"^
           (if SocUtils.is_memory_less soc then "" else "
   if (!strcmp(b,\"#reset\")) "^ctx^"_reset(ctx);
")^"   return;
}

/* Standard Input procedures **************/
_boolean _get_bool("^(
             if SocUtils.is_memory_less soc then "" else
               ctx^"_type* ctx,"
           ) ^"char* n){
   char b[512];
   char c;
   _boolean r = 0;
   int s = 1;
   do {
      if(ISATTY) {
         if((s != 1)||(r == -1)) printf(\"\\a\");
         // printf(\"%s (1,t,T/0,f,F) ? \", n);
      }
      if(scanf(\"%s\", b)==EOF) exit(0);
      r = -1;
      c=b[0];
      if(c == 'q') exit(0);
      if(c == '#') _read_pragma("^
           (if SocUtils.is_memory_less soc then "" else "ctx,")^"b);
      if((c == '0') || (c == 'f') || (c == 'F')) r = 0;
      if((c == '1') || (c == 't') || (c == 'T')) r = 1;
   } while((s != 1) || (r == -1));
   return r;
}
_integer _get_int("^(
             if SocUtils.is_memory_less soc then "" else
               ctx^"_type* ctx,"
           ) ^"char* n){
   char b[512];
   _integer r;
   int s = 1;
   do {
      if(ISATTY) {
         if(s != 1) printf(\"\\a\");
         //printf(\"%s (integer) ? \", n);
      }
      if(scanf(\"%s\", b)==EOF) exit(0);
      if(*b == 'q') exit(0);
      if(*b == '#') {
         _read_pragma("^
           (if SocUtils.is_memory_less soc then "" else "ctx,")^"b);
      } else {
        s = sscanf(b, \"%d\", &r);
      }
   } while(s != 1);
   return r;
}
#define REALFORMAT ((sizeof(_real)==8)?\"%lf\":\"%f\")
_real _get_real("^(
             if SocUtils.is_memory_less soc then "" else
               ctx^"_type* ctx,"
           ) ^"char* n){
   char b[512];
   _real r;
   int s = 1;
   do {
      if(ISATTY) {
         if(s != 1) printf(\"\\a\");
         //printf(\"%s (real) ? \", n);
      }
      if(scanf(\"%s\", b)==EOF) exit(0);
      if(*b == 'q') exit(0);
      if(*b == '#') {
         _read_pragma("^
           (if SocUtils.is_memory_less soc then "" else "ctx,")^"b);
      } else {
         s = sscanf(b, REALFORMAT, &r);
      }
   } while(s != 1);
   return r;
}
char* _get_string("^(
             if SocUtils.is_memory_less soc then "" else
               ctx^"_type* ctx,"
           ) ^"char* n){
   char b[512];
   char* r;
   int s = 1;
   do {
      if(ISATTY) {
         if(s != 1) printf(\"\\a\");
         //printf(\"%s (real) ? \", n);
      }
      if(scanf(\"%s\", b)==EOF) exit(0);
      if(*b == '#') {
         _read_pragma("^
           (if SocUtils.is_memory_less soc then "" else "ctx,")^"b);
      }
   } while(s != 1);
   r =  b;
   return r;
}
/* Standard Output procedures **************/
void _put_bottom(char* n){
   if(ISATTY) printf(\"%s = \", n);
   printf(\"%s \", BB);
   if(ISATTY) printf(\"\\n\");
}
void _put_bool(char* n, _boolean _V){
   if(ISATTY) printf(\"%s = \", n);
   printf(\"%s \", (_V)? TT : FF);
   if(ISATTY) printf(\"\\n\");
}
void _put_int(char* n, _integer _V){
   if(ISATTY) printf(\"%s = \", n);
   printf(\"%d \", _V);
   if(ISATTY) printf(\"\\n\");
}
void _put_real(char* n, _real _V){
   if(ISATTY) printf(\"%s = \", n);
   printf(\"%f \", _V);
   if(ISATTY) printf(\"\\n\");
}
void _put_string(char* n, char* _V){
   if(ISATTY) printf(\"%s = \", n);
   printf(\"%s \", _V);
   if(ISATTY) printf(\"\\n\");
}
"^(Soc2cExtern.gen_getters fn licprg soc)^"
/* Output procedures **********************/
#ifdef CKCHECK
void %s_BOT_n(void* cdata){
   _put_bottom(\"n\");
}
#endif
"^ (gen_memoryless_ctx stbl) ^
"
/* Main procedure *************************/
int main(){
  int _s = 0;" ^ (
  (gen_main_loop_body inputs outputs soc ctx)
           )
    );
    let to_rif_decl (n,t) = ("\\\""^n^"\\\":" ^(type_to_string_rif t)) in
    let inputs_t  = List.map to_rif_decl inputs_io in
    let outputs_t = List.map to_rif_decl outputs_io in
    let inputs_decl = Printf.sprintf "#inputs %s" (String.concat " " inputs_t) in
    let outputs_decl = Printf.sprintf "#outputs %s" (String.concat " " outputs_t) in
    
    putc  ("
  printf(\""^inputs_decl^"\\n\");
  printf(\""^outputs_decl^"\\n\");

  /* Main loop */
  ISATTY = isatty(0);
  while(1){
    if (ISATTY) printf(\"#step %d \\n\", _s+1);
    else if(_s) printf(\"\\n\");
    fflush(stdout);
    ++_s;
");
    List.iter (fun (id,t) -> 
        let t = type_to_string_rif t in
        let clk = "" in (* XXX finish me: get the clock if id using soc.clock_profile
                         and set this string to something like "if (clk_id)" 
                         *)
        let str =
          let ctx_opt = if SocUtils.is_memory_less soc then "" else "ctx," in
          if io_transmit_mode () = Lv6MainArgs.Stack 
          then Printf.sprintf "   %s %s = _get_%s(%s\"%s\");\n" clk id t ctx_opt id 
          else Printf.sprintf "   %s ctx->%s = _get_%s(%s\"%s\");\n" clk id t ctx_opt id 
        in
        putc str
      ) 
      inputs_exp;
    let inputs_fmt  = List.map (fun (_,t) -> type_to_format_string t) inputs_io in
    let outputs_fmt = List.map (fun (_,t) -> type_to_format_string t) outputs_io in
    if io_transmit_mode () = Lv6MainArgs.Stack 
    then
      let i =  fst (List.split inputs) in
      let o = List.map
                (fun (n,t) -> match t with Data.Array(_,_) -> n | _ ->"&"^n)
                outputs
      in
      let io = String.concat "," (i@o) in
      let io = if SocUtils.is_memory_less soc then io
               else if io = "" then "ctx" else io^",ctx"
      in
      putc ("    " ^ step^"("^io^");
    // printf(\"" ^ (String.concat " " inputs_fmt)^ " #outs " ^ 
               (String.concat " " outputs_fmt)^ "\\n\"," ^
                 (String.concat "," (List.map (fun (id,_) -> ""^id )
                                              (inputs_exp@outputs_exp)))^ 
               ");
    printf(\"" ^ 
               (String.concat " " outputs_fmt)^ "\\n\"," ^
               (String.concat "," (List.map (fun (id,_) -> ""^id ) (outputs_exp)))^ 
               ");
  }"
    ) else (
      putc ("    " ^ step^"(ctx);
    // printf(\"" ^ (String.concat " " inputs_fmt)^ " #outs " ^ 
               (String.concat " " outputs_fmt)^ "\\n\"," ^
                 (String.concat "," (List.map (fun (id,_) -> "ctx->"^id )
                                              (inputs_exp@outputs_exp)))^ 
               ");
    printf(\"" ^ 
               (String.concat " " outputs_fmt)^ "\\n\"," ^
             (String.concat "," (List.map (fun (id,_) -> "ctx->"^id ) (outputs_exp)))^ 
             ");
  }"));
    putc "\n  return 1;
   
}
"


let (gen_loop_file4ogensim : Soc.t -> string -> out_channel -> Soc.tbl -> unit) =
  fun soc base oc _stbl -> 
  let putc s = output_string oc s in
  let ctx = get_ctx_name soc.key in
  let step = Soc2cDep.step_name soc.key "step" in
  let inputs,outputs = soc.profile in
  let inputs_io  = SocVar.expand_profile true false inputs  in
  let outputs_io = SocVar.expand_profile true false outputs in
  let inputs_exp = SocVar.expand_profile true true  inputs  in
  let outputs_exp= SocVar.expand_profile true true  outputs in
  let define_define i (var_name,_) =
    putc (Printf.sprintf "#define _%s\t 0xe%07X\n" var_name ((i+1)*8));
  in
  Lv6util.entete oc "/*" "*/";
  
  putc  ("#include \""^base ^".h\"
                              
                              #define tickBegin  0xe0000000
                              ");
  List.iteri define_define (inputs_io@ outputs_io);
  
  putc ("\n int main(){\n" ^ gen_main_loop_body inputs outputs soc ctx);
  putc  ("
          /* Main loop */
          while(1){
          // notify the simulator that it is time to load the inputs from Lurette
          // to the specified address
          *((unsigned int*)tickBegin) = 0;
          // load inputs from the memory locations
          ");
  List.iter2 
    (fun (id,t)  (id_flat,_) -> 
     let t = Soc2cUtil.data_type_to_c t "" in
     let str = 
       Printf.sprintf "    %s = *((%s*)_%s);\n" id t id_flat
     in
     putc str
    ) 
    inputs_exp inputs_io;

  assert (io_transmit_mode () = Lv6MainArgs.Stack);

  let i =  fst (List.split inputs) in
  let o = List.map (fun (n,t) -> match t with Data.Array(_,_) -> n | _ ->"&"^n) outputs in
  let io = String.concat "," (i@o) in
  let io = if SocUtils.is_memory_less soc then io else if io = "" then "ctx" else io^",ctx" in
  putc ("    " ^ step^"("^io^");

// now write the output to the memory which will be output to Lurette
");
  List.iter2
    (fun (id,t) (id_flat,_) -> 
     let t = Soc2cUtil.data_type_to_c t "" in
     let str = Printf.sprintf "	 *((%s*)_%s) = %s;\n" t id id_flat in
     putc str
    ) 
    outputs_io outputs_exp;

  putc "}\n  return 1;       
        }        
        "

(****************************************************************************)
(* The entry point for lv6 --to-c *)
let (f : Lv6MainArgs.t -> Soc.key -> Soc.tbl -> LicPrg.t -> unit) =
  fun args msoc stbl licprg -> 
  let socs = Soc.SocMap.bindings stbl in
  let socs = snd (List.split socs) in 
  (* XXX que fait-on pour les soc predef ? *)
  (*     let _, socs = List.partition is_predef socs in *)
  let dir = Lv6MainArgs.global_opt.Lv6MainArgs.dir in
  let base0 = 
    if args.Lv6MainArgs.outfile = "" then
      string_of_soc_key msoc 
    else
      Filename.basename (
          try Filename.chop_extension args.Lv6MainArgs.outfile
          with Invalid_argument _ -> args.Lv6MainArgs.outfile)
  in
  let base = Filename.concat dir base0 in
  let hfile0 = base0 ^ ".h" in
  let hfile = base ^ ".h" in
  let cfile = base ^ ".c" in
  let ext_cfile = Printf.sprintf "%s_ext.c" base in
  let ext_hfile0 = Printf.sprintf "%s_ext.h" base0 in
  let ext_hfile = Printf.sprintf "%s_ext.h" base in
  let loopfile = base^"_loop.c" in
  let occ = open_out cfile in
  let och = open_out hfile in
  let ocl =
    if Lv6MainArgs.global_opt.Lv6MainArgs.soc2c_dro then stdout else
      open_out loopfile
  in
  let types_h_oc = open_out (Filename.concat dir "lustre_types.h") in
  let consts_h_oc = open_out (Filename.concat dir "lustre_consts.h") in
  let consts_c_oc = open_out (Filename.concat dir "lustre_consts.c") in
  let cfiles_acc = [Filename.concat dir "lustre_consts.c"; cfile] in
  let const_def_h, const_def_c = constdef licprg in
  let assign_ext_types_list = (Soc2cGenAssign.gen_used_types socs) in
  let main_soc = SocUtils.find_no_exc msoc stbl in
    (* Generate ext files if necessary  *)
  let needs_cfile, needs_hfile = 
    Soc2cExtern.gen_files main_soc stbl licprg ext_cfile ext_hfile hfile
  in
  Lv6util.entete consts_h_oc "/*" "*/" ;
  output_string consts_h_oc "#include \"lustre_types.h\"";
  output_string consts_h_oc const_def_h;
  Lv6util.entete consts_c_oc "/*" "*/" ;
  output_string consts_c_oc "#include \"lustre_consts.h\"";
  output_string consts_c_oc const_def_c;
  Lv6util.entete types_h_oc "/*" "*/" ;
  output_string types_h_oc ("
#ifndef _SOC2C_PREDEF_TYPES
#define _SOC2C_PREDEF_TYPES
typedef int _boolean;
typedef int _integer;
typedef char* _string;
typedef double _real;
typedef double _double;
typedef float _float;
#define _false 0
#define _true 1
#endif
// end of _SOC2C_PREDEF_TYPES");
  Lv6util.entete och "/*" "*/";
  output_string och ("
#include \"lustre_types.h\"

// User typedef 
#ifndef _"^base0^"_TYPES
#define _"^base0^"_TYPES\n");
    output_string och (user_typedef licprg); flush och;
    output_string och ((typedef_all licprg stbl main_soc )
                       ^ "#endif // end of  _"^base0^"_TYPES
"
  ^ (if needs_hfile then "#include \""^ base0 ^"_ext.h\"" else ""));
   
    try
    let putc s = output_string occ s in
    let cfmt fmt = Printf.kprintf (fun t -> output_string occ t) fmt in
    let puth s = output_string och s in
    Lv6util.entete occ "/*" "*/" ;
    if Lv6MainArgs.global_opt.Lv6MainArgs.soc2c_dro then () else
    if Lv6MainArgs.global_opt.Lv6MainArgs.gen_wcet then (
      gen_loop_file4ogensim main_soc base ocl stbl;
      gen_main_wcet_file main_soc base stbl
    )
    else
      gen_loop_file loopfile licprg main_soc base ocl stbl;
    
    output_string och "
#include <stdlib.h>
#include <string.h>

#include \"lustre_consts.h\"

";

    if needs_hfile then puth (Printf.sprintf "#include \"%s\"\n" ext_hfile0);
    puth (Printf.sprintf "#ifndef _%s_H_FILE\n" base0);
    puth (Printf.sprintf "#define _%s_H_FILE\n" base0);

    putc (Printf.sprintf "#include \"%s\"\n" hfile0);
(*     putc (Soc2cExtern.cpy_declaration licprg); *)
    putc (Soc2cExtern.const_declaration licprg);
    let cfiles_acc = 
    if io_transmit_mode () = Lv6MainArgs.Heap then (
      if socs = [] then [] else (
      putc "/////////////////////////////////////////////////\n";
      putc "//// Static allocation of memoryless soc ctx\n";
      let cfiles_acc = List.fold_left (soc2c 1 och occ stbl msoc) cfiles_acc socs in
      putc "/////////////////////////////////////////////////\n";
      cfiles_acc
      );
    ) else cfiles_acc
    in
    putc "//// Defining step functions\n";
    let cfiles_acc = List.fold_left (soc2c 2 och occ stbl msoc) cfiles_acc socs in

    let inputs,outputs = main_soc.profile in
    let inputs_exp = SocVar.expand_profile true true  inputs  in
    let outputs_exp= SocVar.expand_profile true true  outputs in
    let inputs_exp2 = SocVar.expand_profile true false  inputs  in
    let outputs_exp2= SocVar.expand_profile true false  outputs in
    let name = get_base_name main_soc.key in
    if Lv6MainArgs.global_opt.Lv6MainArgs.soc2c_dro then (
        cfmt "///////// dro stuff

/* droconf.h begins */
/*
Struct necessary for building a DRO archive
(Dynamically linkable Reactive Object)
Such an archive can be loaded by simec/luciole
*/
#define DROVERSION \"1.1\"
#define xstr(s) str(s)  /* converts macro to string */
#define str(s) #s
/* should be of type type dro_desc_t */
#define DRO_DESC_NAME  dro_desc
struct dro_var_t {
const char* ident;
const char* type;
void* valptr;
};

struct dro_desc_t {
const char* version;
const char* name;
int nbins;
struct dro_var_t* intab;
int nbouts;
struct dro_var_t* outab;
int ( *step )();
void ( *reset )();
void ( *init )();
};
/* droconf.h ends */
";
        cfmt "static %s_ctx_type %s_ctx;\n" name name;
        cfmt "//dro requires : int ( *step )();
static int ze_step(){
	%s_step(& %s_ctx);
	return 0;
}
//dro requires : void ( *reset )();
static void ze_reset(){
	%s
}

#define DRO_REAL(X) ((sizeof(_real)==4)? \"float\" : (sizeof(_real)==8)? \"double\" : \"unknown\")
#define DRO_INT(X) \"int\"
#define DRO_BOOL(X) \"bool\"
#define DRO_EXTERN(X) \"X\"

struct dro_var_t dro_intab[] = {
"
          name name
          (if SocUtils.is_memory_less main_soc then "" else
             Printf.sprintf "%s_ctx_reset(& %s_ctx);" name name);
        let line2str (id, t) (id2, _t) =
          let t = (type_to_string_rif t) in
          let cap_t = String.uppercase_ascii t in
          Printf.sprintf "   {\"%s\", DRO_%s(%s), (void*)&%s_ctx.%s}" id2 cap_t t name id
        in
        putc (String.concat ",\n" (List.map2 line2str inputs_exp inputs_exp2));
        cfmt "\n};
/* dro: output pointers table */
struct dro_var_t dro_outab[] = {
";
        putc (String.concat ",\n" (List.map2 line2str outputs_exp outputs_exp2));
        cfmt "\n};
/* dro: dynamic linking infos table */
struct dro_desc_t DRO_DESC_NAME = {
   DROVERSION, /* expected dro version */
   \"%s\", /* appli name */
   %d, dro_intab, /* nb inputs + table */
   %d, dro_outab, /* nb outputs + table */
   ze_step, /* step proc */
   ze_reset, /* reset proc */
   ze_reset /* init proc = reset proc */
};
" name (List.length inputs_exp) (List.length outputs_exp);
      ); (* end dro stuff *)
    
    puth "/////////////////////////////////////////////////\n";
    if assign_ext_types_list <> [] then (
      output_string och "\n// Defining array and extern types assignments \n";
      if Lv6MainArgs.global_opt.Lv6MainArgs.gen_wcet then 
        List.iter (fun t -> output_string och (Soc2cGenAssign.f_forloop t))
                  assign_ext_types_list
      else
        List.iter (fun t -> output_string och (Soc2cGenAssign.f t))
                  assign_ext_types_list
    );
    puth "#endif\n";
    flush occ; close_out occ;
    flush och; close_out och;
    if not Lv6MainArgs.global_opt.Lv6MainArgs.soc2c_dro then (flush ocl; close_out ocl);
    flush consts_h_oc; close_out consts_h_oc;
    flush consts_c_oc; close_out consts_c_oc;
    if not Lv6MainArgs.global_opt.Lv6MainArgs.soc2c_dro then
         Printf.eprintf "W: %s has been generated.\n" loopfile;
    Printf.eprintf "W: %s has been generated.\n" hfile;
    Printf.eprintf "W: %s has been generated.\n" cfile;
    flush stderr;
    let node =
      if args.Lv6MainArgs.main_node <> "" then args.Lv6MainArgs.main_node else
        Filename.basename (Filename.chop_extension (List.hd args.Lv6MainArgs.infiles))
    in
    let execfile = if args.Lv6MainArgs.outfile = "" then (node^".exec")
      else args.Lv6MainArgs.outfile 
    in
    let cflags = try Sys.getenv "CFLAGS" with Not_found -> "" in
    let ocsh = open_out (Filename.concat dir (node ^".sh")) in
    let main_file, ogensim_main_file, gcc =
      if Lv6MainArgs.global_opt.Lv6MainArgs.gen_wcet then 
        base^"_main.c",base^"_loop.c","$gcc --specs=linux.specs -g" 
      else 
        loopfile, "I am a dead string...", "gcc"
    in
    let ogensim_exe = node^"4ogensim.exec" in
    let cfiles_acc = if needs_cfile then ext_cfile::cfiles_acc else cfiles_acc in
    let cfiles = String.concat " " cfiles_acc in
    let gcc, gcc_ogensim =
      if Lv6MainArgs.global_opt.Lv6MainArgs.soc2c_dro then (
        Printf.sprintf "%s -shared -o %s.dro \\\n\t%s %s" gcc node cfiles cflags,
        "dead string"
      ) else
        Printf.sprintf "%s -o %s \\\n\t%s %s %s"
                       gcc execfile cfiles cflags main_file,
        Printf.sprintf "%s -o %s \\\n\t%s %s %s"
                       gcc ogensim_exe cfiles cflags ogensim_main_file
    in
    let main_step = (string_of_soc_key msoc)^"_step" in
    let gcc = if Lv6MainArgs.global_opt.Lv6MainArgs.gen_wcet then
        ("#!/bin/bash
set -x

otawa=\"true\"
ogensim=\"true\"
xpdf=\"true\"

OGENSIM=${OGENSIM:-\"osim.arm\"}
OTAWA=${OTAWA:-\"owcet.arm\"}
ORANGE=${ORANGE:-\"orange\"}
LUSTREV6=${LUSTREV6:-\"lv6\"}
fixffx=${fixffx:-\"fixffx\"}
mkff=${mkff:-\"mkff\"}
lutin=${lutin:-\"lutin\"}
rdbg=${rdbg:-\"rdbg-batch\"}
getstat=${getstat:-\"getstat.r\"}
gcc=${gcc:-\"arm-elf-gcc\"}


if [ $# -gt 0 ]
    then
    case \"$1\" in
        \"otawa\")
            otawa=\"true\"
            ogensim=\"false\"
            xpdf=\"false\"
            ;;
        \"ogensim\")
            otawa=\"false\"
            ogensim=\"true\"
            xpdf=\"false\"
            ;;
        \"both\")
            otawa=\"true\"
            ogensim=\"true\"
            xpdf=\"false\"
            ;;
    esac
fi
cfile="^cfile^ "
execfile="^execfile^ "
main_step="^main_step^ "
n="^node^ "
n_n="^base ^ "
freeport=`python -c 'import socket; s=socket.socket(); s.bind((\"\", 0)); print(s.getsockname()[1]); s.close()'`
if [ \"$otawa\" = \"true\" ]
then
# ZZZ otawa won't work with programs that use division because of orange (!?)
# Let's compile the c files for otawa\n" ^
          gcc ^ " > \\\n\t$n_n.owcet.log 2>&1 &&\n\n"^
            "$ORANGE $cfile ${n_n}_step -o $n_n.ffx > $n_n.orange.log  2>&1 &&\n" ^

"
IDIR=`readlink -f fixffx`
IDIR=`dirname \"$IDIR\"`
ARM_LOOPLOC=\"$IDIR/arm.looploc\"

$mkff -x $execfile > $n_n.ff
$fixffx $ARM_LOOPLOC -i $n_n.ff >  $n_n.fixed.ffx
# Let's  run otawa (owcet.arm)\n" ^
            "$OTAWA $execfile $main_step  -f $n_n.fixed.ffx -f $n_n.ffx --add-prop otawa::ilp::OUTPUT_PATH=$main_step.lp \\\n\t>"^
              "$n_n.owcet.arm.log 2>&1 && \n"^
              "grep WCET $n_n.owcet.arm.log | cut -d \"=\" -f 2 > "^
              "$n.wcet &&\n\n" ^ 
            "WCET=`cat $n.wcet` \n\n" ^ 
              "# Let's compile the c files for ogensim \n" ^
              "
fi
if [ \"$ogensim\" = \"true\" ]
then
# Now let's run ogensim \n(" ^
           gcc_ogensim ^ " >>$n_n.owcet.log  2>&1 &&\n"^
           "$LUSTREV6 "^(String.concat " " args.Lv6MainArgs.infiles)^" -n $n"^
           " -interface > $n.io &&\n"^
           "$LUSTREV6 "^(String.concat " " args.Lv6MainArgs.infiles)^" -n $n --gen-autotest &&\n\n"^
             "# Now let's run ogensim \n" ^
           "($OGENSIM "^ogensim_exe^" -ul 1 \\\n\t-e $main_step"^
           " -cl $n.cycles -lp $freeport \\\n\t-iol $n.io > $n_n"^
             ".ogensim.log  2>&1&) && \n\nsleep 1 &&\n"^
           "($rdbg -lurette -l 1000 -o "^node^".rif \\\n\t \
            --sut-socket \"127.0.0.1:$freeport\"  \\\n\t \
            --env-stdio \"$lutin -boot -rif _${n}_env.lut -n ${n}_env\" || true)) &&\n\n"^
             "$getstat $n.cycles $WCET > $n.stat
fi
if [ \"$xpdf\" = \"true\" ]
then
           xpdf $n.cycles.pdf &
fi
"
        )
      else gcc
    in
    output_string ocsh (gcc^"\n\n"); 
    flush ocsh; 
    close_out types_h_oc;
    close_out ocsh;
    let call_script = Printf.sprintf "sh %s.sh" (Filename.concat dir node) in
    let call_exec = "./"^node^".exec" in
    if args.Lv6MainArgs.launch_cc then (
      print_string ("sys call: '"^call_script^"'\n");
      if (Sys.command call_script)=0 then (
        if args.Lv6MainArgs.launch_exec then (
          print_string ("sys call: '"^call_exec^"'\n");
          if (Sys.command call_exec)=0 then (
          ) else
            print_string ("sys call: '"^call_exec^"' failed\n")
        )
      ) else
        print_string ("sys call: '"^call_script^"' failed\n")
    ) else
      print_string ("you can compile those files calling:  "^call_script^"\n");
    flush stdout
    with
      | Delete_C_files -> 
         close_out types_h_oc;
         close_out occ;
         close_out och;
         if not Lv6MainArgs.global_opt.Lv6MainArgs.soc2c_dro then close_out ocl;
         if Sys.file_exists hfile then Sys.remove hfile;
         if Sys.file_exists cfile then Sys.remove cfile;
         if Sys.file_exists ext_cfile then Sys.remove ext_cfile;
         if Sys.file_exists ext_hfile then Sys.remove ext_hfile;
         if Sys.file_exists loopfile then Sys.remove loopfile;
         exit 2

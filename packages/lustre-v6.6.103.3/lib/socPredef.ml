(* Time-stamp: <modified the 19/06/2020 (at 16:27) by Erwan Jahier> *)

(** Synchronous Object Code for Predefined operators. *)

open Soc
open Data

(* Some aliases *)
let b = Data.Bool
let _r = Data.Real

let aa t1 t2 =  ["i1", t1], ["out", t2]
let aaa t1 t2 t3 =  ["i1", t1; "i2",t2], ["out", t3]
let _aab t1 t2 =  ["i1", t1; "i2",t2], ["out", Bool]

(* if/then/else *)
let baaa t = ["cond", b; "xthen", t; "xelse", t], ["out", t]

let (soc_profile_of_types : Data.t list -> var list * var list) = 
  function
    | [t1; t2] -> aa t1 t2
    | [t1;t2;t3] -> aaa t1 t2 t3
    | [Bool;t1;_t2;_t3] -> baaa t1 
    | tl  -> 
      print_string ("Unsupported case: "^ (
        String.concat "," (List.map SocUtils.string_of_type_ref tl)));
      flush stdout;
      assert false

(* For diese and nor *)  
let (soc_profile_of_types_nary : Data.t list -> var list * var list) = 
  fun vl ->
    match vl with
      | ta::Bool::[] ->  ["i1", ta], ["out",Bool]
      | _ ->
         let inputs = List.mapi (fun i t -> assert(t=Bool); "i"^(string_of_int i),t) vl in
         List.tl inputs, ["out",Bool]
        

let step11 lxm _str = { (* a useful alias again *)
  name    = "step";
  lxm     = lxm;
  idx_ins  = [0];
  idx_outs = [0];
  impl    = Predef;
}
let step21 lxm _impl _str = { (* a useful alias again *)
  name    = "step";
  lxm     = lxm;
  idx_ins  = [0;1];
  idx_outs = [0];
  impl    = Predef;
}

(* used to build predef soc with no memory *)
let make_soc key profile steps =  {
      key      = key;
      profile  = profile;
      clock_profile = [];
      instances = [];
(*       init     = None; *)
      precedences  = [];
      step     = steps;
      memory   = No_mem;
      assertions = [];
    }

let (get_mem_name : Soc.key -> Data.t -> string) =
  fun (_k,_tl,_) _vt -> 
    "_memory"
(*
    match Str.split (Str.regexp "::") k with        
      | ["Lustre";op] -> (
        match op.[0] with
          | 'i' | 'b' | 'r' -> String.set op 0 '_'; ("mem"^op)
          | _ ->  "mem_"^op
      )
      | _ ->  "mem_"^k
*)

(* let dummy = Lxm.dummy "predef" *)

let of_fby_soc_key :Lxm.t ->  Soc.var_expr -> Soc.key -> Soc.t = 
  fun lxm _init sk -> 
    let _,tl,_ = sk in
    let t = List.hd tl in
    let pre_mem:var = (get_mem_name sk t, t) in
    let prof = soc_profile_of_types tl in
    let _v1,v2,vout = 
      match prof with ([v1;v2],[vout]) -> v1,v2,vout | _ -> assert false 
    in
    {
      key      = sk;
      profile  = prof;
      clock_profile = [];
      instances = [];
      memory    = Mem t; (* so that pre_mem exist *)
      step  = [  
(* faire qque chose de init maintenant !!! *)
        {
          name    = "get";
          lxm     = lxm;
          idx_ins  = [];
          idx_outs = [0];
          impl    = Gaol([pre_mem],[Call([Var(vout)], Assign, [Var(pre_mem)], lxm
                                         )]);
        (* impl    = Gaol([pre_mem],[ *)
        (* Case("$first_step", (["t", [Call([Var(vout)], Assign, [Var(v1)])]; *)
        (* "f", [Call([Var_Expr_Is_Not_A_Slice(vout)], Assign, [Var(pre_mem)])]])) *)
        (*           ]); *)
        };
        {
          name    = "set";  
          lxm     = lxm;
          idx_ins  = [1];
          idx_outs = [];
          impl    = Gaol([pre_mem],[Call([Var(pre_mem)], Assign, [Var(v2)], lxm)]);
        };
      ];
      precedences = ["set", ["get"]];
      assertions = [];
    }
    
(* exported *)
let of_soc_key : Lxm.t -> Soc.key -> Soc.t = 
  fun lxm sk -> 
  let (id, tl, _) = sk in
  let sp = soc_profile_of_types in
  let sp_nary = soc_profile_of_types_nary in
  match id with
  | "Lustre::ruminus" 
    | "Lustre::iuminus" 
    | "Lustre::uminus" -> (make_soc sk (sp tl) [step11 lxm id])
  | "Lustre::not" -> (make_soc sk (sp tl) [step11 lxm id])
  | "Lustre::real2int" -> (make_soc sk (sp tl) [step11 lxm id])
  | "Lustre::int2real" -> (make_soc sk (sp tl) [step11 lxm id])

  | "Lustre::mod" -> (make_soc sk (sp tl) [step21 lxm None id])
  | "Lustre::iplus"  
    | "Lustre::rplus"  
    | "Lustre::plus"  -> (make_soc sk (sp tl) [step21 lxm None id])      
  | "Lustre::times"
    | "Lustre::itimes"
    | "Lustre::rtimes" ->  (make_soc sk (sp tl) [step21 lxm None id])
  | "Lustre::slash" 
    | "Lustre::islash" 
    | "Lustre::rslash" -> (make_soc sk (sp tl) [step21 lxm None id])
  | "Lustre::div"
    | "Lustre::idiv"
    | "Lustre::rdiv" -> (make_soc sk (sp tl) [step21 lxm None id])
  | "Lustre::minus" 
    | "Lustre::iminus" 
    | "Lustre::rminus" -> (make_soc sk (sp tl) [step21 lxm None id])

  | "Lustre::lt"  -> (make_soc sk (sp tl) [step21 lxm None id])
  | "Lustre::gt"  -> (make_soc sk (sp tl) [step21 lxm None id])
  | "Lustre::lte" -> (make_soc sk (sp tl) [step21 lxm None id])
  | "Lustre::gte" -> (make_soc sk (sp tl) [step21 lxm None id])
  | "Lustre::ilt"  -> (make_soc sk (sp tl) [step21 lxm None id])
  | "Lustre::igt"  -> (make_soc sk (sp tl) [step21 lxm None id])
  | "Lustre::ilte" -> (make_soc sk (sp tl) [step21 lxm None id])
  | "Lustre::igte" -> (make_soc sk (sp tl) [step21 lxm None id])
  | "Lustre::rlt"  -> (make_soc sk (sp tl) [step21 lxm None id])
  | "Lustre::rgt"  -> (make_soc sk (sp tl) [step21 lxm None id])
  | "Lustre::rlte" -> (make_soc sk (sp tl) [step21 lxm None id])
  | "Lustre::rgte" -> (make_soc sk (sp tl) [step21 lxm None id])

  | "Lustre::and" -> (make_soc sk (sp tl) [step21 lxm None id])
  | "Lustre::eq" -> (make_soc sk (sp tl) [step21 lxm None id])
  | "Lustre::neq" -> (make_soc sk (sp tl) [step21 lxm None id])
  | "Lustre::or"  -> (make_soc sk (sp tl) [step21 lxm None id])
  | "Lustre::xor" -> (make_soc sk (sp tl) [step21 lxm None id])
  | "Lustre::impl" -> (make_soc sk (sp tl) [step21 lxm None id])
                    
  (* Those have instances *)
  | "Lustre::current" -> (
    match sk with 
    | _,tl, Curr(cc) -> (
      assert(tl<>[]);
      let t = List.hd (List.tl tl) in 
      let mem:var = (get_mem_name sk t, t) in 
      let prof:var list * var list = sp tl in 
      let cv,vin,vout = 
        match prof with ([cv;vin],[vout]) -> cv,vin,vout | _ -> assert false 
      in 
      { 
        key      = sk; 
        profile  = (sp tl); 
        clock_profile = [];
        instances = []; 
        memory   = Mem (t); 
        step  = [ 
            { 
              name    = "step";   
              lxm     = lxm;
              idx_ins  = [0;1]; 
              idx_outs = [0]; 
              impl    = 
                Gaol([], 
                     [Case((fst cv),[
                             (Lv6Id.string_of_long false cc, [Call([Var(mem)],
                                                                   Assign, [Var(vin)],
                                                                   lxm)])],
                           lxm);
                      Call([Var(vout)], Assign, [Var(mem)], lxm )]) 
            }; 
          ]; 
        precedences = []; 
        assertions = [];
      }
    )
    | _,_tl, Nomore  -> (* current is applied to something on the base clock *) 
       raise (Lv6errors.Compile_error(lxm, "current applied on the base clock"))
    | _,_, _  -> assert false
  )
  | "Lustre::pre" ->  
     let _,tl,_ = sk in
     let t = List.hd tl in
     let pre_mem:var = (get_mem_name sk t, t) in
     let prof = sp tl in
     let v1,vout = match prof with ([v1],[vout]) -> v1,vout | _ -> assert false in
     {
       key      = sk;
       profile  = (sp tl);
       clock_profile = [];
       instances = [];
       memory    = Mem (t); (* so that pre_mem exist *)
       step  = [
           {
             name    = "get";
             lxm     = lxm;
             idx_ins  = [];
             idx_outs = [0];
             (*               impl    = Predef; *)
             impl    = Gaol([],[Call([Var(vout)], Assign, [Var(pre_mem)], lxm)]); 
             (*impl    = Gaol([pre_mem],[Call([Var(vout)], Assign, [Var(pre_mem)])]); *)
           };
           {
             name    = "set";  
             lxm     = lxm;
             idx_ins  = [0];
             idx_outs = [];
             (*               impl    = Predef; *)
             impl    = Gaol([],[Call([Var(pre_mem)], Assign, [Var(v1)], lxm )]); 
             (* impl    = Gaol([pre_mem],[Call([Var(pre_mem)], Assign, [Var(v1)])]); *)
           };
         ];
       precedences = ["set", ["get"]];
       assertions = [];
     }
  | "Lustre::arrow"  ->  
     let prof = sp tl in
     {
       key      = sk;
       profile  = prof;
       clock_profile = [];
       instances = [];
       step  = [
           {
             name    = "step";
             lxm     = lxm;
             idx_ins  = [0;1];
             idx_outs = [0];
             impl    = Predef;
           }
         ];
       precedences   = [];
       assertions = [];
       memory = Mem Bool;
     }
  | "Lustre::if"  ->  {
      key      = sk;
      profile  = (sp tl);
      clock_profile = [];
      instances = [];
      (*         init     = None; *)
      precedences  = [];
      assertions = [];
      memory = No_mem;
      step   = [
          {
            name    = "step";
            lxm     = lxm;
            idx_ins  = [0; 1; 2];
            idx_outs = [0];
            impl    = Predef;
        }        ];
    } 
  | "Lustre::nor"  -> 
     let size = match sk with
       | _,[Array(Bool,size);_],_  -> size
       | _,bools,_  ->
          if List.exists (fun t -> t<>Bool) bools then
            failwith "type error in nor"
          else
            List.length bools 
     in
     {
       Soc.key       = sk;
       Soc.profile   = sp_nary tl;
       Soc.clock_profile = [];
       Soc.instances = [] ;
       Soc.step      = [
           {
             name     = "step";
             lxm      = lxm;
             idx_ins  = [0];
             idx_outs = [0];
             impl     = Boolred(0, 0, size);
           }
         ];
       Soc.memory    = No_mem;
       Soc.precedences = [];
       Soc.assertions = [];
     } 
  | "Lustre::diese"  ->
     let size = match sk with
       | _,[Array(Bool,size);_],_  -> size
       | _,bools,_  ->
          if List.exists (fun t -> t<>Bool) bools then
            failwith "type error in #"
          else
            List.length bools 
     in
     {
       Soc.key       = sk;
       Soc.profile   = sp_nary tl;
       Soc.clock_profile = [];
       Soc.instances = [] ;
       Soc.step      = [
           {
             name     = "step";
             lxm      = lxm;
             idx_ins  = [0];
             idx_outs = [0];
             impl     = Boolred(0,1, size);
           }
         ];
       Soc.memory    = No_mem;
       Soc.precedences = [];
       Soc.assertions = [];
     } 
  | _ -> 
     failwith ("*** The soc of "^id ^ " is not defined! \n")

    


(** Instancie un composant polymorphe avec un type concret. *)
let instanciate_soc: Soc.t -> Data.t -> Soc.t = 
  fun c concrete_type ->
    let rec instanciate_type vt =
      match vt with 
        | Alpha _ ->  concrete_type  
        | Struct(sn, fl) -> 
          Struct(sn, List.map (fun (id,vt) -> (id,instanciate_type vt)) fl)
        | Array(vt,i) -> Array(instanciate_type vt,i)
        | vt -> vt
    in
    let new_profile =
      List.map (fun (vn,vt) -> vn, instanciate_type vt) (fst c.profile),
      List.map (fun (vn,vt) -> vn, instanciate_type vt) (snd c.profile)
    in
    let instanciate_key  (key1, key2, key3) = 
      (key1, List.map instanciate_type key2, key3) 
    in
    let new_key = instanciate_key c.key in 
    let new_instances = 
      List.map (fun (id,sk) -> (id,instanciate_key sk)) c.instances
    in
    { 
      c with 
        key = new_key;
        profile = new_profile;
        instances = new_instances;
    }


(*
  XXX Faut-il definir une version générique des composants tranches ? 

  Je les ai défini directement via "make_slice_soc", ce qui
  n'est pas homogene avec la facon dont sont traités les autres
  composants génériques style 'fby'.

  Le truc, c'est que je ne sais pas trop quoi mettre dans la version
  générique, et comme celle-ci est destinée à être instanciée... En
  effet, le type de sortie des composants tranche depend de la
  slice_info passé en parametre lors de l'instanciation des composant
  génériques. Je pourrais mettre un type alpha, mais je trouve ca
  idiot, alors je ne le fais pas...

  Une autre solution pour rendre ce traitement homogene serait de ne
  pas passer par une version générique pour les composants fby et
  consort. A voir.

  idem pour "x^n" (Hat_n).
*)

let make_array_slice_soc : Lxm.t -> Lic.slice_info -> int -> Data.t -> Soc.t = 
  fun lxm si s t -> 
    let size = si.Lic.se_width in
    let array_type_in = Array(t,s) in
    let array_type_out = Array(t,size) in
    let key_prof = [array_type_in; array_type_out] in
    {
      key = ("Lustre::array_slice", key_prof, 
             Slic(si.Lic.se_first,si.Lic.se_last,si.Lic.se_step));
      profile  = (["i1", array_type_in], ["out", array_type_out]);
      clock_profile = [];
      instances = [];
      step  = [
        {
          name    = "step";
          lxm     = lxm;
          idx_ins  = [0];
          idx_outs = [0];
          impl    = Predef;
        };
      ];
      precedences = [];
      assertions = [];
      memory = No_mem;
    } 

let make_array_soc: Lxm.t -> int -> Data.t -> Soc.t = 
  fun lxm i t -> 
    let iprof = 
      let res = ref [] in
      for k=i downto 1 do
        res:= ("i"^(string_of_int k),t) :: !res;
      done;
      !res
    in
    let array_type = Array(t,i) in
    let key_prof = (List.map snd iprof) @ [array_type] in
      {
        key = ("Lustre::array", key_prof, Nomore);
        profile  = (iprof, ["out", array_type]);
        clock_profile = [];
        instances = [];
        step  = [
          {
            name     = "step";
            lxm      = lxm;
            idx_ins  = SocUtils.gen_index_list i;
            idx_outs = [0];
            impl     = Predef;
          };
        ];
        precedences = [];
        assertions = [];
        memory = No_mem;
      } 


let make_array_concat_soc: Lxm.t -> int -> int -> Data.t -> Soc.t = 
  fun lxm s1 s2 t -> 
    let iprof = (["i1", Array(t,s1); "i2",  Array(t,s2)], ["out", Array(t,s1+s2)])in
    let key_prof = [Array(t,s1); Array(t,s2); Array(t,s1+s2)] in
    {
      key = ("Lustre::concat", key_prof, Nomore);
      profile  = iprof;
      clock_profile = [];
      instances = [];
      step  = [
        {
          name    = "step";
          lxm     = lxm;
          idx_ins  = [0;1];
          idx_outs = [0];
          impl    = Predef;
        };
      ];
      precedences   = [];
      assertions = [];
      memory = No_mem;
    } 

let make_hat_soc: Lxm.t -> int -> Data.t -> Soc.t = 
  fun lxm i t -> 
    let array_type = 
      match t with
        | Data.Alpha _ -> assert false 
        | t -> Data.Array(t,i)
    in
    {
      key = ("Lustre::hat", [t;array_type], Nomore);
      profile  = ([("i1", t)], ["out", array_type]);
      clock_profile = [];
      instances = [];
      step  = [
        {
          name    = "step";
          lxm     = lxm ;
          idx_ins  = [0];
          idx_outs = [0];
          impl    = Predef;
        };
      ];
      precedences   = [];
      assertions = [];
      memory = No_mem;
    } 

let output_type_of_op op tl =
  match op with (* beurk, pas bô *)
    | "Lustre::eq"
    | "Lustre::neq"
    | "Lustre::lt" | "Lustre::rlt"  | "Lustre::ilt"
    | "Lustre::gt" | "Lustre::rgt"  | "Lustre::igt"
    | "Lustre::lte"| "Lustre::rlte" | "Lustre::ilte"
    | "Lustre::gte"| "Lustre::rgte" | "Lustre::igte"
    | "Lustre::nor"
    | "Lustre::diese"
      -> Bool
    | "Lustre::real2int" -> Int
    | "Lustre::int2real" -> Real
    | "Lustre::if" -> assert(tl<>[]);List.hd (List.tl tl)
    | "Lustre::hat" -> assert false (* sno? *)
    | "Lustre::array" -> assert false (* sno? *)
    | "Lustre::concat" -> assert false (* sno? *)
    | "Lustre::arrow" -> assert false (* sno? *)
    | "Lustre::current" -> assert false
    | "Lustre::array_slice" -> assert false (* sno? *)

    | _ ->
       (* in all other cases, the outpout type is the same as the first input (really?)*)
       List.hd tl

let (soc_interface_of_pos_op: 
       Lxm.t -> Lic.by_pos_op -> Data.t list -> Soc.var_expr option -> Soc.t) =
  fun lxm op types fby_init_opt ->
  match (op, types,fby_init_opt) with

  | Lic.PREDEF_CALL ({Lxm.it=("Lustre","if"),[];_}),_ ,_  ->
     let concrete_type = List.nth types 1 in
     let soc = of_soc_key lxm ("Lustre::if", types@[concrete_type], Nomore) in
     instanciate_soc soc concrete_type
  | Lic.PREDEF_CALL {Lxm.it=(op,sargs);_}, _, _ ->
     assert (sargs=[]);
     let soc_name = Lv6Id.string_of_long false op in
     let out_type = output_type_of_op soc_name types in
     let soc = of_soc_key lxm (soc_name, types@[out_type], Nomore) in
     soc
  | Lic.FBY, _, Some init ->
     let concrete_type = List.nth types 0 in
     let soc = of_fby_soc_key lxm init (("Lustre::fby"), 
                                    types@[concrete_type], MemInit init) in
     instanciate_soc soc concrete_type
  | Lic.FBY, _, None -> assert false (* should ot occur *)
  | Lic.PRE, _, _ ->
     let concrete_type = List.nth types 0 in
     let soc = of_soc_key lxm (("Lustre::pre"), types@[concrete_type], Nomore) in
     instanciate_soc soc concrete_type
  | Lic.CURRENT (Some(cc)), _, _ ->
     let concrete_type = try List.nth types 1 with _ -> assert false in
     let soc = of_soc_key lxm (("Lustre::current"), types@[concrete_type], Curr(cc)) in
     instanciate_soc soc concrete_type
  | Lic.CURRENT None, _, _ ->
     let concrete_type = try List.nth types 0 with _ -> assert false in
     let soc = of_soc_key lxm (("Lustre::current"), types@[concrete_type], Nomore) in
     instanciate_soc soc concrete_type
  | Lic.ARROW, _, _ ->
     let concrete_type = List.nth types 0 in
     let soc = of_soc_key lxm (("Lustre::arrow"), types@[concrete_type], 
                           MemInit(Const("_true", Data.Bool))) 
     in
     let soc = instanciate_soc soc concrete_type in
     soc
  | Lic.HAT i,_, _ ->
     let elt_type = List.nth types 0 in
     (make_hat_soc lxm i elt_type)

  | Lic.ARRAY, _, _ ->
     let elt_type = List.nth types 0 in
     let i = (List.length types) in
     (make_array_soc lxm i elt_type)

  | Lic.ARRAY_SLICE sinfo, [Array (t, s)], _ -> (make_array_slice_soc lxm sinfo s t)
  | Lic.ARRAY_SLICE _sinfo, _, _ -> assert false

  | Lic.CONCAT, [Array (t1, s1); Array (t2, s2)], _->  
     assert (t1=t2);
     (make_array_concat_soc lxm s1 s2 t1)
  | Lic.CONCAT ,  _, _ ->  assert false

  | Lic.CALL _,_,_ ->  assert false 
  | Lic.CONST _ ,  _,_ ->  assert false

  (* Those are not soc *)
  | Lic.VAR_REF _, _,_ -> assert false
  | Lic.CONST_REF _, _,_ -> assert false
  | Lic.STRUCT_ACCESS _, _,_ -> assert false
  | Lic.WHEN _, _,_ -> assert false
  | Lic.TUPLE, _,_ -> assert false
  | Lic.ARRAY_ACCES _, _,_ -> assert false


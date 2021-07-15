(* Time-stamp: <modified the 15/03/2021 (at 15:44) by Erwan Jahier> *)

let dbg = (Lv6Verbose.get_flag "exec")

open Soc
open Data

(* Meant to represent paths in the call tree. Actually it both
   represent path and variable with a path, depending on the
   context *)
type path = ident list

let (path_to_string: ident list -> string) =
  fun l -> 
    String.concat "->" (List.rev l)

type subst = (path * Data.v)

(* soc environnement holding memory values.

   Each soc have a list of memories, which is actually a list of soc
   instances (in the licc terminology). 

   Hence we need a kind of mapping from instance list (the path in the
   call tree) to values.
*)
(* type substs = subst list *)
type substs =
  | Node of (ident * substs) list
  | Leaf of Data.v

type ctx = { 
  cpath: path; 
  s:substs;
}

(**************************************************************************)
let rec (get_top_var_type : Soc.var_expr -> Data.t) =
  fun ve -> 
    match ve with
      | Var(_,vt)  -> vt
      | Index(ve,_,_)  
      | Field(ve, _, _) | Slice(ve,_,_,_,_,_) -> get_top_var_type ve
      | Const(_id,_) -> assert false


open Data
let (get_access : Soc.var_expr -> Data.access list) =
  fun ve -> 
    let rec aux ve =
      match ve with
        | Const(_id,_) -> assert false
        | Var(_id,_)  -> []
        | Index(ve,i,_)  -> (Idx i)::(aux ve)
        | Field(ve, n,_)  -> (Fld n)::(aux ve)
        | Slice(ve,f,l,s,w,_) -> (Sle (f,l,s,w))::(aux ve)
    in 
    List.rev (aux ve)
        
let (update_leaf : var_expr -> v -> v -> substs) =
  fun ve v pre_v -> 
    let access = get_access ve in
    let new_v = update_val pre_v v access in
    Leaf(new_v)

let (create_leaf : var_expr -> v -> substs) =
  fun ve v -> 
    let access = get_access ve in
    let top_vt = get_top_var_type ve in
    let new_v = create_val top_vt v access in
    Leaf(new_v)

let rec (get_top_id : Soc.var_expr -> ident) =
  function
  | Var(id,_) | Const(id,_) -> id
  | Field(ve, _, _) | Index(ve,_,_) | Slice(ve,_,_,_,_,_) -> get_top_id ve

(* exported *)
let (sadd_partial : substs  -> var_expr -> path -> Data.v -> substs) =
  fun ct ve x v ->
    let top_id = get_top_id ve in
    let x = top_id::x in
    let rec aux ct (x,v) =
    match ct,x with
      | Leaf(pre_v),[] -> update_leaf ve v pre_v
      | Node(l),n::t -> (
        try 
          let s = aux (List.assoc n l) (t,v) in
          Node((n,s)::(List.remove_assoc n l))
        with Not_found -> 
          if t = [] then Node((n, create_leaf ve v)::l)
          else
          let new_substs = aux (Node []) (t,v) in
          Node((n,new_substs)::l)
      )
      | _,[] -> assert false
      | Leaf(_),_ -> assert false
    in
    let res = aux ct (List.rev x,v) in
    res

(* let rec (sadd : substs -> subst -> substs) =  *)
(*   fun ct (x,v) -> *)
(*     (x,v)::(List.remove_assoc x ct) *)

(* exported *)
let (sadd : substs -> path -> Data.v -> substs) = 
  fun ct x v ->
    let rec aux ct (x,v) =
    match ct,x with
      | Leaf(_),[] -> Leaf(v)
      | Node(l),n::t -> (
        try 
          let s = aux (List.assoc n l) (t,v) in
          Node((n,s)::(List.remove_assoc n l))
        with Not_found -> 
          if t = [] then Node((n,Leaf v)::l)
          else
          let new_substs = aux (Node []) (t,v) in
          Node((n,new_substs)::l)
      )
      | _,[] -> assert false
      | Leaf(_),_ -> assert false
    in
    aux ct (List.rev x,v)

(**************************************************************************)
(* let filter_top_subst s = *)
(*   List.fold_left *)
(*     (fun acc (idl,v) ->  *)
(*         match idl with  *)
(*           | [id] -> (id,v)::acc  *)
(*           | _ -> acc  *)
(*     ) *)
(*     [] *)
(*     s *)


let (string_of_subst_list : (path * Data.v) list -> string) =
  fun  s -> 
    let values = List.map
      (fun (var,value) -> (path_to_string var)^"="^
        (Data.val_to_string string_of_float value)) s 
    in
     ((String.concat "\n\t" values) ^ "\n")

let (dump_subst_list : (path * Data.v) list -> unit) =
  fun s -> 
    print_string (string_of_subst_list s);
    flush stdout

(* let (substs_to_list: substs -> (path * t) list) = *)
(*   fun s -> s *)

let (substs_to_list: substs -> (path * Data.v) list) =
  fun s -> 
    let rec aux acc s p =
      match s with 
        | Node(l) -> List.fold_left (fun acc (id,s) -> aux acc s (id::p)) acc l
        | Leaf(v) -> (p,v)::acc
    in
    aux [] s []

let (substs_to_data_subst : substs -> Data.subst list) = 
  fun s -> 
    let l = substs_to_list s in
    List.map (fun (p,v) -> path_to_string p,v) l

let (string_of_substs : substs -> string) =
  fun s -> 
    string_of_subst_list (substs_to_list s)

let (dump_substs : substs -> unit) =
  fun s -> 
    dump_subst_list (substs_to_list s)

(* XXX use a Rif reader *)
let rec (read_enum : ident list -> ident) = 
  fun idl -> 
    print_string ("Enter " ^ (String.concat "," idl)); flush stdout; 
    let str = read_line () in
    if List.mem str idl then str else (print_string "Bad enum; "; read_enum idl)

let (read_value : var -> Data.v) =
  fun (_,t) -> 
    match t with
      | Bool -> print_string "Enter a bool (t/f):";flush stdout; B(read_line () = "t") 
      | Int  -> print_string "Enter an int:"; flush stdout; I(read_int())
      | Real -> print_string "Enter a float:"; flush stdout; F(read_float()) 
      | Enum(_,idl)  -> 
        let e = read_enum(idl) in
        let i = Lv6util.pos_in_list 0 e idl in
          E(e,i)
      | _ -> assert false (* finish me! *)

(* let (get_val : ident -> ctx -> t) = *)
(*   fun id ctx ->  *)
(*     try List.assoc (id::ctx.cpath) ctx.s *)
(*     with Not_found ->  *)
(*       let msg = (path_to_string (id::ctx.cpath)) ^ " unbound in "  *)
(*         ^ (string_of_substs ctx.s)  *)
(*       in *)
(*       print_string msg; flush stdout; *)
(*       assert false *)

let (filter_top_subst : substs -> Data.subst list) =  
  fun s -> 
    let aux acc (id, s) =
      match s with
        | Leaf(v) -> (id,v)::acc
        | _ -> acc
    in
    match s with 
        Node(l) -> List.fold_left aux [] l
      | _ -> assert false


(* returns the set of values in ctx *)
let (get_vals : ctx -> Data.subst list) =
  fun ctx ->
    let rec aux s path =
      match s, path with
        | Node(l),n::t -> aux (List.assoc n l) t
        | Node(l),[] ->
          List.fold_left
            (fun acc (id,s) ->
              match s with
              | Leaf(v) -> (id,v)::acc
              | _ -> acc
            )
            [] l
        | _,_ -> [] (* sno? *)
    in
    aux ctx.s (List.rev (ctx.cpath))
    
let (get_val : ident -> ctx -> Data.v) =
  fun id ctx -> 
    let rec find ct p =
      match ct,p with
        | Node(l),n::t -> find (List.assoc n l) t
        | Leaf(v),[] -> v
        | _,[] -> raise Not_found
        | Leaf(_),_ -> assert false
    in
    try find ctx.s (List.rev (id::ctx.cpath))
    with Not_found -> 
      Lv6Verbose.exe ~flag:dbg (fun () ->
        let msg = "Warning " ^(path_to_string (id::ctx.cpath)) ^ " unbound in \n" 
          ^ (string_of_substs ctx.s) 
        in
        print_string msg; flush stdout);
      U

let (get_enum : ident -> ctx -> ident) =
fun id ctx -> 
  match get_val id ctx with
    | E(e,_) -> e
    | B true -> "Lustre::true"
    | B false -> "Lustre::false"
    | U  ->
(*       print_string (id ^ " undefined\n"); flush stdout; *)
      raise Not_found
    | o -> 
      print_string ("get_enum '" ^ (val_to_string string_of_float o) ^"' failed\n"); flush stdout;
      assert false (* should not fail *) 



let rec (get_value : ctx -> var_expr -> Data.v) =
  fun ctx v -> 
    match v with
      | Var(id,_) -> get_val id ctx
      | Const(("true"|"t"|"_true"),  Bool) -> B true
      | Const(("false"|"f"|"_false"), Bool) -> B false
      | Const(id_in,Int)  -> I (Lv6util.my_int_of_string id_in)
      | Const(id_in,Real) -> F (float_of_string id_in)
      | Const(id,Enum(_,idl)) -> E(id, Lv6util.pos_in_list 0 id idl)
      | Const(id,Array(_vt,_i)) -> get_val id ctx
      | Const(id,Struct _) -> get_val id ctx
      | Const(_id, String) -> assert false
      | Const(_id, Bool) -> assert false
      | Const(_id, Extern _) -> assert false
      | Const(_id, Alias _) -> assert false
      | Const(_id,Alpha _) -> assert false (* todo *) 
      | Field(ve,fn,_t) -> 
        let s = get_value ctx ve in
        (match s with
         | U -> U
         | S fl -> (
             try List.assoc fn fl
             with Not_found -> assert false (* should not occur *)
          )
          | _ ->
            let msg = Printf.sprintf "'%s' is not a struct\n"
                (SocUtils.string_of_filter ve)
            in
            failwith msg
            (*             assert false (* should not occur *) *)
        )
      | Index(ve,i,_vt) -> (
        let a = get_value ctx ve in
        match a with
          | A a -> a.(i)
          | U  -> U
          (*             dump_substs ctx.s;flush stdout; *)
          (*             failwith ((SocUtils.string_of_filter ve) ^ " not defined\n") *)
          | _ -> assert false (* should not occur *)
      )
      | Soc.Slice(ve,f,l,s,w,_vt) -> 
        let a = get_value ctx ve in
        match a with
          | A a ->
            let slice = Array.sub a f w in
            let j = ref 0 in
            for i = f to l do
              if (i - f) mod s = 0 then (
                slice.(!j) <- a.(i);
                incr j
              )
            done;
            A slice
          | U  -> U
          | _ -> assert false (* should not occur *)


(* type substs = subst list *)
(* let (sadd : substs -> subst -> substs) =  *)
(*   fun ss (x,v) ->  *)
(*     let ss = List.remove_assoc x ss in *)
(*     (x,v)::ss *)

(* let (string_of_substs :substs -> string) =  *)
(*   fun s ->  *)
(*     let str_l =  *)
(*       List.map (fun (var,value) -> (path_to_string var) ^ "=" ^ (to_string value)) s  *)
(*     in *)
(*     "{\n\t"^ (String.concat "\n\t" str_l) ^ " }" *)
(*  *)
(*  *)
(* let (get_val : ident -> ctx -> t) = *)
(*   fun id ctx ->  *)
(*     try List.assoc (id::ctx.cpath) ctx.s *)
(*     with Not_found ->  *)
(*       let msg = (path_to_string (id::ctx.cpath)) ^ " unbound in " ^ (string_of_substs ctx.s) in *)
(*       print_string msg; flush stdout; *)
(*       assert false *)

(* exported *)
let (substitute_args_and_params : var_expr list -> var list -> ctx -> substs) =
  fun args params ctx -> 
    assert (List.length args = List.length params);
    let arg_ctx =
      try { ctx with cpath = List.tl ctx.cpath } with _ -> assert false
    in
    let s = 
      assert (List.length args = List.length params);
      List.fold_left2
      (fun acc arg (pn,_) -> sadd acc (pn::ctx.cpath) (get_value arg_ctx arg))
      ctx.s args params 
    in
    s

let (substitute_params_and_args : var list -> var_expr list -> ctx -> substs) = 
  fun params args ctx -> 
    assert (List.length args = List.length params);
    let s =
      assert (List.length args = List.length params);
      List.fold_left2
      (fun acc arg (pn,_) -> 
        let path = try List.tl ctx.cpath with _ -> assert false in
        let v = get_val pn ctx in
        sadd_partial acc arg path v
      )
      ctx.s args params
    in
    s

let empty_ctx: ctx = {
  cpath = [];
  s     = Node [];
}

(* exported *)
let (create_ctx : Soc.tbl -> Soc.t -> ctx) =
  fun soc_tbl soc -> 
    let rec (init_soc: Soc.t -> ident list -> substs -> substs) =
      fun soc cpath mem ->
        let mem =
          match soc.memory, soc.key with
            | Mem(vt), (_,_,MemInit dft_value) -> (
              let name = (SocPredef.get_mem_name soc.key vt)::cpath in
              let value = get_value empty_ctx dft_value in
              sadd mem name value
            )
            | Mem(vt), _ -> (
              let name = (SocPredef.get_mem_name soc.key vt)::cpath in
              let value = U in
              sadd mem name value
            )
            | No_mem,_ -> mem
            | Mem_hidden,_ -> mem
        in
        List.fold_left (init_instances cpath) mem soc.instances

    and (init_instances: ident list -> substs -> Soc.instance -> substs) =
      fun cpath mem (iname, sk) ->
        let soc = SocUtils.find_no_exc sk soc_tbl in
        init_soc soc (iname::cpath) mem
    in
    let substs = init_soc soc [] (Node []) in 
    {
      s = substs;
      cpath = [];
    }


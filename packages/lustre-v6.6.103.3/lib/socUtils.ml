(** Time-stamp: <modified the 29/08/2019 (at 16:48) by Erwan Jahier> *)


open Soc


(** Donne toute les méthodes d'un composant.

    C'est la liste des méthodes du composant, et la méthode d'initialisation le
    cas échéant. *)
let get_all_methods: Soc.t -> step_method list = fun c ->
  c.step
(* DELETE ME !! *)
(*   match c.init with *)
(*     | None   -> c.step *)
(*     | Some m -> m :: c.step *)

(** Fonctions de représentation des objets LOC. *)

(** Aliases *)
let _str_ff       = Format.str_formatter
let _flush_str_ff = Format.flush_str_formatter
let fprintf      = Format.fprintf

(** Encapsule l'appel à une fonction avec formatter pour sortir une string. *)
let call_fun_ff: ((Format.formatter -> unit) -> string) = fun f ->
  let b = Buffer.create 50 in
  let ff = Format.formatter_of_buffer b in
    f ff;
    Format.pp_print_flush ff ();
    let s = Buffer.contents b in
      Buffer.reset b;
      s

(* Type *)
let rec string_of_type_ref_ff: (Data.t -> Format.formatter -> unit) = fun v ff -> 
  let str = Data.type_to_string_alias v in
    fprintf ff "%s" str 

and string_of_type_ref: (Data.t -> string) = fun v ->
  call_fun_ff (string_of_type_ref_ff v)



(* Variable *)
let string_of_var_ff: (Soc.var -> Format.formatter -> unit) = fun (id, type_) ff ->
  fprintf ff "%s: %s" id (string_of_type_ref type_)

let string_of_var: (Soc.var -> string) = fun v ->
  call_fun_ff (string_of_var_ff v)


let string_of_instance_ff: (instance -> Format.formatter -> unit) = 
  fun (name,_sk) ff ->
    fprintf ff "%s" name

let _string_of_instance: (instance -> string) = fun (name,_sk) -> name
  

(* Filtre d'accès *)
let rec string_of_filter_ff: (Soc.var_expr -> Format.formatter -> unit) = 
  fun v ff -> match v with
    | Const(id, _) 
    | Var (id,_)   -> fprintf ff "%s" id
    | Field(f, id,_) -> string_of_filter_ff f ff; fprintf ff ".%s" id
    | Index(f, index,_) -> string_of_filter_ff f ff; fprintf ff "[%d]" index
    | Slice(f,fi,la,st,_wi,_vt) ->
       string_of_filter_ff f ff; fprintf ff "[%d..%d step %d]" fi la st 

let string_of_filter: (Soc.var_expr -> string) = fun v ->
  call_fun_ff (string_of_filter_ff v)


(* Clé de composant *)
let string_of_soc_key_ff: (Soc.key -> Format.formatter -> unit) = 
  fun (id, types, si_opt) ff ->
    (match types with
      | [] -> fprintf ff "%s" id
      | _  -> fprintf ff "%s:%s" id 
          (String.concat " -> " (List.map string_of_type_ref types)));
    (match si_opt with
       | Nomore -> ()
       | Curr(cc) -> fprintf ff "%s" (Lv6Id.string_of_long false cc)
       | Slic(f,l,step) -> fprintf ff "[%d .. %d step %d]"  f l step
       | MemInit ve -> string_of_filter_ff ve ff
    )
     

let string_of_soc_key: (Soc.key -> string) = fun v ->
  call_fun_ff (string_of_soc_key_ff v)

(* Opération *)
let string_of_operation_ff: (atomic_operation -> Format.formatter -> unit) = 
  fun v ff -> match v with
  | Assign          -> () (* On suppose qu'il est déjà affiché dans string_of_gao *)
  | Method((n, _sk),sname) -> fprintf ff "%s.%s" n sname
  | Procedure(proc)    -> fprintf ff "%s" (string_of_soc_key proc)

let string_of_operation: (atomic_operation -> string) = fun v ->
  call_fun_ff (string_of_operation_ff v)

(* Code *)
let rec string_of_gao_ff: (gao -> Format.formatter -> unit) = fun v ff -> match v with
  | Case (ck, cases,_) ->
      let string_of_case: (ident * gao list -> unit) = fun (id, c) ->
        fprintf ff "@[case %s:@[" id;
        string_of_gaos_list_ff c ff;
        fprintf ff "@]@]"
      in
        fprintf ff "switch(%s) {" ck;
        List.iter string_of_case cases;
        fprintf ff "}"

  | Call(dests, op, srcs, _) ->
      let _ =
        match dests with
          | [] -> () (* pas de destinations, on affiche pas de "=" *)
          | _  -> 
              let dests = String.concat ", " (List.map string_of_filter dests) in
                fprintf ff "%s = " dests
      in
      let srcs  = String.concat ", " (List.map string_of_filter srcs) in
        string_of_operation_ff op ff;
        fprintf ff "(%s)" srcs;

and string_of_gaos_list_ff: (gao list -> Format.formatter -> unit) = fun gaos ff ->
  List.iter (
    fun c ->
      fprintf ff "@[";
      string_of_gao_ff c ff;
      fprintf ff ";@]@,"
  ) gaos

let string_of_gao: (gao -> string) = fun v ->
  call_fun_ff (string_of_gao_ff v)

let string_of_gaos_list: (gao list -> string) = fun v ->
  call_fun_ff (string_of_gaos_list_ff v)


(* Profil de méthode *)
let string_interface_of_method_ff: (Soc.t -> step_method -> Format.formatter -> unit) = fun c m ff ->
  let string_var_from_index: (Soc.var list -> int -> string) = fun vl i ->
    if i < 0 then "mem" else
    string_of_var (List.nth vl i)
  in
    fprintf ff "%s(%s) -> (%s)"
      m.name
      (String.concat "; " (List.map (string_var_from_index (fst c.profile)) m.idx_ins))
      (String.concat "; " (List.map (string_var_from_index (snd c.profile)) m.idx_outs))


let string_interface_of_method: (Soc.t -> step_method -> string) = fun c m ->
  call_fun_ff (string_interface_of_method_ff c m)


(* Méthode complète *)
let string_of_method_ff: (Soc.t -> step_method -> Format.formatter -> unit) = 
fun c m ff ->
  fprintf ff "@[<v>@[<v 2>";
  string_interface_of_method_ff c m ff;
  match m.impl with
    | Extern -> assert false
    | Predef -> fprintf ff "@]@]"
    | Boolred(_i,_j,_k) -> assert false (* todo *)
    | Iterator (_id,_key,_size) -> fprintf ff "" (* todo *)
    | Condact(_key,_) -> assert false (* todo *)
    | Gaol (locals, gaos) ->
          fprintf ff ": {@;";
          fprintf ff "@[<v>-- locals vars@;";
          List.iter (
            fun v ->
              string_of_var_ff v ff;
              fprintf ff ";@,";
          ) locals;
          fprintf ff "@]@;@[<v>-- code@;";
          string_of_gaos_list_ff gaos ff;
          fprintf ff "@]@]@ }@]"

let string_of_method: (Soc.t -> step_method -> string) = fun c m ->
  call_fun_ff (string_of_method_ff c m)


(* Ordre des méthodes *)
let string_of_precedence_ff: (string * string list -> Format.formatter -> unit) = fun (m, needs) ff ->
  fprintf ff "%s < [%s]" m (String.concat "; " needs)

let string_of_precedence: (string * string list -> string) = fun v ->
  call_fun_ff (string_of_precedence_ff v)


(** Profile d'un composant *)
let string_of_profile_ff: Soc.var list * Soc.var list -> Format.formatter -> unit = fun (ins, outs) ff ->
  fprintf ff "profile: @[(%s) ->@ (%s)@]"
    (String.concat "; " (List.map string_of_var ins))
    (String.concat "; " (List.map string_of_var outs))

let string_of_profile: Soc.var list * Soc.var list -> string = fun profile ->
  call_fun_ff (string_of_profile_ff profile)


(* Convertion des éléments d'un composant *)
(* Convertion du profil ... *)
let string_of_soc_profile_ff: (Soc.t -> Format.formatter -> unit) = fun comp ff ->
  string_of_profile_ff comp.profile ff

let _string_of_soc_profile: (Soc.t -> string) = fun comp ->
  call_fun_ff (string_of_soc_profile_ff comp)

(* ... des contraintes *)
let string_of_comp_constraints_ff: (Soc.t -> Format.formatter -> unit) = fun comp ff ->
  fprintf ff "constraints: @[";
  match comp.precedences with
    | [] -> fprintf ff "[]@]"
    | _ ->
        fprintf ff "%s@]"
          (String.concat "; " (List.map string_of_precedence comp.precedences))

let string_of_soc_factory_ff: (
  Soc.t -> Format.formatter ->
  (Soc.t -> step_method -> Format.formatter -> unit) -> (* Formatage des méthodes *)
  (instance -> Format.formatter -> unit) option -> (* Formatage des mémoires *)
  unit
) = fun comp ff format_meth format_mem ->
  let display_mem () = 
    match format_mem with
      | None -> ()
      | Some f -> (
        fprintf ff "@[<v 2>instances:@,";
        List.iter (
          fun m ->
            f m ff;
            fprintf ff ";@,"
        ) comp.instances
      )
  in
  let display_init () = ()
(* DELETE ME !! *)
(*     match comp.init with *)
(*       | None -> fprintf ff "@[<v 2>init: -@]" *)
(*       | Some m -> ( *)
(*         fprintf ff "@[<v 2>init:@,"; *)
(*         format_meth comp m ff; *)
(*         fprintf ff "@]" *)
(*       ) *)
  in

  fprintf ff "@[<v>@[<v 2>soc ";
  string_of_soc_key_ff comp.key ff;
  fprintf ff ":@,@[<v>";

  string_of_soc_profile_ff comp ff;
  fprintf ff "@]@,@[<v>";

  display_mem();
  fprintf ff "@]@,@[<v>";

  string_of_comp_constraints_ff comp ff;
  fprintf ff "@]@,@[<v>";

  display_init();
  fprintf ff "@]@,@[<v>";

  fprintf ff "@[<v 2>steps:@,";
  List.iter (
    fun s ->
      fprintf ff "@[";
      format_meth comp s ff;
      fprintf ff "@]@,"
  ) comp.step;

  fprintf ff "@]@]@]@]@."


(* Interface d'un composant *)
let string_interface_of_soc_ff: (Soc.t -> Format.formatter -> unit) = fun comp ff ->
  string_of_soc_factory_ff comp ff
    string_interface_of_method_ff
    None

let string_interface_of_soc: (Soc.t -> string) = fun v ->
    call_fun_ff (string_interface_of_soc_ff v)


(* Composant complet *)
let string_of_soc_ff: (Soc.t -> Format.formatter -> unit) = fun comp ff ->
  string_of_soc_factory_ff comp ff
    string_of_method_ff
    (Some string_of_instance_ff)

let string_of_soc: (Soc.t -> string) = fun v ->
    call_fun_ff (string_of_soc_ff v)

let output: (bool -> string -> Soc.t list -> unit) = 
  fun no_header pkg_name socs ->
    let header = "Package '" ^ pkg_name ^ "' :" in
    let deco   = (String.make (String.length header) '=') in

      if no_header then () else LicDump.dump_entete stdout ;
      print_string (deco ^ "\n" ^ header ^ "\n" ^ deco ^ "\n" ^ "\n");
      print_string (
        String.concat "\n\n" (List.map string_of_soc socs)
      );
      flush stdout

let (find : Lxm.t -> Soc.key -> Soc.tbl -> Soc.t) =
  fun lxm sk soc_tbl ->
    try SocMap.find sk soc_tbl
    with Not_found ->
      let str_of_key = string_of_soc_key in
      let nodes = SocMap.fold (fun sk _ acc -> (str_of_key sk)::acc) soc_tbl [] in
      let nodes_str = String.concat "\n\t" nodes in
      let msg = Printf.sprintf
        ("Cannot find a soc for the node %s. \n Available nodes are: \n\t%s\n")
        (str_of_key sk)
        nodes_str
      in
      raise (Lv6errors.Compile_error(lxm,msg))

let (add: key -> t -> tbl -> tbl) =
  fun k soc tbl ->
  SocMap.add k soc tbl 
            
let (find_no_exc : Soc.key -> Soc.tbl -> Soc.t) =
  fun sk soc_tbl -> 
    try find (Lxm.dummy "") sk soc_tbl
    with Lv6errors.Compile_error(_,msg) ->
      print_string msg; 
      flush stdout;
      assert false

let gen_index_list n =
  let rec aux acc i n =
    if i<0 then acc else aux (i::acc) (i-1) n
  in
  aux [] (n-1) n

let _ = assert (gen_index_list 5 = [0;1;2;3;4])


external format_float: string -> float -> string = "caml_format_float" 
(* external format_float: string -> float -> string = "format_float"   *)
  
let (my_string_of_float_precision : int option -> float -> string) =
  fun p_opt f -> 
    match p_opt with
      | None -> string_of_float f
      | Some p -> 
        let precision_str = string_of_int p in 
        format_float ("%." ^ precision_str ^ "f") f 

let (is_memory_less : Soc.t -> bool) =
  fun soc -> 
    soc.memory = No_mem && soc.instances = []

(* exported *)
let (ctx_is_global : Soc.t -> bool) =
  fun soc -> 
    match Lv6MainArgs.global_opt.Lv6MainArgs.io_transmit_mode with
      | Lv6MainArgs.Heap -> is_memory_less soc
      | Lv6MainArgs.Stack | Lv6MainArgs.HeapStack -> false

let  (get_rank : 'a -> ('a * 'b) list -> int) =
  fun x l -> 
    let rec aux i l =
      match l with
        | [] -> 0
        | (y,_)::l -> if x = y then i else aux (i+1) l
    in
    aux 1 l
        
let _ = assert (get_rank 5 [(1,4);(3,4);(5,5)] = 3)


let (filter_step_params : int list -> 'a list -> 'a list) =
  fun il vl -> (* we suppose that the index list is in increasing order *)
   let rec aux il vl idx acc =
     match il,vl with
       | [],_ -> acc
       | _::_, [] -> assert false
       | i::til,v::tvl -> 
         if i = idx then aux til tvl (idx+1) (v::acc) 
         else if i > idx then aux il tvl (idx+1) acc 
         else assert false
   in 
   List.rev (aux il vl 0 [])


let _ = (
  assert (filter_step_params [0;1;4] ["v1";"v2";"v3";"v4";"v5"] =  ["v1";"v2";"v5"]))

  
let rec (get_top_var : Soc.var_expr  -> Soc.var_expr) =
  fun var -> 
(* if var = t.[2].field, then it returns (also) t.[2] and t  *)
    match var with
      | Soc.Slice(ve,_,_,_,_,_)
      | Soc.Field(ve,_,_)  
      | Soc.Index(ve,_,_) -> get_top_var ve
      | Soc.Var(_,_vt)
      | Soc.Const(_,_vt) -> var 

open Soc2cIdent
let rec (lustre_string_of_var_expr: Soc.var_expr -> string) = 
  function
    | Const("true", _) -> "true"
    | Const("false", _) -> "false"
    | Const(id, _) -> id2s id
    | Var (id,_)   -> id
    | Field(f, id,_) -> Printf.sprintf "%s.%s" (lustre_string_of_var_expr f) (id2s id) 
    | Index(f, index,_) -> Printf.sprintf "%s[%i]" (lustre_string_of_var_expr f) index
    | Slice(_f,_fi,_la,_st,_wi,_vt) -> assert false (* should not occur *)


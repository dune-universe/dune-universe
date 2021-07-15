(* Time-stamp: <modified the 29/08/2019 (at 16:41) by Erwan Jahier> *)


open Lxm
open Lv6errors
open AstCore
open Lic
open IdSolver

(** DEBUG FLAG POUR CE MODULE : *)
let dbg = (Lv6Verbose.get_flag "lazyc")

let finish_me msg = print_string ("\n\tXXX licTab:"^msg^" ->  finish me!\n")

let profile_info = Lv6Verbose.profile_info

(******************************************************************************)
(** Returns the ident on which the recursion was detected, plus an execution
    stack description. 
*)
exception Recursion_error of (Lv6Id.long as 'id) * (string list as 'stack)

exception BadCheckRef_error

let recursion_error (lxm : Lxm.t) (stack : string list) =
  let rec string_of_stack = function
    | [] -> "nostack" 
    | s::[] -> s
    | s::l  -> s^"\n   > "^(string_of_stack l)
  in
    raise ( Compile_error 
              (lxm, "Recursion loop detected: \n***   " ^(string_of_stack stack)
   ))


(******************************************************************************)
(* Structure principale *)
type t = {
  src_tab : AstTab.t;
  (* table des defs *)
  types  : (Lic.item_key, Lic.type_    Lic.check_flag) Hashtbl.t;
  consts : (Lic.item_key, Lic.const    Lic.check_flag) Hashtbl.t;
  nodes  : (Lic.node_key, Lic.node_exp Lic.check_flag) Hashtbl.t;
  (* table des prov *)
  prov_types  : (Lic.item_key, Lic.type_    Lic.check_flag) Hashtbl.t;
  prov_consts : (Lic.item_key, Lic.const    Lic.check_flag) Hashtbl.t;
  prov_nodes  : (Lic.node_key, Lic.node_exp Lic.check_flag) Hashtbl.t
}

(******************************************************************************)
(* exported *)

let (create : AstTab.t -> t) =
  fun tbl -> 
    let nodes_tbl =   Hashtbl.create 0 in
    let prov_nodes_tbl =   Hashtbl.create 0 in
      (* Iterated operators need to be in this table. Ideally, the lazy
         compiler should be able to pull such strings though...
      *)
    List.iter
      (fun op -> 
        let op_str = AstPredef.op2string op in
        let op_eff = LicEvalType.make_simple_node_exp_eff None true op 
                                                          (Lxm.dummy op_str) 
        in
        let op_key = AstPredef.op_to_long op, [] in
        Hashtbl.add nodes_tbl op_key (Lic.Checked op_eff);
        Hashtbl.add prov_nodes_tbl op_key (Lic.Checked op_eff)
      )
      AstPredef.iterable_op;
    {
      src_tab = tbl;
      types = Hashtbl.create 0;
      consts =  Hashtbl.create 0;
      nodes  = nodes_tbl;
      prov_types = Hashtbl.create 0;
      prov_consts =  Hashtbl.create 0;
      prov_nodes  = prov_nodes_tbl;
    } 

(******************************************************************************)

(** Type checking + constant checking/evaluation

   This is performed (lazily) by 10 mutually recursive functions:

   checking types 
   --------------
   (1) [type_check env type_name lxm]: type check the type id [type_name]
   (2) [type_check_do]: untabulated version of [type_check] (do the real work).

   (3) [type_check_interface]: ditto, but for the interface part
   (4) [type_check_interface_do]: untabulated version (do the real work)

   (5) [solve_type_idref] solves constant reference (w.r.t. short/long ident)

   checking constants 
   ------------------
   (6) [const_check env const_name lxm]: eval/check the constant [const_name]
   (7) [const_check_do] : untabulated version (do the real work)

   (8) [const_check_interface]: ditto, but for the interface part
   (9) [const_check_interface_do]: untabulated version (do the real work)

   (10) [solve_const_idref] solves constant reference (w.r.t. short/long ident)

   checking nodes 
   --------------

   (11) [node_check env node_name lxm]: check the node [node_name]
    checking a node means checking its interface and checking it equations/asserts.
    checking an equation means checking that the type and clock of the
    left part is the same as the ones of the rigth part.


   (12) [node_check_do] : untabulated version (do the real work)

   (13) [node_check_interface]: ditto, but for the interface part
   (14) [node_check_interface_do]: untabulated version (do the real work)

   (15) [solve_node_idref] solves constant reference (w.r.t. short/long ident)

    XXX checking clocks 
    -------------------
    Ditto, but todo!


    nb: for x in {type, const, node, clock}, there are several functions 
    that returns [x_eff]:
    - [x_check]
        o tabulates its result
        o takes an x_key and returns an [x_eff]
        o lookups its (syntaxic) definition (x_info) via the symbolTab.t
        o calls [Ast2lic.of_X] to translate its sub-terms

    - [Ast2lic.of_X]
        o takes a [x_exp] (i.e., an expression) and returns an [x_eff]
        o compute the effective static args (for nodes)
        o calls [solve_x_idref] (via [id_solver]) to translate its sub-terms


    - [solve_x_idref]
        o takes an idref (plus a «Lic.static_arg list» for x=node!)
        o perform name resolution
        o calls [x_check] (loop!)


    nb2: the top-level call is [node_check], on a node that necessarily contains
    no static parameters.  
  

*)

(* Before starting, let's define a few utilitary functions. *)

(** Intermediary results are put into a table. This tabulation handling
    is common to type and constant checking, and is performed by the
    2 following functions.

    Since [x] is meant to stand for [type], [const], or [node], those 2
    functions will lead to the definition of 6 functions:
    [type_check], [const_check], [node_check],
    [type_check_interface], [const_check_interface], [node_check_interface].

*)
let x_check 
   (* tab find_x x_check_do lookup_x_eff pack_of_x_key name_of_x_key this x_key lxm = *)
    (tab            : ('x_key, 'x_eff  Lic.check_flag) Hashtbl.t)
    (find_x         : AstTabSymbol.t -> Lv6Id.t -> Lxm.t -> 
                      ('x_info  Lxm.srcflagged) AstTabSymbol.elt)
    (x_check_do     : t -> 'x_key -> Lxm.t -> AstTabSymbol.t -> bool -> 
                      Lv6Id.pack_name -> 'x_info srcflagged -> 'x_eff)
    (x_builtin      : t -> 'x_key -> Lxm.t -> 'x_eff)
    (lookup_x_eff   : ('x_key, 'x_eff Lic.check_flag) Hashtbl.t -> 'x_key -> 
                      Lxm.t -> 'x_eff)
    (pack_of_x_key  : 'x_key -> string )
    (name_of_x_key  : 'x_key -> string)
    (this           : t)
    (x_key          : 'x_key)
    (lxm            : Lxm.t)
    : 'x_eff =
  Lv6Verbose.exe ~flag:dbg 
                 (fun () -> Printf.printf "#DBG: licTab.x_check '%s'\n" 
                                          (Lxm.details lxm));
  try lookup_x_eff tab x_key lxm 
  with Not_found -> (
    let res = try x_builtin this x_key lxm 
      with Not_found -> 
        Hashtbl.add tab x_key Lic.Checking;
        let (x_pack,xn) = (pack_of_x_key x_key, name_of_x_key x_key) in
        let x_pack_symbols = AstTab.pack_body_env this.src_tab x_pack in
        let x_def = match find_x x_pack_symbols xn lxm with
          | AstTabSymbol.Local x_def -> x_def
          | AstTabSymbol.Imported (lid,_) -> 
            print_string ("*** " ^ (Lv6Id.string_of_long false lid) ^ "???\n" ^ 
                             (Lxm.details lxm));
            assert false (* should not occur *)
        in
        x_check_do this x_key lxm x_pack_symbols false x_pack x_def
    in
    Hashtbl.replace tab x_key (Lic.Checked res);
    res
  )

let x_check_interface 
    tab find_x x_check x_check_interface_do x_builtin lookup_x_eff 
    pack_of_x_key name_of_x_key this x_key lxm =
  try lookup_x_eff tab x_key lxm
  with Not_found ->
    let res = (
      try x_builtin this x_key lxm
      with Not_found ->
        Hashtbl.add tab x_key Lic.Checking;
        let (xp,xn) = (pack_of_x_key x_key, name_of_x_key x_key) in
        let xp_prov_symbols_opt = AstTab.pack_prov_env this.src_tab xp in
        match xp_prov_symbols_opt with
        | None -> 
          (* if [xp] have no provided symbol table, the whole package is exported. *)
          x_check this x_key lxm 
        | Some xp_prov_symbols ->
          let x_def = match find_x xp_prov_symbols xn lxm with
            | AstTabSymbol.Local x -> x
            | AstTabSymbol.Imported _ -> assert false (* should not occur *)
          in
          x_check_interface_do this x_key lxm xp_prov_symbols xp x_def
    ) in
    Hashtbl.replace tab x_key (Lic.Checked res);
    res

(* Returns the tabulated [type] or [const], if it has already been computed;
   otherwise, raise [Not_found] otherwise. *)
let lookup_x_eff x_label id_of_x_key x_tab x_key lxm  =
    match Hashtbl.find x_tab x_key with
      | Lic.Checked res -> res
      | Lic.Checking -> 
          raise (Recursion_error (id_of_x_key x_key, [x_label^(Lxm.details lxm)]))
      | Lic.Incorrect -> raise (BadCheckRef_error)

let (lookup_type_eff: (Lic.item_key, Lic.type_ Lic.check_flag) Hashtbl.t -> 
      Lv6Id.long -> Lxm.t -> Lic.type_) = 
  lookup_x_eff "type ref "  (fun k -> k)


let (type_builtin : t -> Lv6Id.long -> Lxm.t -> Lic.type_) = 
fun _ _ _ -> raise Not_found

let (lookup_const_eff:(Lic.item_key, Lic.const Lic.check_flag) Hashtbl.t -> 
      Lv6Id.long -> Lxm.t -> Lic.const) = 
  lookup_x_eff "const ref " (fun k -> k)

let (const_builtin : t -> Lv6Id.long -> Lxm.t -> Lic.const) =
fun _ _ _ -> raise Not_found

(*
LES NOEUDS (MACROS) BUILD-IN
sont trackés ici pour court-circuiter le node_check normal
(qui nécessite un node_info)
*)
let lookup_node_exp_eff
   (tbl: (Lic.node_key, Lic.node_exp Lic.check_flag) Hashtbl.t)
   (key: Lic.node_key)
   (lxm: Lxm.t)
: Lic.node_exp = 
   try 
      let node_exp = lookup_x_eff "node ref "  (fun k -> fst k) tbl key lxm in
      Lv6Verbose.exe ~flag:dbg (fun () -> Printf.printf
        "#DBG: licTab.lookup_node_exp_eff: FOUND node key '%s'\n"
        (Lic.string_of_node_key key)
      ); 
      node_exp
   with Not_found -> (
      Lv6Verbose.exe ~flag:dbg (
         fun () -> 
            Printf.fprintf
              stderr "#DBG: licTab.lookup_node_exp_eff: node key '%s' NOT FOUND\n"
               (Lic.string_of_node_key key);
               flush stderr
         );
      raise Not_found
   )

let node_builtin (this: t) (key: Lic.node_key) (lxm: Lxm.t) : Lic.node_exp = 
   (* 12/07 *)
   (* ICI => courtcircuite les macros built-in *)
   let nk2nd = fun nk ->
      try
         match Hashtbl.find this.nodes nk with
         | Lic.Checked res -> res
         | _ -> assert false
      with Not_found -> assert false
   in
   let node_exp = LicMetaOp.do_node nk2nd key lxm in
      Lv6Verbose.exe ~flag:dbg (fun () -> Printf.printf 
      "#DBG: licTab.lookup_node_exp_eff: BUILT-IN node key '%s'\n"
         (Lic.string_of_node_key key)
      );
   Hashtbl.replace this.nodes key (Lic.Checked node_exp);
   Hashtbl.replace this.prov_nodes key (Lic.Checked node_exp);
   node_exp

(*   lookup_x_eff "node ref "  (fun k -> fst k) *)


(** This function performs the identifier (idref) resolution,
    i.e., when an ident is not explicitely prefixed by a module
    name, we decide here to which module it belongs. 

    The [provide_flag] indicates whether that function was called
    from a « provide » part or not.
*)
let solve_x_idref
    x_check_interface x_check find_x x_label to_x_key this symbols
    provide_flag currpack idr _sargs lxm =
  let s = Lv6Id.name_of_idref idr in
    match Lv6Id.pack_of_idref idr with
      | Some p -> 
          if p = currpack 
          then x_check this (to_x_key currpack s) lxm
          else x_check_interface this (to_x_key p s) lxm
      | None ->
          (* no pack name: it must be in the symbols table *)
          try
            match (find_x symbols s lxm) with
              | AstTabSymbol.Local _x_info ->
                  let x_key = to_x_key currpack s in
                  if provide_flag
                  then x_check_interface this x_key lxm
                  else x_check this x_key lxm

              | AstTabSymbol.Imported(fid,_params) ->
                  let (pi,si) = (Lv6Id.pack_of_long fid, Lv6Id.of_long fid) in
                    (* todo *)
                    (* rien a faire : si on est arrive ici
                       c'est que les params statique ont pu etre evalues *)
                    (* assert(params=[]); *)
                    x_check_interface this (to_x_key pi si) lxm

          with Not_found ->
            (raise (Compile_error(lxm,"unbounded " ^ x_label ^ " ident")))

(******************************************************************************)
(* topologically sort vars wrt their clock dependecency *)
let find_var_info lxm vars id =
  try Hashtbl.find vars.vartable id
  with Not_found -> 
    raise (Compile_error (lxm,"\n*** Unknown ident: " ^ (Lv6Id.to_string id)))

(* returns the clock of a var *)
let find_direct_dep lxm v vars =
  match (find_var_info lxm vars v).it.var_clock with
  | Base -> None
  | NamedClock({it=(_,vclk);_}) -> Some vclk

(* to track clock dependancy loops *)
type var_state = Todo | Doing | Done of Lv6Id.t list
                
let dep_star lxm vl vars =
  let tbl = Hashtbl.create (List.length vl) in
  List.iter (fun v -> Hashtbl.add tbl v Todo) vl ;
  let rec find_deps v = 
    Lv6Verbose.exe ~flag:dbg (fun () -> Printf.printf "  check clock dep : %s\n" v);
    if not (Hashtbl.mem tbl v) then Hashtbl.add tbl v Todo;
    match Hashtbl.find tbl v with
    | Done cl -> cl
    | Todo -> (
      Hashtbl.replace tbl v (Doing);
      match find_direct_dep lxm v vars with
      | None -> Hashtbl.replace tbl v (Done []);[] 
      | Some v2 ->
         let v2_deps = find_deps v2 in
         let v_deps =  v2::v2_deps in
         Hashtbl.replace tbl v (Done v_deps);
         v_deps
    )
    | Doing ->
       let lxm =
         try let vi = Hashtbl.find vars.vartable v in vi.src
         with Not_found -> lxm (* sno *)
       in
       raise(Compile_error (
        lxm, ("A loop in the clock dependancies exists for variable "^ v)))
  in
  List.iter  
    (fun v ->   
     match find_deps v with 
     | [] -> Hashtbl.remove tbl v (* cleaning *) 
     | _::_ -> () 
    ) 
    vl;
  let tbl2 = Hashtbl.create(List.length vl) in
  Hashtbl.iter (fun v dep ->
    match dep with Done cl -> Hashtbl.replace tbl2 v cl  | _ -> ()) tbl;
  tbl2
  
module TopoSortVars = 
  TopoSort.Make(
      struct 
        type elt = Lv6Id.t
        type store = (Lv6Id.t, Lv6Id.t list) Hashtbl.t
        let find_dep tbl x =
          try Hashtbl.find tbl x with Not_found -> []
        let have_dep tbl x = try Hashtbl.find tbl x <> [] with Not_found -> false
        let remove_dep tbl x = Hashtbl.remove tbl x; tbl
      end
    )

(* Looks like the one in Lic *)
let (sort_vars : Lxm.t -> AstCore.node_vars-> Lv6Id.t list -> Lv6Id.t list) =
  fun lxm vars l ->  (* we sort vars according to their clock deps *)
  profile_info "LicTab.sort_vars\n";
  let tbl = dep_star lxm l vars in
  TopoSortVars.f tbl l

(******************************************************************************)
(* And now we can start the big mutually recursive definition... *)

(** Tabulated version of [type_check_do]. *)
let rec type_check 
    (this: t)
    (key: Lv6Id.long)
    (lxm: Lxm.t)
  : Lic.type_ =
  Lv6Verbose.exe ~flag:dbg (fun () -> 
      Printf.printf "#DBG: licTab.type_check '%s'\n" (Lv6Id.string_of_long false key));
  x_check this.types AstTabSymbol.find_type type_check_do type_builtin lookup_type_eff 
    Lv6Id.pack_of_long Lv6Id.of_long this
    key lxm

(** Tabulated version of [const_check_do]. *)
and const_check
    (this: t)
    (key: Lv6Id.long)
    (lxm: Lxm.t)
  : Lic.const =
  Lv6Verbose.exe ~flag:dbg (fun() -> Printf.printf 
                               "#DBG: licTab.const_check '%s'\n" (Lv6Id.string_of_long false key));
  x_check this.consts AstTabSymbol.find_const const_check_do const_builtin
    lookup_const_eff 
    Lv6Id.pack_of_long Lv6Id.of_long this
    key lxm

(** Tabulated version of [type_check_interface_do]. *)
and type_check_interface
    (this: t)
    (key: Lv6Id.long)
    (lxm: Lxm.t)
  : Lic.type_ =
  Lv6Verbose.exe ~flag:dbg (fun() -> Printf.printf
                               "#DBG: licTab.type_check_interface '%s'\n" (Lv6Id.string_of_long false key));
  x_check_interface 
    this.prov_types AstTabSymbol.find_type type_check type_check_interface_do 
    type_builtin lookup_type_eff Lv6Id.pack_of_long Lv6Id.of_long this
    key lxm

(** Tabulated version of [const_check_interface_do]. *)
and const_check_interface
    (this: t)
    (key: Lv6Id.long)
    (lxm: Lxm.t)
  : Lic.const =
  Lv6Verbose.exe ~flag:dbg (fun () -> Printf.printf
                               "#DBG: licTab.const_check_interface '%s'\n" (Lv6Id.string_of_long false key));
  x_check_interface 
    this.prov_consts AstTabSymbol.find_const const_check const_check_interface_do
    const_builtin lookup_const_eff Lv6Id.pack_of_long Lv6Id.of_long this
    key lxm

(** solving type and constant references *)
and (solve_type_idref : t -> AstTabSymbol.t -> bool -> Lv6Id.pack_name -> 
     Lv6Id.idref -> Lxm.t -> Lic.type_) =
  fun this symbols provide_flag currpack idr lxm -> 
    solve_x_idref
      type_check_interface type_check AstTabSymbol.find_type "type"
      (fun p id -> Lv6Id.make_long p id)
      this symbols provide_flag currpack idr [] lxm

and (solve_const_idref : t -> AstTabSymbol.t -> bool -> Lv6Id.pack_name -> 
     Lv6Id.idref -> Lxm.t -> Lic.const) =
  fun this symbols provide_flag currpack idr lxm ->
    solve_x_idref
      const_check_interface const_check AstTabSymbol.find_const "const"
      (fun p id -> Lv6Id.make_long p id)
      this symbols provide_flag currpack idr [] lxm


(* now the real work! *)
and (type_check_interface_do: t -> Lv6Id.long -> Lxm.t -> AstTabSymbol.t ->
     Lv6Id.pack_name -> AstCore.type_info srcflagged -> 
     Lic.type_) =
  fun this type_name lxm prov_symbols pack_name type_def ->
    (* We type check the interface and the body. 
       For non-abstract types, we also check that both effective types are
       the same.  *)
    let body_type_eff = type_check this type_name lxm in
    let prov_type_eff =
      type_check_do this type_name lxm prov_symbols true pack_name type_def
    in
    if Lic.type_are_compatible prov_type_eff body_type_eff then
      prov_type_eff
    else
      raise(Compile_error (
          type_def.src,
          ("provided type \n\t" ^ 
           (Lic.string_of_type prov_type_eff) ^
           "\n is not compatible with its implementation \n\t" ^ 
           (Lic.string_of_type body_type_eff))))


and (const_check_interface_do: t -> Lv6Id.long -> Lxm.t -> AstTabSymbol.t -> 
     Lv6Id.pack_name -> AstCore.const_info srcflagged -> 
     Lic.const) =
  fun this cn lxm prov_symbols p const_def -> 
    let prov_const_eff = const_check_do this cn lxm prov_symbols true p const_def in
    let body_const_eff = const_check this cn lxm in
    match prov_const_eff, body_const_eff with
    | Lic.Extern_const_eff (_), _ -> assert false
    | Lic.Abstract_const_eff (_id, _teff, _v, _is_exported),
      Lic.Abstract_const_eff (_body_id, _body_teff, _body_v, _body_is_exported)
      ->
      assert false
    (* indeed, how can a body constant be extern and have a value? *)

    | Lic.Abstract_const_eff (id, teff, _v, is_exported),
      Lic.Extern_const_eff (body_id, body_teff) 
      -> 
      if (id <> cn) then assert false
      else if not (Lic.type_are_compatible teff body_teff) then 
        raise(Compile_error (
            const_def.src,
            ("provided constant type \n***\t" ^ 
             (Lic.string_of_type teff)  ^ 
             "   is not compatible with its implementation \n***\t" ^ 
             (Lic.string_of_type body_teff) ^ "")))
      else if 
        is_exported 
      then
        raise(Compile_error (const_def.src, " constant values mismatch"))
      else
        Lic.Extern_const_eff (body_id, body_teff)

    | Lic.Abstract_const_eff (id, teff, v, is_exported), _ -> 
      let body_teff = Lic.type_of_const body_const_eff in
      if (id <> cn) then assert false
      else if not (Lic.type_are_compatible teff body_teff) then 
        raise(Compile_error (
            const_def.src,
            ("provided constant type \n***\t" ^ 
             (Lic.string_of_type teff)  ^ 
             "   is not compatible with its implementation \n***\t" ^ 
             (Lic.string_of_type body_teff) ^ "")))
      else 
      if is_exported && body_const_eff <> v then
        raise(Compile_error (const_def.src, " constant values mismatch"))
      else
        Lic.Abstract_const_eff (id, teff, body_const_eff, is_exported)

    | Lic.Enum_const_eff (_, _), _
    | Lic.Bool_const_eff _, _
    | Lic.Int_const_eff _, _
    | Lic.Real_const_eff _, _
    | Lic.Struct_const_eff (_,_), _
    | Lic.Array_const_eff (_,_), _
      ->
      if prov_const_eff = body_const_eff then
        body_const_eff
      else
        raise(Compile_error (
            const_def.src, 
            "\n*** provided constant does not match with its definition."))
    | Lic.Tuple_const_eff _, _ ->
      print_internal_error "licTab.const_check_interface_do" 
        "should not have been called for a tuple";
      assert false


and (type_check_do: t -> Lv6Id.long -> Lxm.t -> AstTabSymbol.t -> bool -> 
     Lv6Id.pack_name -> AstCore.type_info srcflagged -> 
     Lic.type_) =
  fun this type_name lxm symbols provide_flag pack_name type_def -> 
    try (
      (* Solveur d'idref pour les appels à eval_type/eval_const *)
      let id_solver = {
        id2var   = (fun id lxm -> raise (Unknown_var(lxm,id)) (* should not occur *)); 
        id2const = solve_const_idref this symbols provide_flag pack_name;
        id2type  = solve_type_idref this symbols provide_flag pack_name;
        id2node  = solve_node_idref this symbols provide_flag pack_name;
        global_symbols  = symbols;
        all_srcs = this.src_tab;
      }
      in
      let type_eff = 
        match type_def.it with
        | ArrayType _ -> finish_me " array handling "; assert false
        | ExternalType s -> (
            let lid = Lv6Id.make_long pack_name s in
            let idref = Lv6Id.idref_of_long lid in
            try 
              Abstract_type_eff (lid, id_solver.id2type idref lxm)
            with _e ->
              External_type_eff (lid)
          )
        | AliasedType (_s, texp) -> Ast2lic.of_type id_solver texp
        | EnumType (s, clst) -> (
            let n = Lv6Id.make_long pack_name s in
            let add_pack_name x = Lv6Id.make_long pack_name x.it in
            Enum_type_eff (n, List.map add_pack_name clst)
          )
        | StructType sti -> (
            let make_field (fname : Lv6Id.t) =
              let field_def = Hashtbl.find sti.st_ftable fname in
              let teff = Ast2lic.of_type id_solver field_def.it.fd_type in
              match field_def.it.fd_value with
              | None -> (fname, (teff, None))
              | Some vexp -> (
                  let veff = EvalConst.f id_solver vexp in
                  match veff with
                  | [v] -> (
                      let tv = Lic.type_of_const v in
                      if (tv = teff) then (fname, (teff, Some v)) else 
                        raise 
                          (Compile_error(field_def.src, Printf.sprintf
                                           " this field is declared as '%s' but evaluated as '%s'"
                                           (Lic.string_of_type teff)
                                           (Lic.string_of_type tv)))
                    )
                  | [] -> assert false (* should not occur *)
                  | _::_ -> 
                    raise (Compile_error(field_def.src,
                                         "bad field value: tuple not allowed"))
                )
            in
            let n = Lv6Id.make_long pack_name sti.st_name in
            let eff_fields = List.map make_field sti.st_flist in
            Struct_type_eff (n, eff_fields)
          )
      in        
      type_eff
    )
    with
    (* capte et complete/stoppe les recursions *)
      Recursion_error (root, stack) ->
      if (root = type_name) then recursion_error type_def.src stack else
        raise ( Recursion_error (root, ("type ref "^(Lxm.details lxm))::stack))


and (const_check_do : t -> Lv6Id.long -> Lxm.t -> AstTabSymbol.t -> bool -> 
     Lv6Id.pack_name -> AstCore.const_info srcflagged -> 
     Lic.const) =
  fun this cn lxm symbols provide_flag currpack const_def ->
    (* [cn] and [lxm] are used for recursion errors. 
       [symbols] is the current symbol table.
    *)
    try (
      (* Solveur d'idref pour les  les appels à eval_type/eval_const *)
      let id_solver = {
        id2var   = (fun _idref _lxm -> assert false (* should not occur *)); 
        id2const = solve_const_idref this symbols provide_flag currpack;
        id2type  = solve_type_idref  this symbols provide_flag currpack;
        id2node  = solve_node_idref  this symbols provide_flag currpack;
        global_symbols  = symbols;
        all_srcs  = this.src_tab;
      }
      in
      let const_eff =
        match const_def.it with
        | ExternalConst (id, texp, val_opt) ->
          let lid = Lv6Id.make_long currpack id in
          let teff = Ast2lic.of_type id_solver texp in
          if provide_flag then 
            match val_opt with
            | None  -> 
              (* we put a fake value here as we don't know yet the 
                 concrete value. this will be filled in 
                 const_check_interface_do. I could have put an option
                 type, but that would make quite a lot of noise in the
                 remaining...
              *) 
              Abstract_const_eff(lid, teff, Int_const_eff ("-666"), false)
            | Some c -> 
              let ceff = match EvalConst.f id_solver c with
                | [ceff] -> ceff
                | _  -> assert false
              in
              Abstract_const_eff(lid, teff, ceff, true)

          else
            (match val_opt with
             | None  -> Extern_const_eff(lid, teff)
             | Some _c -> assert false
             (* indeed, how can a body constant be extern and have a value? *)
            )
        | EnumConst (id, texp) ->
          Enum_const_eff ((Lv6Id.make_long currpack id), 
                          Ast2lic.of_type id_solver texp)

        | DefinedConst (_id, texp_opt, vexp ) -> (
            match (EvalConst.f id_solver vexp) with
            | [ceff] -> (
                match texp_opt with
                | None -> ceff
                | Some texp -> (
                    let tdecl = Ast2lic.of_type id_solver texp in
                    let teff =  Lic.type_of_const ceff in
                    if (tdecl = teff ) then ceff else 
                      raise 
                        (Compile_error (const_def.src, Printf.sprintf
                                          " this constant is declared as '%s' but evaluated as '%s'"
                                          (Lic.string_of_type tdecl)
                                          (Lic.string_of_type teff)
                                       )))
              )
            | [] -> assert false (* should not occur *)
            | _::_ -> raise (Compile_error(const_def.src, 
                                           "bad constant value: tuple not allowed"))
          )
      in
      const_eff
    ) with Recursion_error (root, stack) -> (
        (* capte et complete/stoppe les recursions *)
        if (root = cn) then recursion_error const_def.src stack else 
          (* on complete la stack *)
          raise (Recursion_error (root, ("const ref "^(Lxm.details lxm))::stack))
      )


(******************************************************************************)



and (node_check_interface_do: t -> Lic.node_key -> Lxm.t ->
     AstTabSymbol.t -> Lv6Id.pack_name -> AstCore.node_info srcflagged ->
     Lic.node_exp) =
  fun this nk lxm symbols pn node_def ->
    (* DEUX checks :
       - le "complet" donne 'body_node_exp_eff' qui sera stocké comme le vrai résultat 
       - le "provide" donne 'prov_node_exp_eff', non stocké, sert à vérifier la
       cohérence avec l'éventuelle déclaration 'provide' 
    *)
    let body_node_exp_eff = node_check this nk lxm in
    let prov_node_exp_eff = node_check_do this nk lxm symbols true pn node_def in
    (* [type_eff_are_compatible t1 t2] checks that t1 is compatible with t2, i.e., 
        if t1 = t2 or t1 is abstract and and t2.
    *)
    let msg_prefix = 
      ("provided node for " ^ (Lv6Id.string_of_long false (fst nk)) ^ 
       " is not compatible with its implementation: ")
    in
    let str_of_var = Lic.string_of_var_info in
    let type_is_not_comp v1 v2 = not (Lic.var_are_compatible v1 v2) in

    (* Checking the type profile (w.r.t the body and the provided part) *)
    let ibtypes = List.map (fun v -> v.var_type_eff) body_node_exp_eff.inlist_eff
    and iptypes = List.map (fun v -> v.var_type_eff) prov_node_exp_eff.inlist_eff 
    and obtypes = List.map (fun v -> v.var_type_eff) body_node_exp_eff.outlist_eff
    and optypes = List.map (fun v -> v.var_type_eff) prov_node_exp_eff.outlist_eff 
    in
    let _topt = UnifyType.profile_is_compatible nk
        node_def.src (iptypes,ibtypes) (optypes,obtypes)
    in
    if
      prov_node_exp_eff.node_key_eff <> body_node_exp_eff.node_key_eff
    then
      raise(Compile_error (node_def.src, msg_prefix ^ " ??? "))
    else if
      (* ougth to be checked above: well, it eats no bread to keep that check *)
      (List.exists2 type_is_not_comp 
         prov_node_exp_eff.inlist_eff body_node_exp_eff.inlist_eff) 
    then
      let msg = msg_prefix ^ "bad input profile. \n*** " ^ 
                (String.concat "*" (List.map str_of_var prov_node_exp_eff.inlist_eff)) ^
                " <> " ^
                (String.concat "*" (List.map str_of_var body_node_exp_eff.inlist_eff))
      in
      raise(Compile_error (node_def.src, msg))
    else if 
      (List.exists2 type_is_not_comp
         prov_node_exp_eff.outlist_eff body_node_exp_eff.outlist_eff) 
      (* ougth to be checked above: well, it eats no bread to keep that check *)
    then
      let msg = msg_prefix ^ "bad output profile. \n*** " ^ 
                (String.concat "*" (List.map str_of_var prov_node_exp_eff.outlist_eff)) ^
                " <> " ^
                (String.concat "*" (List.map str_of_var body_node_exp_eff.outlist_eff))
      in
      raise(Compile_error (node_def.src, msg))
    else if     
      prov_node_exp_eff.has_mem_eff <> body_node_exp_eff.has_mem_eff 
    then
      raise(Compile_error (node_def.src, msg_prefix ^ " node or function?"))
    else if 
      prov_node_exp_eff.is_safe_eff <> body_node_exp_eff.is_safe_eff
    then
      raise(Compile_error (node_def.src, msg_prefix ^ "safe or unsafe?"))
    else if 
      match prov_node_exp_eff.def_eff, body_node_exp_eff.def_eff with
      | (AbstractLic _,_) -> false
      | (_,_) -> prov_node_exp_eff.def_eff <> body_node_exp_eff.def_eff
    then
      raise(Compile_error (node_def.src, msg_prefix ^ "abstract or not?"))
    else
      match prov_node_exp_eff.def_eff, body_node_exp_eff.def_eff with
      | AbstractLic None, BodyLic _node_body -> 
        { prov_node_exp_eff with def_eff = 
                                   AbstractLic (Some body_node_exp_eff) }
      | _,_ -> 
        prov_node_exp_eff

(* 
       LE GROS DU BOULOT 
       - suivant "provide_flag" : check d'interface (provide) ou le check 
         de la définition
       (n.b. provide_flag influence la résolution des idents dans l'env local de check)
    *)
and node_check_do
    (this: t)
    (nk: Lic.node_key)
    (lxm: Lxm.t)
    (symbols: AstTabSymbol.t)
    (provide_flag: bool)
    (pack_name: Lv6Id.pack_name)
    (node_def: AstCore.node_info srcflagged)
  : Lic.node_exp =
  (* START node_check_do *)
  profile_info "node_check_do\n";
  (
    Lv6Verbose.exe ~flag:dbg (fun () -> Printf.printf
                                 "#DBG: ENTERING node_check_do '%s'\n     (%s)\n"
                                 (Lic.string_of_node_key nk)
                                 (Lxm.details lxm)
                             ); 
    let lxm = node_def.src in
    (* Creates a local_env with just the global bindings,
            local bindinds will be added later (side effect)
    *)
    let local_env = make_local_env nk in
    let _ =
      Lv6Verbose.exe ~flag:dbg (fun () -> 
          Printf.printf "#  local_env while entering (node_check_do %s):\n" 
            (Lic.string_of_node_key nk);
          IdSolver.dump_local_env stderr local_env;
          flush stdout
        )
    in
    let node_id_solver = {
      (* a [node_id_solver] is a [id_solver] where we begin to look
              into the local environement before looking at the global
              one.  *)
      id2var = (* var can only be local to the node *)
        (fun id lxm ->
           try IdSolver.lookup_var local_env id lxm
           with Not_found ->
             raise (Unknown_var(lxm,id))
        );
      id2const =
        (fun id lxm ->
           try IdSolver.lookup_const local_env id lxm
           with Not_found ->
             solve_const_idref this symbols provide_flag pack_name id lxm
        );
      id2type  =
        (fun id lxm ->
           try IdSolver.lookup_type local_env id lxm
           with Not_found ->
             Lv6Verbose.exe ~level:3 (
               fun () ->
                 Printf.printf "*** Dont find type %s in local_env\n"
                   (Lv6Id.string_of_idref false id);
                 Printf.printf "*** local_env.lenv_types contain def for: ";
                 Hashtbl.iter 
                   (fun id _t -> 
                      Printf.printf "%s, " (Lv6Id.to_string id) )
                   local_env.lenv_types;
                 Printf.printf "\n";
                 flush stdout);
             solve_type_idref  this symbols provide_flag pack_name id lxm);
      id2node  =
        (fun id sargs lxm ->
           (try
              let (node_id,sargs) = IdSolver.lookup_node local_env id lxm in
              let node_id = Lv6Id.idref_of_long node_id in
              solve_node_idref this symbols provide_flag pack_name node_id sargs lxm
            (*                node_check this (node_id,[]) lxm   *)

            with 
              Not_found -> 
              solve_node_idref this symbols provide_flag pack_name id sargs lxm
            | _ -> assert false)
        );

      (* ATTENTION EN SE SERVANT DE CA !
              ne tient pas compte des params statiques du noeud ! *)
      global_symbols  = symbols;
      all_srcs  = this.src_tab;
    }
    in
    let make_node_eff id node_def_eff = (
      (* building not aliased nodes *)
      Lv6Verbose.exe ~level:3 
        (fun () -> Printf.printf
            "*** local_env while entering (make_node_eff %s):\n" (Lv6Id.to_string id);
          IdSolver.dump_local_env stderr local_env
        );
      (********************************************************)
      (* LOCAL CONSTANTS are evaluated and added to local_env *)
      (********************************************************)
      (* init intermediate table *)
      let sz = List.length node_def.it.loc_consts in
      let temp_const_eff_tab : (Lv6Id.long, Lic.const Lic.check_flag) Hashtbl.t =
        Hashtbl.create sz
      in
      let temp_const_def_tab :
        (Lv6Id.t,(Lxm.t * AstCore.type_exp option * AstCore.val_exp)) Hashtbl.t =
        Hashtbl.create sz
      in
      let init_local_const (lxm, cinfo) = (
        match cinfo with
        | DefinedConst (i,topt,ve) -> (
            Lv6Verbose.printf ~level:3 " * local const %s will be treated\n" i;
            Hashtbl.add temp_const_def_tab i (lxm,topt,ve)
          )
        | ExternalConst _ 
        | EnumConst _ -> (
            let msg = "*** abstract constant bot allowed within node "
            in
            raise (Compile_error(lxm, msg))
          )
      ) in
      List.iter init_local_const node_def.it.loc_consts ;
      (* differs from node_id_solver only on id2const *)
      let rec local_id_solver = {
        id2var   = node_id_solver.id2var;
        id2const = local_id2const;
        id2type  = node_id_solver.id2type;
        id2node  = node_id_solver.id2node;
        global_symbols  = node_id_solver.global_symbols;
        all_srcs  = node_id_solver.all_srcs;
      }
      and treat_local_const id = (
        Lv6Verbose.printf ~level:3 " * call treat_local_const %s\n" id;
        let id_key = ("", id) in
        try (
          let ce = lookup_const_eff temp_const_eff_tab id_key lxm in
          Lv6Verbose.exe
            ~level:3 (fun() -> Printf.printf 
                         " * const %s already treated = %s\n" 
                         id (LicDump.string_of_const_eff false ce));
          ce
        ) with Not_found -> (
            let (lxmdef, toptdef, vedef) = Hashtbl.find temp_const_def_tab id in
            Lv6Verbose.printf ~level:3 " * const %s not yet treated ...\n" id ;
            (* yes, not yet checked *) 
            Hashtbl.add temp_const_eff_tab id_key Checking ;
            (* computes the value with EvalConst.f id_solver ve ... *)
            let ce = match (EvalConst.f local_id_solver vedef) with
              | [ceff] -> (
                  match toptdef with
                  | None -> ceff
                  | Some texp -> (
                      let tdecl = Ast2lic.of_type local_id_solver texp in
                      let teff =  Lic.type_of_const ceff in
                      if (tdecl = teff ) then ceff else 
                        raise (Compile_error (
                            lxmdef, Printf.sprintf
                              " this constant is declared as '%s' but evaluated as '%s'"
                              (Lic.string_of_type tdecl)
                              (Lic.string_of_type teff)
                          )))
                )
              | [] -> assert false (* should not occur *)
              | _::_ -> raise (Compile_error(lxmdef, "bad constant value: tuple not allowed"))
            in
            Lv6Verbose.exe
              ~level:3 (fun() -> Printf.printf " * const %s evaluated to %s\n"
                           id (LicDump.string_of_const_eff false ce));
            Hashtbl.replace temp_const_eff_tab id_key (Checked ce) ;
            ce
          )
      )
      and local_id2const idrf lxm = (
        (* is id a local const ? *)
        try (
          (* certainly NOT if id has a pack *)
          let id = if (Lv6Id.pack_of_idref idrf = None)
            then Lv6Id.name_of_idref idrf
            else raise Not_found
          in
          let ce = treat_local_const id in
          ce
        ) with Not_found -> (
            (* not a local constant -> search in global env *)
            Lv6Verbose.printf ~level:3 
              " * %s not a local const, should be global ?" 
              (Lv6Id.string_of_idref false idrf);
            let ce = node_id_solver.id2const idrf lxm in
            Lv6Verbose.exe ~level:3 (fun() -> Printf.printf
                                        " YES -> %s\n" (LicDump.string_of_const_eff false ce));
            ce
          )
      ) in
      (* iters local_id2const n eeach declared constant *)
      Hashtbl.iter (fun id _ -> let _ = treat_local_const id in ())
        temp_const_def_tab ;
      (* Finally, adds each local const to ICI *)
      let add_local_const idref ceck = (
        Lv6Verbose.exe
          ~level:3
          (fun() -> Printf.printf 
              " * add_local_const %s = %s\n" (snd idref)
              (match ceck with
               | Checking -> "Checking"
               | Checked ce -> (LicDump.string_of_const_eff false ce)
               | Incorrect -> "Incorrect"
              ));
        match ceck with
        | Checked ce -> Hashtbl.add local_env.lenv_const (snd idref) ce
        | _ -> assert false
      ) in
      Hashtbl.iter add_local_const temp_const_eff_tab ;

      (********************************************************)
      (* LOCAL FLOWS are added to local_env                   *)
      (********************************************************)
      (* (i.e. ins,outs,locs) *)
      match node_def.it.vars with
      | None -> assert false (* a node with a body should have a profile *)
      | Some vars ->
        (* let is_polymorphic = ref false in *)
        let type_args id =
          let vi = find_var_info lxm vars id in
          let t_eff = Ast2lic.of_type node_id_solver vi.it.var_type in
          (* let _ = if Lic.is_polymorphic t_eff then is_polymorphic := true in *)
          let c_eff = Ast2lic.of_clock node_id_solver vi.it in
          let vi_eff = {
            var_name_eff   = vi.it.var_name;
            var_nature_eff = vi.it.var_nature;
            var_number_eff = vi.it.var_number;
            var_type_eff  = t_eff;
            var_clock_eff = c_eff;
          }
          in
          Hashtbl.add local_env.lenv_types id t_eff;
          Hashtbl.add local_env.lenv_vars id vi_eff;
          vi_eff
        in
        let vars_in_sorted  = sort_vars lxm vars vars.inlist
        and vars_out_sorted = sort_vars lxm vars vars.outlist in
        let inlist  = List.map type_args vars_in_sorted
        and outlist = List.map type_args vars_out_sorted
        and loclist = 
          match vars.loclist with
          | None -> None
          | Some loclist -> 
            let vars_loc_sorted = sort_vars lxm vars loclist in
            Some (List.map type_args vars_loc_sorted)
        in
        let unsort l_id l_vi =
          let tab = List.map (fun vi -> vi.var_name_eff, vi) l_vi in
          try List.map (fun id -> List.assoc id tab) l_id
          with Not_found -> assert false
        in
        profile_info "LicTab.unsort\n";
        let inlist2 = unsort vars.inlist inlist
        and outlist2 = unsort vars.outlist outlist in
        {
          node_key_eff = nk;
          inlist_eff   = inlist2;
          outlist_eff  = outlist2;
          loclist_eff  = loclist;
          def_eff = node_def_eff ();
          has_mem_eff  = node_def.it.has_mem;
          is_safe_eff  = node_def.it.is_safe;
          lxm = node_def.src;
          (* is_polym_eff = !is_polymorphic *)
        }
    ) in
    (* let's go *)
    let res =
      match node_def.it.def with
      | Abstract -> make_node_eff node_def.it.name (fun () -> AbstractLic None)
      | Extern   -> make_node_eff node_def.it.name (fun () -> ExternLic)
      | Body nb  ->
        make_node_eff node_def.it.name ( 
          (fun () -> (* trick to force to delay this evaluation 
                        after the local_env.lenv_vars has been
                        filled
                     *)
             let eq_eff = List.map (Ast2lic.of_eq node_id_solver) nb.eqs in
             BodyLic {
               asserts_eff = 
                 List.map (Ast2lic.of_assertion node_id_solver) nb.asserts;
               eqs_eff = eq_eff; 
             }
          )
        )

      | Alias({it= alias;src=lxm}) -> (
          let aliased_node = 
            match alias with
            (* 12/07 SOLUTION INTERMEDIAIRE 
                        - les macros predefs sont traitées comme des call 
            *)
            | Predef_n(op) -> 
              let predef_op = op.it in
              let _ = match predef_op with
                | AstPredef.NOR_n 
                | AstPredef.DIESE_n ->
                  raise (Compile_error (lxm, "Can not alias 'nor' nor '#', sorry"))
                | _ -> ()
              in
              let predef_op_eff = LicEvalType.make_node_exp_eff 
                  node_id_solver (Some node_def.it.has_mem) 
                  true predef_op lxm
              in
              predef_op_eff

            | CALL_n(node_alias) -> 
              Ast2lic.of_node node_id_solver node_alias 
            | (ARRAY_SLICE_n _|ARRAY_ACCES_n _|STRUCT_ACCESS_n _
              |IDENT_n _|ARRAY_n|HAT_n|CONCAT_n|WITH_n(_)|TUPLE_n|WHEN_n _
              |CURRENT_n|FBY_n|ARROW_n|PRE_n)
              -> 
              raise (Compile_error (lxm, "can not alias this operator, sorry"))
              (* does it make sense to alias when, pre, etc? *)
          in
          let (vil, vol) = 
            match node_def.it.vars with
            | None -> aliased_node.inlist_eff, aliased_node.outlist_eff
            | Some (vars) ->
              (* a type profile is declared; let's check there are compatible *)
              let (il,ol) = profile_of_node_exp aliased_node in
              let (il_decl, ol_decl) = 
                let vi_il, vi_ol = 
                  List.map (fun id -> find_var_info lxm vars id) vars.AstCore.inlist,
                  List.map (fun id -> find_var_info lxm vars id) vars.AstCore.outlist
                in
                let aux vi = Ast2lic.of_type node_id_solver vi.it.var_type in
                let (il_decl, ol_decl) = List.map aux vi_il, List.map aux vi_ol in
                let i_unif_res = UnifyType.f il_decl il
                and o_unif_res = UnifyType.f ol_decl ol
                in
                (match i_unif_res with
                 | UnifyType.Ko msg -> raise(Compile_error(lxm, msg))
                 | UnifyType.Equal -> ()
                 | UnifyType.Unif _t -> () 
                 (* Ast2lic.dump_polymorphic_nodes t *)
                );
                (match o_unif_res with
                 | UnifyType.Ko msg -> raise(Compile_error (lxm, msg))
                 | UnifyType.Equal -> ()
                 | UnifyType.Unif _t ->  ()
                 (* Ast2lic.dump_polymorphic_nodes t *)
                );
                (* ok, there are compatible. We use the declared profile. *)
                (il_decl, ol_decl)
              in
              let instanciate_var_info vi t = { vi with var_type_eff = t } in
              let vil = List.map2 instanciate_var_info aliased_node.inlist_eff
                  il_decl 
              and vol = List.map2 instanciate_var_info aliased_node.outlist_eff
                  ol_decl in
              vil,vol
          in
          let (alias_node : Lic.node_exp) = 
            try make_alias_node  aliased_node nk local_env node_id_solver
                  vil vol node_def.src
            with Not_found -> assert false (* defense against List.assoc *)
          in
          alias_node
        )
      (* End Alias *)
    in
    L2lCheckOutputs.check_node res;
    (* gen_code provide_flag current_env res; *)
    Lv6Verbose.exe ~flag:dbg (fun() -> Printf.printf
                                 "#DBG: EXITING  node_check_do '%s'\n"
                                 (Lic.string_of_node_key nk)
                             ); 
    res
  )
(*END node_check_do *)

(* 
       [make_alias_node aliased_node alias_nk node_id_solver_vars_opt lxm]
       builds a node that  calls the aliased node. It looks like:
       node  alias_node(ins) returns (outs);  
       let 
       outs = aliased_node(ins); 
       tel

       When instanciating models with polymorphic operators, it
       may happen that some exported user nodes become
       polymorphic (via node alias precisely). But in that case,
       a non-polymorphic profile is given in the package provided
       part. In such a case, we can use the types of the provided
       part (itl and otl) instead of the polymorphic ones.  *)

and make_alias_node
    (aliased_node: node_exp)
    (alias_nk: node_key)
    (local_env: local_env)
    (_node_id_solver: IdSolver.t)
    (vil: var_info list)
    (vol: var_info list)
    (lxm: Lxm.t)
  : node_exp
  =
  Lv6Verbose.printf ~level:3 
    "*** Lic.make_alias_node %s \n" (Lv6Id.string_of_long false (fst alias_nk));
  flush stdout;

  let (outs:left list) = List.map  (fun vi -> LeftVarLic (vi, lxm)) vol in
  let tl = List.map type_of_left outs in
  let cl = List.map (fun l -> (var_info_of_left l).var_clock_eff) outs in
  let (aliased_node_call : val_exp) =
    { ve_core = 
        CallByPosLic(
          (Lxm.flagit (CALL(Lxm.flagit aliased_node.node_key_eff lxm)) lxm, 
           (List.map 
              (fun vi -> (* build operands*)
                 let ve = { 
                   ve_typ = [vi.var_type_eff]; 
                   ve_clk = [snd vi.var_clock_eff];
                   ve_core = CallByPosLic(
                       Lxm.flagit (VAR_REF(vi.var_name_eff)) lxm, []);
                   ve_src = lxm
                 }
                 in
                 ve
              )
              vil)));
      ve_typ = tl;
      ve_clk = List.map snd cl;
      ve_src = lxm
    }
  in
  let alias_node = 
    match aliased_node.def_eff with
    | BodyLic _ -> { aliased_node with node_key_eff = alias_nk }
    | _  -> {
        aliased_node with
        node_key_eff = alias_nk;
        inlist_eff = vil;
        outlist_eff = vol;
        loclist_eff = None;
        def_eff = BodyLic(
            { asserts_eff = []; 
              eqs_eff = [Lxm.flagit (outs, aliased_node_call) lxm] 
            });
        (* is_polym_eff = List.exists is_polymorphic (List.map (fun vi 
             -> vi.var_type_eff) (vil@vol)); *)
      }
  in
  (* update the local_env table *)
  let _ = 
    let update_local_env_table vi =
      Hashtbl.add local_env.lenv_vars vi.var_name_eff vi
    in
    List.iter update_local_env_table alias_node.inlist_eff;
    List.iter update_local_env_table alias_node.outlist_eff;
    match alias_node.loclist_eff with 
      None -> () | Some l -> List.iter update_local_env_table l;
  in
  alias_node

(** builds a [node_key] and calls [node_check] *)
and solve_node_idref
    (this: t)
    (symbols: AstTabSymbol.t)
    (provide_flag: bool)
    (currpack: Lv6Id.pack_name)
    (idr: Lv6Id.idref)
    (sargs: Lic.static_arg list)
    (lxm: Lxm.t)
  : Lic.node_exp =

  solve_x_idref
    node_check_interface node_check AstTabSymbol.find_node "node"
    (fun p id ->
       (* builds a [node_key] from a [pack_name] and a [node] id, 
          and a Lic.static_arg list *)
       let long = Lv6Id.make_long p id in
       let node_key = long, sargs in
       node_key
    )
    this symbols provide_flag currpack idr sargs lxm

and node_check (this: t) (nk: Lic.node_key) (lxm: Lxm.t) : Lic.node_exp =
  Lv6Verbose.printf ~flag:dbg 
    "#DBG: licTab.node_check '%s'\n" (Lic.string_of_node_key nk);
  try (
    let pack_of_x_key = fun nk -> Lv6Id.pack_of_long (fst nk) in
    let name_of_x_key = fun nk -> Lv6Id.of_long (fst nk) in
    x_check this.nodes AstTabSymbol.find_node node_check_do 
      node_builtin lookup_node_exp_eff
      pack_of_x_key
      name_of_x_key
      this nk lxm
  ) with
    Recursion_error (n, stack) -> 
    let msg = "Recursion loop detected in node " ^ 
              (Lv6Id.string_of_long false (fst nk)) in
    let msg = msg ^ "\n*** "^ (Lv6Id.string_of_long false n) ^
              " depends on itself\n " ^ (String.concat "\n*****" stack) in
    raise (Compile_error (lxm, msg))

and node_check_interface
    (this: t)
    (nk: Lic.node_key)
    (lxm: Lxm.t)
  : Lic.node_exp =
  Lv6Verbose.printf ~flag:dbg "#DBG: licTab.node_check_interface '%s'\n" 
    (Lic.string_of_node_key nk);
  x_check_interface this.prov_nodes AstTabSymbol.find_node node_check
    node_check_interface_do node_builtin lookup_node_exp_eff
    (fun nk -> Lv6Id.pack_of_long (fst nk))
    (fun nk -> Lv6Id.of_long (fst nk)) this nk
    lxm
      

    
(*-------------------------------------------------------------------------
compile all items
  ---------------------------------------------------------------------------*)

let compile_all_item this _label x_check_interface _string_of_x_key
    _string_of_x_eff to_key id item_def =
  match item_def with
    | AstTabSymbol.Local _item_def ->
        ignore
          (x_check_interface this (to_key id) (Lxm.dummy "compile all items"))
(*        Printf.printf "\t\t%s %s = %s\n" *)
(*          label (string_of_x_key (to_key id)) (string_of_x_eff x_eff) *)

    | AstTabSymbol.Imported(_item_def,_) -> ()
(*      Printf.printf "\t\t%s %s = %s (imported)\n" *)
(*        label (string_of_x_key (to_key id)) (Lv6Id.string_of_long item_def) *)
   

let compile_all_types pack_name this =
  compile_all_item this "type" type_check_interface (Lv6Id.string_of_long false)
    Lic.string_of_type (fun id -> Lv6Id.make_long pack_name id)

let compile_all_constants pack_name this = 
  compile_all_item this "const" const_check_interface  (Lv6Id.string_of_long false)
    (LicDump.string_of_const_eff true) (fun id -> Lv6Id.make_long pack_name id)


let (get_static_params : (node_info Lxm.srcflagged) AstTabSymbol.elt -> 
      static_param srcflagged list) =
  fun node_info_flagged -> 
    match node_info_flagged with
      | AstTabSymbol.Local nif -> nif.it.static_params
      | AstTabSymbol.Imported(_id,sparams) -> sparams

let compile_all_nodes pack_name this id ni_f =
  let sp = get_static_params ni_f in
    if sp <> [] then () (* we need static arg to compile such kind of things *)
    else
      compile_all_item this "node" node_check_interface 
        (LicDump.string_of_node_key_rec true false)
        Lic.profile_of_node_exp 
        (fun id -> (Lv6Id.make_long pack_name id, [])) id ni_f

(**** to_lic : translate the (finalized) internal structure
  into a proper LicPrg, for forthcoming manip and other prg 2 prg
  transformations
  N.B. items belonging to the "Lustre" virtual pack are not 
  taken into account
*)

let to_lic_prg (this:t) : LicPrg.t =
   (* normally, only checked and correct programs are lic'ified *)
   let unflag = function Checked x -> x | _ -> assert false in
   let add_item add_x k v prg =
      match Lv6Id.pack_of_long k with
      | "Lustre" -> prg
      | _ -> add_x k (unflag v) prg
   in
   let add_node k v prg =
      Lv6Verbose.printf ~flag:dbg "#DBG: licTab.to_lic: node key '%s'\n"
        (Lic.string_of_node_key k);
      match Lv6Id.pack_of_long (fst k) with
(*         | "Lustre" -> prg *)
        | _ -> LicPrg.add_node k (unflag v) prg
   in
   let res = LicPrg.empty in
   let res = Hashtbl.fold (add_item LicPrg.add_type) this.types res in 
   let res = Hashtbl.fold (add_item LicPrg.add_const) this.consts res in 
   let res = Hashtbl.fold add_node this.nodes res in 
   res

(**** Entry points of the module :
   either compile a single node or everithing ...  
*)
let compile_all (this:t) : t =
  let testpack pack_name = (
    Lv6Verbose.printf ~level:3 " * package %s\n" (Lv6Id.pack_name_to_string pack_name);
    let prov_symbols =
      match AstTab.pack_prov_env this.src_tab pack_name with
        | Some tab -> tab
        | None -> AstTab.pack_body_env this.src_tab pack_name
    in
    Lv6Verbose.print_string ~level:3 "\tExported types:\n";
    AstTabSymbol.iter_types prov_symbols (compile_all_types pack_name this);
    flush stdout;
    Lv6Verbose.print_string ~level:3 "\tExported constants:\n";
    AstTabSymbol.iter_consts prov_symbols (compile_all_constants pack_name this);
    flush stdout;
    Lv6Verbose.print_string ~level:3 "\tExported nodes:\n";
    AstTabSymbol.iter_nodes prov_symbols (compile_all_nodes pack_name this);
    flush stdout
  )
  in
  let plist = AstTab.pack_list this.src_tab in
  Lv6Verbose.print_string ~level:3 "*** Dump the exported items of the packages.\n";
  try
    List.iter testpack plist;
    this
  with
      Recursion_error (n, stack) -> 
        let msg = "Recursion loop detected in node " ^ (Lv6Id.string_of_long false n) in
        let msg = msg ^ "\n*****" ^ (String.concat "\n*****" stack) in
        raise (Compile_error (Lxm.dummy "", msg))

let compile_node (this:t) (main_node:Lv6Id.idref) : t =
   (* la clée "absolue" du main node (pas d'args statiques) *)
   let main_node_key = node_key_of_idref main_node in
   profile_info "LicTab.compile_node\n";
   Lv6Verbose.printf
      "-- MAIN NODE: \"%s\"\n"
      (LicDump.string_of_node_key_rec true false main_node_key);

   let lxm = match Lv6Id.pack_of_idref main_node with
      | None -> Lxm.dummy ""
      | Some pn  -> Lxm.dummy (Lv6Id.pack_name_to_string pn)
   in
   let _ = node_check this main_node_key lxm in
   this


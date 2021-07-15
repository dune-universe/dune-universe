(* Time-stamp: <modified the 29/08/2019 (at 16:26) by Erwan Jahier> *)

open AstPredef
open Lv6errors
open UnifyType
open Lic
open LicDump

(* exported *)
type typer = Lic.type_ AstPredef.evaluator

(* exported *)
exception EvalType_error of string


(* exported *)
let (type_error : Lic.type_ list -> string -> 'a) =
  fun tel expect -> 
    let str_l = List.map Lic.string_of_type tel in
    let str_provided = String.concat "*" str_l in 
      raise (EvalType_error(
               ("\n*** type '" ^ str_provided ^ "' was provided" ^
                  (if expect = "" then "" 
                   else (" whereas\n*** type '" ^expect^"' was expected")))))

let raise_type_error
    (prov:  Lic.type_ list)
    (expec:  Lic.type_ list)
    (msg: string)
    = raise (EvalType_error(
      let pr = Lic.string_of_type_list prov in
      let ex = Lic.string_of_type_list expec in
      (
        "'" ^ pr ^ "' was provided" ^ (
          if ex = "" then "" else (" whereas\n*** type '" ^ex^"' was expected")
        ) ^ (
          if msg = "" then "" else ("\n*** " ^ msg)
        )
      )
    )
    )

let raise_arity_error (msg:string) (get:int) (expect:int) =
  raise (EvalType_error(
    Printf.sprintf "bad arity (%s): %d argument%s, whereas %d were expected"
      msg get (if get>1 then "s" else "") expect))

(*********************************************************************************)
(* a few local alias to make the node profile below more readable. *)
let i = Int_type_eff
let r = Real_type_eff
let b = Bool_type_eff
let id str = Lv6Id.of_string str

(** A few useful type profiles for simple operators *)
let bb_profile  = [(id "i1", b)], [(id "out", b)]               (* bool -> bool *)
let bbb_profile = [(id "i1", b);(id "i2", b)], [(id "out", b)] (* bool*bool -> bool *)
let ii_profile  = [(id "i1", i)], [(id "out", i)]               (* int -> int *)
let iii_profile = [(id "i1", i);(id "i2", i)], [(id "out", i)] (* int*int -> int *)
let rr_profile  = [(id "i1", r)], [(id "out", r)]               (* real -> real *)
let rrr_profile = [(id "i1", r);(id "i2", r)], [(id "out", r)] (* real*real -> real *)
let ri_profile  = [id "i1", r], [id "out", i] (* real -> int  *)
let ir_profile  = [id "i1", i], [id "out", r] (* int  -> real *)

(* for nor and diese, I cannot know the size of the array, so I put an 'any' type *)
let ab_profile = [(id "i1", TypeVar Any)], [(id "out", b)] (* bool^s -> bool *)

(** Constant profiles  *)
let b_profile = [],[id "out", b]
let i_profile = [],[id "out", i]
let r_profile = [],[id "out", r]

(** polymorphic operator profiles *)
let aab_profile = [(id "i1",(TypeVar Any));(id "i2",(TypeVar Any))], [(id "out", b)] (* 'a -> 'a -> bool*)
let baaa_profile = [(id "c", b);(id "b1",(TypeVar Any));(id "b2",(TypeVar Any))], [(id "out",(TypeVar Any))] 
  (* for if-then-else *)

(** overloaded operator profiles *)
let oo_profile  = [(id "i1",(TypeVar AnyNum))], [(id "out",(TypeVar AnyNum))]
let ooo_profile = [(id "i1",(TypeVar AnyNum));(id "i2",(TypeVar AnyNum))], [(id "out",(TypeVar AnyNum))]
let oob_profile = [(id "i1",(TypeVar AnyNum));(id "i2",(TypeVar AnyNum))], [(id "out",b)]
let iib_profile = [(id "i1",i);(id "i2",i)], [(id "out",b)]
let rrb_profile = [(id "i1",r);(id "i2",r)], [(id "out",b)]

(* let diese_profile = assert false *)

(** iterators profiles *)
(* [type_to_array_type [x1;...;xn] c] returns the array type [x1^c;...;xn^c] *)
let (type_to_array_type: Lic.var_info list -> int -> (Lv6Id.t * Lic.type_) list) =
  fun l c ->
    List.map (fun vi -> vi.var_name_eff, Array_type_eff(vi.var_type_eff,c)) l

(* Extract the node and the constant from a list of static args *)
let get_node_and_int_const
    (lxm: Lxm.t)  (sargs: Lic.static_arg list) : (Lic.node_key * int) =
  match sargs with
    | [ NodeStaticArgLic (_,nk); ConstStaticArgLic carg ] -> (
      let c = match carg with
        | (_, Int_const_eff c) ->  c
        | (_, Abstract_const_eff(_,_,Int_const_eff c, true)) -> c
        | (_, zcl) -> 
          let msg = "immediate integer expected, but get \""
            ^ (LicDump.string_of_const_eff false zcl)
            ^ "\"\n"
          in raise (Compile_error(lxm, msg))
      in
      (nk, int_of_string c)
    )
    | _ ->
      let msg = "*** an integer and a node are expected.\n" in
      raise (Compile_error(lxm, msg))

(*---------------------------------------------------------------------
Typers for predef macros/iterators
---------------------------------------------------------------------*)

let get_id_type vi = vi.var_name_eff, vi.var_type_eff


let _condact_profile
    (id_solver: IdSolver.t)
    (lxm: Lxm.t)
    (sargs: Lic.static_arg list)
    : Lic.node_profile =
  try
   (*--------------------------------------------------------------------
     CONDACT
     ----------------------------------------------------------------------
     Given : 
     - A node n of type:       a_1 * ... *  a_n ->  b_1 * ... * b_k
     - A (tuple) const:                             b_1 * ... * b_k
     Gen a node of type :  bool * a_1 * ... *  a_n ->  b_1 * ... * b_k  
     ---------------------------------------------------------------------*)
    let nk, dflt = 
      match sargs with
        | [NodeStaticArgLic(_,nk) ; ConstStaticArgLic(_,dflt)] -> nk, dflt
        | _ -> assert false
    in
   (* recherche le profil de nk ... *)
    let ne = IdSolver.node_exp_of_node_key id_solver nk lxm in
    let inlist = ne.inlist_eff in
    let outlist = ne.outlist_eff in

   (* dflt_types doit êre compatiple avec outlist *)
    let dflt_types = types_of_const dflt in
    let dl = List.length dflt_types in
    let ol = List.length outlist in
        Lv6Verbose.exe  ~level:3  (fun () ->
          Lv6Verbose.printf "  condact_profile: dflt=%s\n" 
                            (string_of_const_eff false dflt));
    let _ = if (dl <> ol) then
        raise_arity_error "in condact default arg" dl ol in
    let out_types = List.map (fun x -> x.var_type_eff) outlist in

    let _ = if dflt_types <> out_types then
        raise_type_error dflt_types out_types "in condact default arg" 
    in
   (* ok pour les args statiques, le profil dynamique est : *)
    (("_", Bool_type_eff)::(List.map get_id_type inlist), List.map get_id_type outlist)
  with
    | EvalType_error msg -> raise (Compile_error(lxm, "type error: "^msg))


let _map_profile
   (id_solver: IdSolver.t)
   (lxm: Lxm.t)
   (sargs: Lic.static_arg list)
: Lic.node_profile =
(*--------------------------------------------------------------------
MAP
----------------------------------------------------------------------
 Given : 
   - A node n of type:   a_1 * ... *  a_n ->  b_1 * ... * b_k
   - A (int) const c
Gen a node of type :     a_1^c * ... *  a_n^c ->  b_1^c * ... * b_k^c 
--------------------------------------------------------------------*)
   let (nk, c) = get_node_and_int_const lxm sargs in
   (* recherche le profil de nk ... *)
   let ne = IdSolver.node_exp_of_node_key id_solver nk lxm in
   let inlist = ne.inlist_eff in
   let outlist = ne.outlist_eff in
   let lti = type_to_array_type inlist c in
   let lto = type_to_array_type outlist c in
   let res = (lti, lto) in
      res

let _fillred_profile
   (id_solver: IdSolver.t)
   (lxm: Lxm.t)
   (sargs: Lic.static_arg list)
: Lic.node_profile =
(*--------------------------------------------------------------------
FILLRED
----------------------------------------------------------------------
 Given : 
   - A node   :   aa * a_1   * ... *  a_n   -> aa * b_1   * ... * b_k
   - An int c
Gen a node    :   aa * a_1^c * ... *  a_n^c -> aa * b_1^c * ... * b_k^c 
--------------------------------------------------------------------*)

   let (nk, c) = get_node_and_int_const lxm sargs in
   (* recherche le profil de nk ... *)
   let ne = IdSolver.node_exp_of_node_key id_solver nk lxm in
   let inlist = ne.inlist_eff in
   let outlist = ne.outlist_eff in

   let _ = assert(inlist <> [] && outlist <> []) in
   let lti = (get_id_type (List.hd inlist))::
     type_to_array_type (List.tl inlist) c in
   let lto = (get_id_type (List.hd outlist))::
     type_to_array_type (List.tl outlist) c in
   let (_id1, t1) = List.hd lti and (_id2, t2) = List.hd lto in
   let res = 
     if t1 = t2 then (lti,lto) else
       (* if they are not equal, they migth be unifiable *)
       match UnifyType.f [t1] [t2] with
         | Equal  -> (lti,lto)
         | Unif t -> 
             (List.map (fun (id,tid) -> id, subst_type t tid) lti,
              List.map (fun (id,tid) -> id, subst_type t tid) lto)
         | Ko(msg) -> raise (Compile_error(lxm, msg))
   in
     res

(* let fill_profile = fillred_profile *)
  (* Given 
     - a node N of type tau -> tau * teta_1 * ... * teta_l
     - a constant c (nb : sargs = [N,c])       
     returns the profile: [tau -> tau * teta_1^c * ... * teta_l^c] 
    *)

(* let red_profile = fillpred_profile *)
  (* Given 
     - a node N of type tau * tau_1 * ... * tau_n -> tau 
     - a constant c (nb : sargs = [N,c])
       returns the profile tau * tau_1^c * ... * tau_n^c -> tau
    *)


(* Given 
   - 3 integer constant i, j, k 
   
   returns the profile bool^k -> bool
*)
let _boolred_profile
   (_id_solver: IdSolver.t)
   (lxm: Lxm.t)
   (sargs: Lic.static_arg list)
: Lic.node_profile =
    let (get_three_constants: Lxm.t -> Lic.static_arg list -> string * string * string) =
      fun lxm sargs ->
        match sargs with
          | [ConstStaticArgLic(_,Int_const_eff i);
             ConstStaticArgLic(_,Int_const_eff j);
             ConstStaticArgLic(_,Int_const_eff k)] -> i,j,k
          | _ -> raise (Compile_error(lxm, "\n*** type error: 3 int were expected"))
    in
    let (_i,_j,k) = get_three_constants lxm sargs in
      [id "i1",  (Array_type_eff(Bool_type_eff, int_of_string k))], [id "out", b]


(*---------------------------------------------------------------------*)

let op2profile
(* BEQUILLE *)
   (_id_solver_opt: IdSolver.t option) (op: AstPredef.op) (_lxm: Lxm.t) 
   : Lic.node_profile =
    let res =
      match op with
        | TRUE_n | FALSE_n -> b_profile
        | ICONST_n _id      -> i_profile
        | RCONST_n _id      -> r_profile
        | NOT_n            -> bb_profile
        | REAL2INT_n       -> ri_profile
        | INT2REAL_n       -> ir_profile
        | IF_n             -> baaa_profile
        | UMINUS_n         -> oo_profile
        | IUMINUS_n        -> ii_profile
        | RUMINUS_n        -> rr_profile
        | IMPL_n | AND_n | OR_n | XOR_n  -> bbb_profile 
        | NEQ_n 
        | EQ_n  -> aab_profile 
        | RLT_n | RLTE_n | RGT_n | RGTE_n -> rrb_profile 
        | ILT_n | ILTE_n | IGT_n | IGTE_n -> iib_profile 
        | LT_n | LTE_n | GT_n | GTE_n -> oob_profile 
        | MINUS_n  |  PLUS_n |  TIMES_n |  SLASH_n | DIV_n -> ooo_profile 
        | RMINUS_n | RPLUS_n | RTIMES_n | RSLASH_n         -> rrr_profile
        | MOD_n | IMINUS_n | IPLUS_n | ISLASH_n | ITIMES_n -> iii_profile
        | NOR_n | DIESE_n  -> ab_profile
    in
      res

(* exported *)
(* VERSION GÉNÉRALE, valable pour les MACROS, et qui necessite donc
   un IdSolver.t
*)
let make_node_exp_eff (id_solver: IdSolver.t) (has_mem: bool option) (is_safe:bool)
    (op: op) (lxm: Lxm.t) : Lic.node_exp =
    let id = AstPredef.op_to_long op in
    let (lti,lto) = op2profile (Some id_solver) op lxm in
    let i = ref 0 in
    (* let is_polymorphic = ref false in *)
    let to_var_info_eff nature (vid, te) =
      let res =
        (* if Lic.is_polymorphic te then is_polymorphic := true ; *)
        {
          var_name_eff = vid;
          var_nature_eff = nature;
          var_number_eff = !i;
          var_type_eff   = te;
          var_clock_eff  = vid,BaseLic;
        }
      in
        incr i;
        res
    in
    let inlist_eff = List.map (to_var_info_eff AstCore.VarInput) lti in
    let outlist_eff = (i:=0;List.map (to_var_info_eff AstCore.VarOutput) lto) in
    let res = 
      {
        node_key_eff = id,[];
        inlist_eff   = inlist_eff;
        outlist_eff  = outlist_eff;
        loclist_eff  = None;
        def_eff      = ExternLic;
        has_mem_eff  = (match has_mem with Some b -> b | None -> false);
        is_safe_eff  = is_safe;
        lxm          = lxm;
        (* is_polym_eff =  *)
(*           List.exists (fun vi -> Lic.is_polymorphic vi.var_type_eff) inlist_eff || *)
(*           List.exists (fun vi -> Lic.is_polymorphic vi.var_type_eff) outlist_eff *)
          (* !is_polymorphic *)
      }
    in 
      res

(* VERSION SIMPLE, valable
   UNIQUEMENT pour les NON MACROS
*)
let make_simple_node_exp_eff (has_mem: bool option) (is_safe:bool)
    (op: op) (lxm: Lxm.t) : Lic.node_exp =
  let id = AstPredef.op_to_long op in
  let (lti,lto) = op2profile None op lxm in
  let i = ref 0 in
    (* let is_polymorphic = ref false in *)
  let to_var_info_eff nature (vid, te) =
    let res =
        (* if Lic.is_polymorphic te then is_polymorphic := true ; *)
      {
        var_name_eff = vid;
        var_nature_eff = nature;
        var_number_eff = !i;
        var_type_eff   = te;
        var_clock_eff  = vid,BaseLic;
      }
    in
    incr i;
    res
  in
  let inlist_eff = List.map (to_var_info_eff AstCore.VarInput) lti in
  let outlist_eff = (i:=0;List.map (to_var_info_eff AstCore.VarOutput) lto) in
  let res = 
    {
      node_key_eff = id,[] ;
      inlist_eff   = inlist_eff;
      outlist_eff  = outlist_eff;
      loclist_eff  = None;
      def_eff      = ExternLic;
      has_mem_eff  = (match has_mem with Some b -> b | None -> false );
      is_safe_eff  = is_safe;
      lxm          = lxm;
      (* is_polym_eff =  *)
      (*           List.exists (fun vi -> Lic.is_polymorphic vi.var_type_eff) inlist_eff || *)
      (*           List.exists (fun vi -> Lic.is_polymorphic vi.var_type_eff) outlist_eff *)
      (* !is_polymorphic *)
    }
  in 
  res

(* exported *)
let f (id_solver: IdSolver.t) (op: op) (lxm: Lxm.t) : typer = fun ll ->
      match op with
        | IF_n  ->  (
          (* VERRUE 1 *)
          (* j'arrive pas a traiter le if de facon generique (pour l'instant...) 
             a cause du fait que le if peut renvoyer un tuple.
          *)
          match ll with
            | [[Bool_type_eff]; t; e] -> 
              if t = e then t else 
                (type_error (List.flatten [[Bool_type_eff]; t; e]) "bool*any*any")
            | x -> (raise_arity_error "if/then/else" (List.length x) 3)
        )
        | (NOR_n | DIESE_n) -> 
          (* VERRUE 2 : cannot check the arity for them. *)
          let check_nary_iter acc ceff =
            match ceff with 
              | Array_type_eff(Bool_type_eff,_) -> acc
              | Bool_type_eff -> acc 
              | _ -> (type_error [ceff] "bool")
          in
          List.fold_left check_nary_iter () (List.flatten ll);
          [Bool_type_eff]
        | _ -> 
          (* general case *)
          let node_eff = make_node_exp_eff id_solver (Some false) true op lxm in
          let lti = List.map (fun v -> v.var_type_eff) node_eff.inlist_eff
          and lto = List.map (fun v -> v.var_type_eff) node_eff.outlist_eff in
          let l = List.flatten ll in
          if (List.length l <> List.length lti) then
            raise_arity_error (op2string op) (List.length l) (List.length lti)
          else if (l = []) then
            (* useless to call UnifyType.f ! *)
            lto
          else
            match UnifyType.f lti l with
              | Equal  -> lto
              | Unif (TypeVar Any) -> 
                raise_type_error l lti
                  "could not instanciate polymorphic type"
              | Unif (TypeVar AnyNum) -> 
                raise_type_error l lti
                  "could not instanciate overloaded type"
              | Unif t -> 
                List.map (subst_type t) lto

              | Ko(str) -> 
                raise_type_error l lti str

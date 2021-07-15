(* Time-stamp: <modified the 29/08/2019 (at 16:16) by Erwan Jahier> *)

(* *)

open Lic
open Lv6errors

(* exported *)
let get_node_and_int_const (lxm: Lxm.t) (sargs: Lic.static_arg list)
    : (Lic.node_key * int) =
  match sargs with
    | (NodeStaticArgLic (_,nk))::(ConstStaticArgLic carg)::_  -> (
      let c = match carg with
        | (_, Int_const_eff c) -> c
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

(* transforme en array une variable *)
let var_to_array (c:int) (vi: Lic.var_info) : Lic.var_info = 
  { vi with var_type_eff = Array_type_eff(vi.var_type_eff,c) }


(*
On a éventuellement besoin du node_exp des args 
*)
let rec do_node
    (nk2nd: Lic.node_key -> Lic.node_exp)
    (nk: Lic.node_key)
    (lxm: Lxm.t)
    : (Lic.node_exp) =
  let (pk,id) = fst nk in 
  match (pk, id) with
    | ("Lustre", "map") -> do_map nk2nd nk lxm
    | ("Lustre", "red")
    | ("Lustre", "fill")
    | ("Lustre", "fillred") -> do_fillred nk2nd nk lxm
    | ("Lustre", "boolred") -> do_boolred nk2nd nk lxm
    | ("Lustre", "condact") -> do_condact nk2nd nk lxm
    | _ -> raise Not_found

(*--------------------------------------------------------------------
  MAP
  ----------------------------------------------------------------------
  Given : 
  - A node n of type:   a_1 * ... *  a_n ->  b_1 * ... * b_k
  - A (int) const c
  Gen a node of type :     a_1^c * ... *  a_n^c ->  b_1^c * ... * b_k^c 
  --------------------------------------------------------------------*)
and do_map nk2nd nk lxm =
  let sargs = snd nk in
  let (np, c) = get_node_and_int_const lxm sargs in
  let nd = nk2nd np in
  let ins = nd.inlist_eff in
  let outs = nd.outlist_eff in
  {
    node_key_eff = nk;
    inlist_eff   = List.map (var_to_array c) ins;
    outlist_eff  = List.map (var_to_array c) outs;
    loclist_eff  = None;
    def_eff      = MetaOpLic;
    has_mem_eff  = nd.has_mem_eff; 
    is_safe_eff  = nd.is_safe_eff; 
    lxm = lxm;
  }
(*--------------------------------------------------------------------
  FILLRED
  ----------------------------------------------------------------------
  Given : 
  - A node   :   aa * a_1   * ... *  a_n   -> aa * b_1   * ... * b_k
  - An int c
  Gen a node    :   aa * a_1^c * ... *  a_n^c -> aa * b_1^c * ... * b_k^c 
  --------------------------------------------------------------------*)
and do_fillred nk2nd nk lxm =
  let sargs = snd nk in
  let (np, c) = get_node_and_int_const lxm sargs in
  let nd = nk2nd np in
  let ins = nd.inlist_eff in
  let outs = nd.outlist_eff in
  let _ = assert (ins <> [] && outs <> []) in
  let ins' = (List.hd ins)::(List.map (var_to_array c) (List.tl ins)) in
  let outs' = (List.hd outs)::(List.map (var_to_array c) (List.tl outs)) in
  (* pas d'unif : egalité et c'est tout ! *)
  let t1 = (List.hd ins').var_type_eff in
  let t2 = (List.hd outs').var_type_eff in
  if t1 <> t2 then
    let msg = Printf.sprintf
      "node can't be used in iterator, first input type '%s' differs from first output type '%s'"
      (LicDump.string_of_type_eff false t1)
      (LicDump.string_of_type_eff false t2)
    in
    raise (Compile_error(lxm, msg))
  else 
    {
      node_key_eff = nk;
      inlist_eff   = ins';
      outlist_eff  = outs';
      loclist_eff  = None;
      def_eff      = MetaOpLic;
      has_mem_eff  = nd.has_mem_eff; 
      is_safe_eff  = nd.is_safe_eff; 
      lxm = lxm;
    }
(*--------------------------------------------------------------------
  CONDACT
  ----------------------------------------------------------------------
  Given : 
  - A node n of type:       a_1 * ... *  a_n ->  b_1 * ... * b_k
  - A (tuple) const:                             b_1 * ... * b_k
  Gen a node of type :  bool * a_1 * ... *  a_n ->  b_1 * ... * b_k  
  ---------------------------------------------------------------------*)
(*
nb :

node condact_xx(c,i1,...,in) returns(res1,...,resk);
let
 res1,...,resk = condact<<node,(dft_res1,...,dft_resk)>>(c,i1,...,in)
tel

could be translated into

node condact_xx(c,i1,...,in) returns(res1,...,resk);
let
res1,...,resk =
  merge c (true -> node(i1,...,in))
          (false -> (dft_res1,...,dft_resk) fby (res1,...,resk)
tel

is it a good idea?
*)
and (do_condact : (Lic.node_key -> Lic.node_exp) -> node_key -> Lxm.t -> Lic.node_exp) =
fun nk2nd nk lxm -> 
  try
    let sargs = snd nk in
    let np, dflt =
      match sargs with
        | [NodeStaticArgLic(_,np) ; ConstStaticArgLic(_,dflt)] -> np, dflt
        | _ -> assert false
    in
    (* recherche le profil de np ... *)
    let ne = nk2nd np in
    let inlist = ne.inlist_eff in
    let outlist = ne.outlist_eff in
    (* dflt_types doit êre compatiple avec outlist *)
    let dflt_types = types_of_const dflt in
    let out_types = List.map (fun x -> x.var_type_eff) outlist in
    let matches = try
                    UnifyType.is_matched out_types dflt_types
      with UnifyType.Match_failed msg -> 
        raise (Compile_error(lxm,  "in condact default output "^msg))
    in
    let out_types = Lic.apply_type_matches matches out_types in
    let in_types = Lic.apply_type_matches  matches
      (Bool_type_eff::(List.map (fun x -> x.var_type_eff) inlist))
    in
    (* ok pour les args statiques, le profil dynamique est : *)

    let clk = Lic.create_var AstCore.VarInput Bool_type_eff "activate" in
    assert(in_types<>[]);
    let ins = clk::
      Lic.create_var_list AstCore.VarInput (List.tl in_types) in
    let outs = Lic.create_var_list AstCore.VarOutput out_types in
    {
      node_key_eff = nk;
      inlist_eff   = ins;
      outlist_eff  = outs;
      loclist_eff  = None;
      def_eff      = MetaOpLic;
      has_mem_eff  = ne.has_mem_eff; 
      is_safe_eff  = ne.is_safe_eff; 
      lxm = lxm;
    }
  with
    | LicEvalType.EvalType_error msg -> raise (Compile_error(lxm, "type error: "^msg))


(*--------------------------------------------------------------------
  BOOLRED
  ----------------------------------------------------------------------
  Given 
  - 3 integer constant i, j, k 
  
  returns the profile bool^k -> bool
  ---------------------------------------------------------------------*)
and do_boolred _nk2nd nk lxm =
  let sargs = snd nk in
  let (_i,_j,k) = match sargs with
    | [
      ConstStaticArgLic(_,Int_const_eff i);
      ConstStaticArgLic(_,Int_const_eff j);
      ConstStaticArgLic(_,Int_const_eff k)
    ] -> i,j,k
    | _ -> raise (Compile_error(lxm, "\n*** type error: 3 int were expected"))
  in
  let k = int_of_string k in
  let ins = Lic.create_var AstCore.VarInput  (Array_type_eff(Bool_type_eff,k)) "i1" in
  let outs = Lic.create_var AstCore.VarOutput  Bool_type_eff "out" in
  {
    node_key_eff = nk;
    inlist_eff   = [ins];
    outlist_eff  = [outs];
    loclist_eff  = None;
    def_eff      = MetaOpLic;
    has_mem_eff  = false;
    is_safe_eff  = true;
    lxm = lxm;
  }


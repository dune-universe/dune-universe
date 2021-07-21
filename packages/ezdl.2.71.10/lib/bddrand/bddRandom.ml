(* Time-stamp: <modified the 02/09/2019 (at 08:54) by Erwan Jahier> *)

(** todo : Parse constraints given in Lustre/Lutin like syntax *)
let (_contraints : string -> (string * bool) list) = fun _f -> assert false
let (_contraints_file : string -> (string * bool) list) = fun _f -> assert false

let verbose = ref 0

let empty = Value.OfIdent.empty 

(* make a comb from a list of var *)
let (get_vars_f : Exp.var list -> Exp.formula) =
  fun vl ->
  match vl with
  | [] -> assert false
  | v1::t -> 
  List.fold_left (fun acc v -> Exp.And (Exp.Bvar v, acc)) (Exp.Bvar v1) t

let (draw : ?number:int -> Exp.var list -> Exp.var list -> Exp.formula ->
     (string * bool) list list) =
  fun ?(number=1) bvl nvl f ->
    let bool_vars_to_gen: Exp.formula  = get_vars_f bvl in
    let output_var_names : Var.name list list = [List.map Var.name bvl] in
    let snt = Solver.init () in
    let _snt, outs_l =
      Solver.solve_formula snt empty empty !verbose "[BddRandom.draw]"
        output_var_names number (1, 0, AtMost 0) bool_vars_to_gen nvl f
    in
    let outs_l = fst (List.split outs_l) in (* throw numerics away for the time being *)
    let outs_l =
      List.map
        (fun outs -> Value.OfIdent.fold (fun n v acc -> (n,v)::acc) outs [])
        outs_l
    in
    let outs_l =
      List.map
        (fun outs ->
           List.map
             (fun (n,v)-> match v with Value.B b -> n,b | Value.N _ -> assert false) outs)
        outs_l
    in
    outs_l

 
(*********************************************************************************)

let draw_in_dimacs_file ?(number=1) f = 
  let ll, size = Dimacs.parse f in
  let vl,f = Dimacs.to_formula (ll,size) in
  let res = draw ~number:number vl [] f in
  res

(*-----------------------------------------------------------------------
** Copyright (C) - Verimag.
** This file may only be copied under the terms of the CeCill
** Public License
**-----------------------------------------------------------------------
**
** File: rif.ml
** Author: erwan.jahier@univ-grenoble-alpes.fr
*)

    
(*------------------------------------------------------------------------*)
let (to_subst_list : Value.OfIdent.t -> Data.subst list) =
  fun x -> 
    Value.OfIdent.fold
      (fun name value acc -> 
         (name, Value.to_data_val value)::acc
      )
      x
      []

(*------------------------------------------------------------------------*)

(* exported *)
let (read : bool -> in_channel -> out_channel option -> Exp.var list ->  Var.env_in) =
  fun _debug ic oc vars -> 
    let sl = RifIO.read ic oc
      (List.map (fun v -> Var.name v, Type.to_data_t (Var.typ v)) vars) 
    in
    let sl = List.map (fun (n,value) -> (n, Value.from_data_val value)) sl in
      Value.OfIdent.from_list sl

(*------------------------------------------------------------------------*)
(* exported *)
let (write : out_channel -> string -> unit) = RifIO.write

(* exported *)
let (flush : out_channel -> unit) = RifIO.flush

(*------------------------------------------------------------------------*)

let (write_interface : out_channel -> Exp.var list -> Exp.var list -> Exp.var list option -> 
      Exp.var list list option -> unit) =
  fun oc in_vars out_vars loc_vars_opt oracle_vars_opt -> 
    RifIO.write_interface oc
      (List.map (fun v -> Var.name v, Type.to_data_t (Var.typ v)) in_vars)
      (List.map (fun v -> Var.name v, Type.to_data_t (Var.typ v)) out_vars)
      (match loc_vars_opt with
         | None -> None
         | Some vars -> 
           Some (List.map (fun v -> Var.name v, Type.to_data_t (Var.typ v)) vars)
      )
      (match oracle_vars_opt with
         | None -> None
         | Some vars_l -> 
           Some (List.map 
                   (List.map (fun v -> Var.name v, Type.to_data_t (Var.typ v)))
                   vars_l)
      )


(*------------------------------------------------------------------------*)
(* exported *)


let (write_outputs : out_channel -> Exp.var list -> Value.OfIdent.t -> unit) =
  fun oc vars x ->
    RifIO.write_outputs oc Util.my_string_of_float
      (List.map (fun v -> Var.name v, Type.to_data_t (Var.typ v)) vars) 
      (to_subst_list x)


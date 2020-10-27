type var = string

exception Not_linear

module Var = struct
  type t = var
  let compare = String.compare
end

module VarMap = struct
  include Map.Make(Var)
end

type t = float VarMap.t * float
let atom s v : t = VarMap.singleton s v, 0.
let cst v : t = VarMap.empty, v
let add (e1,cst1) (e2,cst2) : t =
  let cst = cst1 +. cst2 in
  let aux _ v1 v2 =
    match v1, v2 with
    | None, _ -> v2
    | _, None -> v1
    | Some v1, Some v2 ->
      let r = v1 +. v2 in
      if classify_float r = FP_zero
      then None
      else Some r
  in
  VarMap.merge aux e1 e2, cst

let sub (e1,cst1) (e2,cst2) : t =
  let cst = cst1 -. cst2 in
  let aux _ v1 v2 =
    match v1, v2 with
    | None, Some v2 -> Some (-. v2)
    | _, None -> v1
    | Some v1, Some v2 ->
      let r = v1 -. v2 in
      if classify_float r = FP_zero
      then None
      else Some r
  in
  VarMap.merge aux e1 e2, cst

let mul_cst v (e,cst) =
  if classify_float v = FP_zero
  then VarMap.empty, 0.
  else VarMap.map (fun x -> x *. v) e, v *. cst

(* mul fail if one of the two expression is not a constant.  If it
   is not the case then this is not linear *)
let mul ((e1,cst1) as v1) ((e2,cst2) as v2) : t =
  match VarMap.is_empty e1, VarMap.is_empty e2 with
  | true, true -> VarMap.empty, cst1 *. cst2
  | true, false -> mul_cst cst1 v2
  | false, true -> mul_cst cst2 v1
  | _ -> raise Not_linear

let div_cst (e,cst) v =
  VarMap.map (fun x -> x /. v) e, cst /. v

let div v1 (e2,cst2) : t =
  if VarMap.is_empty e2
  then div_cst v1 cst2
  else raise Not_linear

let is_constant (e,_) = VarMap.is_empty e

let to_list (m,cst) = VarMap.bindings m, cst

let zero = cst 0.

let compare (e1,cst1) (e2,cst2) =
  let cmp = compare cst1 cst2 in
  if cmp = 0
  then VarMap.compare compare e1 e2
  else cmp

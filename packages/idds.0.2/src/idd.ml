open Base

type t = Dd.t
let ident = Dd.ctrue
let empty = Dd.cfalse
let equal = Dd.equal


module Pair = struct
  type t = int * int
    [@@deriving sexp, compare, hash]
end

type manager = {
  dd : Dd.manager;
  bdd : Bdd.manager;
  union_cache : (Pair.t, t) Hashtbl.t;
  seq_cache : (Pair.t, t) Hashtbl.t;
}

let manager ?bdd_mgr () : manager =
  let bdd = Option.value bdd_mgr ~default:(Bdd.manager ()) in
  { bdd;
    dd = Bdd.get_dd_manager bdd;
    union_cache = Hashtbl.create (module Pair);
    seq_cache = Hashtbl.create (module Pair);
  }

let get_bdd_manager mgr = mgr.bdd

let test mgr i b =
  Dd.branch mgr.dd (Var.inp i) (if b then Dd.ctrue else Dd.cfalse)
    (if b then Dd.cfalse else Dd.ctrue)

let set mgr i b =
  Dd.branch mgr.dd (Var.out i) (if b then Dd.ctrue else Dd.cfalse)
    (if b then Dd.cfalse else Dd.ctrue)

let rec eval' expl (tree:t) (env:Var.t -> bool) (n:int) =
  match tree with
  | False ->
    false
  | True ->
    Sequence.range 0 n
    |> Sequence.for_all ~f:(fun i ->
      Set.mem expl i || Bool.equal (env (Var.inp i)) (env (Var.out i))
    )
  | Branch { var; hi; lo; _ } ->
    let expl = if Var.is_inp var then expl else Set.add expl (Var.index var) in
    eval' expl (if (env var) then hi else lo) env n

let eval = eval' (Set.empty (module Int))


(* let enforce_ordered (t0 : t) : t =
  let rec ordered (x : Var.t) (t0 : t) =
    match t0 with
    | True | False -> true
    | Branch { var=y; hi; lo; _ } ->
      begin match Var.closer_to_root x y with
      | Left -> ordered y hi && ordered y lo
      | _ -> false
      end
  in
  match t0 with
  | True | False -> t0
  | Branch { var; hi; lo; _ } ->
    if ordered var hi && ordered var lo then t0 else
    failwith ("unordered: " ^ Dd.to_string t0) *)

let branch (mgr : manager) (x : Var.t) (hi : t) (lo : t) : t =
  (* enforce_ordered ( *)
  if Var.is_out x then
    begin match hi, lo with
    | False, False -> hi
    | _ -> Dd.branch mgr.dd x hi lo
    end
  else (* Var.is_inp var *)
    if equal hi lo then hi else
    let x' = Var.to_out x in
    (* make identity x=x' implicit *)
    let hi = match hi with
      | Branch { hi; lo=False; var; _ } when Var.(equal var x') -> hi
      | _ -> hi
    in
    let lo = match lo with
      | Branch { hi=False; lo; var; _ } when Var.(equal var x') -> lo
      | _ -> lo
    in
    (* is testing x redundant? *)
    if equal hi lo then hi else
    begin match hi, lo with
    | Branch { hi=False; lo=lo'; var; _ }, _
      when Var.(equal var x') && equal lo lo' ->
      hi
    | _, Branch { hi=hi'; lo=False; var; _ }
      when Var.(equal var x') && equal hi hi' ->
      lo
    | _ ->
      Dd.branch mgr.dd x hi lo
    end
  (* ) *)

let extract (d:t) (side:bool) : t =
  match d with
  | False | True -> d
  | Branch { hi; lo; _ } -> if side then hi else lo

(** [split d root] are the four subtrees of [d] corresponding to the four
    possible values of the variable pair [(inp root, out root)],
    i.e. [(1, 1), (1, 0), (0, 1), (0, 0)].
      - requires: [Var.idx_closer_to_root root (Dd.index d)] or [root = Dd.index d] *)
let split (d:t) (root:int) =
  if Var.idx_strictly_closer_to_root root (Dd.index d) then
    (d, empty, empty, d)
  else
    match d with
    | Branch { var; hi; lo; _ } when Var.is_out var ->
      (hi, lo, hi, lo)
    | Branch { hi; lo; _ } -> (* var is input variable *)
      let d11, d10 = if Dd.index hi = root then extract hi true, extract hi false
        else hi, empty in
      let d01, d00 = if Dd.index lo = root then extract lo true, extract lo false
        else empty, lo in
      d11, d10, d01, d00
    | _ -> failwith "Impossible" (* by precondition + if guard *)

let rec apply mgr (op : bool -> bool -> bool) (d0 : t) (d1 : t) =
  match d0, d1 with
  | False, False | False, True | True, False | True, True ->
    let val0 = equal d0 ident in
    let val1 = equal d1 ident in
    if op val0 val1 then ident else empty
  | Branch _, _ | _, Branch _ ->
    let root_index = if Var.idx_strictly_closer_to_root (Dd.index d0) (Dd.index d1)
      then Dd.index d0 else Dd.index d1 in
    let (d0_11, d0_10, d0_01, d0_00) = split d0 root_index in
    let (d1_11, d1_10, d1_01, d1_00) = split d1 root_index in
    Var.(branch mgr (inp root_index)
           (branch mgr (out root_index) (apply mgr op d0_11 d1_11)
              (apply mgr op d0_10 d1_10))
           (branch mgr (out root_index) (apply mgr op d0_01 d1_01)
              (apply mgr op d0_00 d1_00)))

let rec union mgr (d0 : t) (d1 : t) =
  match d0, d1 with
  | False, d | d, False ->
    d
  | True, True ->
    d0
  | _ ->
    let id0, id1 = Dd.id d0, Dd.id d1 in
    (* union is idempotent... *)
    if id0 = id1 then d0 else
    (* ... and commutative *)
    let key = if id0 <= id1 then (id0, id1) else (id1, id0) in
    Hashtbl.find_or_add mgr.union_cache key ~default:(fun () ->
      let root_index = if Var.idx_strictly_closer_to_root (Dd.index d0) (Dd.index d1)
        then Dd.index d0 else Dd.index d1 in
      let (d0_11, d0_10, d0_01, d0_00) = split d0 root_index in
      let (d1_11, d1_10, d1_01, d1_00) = split d1 root_index in
      Var.(branch mgr (inp root_index)
             (branch mgr (out root_index) (union mgr d0_11 d1_11)
                (union mgr d0_10 d1_10))
             (branch mgr (out root_index) (union mgr d0_01 d1_01)
                (union mgr d0_00 d1_00)))
    )

let rec seq mgr (d0:t) (d1:t) =
  match d0, d1 with
  | (False as d), _ | _, (False as d)
  | True, d | d, True ->
    d
  | Branch { id=id1; _ }, Branch { id=id2; _ } ->
    Hashtbl.find_or_add mgr.seq_cache (id1, id2) ~default:(fun () ->
      let root_index = if Var.idx_strictly_closer_to_root (Dd.index d0) (Dd.index d1)
        then Dd.index d0 else Dd.index d1 in
      let (d0_11, d0_10, d0_01, d0_00) = split d0 root_index in
      let (d1_11, d1_10, d1_01, d1_00) = split d1 root_index in
      branch mgr (Var.inp root_index)
        (branch mgr (Var.out root_index)
          (union mgr (seq mgr d0_11 d1_11) (seq mgr d0_10 d1_01))
          (union mgr (seq mgr d0_11 d1_10) (seq mgr d0_10 d1_00)))
        (branch mgr (Var.out root_index)
          (union mgr (seq mgr d0_01 d1_11) (seq mgr d0_00 d1_01))
          (union mgr (seq mgr d0_01 d1_10) (seq mgr d0_00 d1_00)))
    )

let star mgr (d0:t) =
  let rec loop curr prev =
    if equal curr prev then prev else
      loop (seq mgr curr curr) curr
  in
  loop (union mgr ident d0) ident

let subseteq mgr (d0:t) (d1:t) =
  equal (union mgr d0 d1) d1

let of_bdd (bdd:Bdd.t) : t = (bdd :> Dd.t)



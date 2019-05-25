open Base

type t = Dd.t

let rec eval (t : t) ~(env : Var.t -> bool) : bool =
  match t with
  | True -> true
  | False -> false
  | Branch { var; hi; lo; _ } ->
    if env var then eval hi env else eval lo env

let equal = Dd.equal
let ctrue = Dd.ctrue
let cfalse = Dd.cfalse

module Pair = struct
  type t = int * int
    [@@deriving sexp, compare, hash]
end

type manager = {
  dd : Dd.manager;
  conj_cache : (Pair.t, t) Hashtbl.t;
  disj_cache : (Pair.t, t) Hashtbl.t;
  neg_cache : (int, t) Hashtbl.t
}

let manager () : manager = {
  dd = Dd.manager ();
  conj_cache = Hashtbl.create (module Pair);
  disj_cache = Hashtbl.create (module Pair);
  neg_cache = Hashtbl.create (module Int);
}

let branch mgr var hi lo =
  if Dd.equal hi lo then
    hi
  else
    Dd.branch mgr var hi lo

let conj mgr =
  let rec conj (d1 : t) (d2 : t) : t =
    match d1, d2 with
    | False, _ | _, False ->
      Dd.cfalse
    | True, w | w, True ->
      w
    | Branch { var=x1; hi=hi1; lo=lo1; id=id1 },
      Branch { var=x2; hi=hi2; lo=lo2; id=id2 } ->
      (* conjunction is idempotent... *)
      if id1 = id2 then d1 else
      (* ...and commutative *)
      let key = if id1 <= id2 then (id1, id2) else (id2, id1) in
      Hashtbl.find_or_add mgr.conj_cache key ~default:(fun () ->
        match Var.closer_to_root x1 x2 with
        | Equal ->
          branch mgr.dd x1 (conj hi1 hi2) (conj lo1 lo2)
        | Left ->
          branch mgr.dd x1 (conj hi1 d2) (conj lo1 d2)
        | Right ->
          branch mgr.dd x2 (conj d1 hi2) (conj d1 lo2)
      )
  in
  conj

let disj mgr =
  let rec disj (d1 : t) (d2 : t) : t =
    match d1, d2 with
    | False, w | w, False ->
      w
    | True, _ | _, True ->
      Dd.ctrue
    | Branch { var=x1; hi=hi1; lo=lo1; id=id1 },
      Branch { var=x2; hi=hi2; lo=lo2; id=id2 } ->
      (* disjunction is idempotent... *)
      if id1 = id2 then d1 else
      (* ...and commutative *)
      let key = if id1 <= id2 then (id1, id2) else (id2, id1) in
      Hashtbl.find_or_add mgr.disj_cache key ~default:(fun () ->
        match Var.closer_to_root x1 x2 with
        | Equal ->
          branch mgr.dd x1 (disj hi1 hi2) (disj lo1 lo2)
        | Left ->
          branch mgr.dd x1 (disj hi1 d2) (disj lo1 d2)
        | Right ->
          branch mgr.dd x2 (disj d1 hi2) (disj d1 lo2)
      )
  in
  disj

let neg mgr =
  let rec neg (d : t) =
    match d with
    | True -> Dd.cfalse
    | False -> Dd.ctrue
    | Branch { var; hi; lo; id } ->
      begin match Hashtbl.find mgr.neg_cache id with
      | Some d ->
        d
      | None ->
        let neg_d = Dd.branch mgr.dd var (neg hi) (neg lo) in
        Hashtbl.add_exn mgr.neg_cache ~key:id ~data:neg_d;
        (* neg (neg d) = d *)
        Hashtbl.add_exn mgr.neg_cache ~key:Dd.(id neg_d) ~data:d;
        neg_d
      end
  in
  neg

let ite mgr var hi lo =
  disj mgr
    (conj mgr (branch mgr.dd var ctrue cfalse) hi)
    (conj mgr (branch mgr.dd var cfalse ctrue) lo)



module Make () : Boolean.Algebra with type t = t = struct
  let vars : (string, int) Hashtbl.t = Hashtbl.create (module String)
  let next_id = ref 0
  let declare_var s =
    if Hashtbl.mem vars s then `Duplicate else
    let id = !next_id in
    Int.incr next_id;
    Hashtbl.add_exn vars ~key:s ~data:id;
    `Ok
  let mgr = manager ()

  type nonrec t = t
  let tru = Dd.ctrue
  let fls = Dd.cfalse
  let of_bool = function
    | true -> tru
    | false -> fls
  let var s =
    let var = Var.inp (Hashtbl.find_exn vars s) in
    Dd.branch mgr.dd var tru fls
  let ( && ) = conj mgr
  let ( || ) = disj mgr
  let ( ! ) = neg mgr
  let ( == ) = equal
end

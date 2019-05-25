open Base

type t =
  | True
  | False
  | Branch of { var : Var.t; hi : t; lo: t; id: int }

let id (t : t) : int =
  match t with
  | False -> -2
  | True -> -1
  | Branch { id; _ } -> id

let index (d:t) : int =
  match d with
  | True | False -> Var.leaf_idx
  | Branch { var } -> Var.index var


module Triple = struct
  type t = Var.t * int * int
    [@@deriving hash, compare, sexp]
end

type manager = {
  mutable next_id : int;
  branch_cache : (Triple.t, t) Hashtbl.t;
}

let manager () = {
  next_id = 0;
  branch_cache = Hashtbl.create (module Triple);
}

let ctrue = True
let cfalse = False

let branch (mgr : manager) (var : Var.t) (hi : t) (lo : t) : t =
  let triple = (var, id hi, id lo) in
  Hashtbl.find_or_add mgr.branch_cache triple ~default:(fun () ->
    let id = mgr.next_id in
    mgr.next_id <- id + 1;
    Branch { var; hi; lo; id; }
  )

let equal (t1 : t) (t2 : t) : bool =
  match t1, t2 with
  | True, True | False, False ->
    true
  | Branch { id=id1; _ }, Branch { id=id2; _ } ->
    id1 = id2
  | _ ->
    false

let rec to_string (t : t) : string =
  match t with
  | True -> "1"
  | False -> "0"
  | Branch { var; hi; lo } ->
    Caml.Format.sprintf "(%s ? %s : %s)"
      (Var.to_string var)
      (to_string hi)
      (to_string lo)

(***********************************************************************)
(*                                                                     *)
(*                           FaCiLe                                    *)
(*                 A Functional Constraint Library                     *)
(*                                                                     *)
(*            Nicolas Barnier, Pascal Brisset, LOG, CENA               *)
(*                                                                     *)
(* Copyright 2004 CENA. All rights reserved. This file is distributed  *)
(* under the terms of the GNU Lesser General Public License.           *)
(***********************************************************************)
(* $Id: fcl_invariant.ml,v 1.2 2004/08/12 15:22:07 barnier Exp $ *)

type ('a, 'b) t = {
    x : 'a Fcl_stak.ref; 
    event : Fcl_cstr.event;
    id : int;
    name : string
  }
type setable
type unsetable
type 'a setable_t = ('a, setable) t
type 'a unsetable_t = ('a, unsetable) t

let gen_int = Fcl_misc.gen_int_fun ()
let create ?(name = "") v =
  let id = gen_int () in
  let name = if name = "" then Printf.sprintf "_%d" id else name in
  { 
    x = Fcl_stak.ref v;
    event = Fcl_cstr.new_event ();
    id = id;
    name = name
  }
let constant = create
let get r = Fcl_stak.get r.x
let event r = r.event
let name r = r.name
let id r = r.id
let fprint c ?(printer = (fun _ _ -> ())) r =
  Printf.fprintf c "%s%a" r.name printer (get r)
let set r v =
  let old = Fcl_stak.get r.x in
  if old <> v then begin
    Fcl_stak.set r.x v;
    Fcl_cstr.schedule r.event;
    Fcl_cstr.wake_all ()
  end 

let unary ?(name = "Invariant.unary") f x =
  let yname = Printf.sprintf "%s(%s)" name x.name in
  let y = create ~name:yname (f (get x)) in
  let update _ =
    set y (f (get x));
    false
  and delay c =
    Fcl_cstr.register (event x) c in
  Fcl_cstr.post (Fcl_cstr.create ~name update delay);
  y

let sum array =
  let name = "Invariant.sum" in
  let n = Array.length array in
  if n = 0 then raise (Invalid_argument name);

  let rname = Printf.sprintf "sum(%s...%s)" array.(0).name array.(n-1).name in
  let r =
    create ~name:rname (Array.fold_left (fun r x -> get x + r) 0 array)
  and lasts = Array.map get array in
  let update i =
    let new_ai = get array.(i) in
    set r (get r - lasts.(i) + new_ai);
    Fcl_data.Array.set lasts i new_ai;
    false

  and delay c =
    Array.iteri
      (fun i ai -> Fcl_cstr.register (event ai) ~waking_id:i c)
      array in

  let c = Fcl_cstr.create ~name ~nb_wakings:n update delay in
  Fcl_cstr.post c;
  r

let prod array =
  let name = "Invariant.prod" in
  let n = Array.length array in
  if n = 0 then raise (Invalid_argument name);

  let rname = Printf.sprintf "prod(%s...%s)" array.(0).name array.(n-1).name in
  let r = create ~name:rname (Array.fold_left (fun r x -> get x * r) 1 array)
  and lasts = Array.map get array in
  let update i =
    let new_ai = get array.(i) in
    if lasts.(i) <> 0 then
      set r (get r / lasts.(i) * new_ai)
    else begin
      assert(new_ai <> 0);
      set r (Array.fold_left (fun r x -> get x * r) 1 array) end;
    Fcl_data.Array.set lasts i new_ai;
    false

  and delay c =
    Array.iteri
      (fun i ai -> Fcl_cstr.register (event ai) ~waking_id:i c)
      array in

  let c = Fcl_cstr.create ~name ~nb_wakings:n update delay in
  Fcl_cstr.post c;
  r

let binary ?(name = "Invariant.binary") f x y =
  let zname = Printf.sprintf "%s_%s" x.name y.name in
  let z = create ~name:zname (f (get x) (get y)) in
  let update _ =
    set z (f (get x) (get y));
    false
  and delay c =
    Fcl_cstr.register (event x) c; Fcl_cstr.register (event y) c; in
  Fcl_cstr.post (Fcl_cstr.create ~name update delay);
  z

let ternary ?(name = "Invariant.ternary") f x y t =
  let zname = Printf.sprintf "%s_%s_%s" x.name y.name t.name in
  let z = create ~name:zname(f (get x) (get y) (get t)) in
  let update _ =
    set z (f (get x) (get y) (get t));
    false
  and delay c =
    Fcl_cstr.register (event x) c; Fcl_cstr.register (event y) c;
    Fcl_cstr.register (event t) c in
  Fcl_cstr.post (Fcl_cstr.create ~name update delay);
  z


module Array = struct
let argmin array f =
  let name = "Invariant.Array.argmin" in
  let n = Array.length array in
  if n = 0 then raise (Invalid_argument name);
  let idxname =
    Printf.sprintf "argmin(%s...%s)" array.(0).name array.(n-1).name in
  if n = 1 then constant ~name:idxname 0 else

  let values = Array.map (fun ai -> f (get ai)) array in
  
  let module Ord =
    struct
      type t = int
      let compare i j =	compare (values.(i), i) (values.(j), j)
    end in
  let module S = Set.Make(Ord) in

  let s = Fcl_stak.ref (Fcl_misc.goedel S.add n S.empty) in
  let idx_min = create (S.min_elt (Fcl_stak.get s)) in

  let update i =
    let last_idx_min = get idx_min in
    let last_min = values.(last_idx_min) in
    let s' = S.remove i (Fcl_stak.get s) in
    Fcl_data.Array.set values i (f (get array.(i)));
    let s'' = S.add i s' in
    Fcl_stak.set s s'';
    if (values.(i), i) < (last_min, last_idx_min) then
      set idx_min i
    else if i = last_idx_min then
      set idx_min (S.min_elt s'');
    false
  and delay c =
    Array.iteri
      (fun i ai -> Fcl_cstr.register (event ai) ~waking_id:i c)
      array in

  Fcl_cstr.post (Fcl_cstr.create ~name:"Invariant.Array.argmin" ~nb_wakings:n update delay);
  
  idx_min


  let unary_get a idx =
    let name = "Invariant.Array.unary_get" in
    let n = Array.length a in
    if n = 0 then raise (Invalid_argument name);

    let rname =
      Printf.sprintf "unary_get(%s...%s).(%s)"
	a.(0).name a.(n-1).name idx.name in
    let r = create ~name:rname (get a.(get idx)) in
    let update _ = set r (get a.(get idx)); false
    and delay c = Fcl_cstr.register (event idx) c in
    Fcl_cstr.post (Fcl_cstr.create ~name update delay);
    r

  let min a f = unary_get a (argmin a f)

  let get array idx =
    let name = "Invariant.Array.get" in
    let n = Array.length array in
    if n = 0 then raise (Invalid_argument name);

    let rname =
      Printf.sprintf "get(%s...%s).(%s)"
	array.(0).name array.(n-1).name idx.name in
    let r = create ~name:rname (get array.(get idx)) in
    let update i =
      let nidx = get idx in
      assert(0 <= nidx && nidx < n);
      if i = n then set r (get array.(nidx))
      else if i = nidx then set r (get array.(nidx));
      false

    and delay c =
      Array.iteri
	(fun i ai -> Fcl_cstr.register (event ai) ~waking_id:i c)
      array;
      Fcl_cstr.register (event idx) ~waking_id:n c in

    let c = Fcl_cstr.create ~name ~nb_wakings:(n + 1) update delay in
    Fcl_cstr.post c;
    r
end

module type FD = sig
  type fd
  type elt
  val min : fd -> elt unsetable_t
  val max : fd -> elt unsetable_t
  val size : fd -> int unsetable_t
  val is_var : fd -> bool unsetable_t
  val unary : ?name:string -> (fd -> 'a) -> fd -> 'a unsetable_t
end

module MakeFd(Fd : Fcl_var.BASICFD)(Attr : Fcl_var.ATTR with type event = Fd.event) = struct
  type fd = Fd.t
  type elt = Fd.elt
  let min v =
    let name = Printf.sprintf "fd_min(%s)" (Fd.name v) in
    let inv = create ~name (Fd.min v) in
    let update _ = set inv (Fd.min v); not (Fd.is_var v)
    and delay c = Fd.delay [Fd.on_min] v c in
    Fcl_cstr.post (Fcl_cstr.create ~name:"Invariant.XxxFd.min" update delay);
    inv
  let max v =
    let name = Printf.sprintf "fd_max(%s)" (Fd.name v) in
    let inv = create ~name (Fd.max v) in
    let update _ = set inv (Fd.max v); not (Fd.is_var v)
    and delay c = Fd.delay [Fd.on_max] v c in
    Fcl_cstr.post (Fcl_cstr.create ~name:"Invariant.XxxFd.max" update delay);
    inv
  let size v =
    let name = Printf.sprintf "fd_size(%s)" (Fd.name v) in
    let inv = create ~name (Fd.size v) in
    let update _ = set inv (Fd.size v); not (Fd.is_var v)
    and delay c = Fd.delay [Fd.on_refine] v c in
    Fcl_cstr.post (Fcl_cstr.create ~name:"Invariant.XxxFd.size" update delay);
    inv
  let is_var v =
    let name = Printf.sprintf "fd_is_var(%s)" (Fd.name v) in
    let inv = create ~name (Fd.is_var v) in
    let update _ = set inv (Fd.is_var v); not (Fd.is_var v)
    and delay c = Fd.delay [Fd.on_subst] v c in
    Fcl_cstr.post (Fcl_cstr.create ~name:"Invariant.XxxFd.is_var" update delay);
    inv
  let unary ?(name = "Invariant.XxxFd.unary") h v =
    let invname = Printf.sprintf "%s(%s)" name (Fd.name v) in
    let inv = create ~name:invname (h v) in
    let update _ = set inv (h v); not (Fd.is_var v)
    and delay c = Fd.delay [Fd.on_refine] v c in
    Fcl_cstr.post (Fcl_cstr.create ~name update delay);
    inv
end

module Fd = MakeFd(Fcl_var.Fd)(Fcl_var.Attr)
module SetFd = MakeFd(Fcl_var.SetFd)(Fcl_var.SetAttr)


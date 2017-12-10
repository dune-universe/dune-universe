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
(* $Id: fcl_var.ml,v 1.49 2004/09/03 13:25:55 barnier Exp $ *)

module C = Fcl_cstr

module type DOMAIN = sig
  type elt
  type t
  val compare_elt : elt -> elt -> int
  val fprint_elt : out_channel -> elt -> unit
  val fprint : out_channel -> t -> unit
  val min : t -> elt
  val max : t -> elt
  val mem : elt -> t -> bool
  val interval : elt -> elt -> t
  val remove_low : elt -> t -> t
  val remove_up : elt -> t -> t
  val included : t -> t -> bool
  val min_max : t -> elt * elt
  val size : t -> int
  val strictly_inf : elt -> elt -> bool
      (* [strictly_inf x1 x2] checks if [x1 < x2] when it is already known that [x1 <= x2] *)
end

type 'a attr = {
    dom : 'a Fcl_stak.ref;
    on_refine : Fcl_cstr.event; 
    on_min : Fcl_cstr.event; 
    on_max : Fcl_cstr.event; 
    on_subst : Fcl_cstr.event;
    id : int;
    name : string
  }

let gen_int = Fcl_misc.gen_int_fun ()

module MakeAttr(Dom : DOMAIN) = struct
  module Domain = Dom
  type elt = Domain.elt
  type domain = Domain.t
  type t = Domain.t attr
  let create ?(name = "") domain =
    let id = gen_int () in
    let name = if name = "" then Printf.sprintf "_%d" id else name in
    {
      dom = Fcl_stak.ref domain;
      on_refine = Fcl_cstr.new_event (); 
      on_min = Fcl_cstr.new_event (); 
      on_max = Fcl_cstr.new_event (); 
      on_subst = Fcl_cstr.new_event ();
      id = id;
      name = name
    } 

  let dom a = Fcl_stak.get a.dom
  let fprint c a = 
    Printf.fprintf c "%s%a" a.name Domain.fprint (Fcl_stak.get a.dom)
  let min a = Domain.min (Fcl_stak.get a.dom)
  let max a = Domain.max (Fcl_stak.get a.dom)
  let member a y = Domain.mem y (Fcl_stak.get a.dom)
  let id a = a.id

  let count_cstrs l =
    let n = List.length l in
    let h = Hashtbl.create n in
    List.iter
      (fun (c, _) ->
	if not (C.is_solved c) then Hashtbl.replace h (C.id c) ())
      l;
    Hashtbl.fold (fun _ _ r -> 1 + r) h 0

  let constraints_number a =
(* Not all constraints are suspended on on_subst (e.g. <~), so
   all the constraints lists have to be taken into account *)
    let cstrs_subst = C.registered a.on_subst
    and cstrs_min = C.registered a.on_min
    and cstrs_max = C.registered a.on_max
    and cstrs_refine = C.registered a.on_refine in
    let cstrs =
      List.concat [cstrs_subst; cstrs_min; cstrs_max; cstrs_refine] in
    count_cstrs cstrs

  type event = t -> Fcl_cstr.event
  let on_refine a = a.on_refine
  let on_subst a = a.on_subst
  let on_min a = a.on_min
  let on_max a = a.on_max
  let size a = Domain.size (Fcl_stak.get a.dom)
end

module type CONCRETEATTR = sig
  module Domain : DOMAIN
  type t = Domain.t attr
  type event
  val create : ?name:string -> Domain.t -> t
  val dom : t -> Domain.t
  val on_refine : event
  val on_subst : event
  val on_min : event
  val on_max : event
  val fprint : out_channel -> t -> unit
  (*val min : t -> Domain.elt*)
  val max : t -> Domain.elt
  val member : t -> Domain.elt -> bool
  val id : t -> int
  (*val constraints_number : t -> int*)
  val size : t -> int
end

module type ATTR = sig
  type elt
  type domain
  type t
  type event
  val dom : t -> domain
  val on_refine : event
  val on_subst : event
  val on_min : event
  val on_max : event
  val fprint : out_channel -> t -> unit
  val min : t -> elt
  val max : t -> elt
  val member : t -> elt -> bool
  val id : t -> int
  val constraints_number : t -> int
  val size : t -> int
end

module Attr = MakeAttr(Fcl_domain)
module SetAttr = MakeAttr(Fcl_setDomain)
module FloatAttr = MakeAttr(Fcl_float)

type ('a, 'b) concrete = Unk of 'a | Val of 'b

module MakeFd(Attr : CONCRETEATTR) = struct
  module D = Attr.Domain
  type attr = Attr.t
  type event = Attr.event
  type domain = D.t
  type elt = D.elt
  type t = (attr, elt) concrete Fcl_stak.ref
  let elt i = Fcl_stak.ref (Val i)
  let int = elt
  let create ?name domain =
    Fcl_stak.ref
      (match D.size domain with
	   0 -> Fcl_stak.fail "Var.XxxFd.create: empty initial domain"
	 | 1 -> Val (D.min domain)
	 | _ -> Unk (Attr.create ?name domain))

  let interval ?name min max = create ?name (D.interval min max)

  let array ?name n min max =
    let dom = D.interval min max in
    let name_elt =
      match name with
	None -> fun _i -> None
      |	Some n -> fun i -> Some (Printf.sprintf "%s_%d" n i) in
    Array.init n (fun i -> create ?name:(name_elt i) dom)

  let subst v new_v =
    match Fcl_stak.get v with
      Unk a ->
      	if D.mem new_v (Attr.dom a)
      	then begin
	  Fcl_stak.set v (Val new_v);
	  Fcl_cstr.schedule a.on_subst;
	  Fcl_cstr.schedule a.on_refine;
	  Fcl_cstr.schedule a.on_min;
	  Fcl_cstr.schedule a.on_max;
	  Fcl_cstr.wake_all ()
      	end
      	else Fcl_stak.fail "Var.XxxFd.subst"
    | Val _ ->
	Fcl_debug.fatal_error "XxxFd.subst: bound variable (use XxxFd.unify on possibly bound variable)"
    
  let value (v : t) = Fcl_stak.get v

  let fprint c v =
    match Fcl_stak.get v with
      Val t -> D.fprint_elt c t
    | Unk a -> Attr.fprint c a

  let refine v new_a =
    match value v with
      Unk a ->
      	let vala = Attr.dom a in
	Fcl_debug.call 't' (fun s -> Printf.fprintf s "refine %a with %a\n" fprint v D.fprint new_a);
      	assert (D.included new_a vala);
	let new_size = D.size new_a in
      	begin match new_size with
	  0 -> Fcl_stak.fail "Var.XxxFd.refine"
	| 1 -> subst v (D.min new_a)
	| _ ->
	    if new_size <> D.size vala then begin
      	      Fcl_stak.set a.dom new_a;
      	      Fcl_cstr.schedule a.on_refine;
      	      if D.strictly_inf (D.min vala) (D.min new_a) then
		Fcl_cstr.schedule a.on_min;
      	      if D.strictly_inf (D.max new_a) (D.max vala) then
		Fcl_cstr.schedule a.on_max;
	      Fcl_cstr.wake_all () end end
    | Val v ->
	if not (D.mem v new_a) then Fcl_stak.fail "Var.XxxFd.refine"

  let unify (v : t) new_v =
    match Fcl_stak.get v with
      Val v -> if not (v = new_v) then Fcl_stak.fail "Var.XxxFd.unify"
    | Unk _a -> subst v new_v

  let unify_cstr var value =
    let update _ = unify var value; true
    and delay _ = () in (* Solved when posted *)
    Fcl_cstr.create ~name:"unify_cstr" ~priority:Fcl_cstr.immediate update delay

  (* refinements shortcuts to avoid explicit Fd.value matchings *)
  let refine_up x x_max =
    match value x with
      Val x -> if D.compare_elt x x_max > 0 then Fcl_stak.fail "Var.XxxFd.refine_up"
    | Unk a -> refine x (D.remove_up x_max (Attr.dom a))

  let refine_low x x_min =
    match value x with
      Val x -> if D.compare_elt x x_min < 0 then Fcl_stak.fail "Var.XxxFd.refine_low"
    | Unk a -> refine x (D.remove_low x_min (Attr.dom a))
	  
  let refine_low_up z z_min z_max =
    Fcl_debug.call 'v'
      (fun s -> Printf.fprintf s "Var.XxxFd.refine_min_max: %a %a\n"
	  D.fprint_elt z_min D.fprint_elt z_max);
    match value z with
      Val x ->
	if D.compare_elt x z_min < 0 || D.compare_elt x z_max > 0 then
	  Fcl_stak.fail "Var.XxxFd.refine_low_up"
    | Unk a ->
	refine z (D.remove_up z_max (D.remove_low z_min (Attr.dom a)))

  let fprint_array c vs =
    let n = Array.length vs in
    Printf.fprintf c "[|";
    for i = 0 to n - 2 do
      Printf.fprintf c "%a; " fprint vs.(i)
    done;
    if n = 0 then Printf.fprintf c "|]" else
    Printf.fprintf c "%a|]" fprint vs.(n-1)

  let min v =
    match Fcl_stak.get v with
      Val t -> t
    | Unk a -> D.min (Fcl_stak.get a.dom)
	  
  let max v =
    match Fcl_stak.get v with
      Val t -> t
    | Unk a -> Attr.max a

  let min_max v =
    match Fcl_stak.get v with
      Val t -> (t,t)
    | Unk a -> D.min_max (Fcl_stak.get a.dom)
	  
  let is_var v =
    match Fcl_stak.get v with
      Val _x -> false
    | Unk _a -> true

  let is_bound v = not (is_var v)
	  
  let elt_value v =
    match Fcl_stak.get v with
      Val x -> x
    | Unk a -> Fcl_debug.fatal_error ("Var.XxxFd.elt_value: unbound variable: " ^ a.name)
  let int_value = elt_value
	  
  let id v =
    match Fcl_stak.get v with
      Val _x -> Fcl_debug.fatal_error "Var.XxxFd.id: bound variable"
    | Unk a -> a.id

  let name v =
    match Fcl_stak.get v with
      Val _x -> Fcl_debug.fatal_error "Var.XxxFd.name: bound variable"
    | Unk a -> a.name

  let member v x =
    match Fcl_stak.get v with
      Val v -> v = x
    | Unk a -> Attr.member a x

  let compare v1 v2 =
    match Fcl_stak.get v1, Fcl_stak.get v2 with
      (Val n1, Val n2) -> compare n1 n2
    | (Val _, Unk _) -> -1
    | (Unk _, Val _) -> 1
    | (Unk a1, Unk a2) -> compare (Attr.id a1) (Attr.id a2)

  let equal v1 v2 = compare v1 v2 = 0

  let on_refine = Attr.on_refine
  let on_subst = Attr.on_subst
  let on_min = Attr.on_min
  let on_max = Attr.on_max

  let delay es x ?waking_id c =
    match value x with
      Val _ -> ()
    | Unk a ->
      	Fcl_cstr.delay (List.map (fun e -> e a) es) ?waking_id c
  let size v =
    match Fcl_stak.get v with
      Val _ -> 1
    | Unk a -> Attr.size a
end


module type BASICFD = sig
  type t
  type elt
  type domain
  type attr
  type event
  val create : ?name:string -> domain -> t
  val interval : ?name:string -> elt -> elt -> t
  val array : ?name:string -> int -> elt -> elt -> t array
  val elt : elt -> t
  val is_var : t -> bool
  val is_bound : t -> bool
  val value : t -> (attr, elt) concrete
  val min : t -> elt
  val max : t -> elt
  val min_max : t -> elt * elt
  val elt_value : t -> elt
  val int_value : t -> elt
  val size : t -> int
  val member : t -> elt -> bool
  val id : t -> int
  val name : t -> string
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val fprint : out_channel -> t -> unit
  val fprint_array : out_channel -> t array -> unit
  val unify : t -> elt -> unit
  val refine : t -> domain -> unit
  val refine_low : t -> elt -> unit
  val refine_up : t -> elt -> unit
  val refine_low_up : t -> elt -> elt -> unit
  val on_refine : event
  val on_subst : event
  val on_min : event
  val on_max : event      
  val delay : event list -> t -> ?waking_id:int -> Fcl_cstr.t -> unit
  val int : elt -> t
  val subst : t -> elt -> unit
  val unify_cstr : t -> elt -> Fcl_cstr.t
end

module type FD = sig
  include BASICFD
  val remove : t -> elt -> unit
  val values : t -> elt list
  val iter : (elt -> unit) -> t -> unit 
end

module BasicFd = MakeFd(Attr)
module Fd = struct
  include BasicFd

  let values v =
    match Fcl_stak.get v with
      Val t -> [t]
    | Unk a -> Fcl_domain.values (Attr.dom a)

  let iter f v =
    match Fcl_stak.get v with
      Val x -> f x
    | Unk a -> Fcl_domain.iter f (Attr.dom a)

  let remove x a =
    match value x with
      Val v -> if D.compare_elt v a = 0 then Fcl_stak.fail "Var.XxxFd.remove"
    | Unk attr -> refine x (Fcl_domain.remove a (Attr.dom attr))
end

module SetFd = MakeFd(SetAttr)
module FloatInterval = MakeFd(FloatAttr)

(* Deprecated *)
type concrete_fd = (Fd.attr, Fd.elt) concrete
let delay = Fd.delay

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
(* $Id: fcl_goals.ml,v 1.60 2004/08/12 15:22:07 barnier Exp $ *)

open Fcl_var
module C = Fcl_cstr

type t = Fcl_stak.gl = { name : string; call : unit -> t option }

exception Success

let goal_stack = ref [];;

let reset () = Fcl_stak.reset (); C.reset_queue ();;

let while_true = fun _ -> ()

let name c = c.name

let fprint c g = Printf.fprintf c "%s" g.name

let create_rec ?(name = "create_rec") f =
  let rec g = { name = name; call = fun () -> Some (f g) } in
  g;;

let create ?(name = "create") f a =
  { name = name; call = fun () -> Some (f a) }

let atomic ?(name = "atomic") f =
  { name = name; call = fun () -> f (); None }

let success = { name = "success"; call = fun () -> None }

let fail = { name = "fail"; call = fun () -> Fcl_stak.fail "Goals.fail"}

let (&&~) g1 g2 =
  { name = "&&~"; call = (fun () -> goal_stack := g2:: !goal_stack; Some g1) }

let (||~) g1 g2 =
  { name = "||~";
    call =
      (fun () -> ignore (Fcl_stak.save (g2 :: !goal_stack)); Some g1) }

let on_choice_point = C.new_event ()

let and_loop () =
  while true do
    match !goal_stack with
      [] -> raise Success
    | goal::goals ->
        goal_stack := goals;
        begin
	  match goal.call () with
	    None -> ()
	  | Some g -> goal_stack := g :: !goal_stack
	end
  done

let solve ?(control = while_true) goal =
  Fcl_debug.call 'g' (fun f ->  Printf.fprintf f "|on_choice_point|=%d\n" (List.length (C.registered on_choice_point)));
  Fcl_stak.reset ();
  let backtracks = ref 0 in
      (* A choice point is systematically added before a search in order
	 to get a correct backtrack on events (constraints, refinnments, ...)
	 done before the first user choice point.
	 This case occurs in minimize where a constraint attached to
	 "choice_point" is posted before the search *)
  goal_stack := [goal ||~ fail];
  try
    while true do (* OR loop *)
      try
        control !backtracks;
	C.schedule on_choice_point;
	C.wake_all ();
	C.assert_empty_queue ();
	and_loop ()
      with
	Fcl_stak.Fail s ->
          Fcl_debug.call 'g' (fun f ->  Printf.fprintf f "fail %s\n" s);
          goal_stack := Fcl_stak.backtrack (); (* May raise Empty_stack *)
          incr backtracks
    done;
    failwith "end of while true"
  with
    Fcl_stak.Empty_stack -> false
  | Success -> control !backtracks; true;;


let indomain var =
  create_rec ~name:"indomain"
    (fun self ->
      match Fd.value var with
      Val _ -> success
    | Unk var_ -> 
    	let mini = Fcl_domain.min (Attr.dom var_) in
	  atomic (fun () -> Fd.subst var mini) ||~
	  atomic
	    (fun () -> Fd.refine var (Fcl_domain.remove mini (Attr.dom var_)))
	    &&~ self);;
 (* can t use remove_min because min may have been changed in the
    choice point (Opti.minimize Continue does that) *)

let dichotomic var =
  create_rec ~name:"dichotomic"
    (fun self ->
      match Fd.value var with
      	Val _ -> success
      | Unk var_ -> 
      	  let d = Attr.dom var_ in
      	  let mini = Fcl_domain.min d and maxi = Fcl_domain.max d in
      	  let middle = (maxi + mini) / 2 in
	  (atomic (fun () -> Fd.refine var (Fcl_domain.remove_up middle d)) ||~
	  atomic (fun () -> Fd.refine var (Fcl_domain.remove_low (middle+1) d))) &&~
	  self)

let instantiate choose var =
  create_rec ~name:"instantiate"
    (fun self ->
      match Fd.value var with
      Val _ -> success
    | Unk var_ -> 
    	let x = choose (Attr.dom var_) in
	atomic (fun () -> Fd.subst var x) ||~
	atomic (fun () -> Fd.refine var (Fcl_domain.remove x (Attr.dom var_))) &&~ self);;


(* unused
let once goal =
  let l = ref (Fcl_stak.level ()) in
  atomic (fun () -> l := (Fcl_stak.level ())) &&~
  goal &&~
  atomic (fun () -> Fcl_stak.cut !l)
*)

let forto min max g =
  let rec la i =
    create
      (fun j ->
	 if j <= max then g j &&~ la (j+1) else success)
      i in
  la min

let foreachto min max g =
  if min > max then success else
  let rec la i =
    create
      (fun j ->
	 if j < max then g j ||~ la (j+1) else g max)
      i in
  la min

let fordownto max min g =
  forto min max (fun i -> g (max - i + min));;

module Array = struct
  let fold_hi select lab_one a init =
    create_rec
      (fun self ->
	match try let i = select a in Some i with Not_found -> None with
	  Some i -> lab_one i a.(i) self
	| None -> init)

  let foldi lab_one a init =
    let size = Array.length a in
    let rec la i =
      create
	(fun j -> if j < size then lab_one j a.(j) (la (j+1)) else init)
	i in
    la 0

  let foralli ?select f a =
    match select with
      None -> foldi (fun i x r -> f i x &&~ r) a success
    | Some s -> fold_hi s (fun i x r -> f i x &&~ r) a success

  let forall ?select f a = foralli ?select (fun _ -> f) a

  let existsi ?select f a =
    match select with
      None -> foreachto 0 (Array.length a - 1) (fun i -> f i a.(i))
    | Some s -> fold_hi s (fun i x r -> f i x ||~ r) a fail

  let exists ?select f a = existsi ?select (fun _ -> f) a

  let choose_index order tab =
    let n = Array.length tab in

    (* Recherche de la premiere variable libre *)
    let rec first_unbound i = 
      if i < n then
      	match Fd.value tab.(i) with
	  Unk attr -> i, attr
	| Val _ -> first_unbound (i+1)
      else
	raise Not_found in
    
    let b, attr = first_unbound 0 in
    let best = ref b and attr_best = ref attr in
    (* Recherche de la meilleure variable pour le critere *)
    for i = b+1 to n - 1 do
      match Fd.value tab.(i) with
	Unk tabi ->
	  if order tabi !attr_best then begin
	    best := i; attr_best := tabi
	  end
      |	Val _ -> ()
    done;
    !best

  let labeling (a : Fcl_var.Fd.t array) = forall indomain a

  exception Return of int
  let not_instantiated_fd fds =
    try
      Array.iteri
      (fun i fdsi -> if Fd.is_var fdsi then raise (Return i))
	fds;
      raise Not_found
    with
      Return i -> i
end

module List = struct
  let rec fold fgoal l init =
    create
      (function
	   [] -> init
	 | x::xs -> fgoal x (fold fgoal xs init))
      l

  let rec fold_h select fgoal l init =
    create
      (function
	  [] -> init
       	| _ ->
            let x,xs = select l in
            fgoal x (fold_h select fgoal xs init))
      l

  let forall ?select f l =
    match select with
      None -> fold (fun x r -> f x &&~ r) l success
    | Some s -> fold_h s (fun x r -> f x &&~ r) l success

  let exists ?select f l =
    match select with
      None -> fold (fun x r -> f x ||~ r) l fail
    | Some s -> fold_h s (fun x r -> f x ||~ r) l fail

  let member v l = exists (fun x -> atomic (fun () -> Fd.unify v x)) l

  let labeling = forall indomain
end

let unify v x = atomic (fun () -> Fd.unify v x)


let level g =
  create (fun () -> let l = Fcl_stak.level () in g l) ()

let sigma ?(domain = Fcl_domain.int) g =
  create (fun () -> g (Fd.create domain)) ()

let once g =
  level (fun l -> g &&~ atomic (fun () -> Fcl_stak.cut l))

let minimize_restart goal (cost : Fd.t) step compute_solution =
  let best_cost = ref 0
  and once_more = ref true in
  (* +step because we constrain the cost before the search starts in
     the recursive goal *)
  atomic
    (fun () ->
      best_cost := Fd.max cost + step;
      once_more := true)
    &&~
  create_rec
    (fun self ->
      if !once_more then begin (* First iteration or last one succeeded *)
      	let loop_level = ref (Fcl_stak.level ()) in
	once_more := false;
        (* Stores the choice-point and constrains the cost *)
	({ name = "restart_store";
	  call =
	  fun () ->
	    let ub = !best_cost - step in
	    begin match Fd.value cost with
	      Val c -> if c > ub then Fcl_stak.fail "Goals.minimize"
	    | Unk a ->
		Fd.refine cost (Fcl_domain.remove_up ub (Attr.dom a)) end;
	    loop_level := Fcl_stak.level ();
	    Some goal}
 	   &&~
	 { name = "one_found"; (* One solution found *)
	   call =
	   fun () ->
	     once_more := true;
   (* Dire dans la doc que goal doit obligatoirement instancier le cout *)
	     let m = Fd.int_value cost in
	     compute_solution m;
	     best_cost := m;
	     Fcl_stak.cut !loop_level;
	     Fcl_stak.fail "Goals.minimize" })
      ||~
	self end
   (* Last try failed *)
      else Fcl_stak.fail "Goals.minimize")


let minimize_continue goal (cost : Fd.t) step compute_solution =
  let best_cost = ref 0 in

  let rec bt_until l =
    (* Backtrack until lower bound better than current cost, staying above [l] *)
    let gs = Fcl_stak.backtrack () in
    if Fd.min cost <= !best_cost then ignore (Fcl_stak.save gs)
    else if Fcl_stak.older (Fcl_stak.level ()) l then Fcl_stak.fail "continue"
    else bt_until l in

  let restore_max =
    let update _ =
(***       Printf.printf "cost=%a best_cost=%d\n" Fd.fprint cost !best_cost; flush stdout; ***)
      match Fd.value cost with
	Val v -> if v > !best_cost then Fcl_stak.fail "Goals.restore_max" else true
      | Unk attr ->
	  Fd.refine cost (Fcl_domain.remove_up !best_cost (Attr.dom attr));
	  false
    and delay x = C.delay [on_choice_point] x in
    C.create ~name:"restore_cost" update delay in

  let found_one l =
    { name = "found_one";
      call =
      fun () ->
        let c = Fd.int_value cost in
      	compute_solution c;
	best_cost := c - step;
	bt_until l;
      	Fcl_stak.fail "Goals.minimize_more" } in

  let init =
    { name = "continue_init";
      call =
      fun () ->
	best_cost := Fd.max cost + 1; C.post restore_max; Some goal } in

  level (fun l -> init &&~ found_one l);;

type bb_mode = Restart | Continue

let minimize ?(step=1) ?(mode = Continue) g c cs =
  if step <= 0 then invalid_arg "Goals.minimize: step must be non negative";
  match mode with
    Restart -> minimize_restart g c step cs
  | Continue -> minimize_continue g c step cs


let lds ?(step = 1) goal =
  let lds_max = ref 0 and more = ref true and lds = Fcl_stak.ref 0 in

  let lds_check =
    let update _ =
      Fcl_stak.set lds (Fcl_stak.get lds + 1);
      if Fcl_stak.get lds > !lds_max then begin
	more := true; Fcl_stak.fail "Goals.lds_check" end
      else false
    and delay x = C.delay [on_choice_point] x
    and init _ = () in
    C.create ~name:"lds_check" ~init update delay in

  { name = "lds_init";
    call = (fun () ->
      (* lds must be less than lds_max for lds_check not to fail when
	 backtracking just before executing "self" *)
      lds_max := -step; more := true; Fcl_stak.set lds (!lds_max - 1);
      C.post lds_check; None) }
    &&~
  (create_rec ~name:"lds_iterate"
     (fun self ->
       if not !more then Fcl_stak.fail "Goals.lds" else begin
	 lds_max := !lds_max + step;
	 more := false;
	 Fcl_debug.call 'l' (fun f -> Printf.fprintf f "lds_max=%d\n" !lds_max);
	 (atomic (fun () -> Fcl_stak.set lds 0; ()) &&~ goal) ||~ self end))
    

module Conjunto = struct
  let indomain d =
    create_rec ~name:"Goals.Conjunto.indomain"
      (fun self ->
	match SetFd.value d with
	  Val _ -> success
	| Unk a ->
	    let glb = SetAttr.min a
	    and lub = SetAttr.max a in
	    let diff = Fcl_setDomain.S.diff lub glb in
	    let x = Fcl_setDomain.S.choose diff in
	    ( ( atomic (fun () -> Fcl_conjunto.outside x d)
	     ||~ atomic (fun () -> Fcl_conjunto.inside x d)) &&~ self) )
end

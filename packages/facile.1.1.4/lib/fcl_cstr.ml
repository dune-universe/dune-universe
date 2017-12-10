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
(* $Id: fcl_cstr.ml,v 1.43 2004/09/03 13:23:11 barnier Exp $ *)

open Printf

exception DontKnow

type priority = int
let nb_priorities = 3

let immediate = 0
let normal = 1
let later = 2

type t = {
    id : int;
    name: string;
    priority : priority;
    solved : bool array;
    woken : bool array;
    nb_solved : int Fcl_stak.ref;
    fprint : out_channel -> unit;
    update : int -> unit;
    init : unit -> unit;
    check : unit -> bool;
    delay : t -> unit;
    not : unit -> t
  } 

let gen_int = Fcl_misc.gen_int_fun ()

let array_set_true t i =
  t.(i) <- true;
  Fcl_stak.trail (fun () -> t.(i) <- false)

let create ?(name = "anonymous") ?(nb_wakings = 1) ?fprint ?(priority = normal) ?init ?check ?not update delay =
  if nb_wakings < 1 then begin
    let msg = "Cstr.create: nb_wakings must be greater or equal to 1" in
    Fcl_debug.fatal_error msg end;
  let solved = Array.make nb_wakings false
  and nb_solved = Fcl_stak.ref 0 in
  let update i =
    if update i then
      if Pervasives.not solved.(i) then begin
       	Fcl_stak.set nb_solved (Fcl_stak.get nb_solved + 1);
       	array_set_true solved i
      end in
  {
  id = gen_int ();
  name = name;
  priority = priority;
  update = update;
  delay = delay;
  solved = solved;
  woken = Array.make nb_wakings false;
  nb_solved = nb_solved;
  fprint = (match fprint with Some f -> f | None -> fun c -> fprintf c "%s" name);
  init =
   (match init with
     Some i -> i
   (* For not breaking constraints that don't use waking ids and
      rely on being woken at post time. If update must not be called
      at post time (e.g. because it is suspended to on_subst and the
      code rely on the fact that the variable really is instantiated)
      and waking ids are not used, init must be defined. *)
   | None when nb_wakings = 1 -> fun () -> ignore (update 0)
   (* otherwise we do nothing *)
   | _ -> fun () -> ());
  check = (match check with Some c -> c | None -> fun () -> Fcl_debug.fatal_error (name ^ ": check callback undefined"));
  not = (match not with Some n -> n | None -> fun () -> Fcl_debug.fatal_error (name ^ ": not callback undefined"))
} 
    

let fprint chan ct = Printf.fprintf chan "%d: " ct.id; ct.fprint chan
let self_delay c = c.delay
let check c = c.check ()
(* unused  value let solved c = c.solved *)

let is_solved ct = Fcl_stak.get ct.nb_solved = Array.length ct.woken

let queue = Array.make nb_priorities []
and already_in_wake = ref false
and next_priority = ref nb_priorities

let reset_queue () =
  for i = 0 to nb_priorities -1 do
    queue.(i) <- []
  done;
  next_priority := nb_priorities;
  already_in_wake := false;;

let assert_empty_queue () =
  assert(
    try
      for i = 0 to nb_priorities -1 do
	if queue.(i) <> [] then raise Exit
      done;
      !next_priority = nb_priorities && not !already_in_wake
    with
      Exit -> false);;

(* unused exception Wake_all *)

let wake_all () =
  if not !already_in_wake
  then begin
    already_in_wake := true;
    try
      while !next_priority < nb_priorities do
	match queue.(!next_priority) with
      	    [] -> incr next_priority
	  | (c, i) :: cs ->
	      queue.(!next_priority) <- cs;
	      Fcl_debug.call 'c' (fun s -> fprintf s "%s(%d)#update(%d)\n" c.name c.id i);
	      if not c.solved.(i) then c.update i;
	      Fcl_debug.call 'c' (fun s -> fprintf s "%s(%d)#updated(%d)%s\n" c.name c.id i (if is_solved c then "*" else ""));
	      c.woken.(i) <- false (* not trailed *)
      done;
      already_in_wake := false
   (* To avoid being in a state where already_in_wake = true after an
      uncaught exception during the while loop. *)
    with e -> 
      reset_queue ();
      raise e
  end


let schedule_one_cstr ((cstr, i) as c) =
  Fcl_debug.call 'c' (fun s -> fprintf s "%s(%d)#scheduled(%d) - (woken:%b, solved:%b)\n" cstr.name cstr.id i cstr.woken.(i) cstr.solved.(i));
  if not (cstr.woken.(i) || cstr.solved.(i)) then begin
    Fcl_debug.call 'c' (fun s -> fprintf s "wake %d(%d): " cstr.id i; cstr.fprint s; fprintf s "\n");
    let p = cstr.priority in
    queue.(p) <- c :: queue.(p);
    next_priority := Fcl_misc.Operators.min !next_priority p;
    array_set_true cstr.woken i end


(* Management of active contraints *)
module Store = struct
  let size_store = 1024
  let store = ref (Weak.create size_store)
  let next_free = ref 0
    
  let compress_or_extend () =
    let size = Weak.length !store in
    let rec look_for_free from to_ =
      if to_ < size then
      	match Weak.get !store to_ with
      	  None -> copy (max (to_ + 1) from) to_
      	| Some _ -> look_for_free from (to_ + 1)
      else size (* Full *)
    and copy from to_ =
      assert(from > to_);
      if from < size then
      	match Weak.get !store from with
      	  None -> copy (from + 1) to_
      	| some_c ->
	    Weak.set !store to_ some_c;
	    Weak.set !store from None;
	    look_for_free (from + 1) (to_ + 1)
      else to_ in
    next_free := look_for_free 0 0;
    if !next_free > size / 2 then begin
      let old_store = !store in
      store := Weak.create (size * 2);
      Weak.blit old_store 0 (!store) 0 size;
    end
      
  let add = fun c ->
    if not (is_solved c) then begin
      let size = Weak.length !store in
      if !next_free >= size then begin
      	assert(!next_free = size);
      	compress_or_extend () (* Set next_free *)
      end;
      Weak.set !store !next_free (Some c);
      incr next_free;
      let id = c.id in
      Fcl_stak.trail
      	(fun () ->
        (* le weak pointer de c a ete eventuellement supprime par le GC et
	   la compression *)
	  match Weak.get !store (!next_free - 1) with
	    Some c' when c'.id <> id -> ()
	  | _ -> decr next_free)
    end
	
  let active_store () =
    let rec loop active i = 
      if i < !next_free then
      	loop
	  (match Weak.get !store i with
	    None -> active
      	  | Some c ->
	      if is_solved c then active else (c::active))
	  (i+1)
      else
      	active in
    loop [] 0
end

let active_store = Store.active_store
  

let post c =
  Fcl_debug.call 'c' (fun s -> fprintf s "post: "; c.fprint s; fprintf s "\n");
  let current_status = !already_in_wake in
  already_in_wake := true; (* Because #init may wake constraints and we want
			      to schedule them correctly *)
  begin
    try (* Because #init may fail or raise any other exception *)
      c.init ()
    with
      e ->
  	reset_queue ();
	raise e
  end;
  if not (is_solved c) then begin
    c.delay c;
    Store.add c;
  end;
  already_in_wake := current_status;
  wake_all ()

  (* post pour les démons *)
let init c =
  c.init ();
  c.delay c;;


let rec one () =
  let delay _ = ()
  and check () = true
  and update _ = true
  and not = zero in 
  create ~priority:immediate ~name:"one" ~check ~not update delay
and zero () =
  let delay _ = ()
  and check () = false
  and update _ = Fcl_stak.fail "zero"
  and not = one in 
  create ~priority:immediate ~name:"zero" ~check ~not update delay

let one = one ()
let zero = one.not ()

let id c = c.id
let name c = c.name
let priority c = c.priority

(* Un objet avec des contraintes qui lui sont attachées *)
type event = (t * int) list Fcl_stak.ref
let new_event () = Fcl_stak.ref []
let schedule (event : event) = List.iter schedule_one_cstr (Fcl_stak.get event)
let register event ?(waking_id=0) cstr =
  let nb_wakings = Array.length cstr.woken in
  if waking_id >= nb_wakings then begin
    let msg =
      Printf.sprintf
	"nb_wakings less one (%d) must be equal to maximum waking_id (here %d) in constraint %s" (nb_wakings - 1) waking_id cstr.name in
    Fcl_debug.fatal_error msg end;
    
  let current = Fcl_stak.get event in
  if not (is_solved cstr)
  then Fcl_stak.set event ((cstr, waking_id) :: current)
let registered = Fcl_stak.get

let delay events ?waking_id c =
  List.iter
    (fun event -> register event ?waking_id c)
    events

let conjunction = function
    [] -> one
  | [cstr] -> cstr
  | cstrs ->
      let update _ = true
      and delay _ = ()
      and init () = List.iter (fun c -> post c) cstrs
      and fprint chan =
	List.iter (fun c -> Printf.fprintf chan "%a\n" fprint c) cstrs in
      create ~fprint ~init ~name:"conjunction" update delay

let not ct = ct.not ()

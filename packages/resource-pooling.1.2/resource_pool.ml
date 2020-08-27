[@@@ocaml.warning "+A-44-48"]

(* this module is a copy of Lwt_sequence from
   Lwt version 17c8d5e2071f3690850a99c3ce0f2e6a79a8ac7f *)
module Sequence = struct
[@@@ocaml.warning "-32"]
(* start copy & paste ------------------------------------------------------- *)
(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



exception Empty

type 'a t = {
  mutable prev : 'a t;
  mutable next : 'a t;
}

type 'a node = {
  mutable node_prev : 'a t;
  mutable node_next : 'a t;
  mutable node_data : 'a;
  mutable node_active : bool;
}

external seq_of_node : 'a node -> 'a t = "%identity"
external node_of_seq : 'a t -> 'a node = "%identity"

(* +-----------------------------------------------------------------+
   | Operations on nodes                                             |
   +-----------------------------------------------------------------+ *)

let get node =
  node.node_data

let set node data =
  node.node_data <- data

let remove node =
  if node.node_active then begin
    node.node_active <- false;
    let seq = seq_of_node node in
    seq.prev.next <- seq.next;
    seq.next.prev <- seq.prev
  end

(* +-----------------------------------------------------------------+
   | Operations on sequences                                         |
   +-----------------------------------------------------------------+ *)

let create () =
  let rec seq = { prev = seq; next = seq } in
  seq

let is_empty seq = seq.next == seq

let length seq =
  let rec loop curr len =
    if curr == seq then
      len
    else
      let node = node_of_seq curr in loop node.node_next (len + 1)
  in
  loop seq.next 0

let add_l data seq =
  let node = { node_prev = seq; node_next = seq.next; node_data = data; node_active = true } in
  seq.next.prev <- seq_of_node node;
  seq.next <- seq_of_node node;
  node

let add_r data seq =
  let node = { node_prev = seq.prev; node_next = seq; node_data = data; node_active = true } in
  seq.prev.next <- seq_of_node node;
  seq.prev <- seq_of_node node;
  node

let take_l seq =
  if is_empty seq then
    raise Empty
  else begin
    let node = node_of_seq seq.next in
    remove node;
    node.node_data
  end

let take_r seq =
  if is_empty seq then
    raise Empty
  else begin
    let node = node_of_seq seq.prev in
    remove node;
    node.node_data
  end

let take_opt_l seq =
  if is_empty seq then
    None
  else begin
    let node = node_of_seq seq.next in
    remove node;
    Some node.node_data
  end

let take_opt_r seq =
  if is_empty seq then
    None
  else begin
    let node = node_of_seq seq.prev in
    remove node;
    Some node.node_data
  end

let peek_opt_l seq =
  if is_empty seq then
    None
  else begin
    let node = node_of_seq seq.next in
    Some node.node_data
  end

let transfer_l s1 s2 =
  s2.next.prev <- s1.prev;
  s1.prev.next <- s2.next;
  s2.next <- s1.next;
  s1.next.prev <- s2;
  s1.prev <- s1;
  s1.next <- s1

let transfer_r s1 s2 =
  s2.prev.next <- s1.next;
  s1.next.prev <- s2.prev;
  s2.prev <- s1.prev;
  s1.prev.next <- s2;
  s1.prev <- s1;
  s1.next <- s1

let iter_l f seq =
  let rec loop curr =
    if curr != seq then begin
      let node = node_of_seq curr in
      if node.node_active then f node.node_data;
      loop node.node_next
    end
  in
  loop seq.next

let iter_r f seq =
  let rec loop curr =
    if curr != seq then begin
      let node = node_of_seq curr in
      if node.node_active then f node.node_data;
      loop node.node_prev
    end
  in
  loop seq.prev

let iter_node_l f seq =
  let rec loop curr =
    if curr != seq then begin
      let node = node_of_seq curr in
      if node.node_active then f node;
      loop node.node_next
    end
  in
  loop seq.next

let iter_node_r f seq =
  let rec loop curr =
    if curr != seq then begin
      let node = node_of_seq curr in
      if node.node_active then f node;
      loop node.node_prev
    end
  in
  loop seq.prev

let fold_l f seq acc =
  let rec loop curr acc =
    if curr == seq then
      acc
    else
      let node = node_of_seq curr in
      if node.node_active then
        loop node.node_next (f node.node_data acc)
      else
        loop node.node_next acc
  in
  loop seq.next acc

let fold_r f seq acc =
  let rec loop curr acc =
    if curr == seq then
      acc
    else
      let node = node_of_seq curr in
      if node.node_active then
        loop node.node_prev (f node.node_data acc)
      else
        loop node.node_prev acc
  in
  loop seq.prev acc

let find_node_l f seq =
  let rec loop curr =
    if curr != seq then
      let node = node_of_seq curr in
      if node.node_active then
        if f node.node_data then
          node
        else
          loop node.node_next
      else
        loop node.node_next
    else
      raise Not_found
  in
  loop seq.next

let find_node_r f seq =
  let rec loop curr =
    if curr != seq then
      let node = node_of_seq curr in
      if node.node_active then
        if f node.node_data then
          node
        else
          loop node.node_prev
      else
        loop node.node_prev
    else
      raise Not_found
  in
  loop seq.prev

let find_node_opt_l f seq =
  try Some (find_node_l f seq) with Not_found -> None

let find_node_opt_r f seq =
  try Some (find_node_r f seq) with Not_found -> None
(* end copy & paste --------------------------------------------------------- *)
end
[@@@ocaml.warning "+32"]


open Lwt.Infix

type 'a t = {
  create : unit -> 'a Lwt.t;
  (* Create a new pool member. *)
  check : exn -> 'a -> bool Lwt.t;
  (* Check validity of a pool member when use resulted in failed promise. *)
  validate : 'a -> bool Lwt.t;
  (* Validate an existing free pool member before use. *)
  dispose : 'a -> unit Lwt.t;
  (* Dispose of a pool member. *)
  cleared : bool ref ref;
  (* Have the current pool elements been cleared out? *)
  mutable max : int;
  (* Size of the pool. *)
  mutable count : int;
  (* Number of elements in the pool. *)
  list : 'a Queue.t;
  (* Available pool members. *)
  waiters : ('a Lwt.u * float) Sequence.t;
  (* Promise resolvers waiting for a free member. *)
}

let create
  ?(validate = fun _ -> Lwt.return_true)
  ?(check = fun _ _ -> Lwt.return_true)
  ?(dispose = fun _ -> Lwt.return_unit)
  max
  create = {
    max; create; validate; check; dispose;
    cleared = ref (ref false);
    count = 0;
    list = Queue.create ();
    waiters = Sequence.create ()
  }

let set_max p n = p.max <- n

(* Create a pool member. *)
let create_member p =
  Lwt.catch
    (fun () ->
       (* Must be done before p.create to prevent other resolvers from
          creating new members if the limit is reached. *)
       p.count <- p.count + 1;
       p.create ())
    (fun exn ->
       (* Creation failed, so don't increment count. *)
       p.count <- p.count - 1;
       Lwt.fail exn)

(* Release a pool member. *)
let release p c =
  match Sequence.take_opt_l p.waiters with
  | Some (wakener, _) ->
    (* A promise resolver is waiting, give it the pool member. *)
    Lwt.wakeup_later wakener c
  | None ->
    (* No one is waiting, queue it. *)
    Queue.push c p.list

exception Resource_limit_exceeded

let add ?(omit_max_check = false) p c =
  if not omit_max_check && p.count >= p.max then raise Resource_limit_exceeded;
  p.count <- p.count + 1;
  release p c

(* Dispose of a pool member. *)
let dispose p c =
  p.dispose c >>= fun () ->
  p.count <- p.count - 1;
  Lwt.return_unit

(* Create a new member when one is thrown away. *)
let replace_disposed p =
  match Sequence.take_opt_l p.waiters with
  | None ->
    (* No one is waiting, do not create a new member to avoid
       losing an error if creation fails. *)
    ()
  | Some (wakener, _) ->
    Lwt.on_any
      (Lwt.apply p.create ())
      (fun c ->
         Lwt.wakeup_later wakener c)
      (fun exn ->
         (* Creation failed, notify the waiter of the failure. *)
         Lwt.wakeup_later_exn wakener exn)

(* Verify a member is still valid before using it. *)
let validate_and_return p c =
  Lwt.try_bind
      (fun () ->
         p.validate c)
      (function
        | true ->
          Lwt.return c
        | false ->
          (* Remove this member and create a new one. *)
          dispose p c >>= fun () ->
          create_member p)
      (fun e ->
         (* Validation failed: create a new member if at least one
            resolver is waiting. *)
         dispose p c >>= fun () ->
         replace_disposed p;
         Lwt.fail e)

exception Resource_invalid of {safe : bool}

let add_task_r sequence =
  let p, r = Lwt.task () in
  let node = Sequence.add_r (r, Unix.gettimeofday ()) sequence in
  Lwt.on_cancel p (fun () -> Sequence.remove node);
  p

(* Acquire a pool member. *)
let acquire ~attempts p =
  assert (attempts > 0);
  let once () =
    if Queue.is_empty p.list then
      (* No more available member. *)
      if p.count < p.max then
        (* Limit not reached: create a new one. *)
        create_member p
      else
        (* Limit reached: wait for a free one. *)
        (add_task_r [@ocaml.warning "-3"]) p.waiters >>= validate_and_return p
    else
      (* Take the first free member and validate it. *)
      let c = Queue.take p.list in
      validate_and_return p c
  in
  let rec keep_trying ?(e = Resource_invalid {safe = true}) attempts =
    if attempts > 0
      then Lwt.catch once @@ fun e ->
        match e with
        | Resource_invalid {safe = true} -> keep_trying ~e (attempts - 1)
        | e -> Lwt.fail e
      else Lwt.fail e
  in keep_trying attempts

(* Release a member when use resulted in failed promise if the member
   is still valid. *)
let check_and_release e p c cleared =
  p.check e c >>= fun ok ->
  if !cleared || not ok then (
    (* Element is not ok or the pool was cleared - dispose of it *)
    dispose p c
  )
  else (
    (* Element is ok - release it back to the pool *)
    release p c;
    Lwt.return_unit
  )

let use ?(creation_attempts = 1) ?(usage_attempts = 1) p f =
  assert (usage_attempts > 0);
  let cleared = !(p.cleared) in
  (* Capture the current cleared state so we can see if it changes while this
     element is in use *)
  let rec make_promise attempts =
    if attempts <= 0 then Lwt.fail @@ Resource_invalid {safe = true} else
    acquire ~attempts:creation_attempts p >>= fun c ->
    Lwt.catch
      (fun () -> f c >>= fun res -> Lwt.return (c,res))
      (fun e ->
         check_and_release e p c cleared >>= fun () ->
         match e with
         | Resource_invalid {safe = true} -> make_promise (attempts - 1)
         | e -> Lwt.fail e)
  in
  let promise = make_promise usage_attempts in
  promise >>= fun (c,_) ->
  if !cleared then (
    (* p was cleared while promise was resolving - dispose of this element *)
    dispose p c >>= fun () ->
    Lwt.map snd promise
  )
  else (
    release p c;
    Lwt.map snd promise
  )

let clear p =
  let elements = Queue.fold (fun l element -> element :: l) [] p.list in
  Queue.clear p.list;
  (* Indicate to any currently in-use elements that we cleared the pool *)
  let old_cleared = !(p.cleared) in
  old_cleared := true;
  p.cleared := ref false;
  Lwt_list.iter_s (dispose p) elements

let wait_queue_length p = Sequence.length p.waiters
let wait_queue_delay p =
  match Sequence.peek_opt_l p.waiters with
  | None -> 0.
  | Some (_, d) -> Unix.gettimeofday () -. d

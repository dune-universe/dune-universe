module T = Thread_map_core

type mask = { mutable on : bool }

let mask_tls : mask T.t = T.create ()
(* whether the current thread is masked *)

let create_mask () =
  let r = { on = false } in
  T.set mask_tls (Some r) ;
  r

let delete_mask () = T.set mask_tls None

let is_blocked () =
  match T.get mask_tls with
  | None -> false
  | Some r -> r.on

let assert_blocked () = assert (is_blocked ())

(* The current goal is only to protect from those asynchronous
   exceptions raised after dutifully checking that [is_blocked ()]
   evaluates to false, and that expect the asynchronous callback to be
   called again shortly thereafter (e.g. memprof callbacks). There is
   currently no mechanism to delay asynchronous callbacks, so this
   strategy cannot work for other kinds of asynchronous callbacks. *)
let with_resource ~acquire arg ~scope ~(release : _ -> unit) =
  let mask, delete_after = match T.get mask_tls with
    | None -> create_mask (), true
    | Some r -> r, false
  in
  let old_mask = mask.on in
  let remove_mask () =
    (* remove the mask flag from the TLS to avoid it growing
       uncontrollably when there are lots of threads. *)
    if delete_after then delete_mask () else mask.on <- old_mask
  in
  let release_and_unmask r x =
    match release r with
    | () -> remove_mask () ; x
    | exception e -> remove_mask () ; raise e
  in
  mask.on <- true ;
  let r = try acquire arg with
    | e -> mask.on <- old_mask ; raise e
  in
  match
    mask.on <- old_mask ;
    scope r
  with
  | (* BEGIN ATOMIC *) y -> (
      mask.on <- true ;
      (* END ATOMIC *)
      release_and_unmask r y
    )
  | (* BEGIN ATOMIC *) exception e -> (
      mask.on <- true ;
      (* END ATOMIC *)
      match Printexc.get_raw_backtrace () with
      | bt -> (
          let e = release_and_unmask r e in
          Printexc.raise_with_backtrace e bt
        )
      | exception Out_of_memory -> raise (release_and_unmask r e)
    )

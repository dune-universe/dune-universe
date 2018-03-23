open Core
open Async

type 'a t = unit -> [`Done | `Ok of 'a] Deferred.t

let done_deferred () = return `Done

let empty () = done_deferred ()

let create f = f

let unfold init f =
  let state = ref init in
  fun () ->
    f !state >>| function
      | `Done -> `Done
      | `Ok (res, s) ->
          state := s;
          `Ok res
;;

let append t1 t2 =
  let rec self = ref init
  and init () =
    t1 () >>= function
      | `Ok v -> return (`Ok v)
      | `Done -> self := t2; t2 ()
  in
  fun () -> !self ()
;;

let concat tt =
  let rec self = ref start
  and continue f = self := f; f ()
  and start () =
    tt () >>= function
    | `Done -> return `Done
    | `Ok t -> continue (prefix t)
  and prefix t = (); fun () ->
    t () >>= function
    | `Ok v -> return (`Ok v)
    | `Done -> continue start
  in
  fun () -> !self ()
;;

let of_list xs =
  let cursor = ref xs in
  (fun () -> match !cursor with
    | [] -> done_deferred ()
    | x::xs -> cursor := xs; return (`Ok x))

let singleton x =
  of_list [x]

let next t = t ()

let fold' t ~init ~f = Deferred.create (fun result ->
  let rec loop accum =
    upon (next t) (function
      | `Done -> Ivar.fill result accum
      | `Ok x -> upon (f accum x) loop)
  in
  loop init)

let fold t ~init ~f = fold' t ~init ~f:(fun accum x -> return (f accum x))

let iter' t ~f = fold' t ~init:() ~f:(fun () x -> f x)

let iter t ~f = iter' t ~f:(fun x -> f x; Deferred.unit)

let to_list t =
  Deferred.create (fun ivar ->
    let rec collect results =
      upon (next t) (function
        | `Done -> Ivar.fill ivar (List.rev results)
        | `Ok result -> collect (result::results))
    in
    collect [])

let find t ~f =
  Deferred.create (fun ivar ->
    let rec do_find () =
      upon (next t) (function
        | `Done -> Ivar.fill ivar None
        | `Ok item ->
            if f item
            then Ivar.fill ivar (Some item)
            else do_find ())
    in
    do_find ())

let map' t ~f = create (fun () ->
  next t >>= function
    | `Ok x -> f x >>| fun result -> `Ok result
    | `Done -> return `Done)

let map t ~f = create (fun () ->
  next t >>| function
    | `Ok x -> `Ok (f x)
    | `Done -> `Done)

let filter_map t ~f =
  let rec loop result = upon (next t) (function
    | `Done -> Ivar.fill result `Done
    | `Ok a -> begin match f a with
       | Some b -> Ivar.fill result (`Ok b)
       | None -> loop result
    end)
  in
  (fun () -> Deferred.create loop)

let filter_opt t = filter_map t ~f:ident

let to_stream t =
  let tail = Tail.create () in
  don't_wait_for (
    iter' t ~f:(fun x -> Tail.extend tail x; Deferred.unit) >>| fun () ->
    Tail.close_exn tail);
  Tail.collect tail


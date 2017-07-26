open Core
open Async

type interest_status = [`Continue | `Leave]

type 'a message =
| Message of 'a
| Register of ('a -> interest_status Deferred.t)
| Register_init of (unit -> unit Deferred.t) * ('a -> interest_status Deferred.t)

type 'a t = 'a message Tail.t

let start_sync_loop m =
  let rec loop s hs =
    Deferred.upon (Stream.next s) (function
        | Stream.Nil -> ()
        | Stream.Cons (m, s) ->
            match m with
            | Message v ->
                let statuslst = List.map hs ~f:(fun h -> h v) in
                Deferred.upon (Deferred.all statuslst) (fun sl ->
                  let new_hs = List.filter_map (List.zip_exn sl hs) ~f:(fun (s,f) ->
                      match s with
                      | `Continue -> Some f
                      | `Leave -> None
                    ) in
                  loop s (List.rev new_hs))
            | Register h -> loop s (hs @ [h])
            | Register_init (i,h) ->
              upon (i ()) (fun () -> loop s (hs @ [h]))
      )
  in
  loop (Tail.collect m) []

let create () =
  let m = Tail.create () in
  start_sync_loop m;
  m

let close t = Tail.close_exn t

let register t ~f:h = Tail.extend t (Register h)

let register_init t ~i:i ~f:h = Tail.extend t (Register_init (i,h))

let on_update t ~f:h =
  let h' a =
    h a;
    Deferred.return `Continue
  in
  register t ~f:h'

let upon t ~f:h = register t ~f:(fun a -> h a; Deferred.return `Leave)

let next t = Deferred.create (fun i -> upon t ~f:(fun v -> Ivar.fill i v))

let send t v = Tail.extend t (Message v)

(*
let merge ta tb =
  let sc = create () in
  on_update ta ~f:(fun a -> send sc a);
  on_update tb ~f:(fun b -> send sc b);
  sc

let tagged_merge (taga,ta) (tagb,tb) =
  let sc = create () in
  on_update ta ~f:(fun a -> send sc (taga,a));
  on_update tb ~f:(fun b -> send sc (tagb,b));
  sc
*)

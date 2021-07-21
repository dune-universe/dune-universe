type void

module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module type S = sig
  type 'a monad

  type 'a thunk = unit -> 'a

  type finalizer = (unit -> unit monad) option

  type ('i, 'o, 'r) t =
    | Has_output of 'o * ('i, 'o, 'r) t thunk * finalizer
    | Needs_input of ('i option -> ('i, 'o, 'r) t)
    | Done of 'r
    | PipeM of ('i, 'o, 'r) t monad thunk

  type 'a source = (void, 'a, unit) t
  type 'a sink = ('a, void, unit) t

  val return : 'r -> (_, _, 'r) t
  val bind : ('i, 'o, 'a) t -> ('a -> ('i, 'o, 'b) t) -> ('i, 'o, 'b) t

  module Monad_infix : sig
    val ( >>= ) : ('i, 'o, 'a) t -> ('a -> ('i, 'o, 'b) t) -> ('i, 'o, 'b) t
  end

  val await : unit -> ('a, _, 'a option) t
  val yield : 'o -> (_, 'o, unit) t
  val compose : ('i, 'a, _) t -> ('a, 'o, 'r) t -> ('i, 'o, 'r) t
  val ( $$ ) : ('i, 'a, _) t -> ('a, 'o, 'r) t -> ('i, 'o, 'r) t
  val run : (void, void, 'r) t -> 'r monad

  val bracket :
    (unit -> 'a monad) ->
    ('a -> unit monad) ->
    ('a -> ('i, 'o, 'r) t) ->
    ('i, 'o, 'r) t

  val fold : 'r -> ('i -> 'r -> 'r) -> ('i, void, 'r) t
  val map : ('i -> 'o) -> ('i, 'o, unit) t
  val mapi : (int -> 'i -> 'o) -> ('i, 'o, unit) t
  val filter : ('i -> bool) -> ('i, 'i, unit) t
  val filter_map : ('i -> 'o option) -> ('i, 'o, unit) t

  val from_list : 'a list -> 'a source
  val to_list : unit -> ('a, void, 'a list) t

  val all : unit -> (('a, 'b) result,
                     void,
                     ('a list, 'b) result) t

  val loop : ('a -> 'b option -> 'a * 'c list) -> 'a -> ('b, 'c, unit) t
  val loop' : ('a -> 'b option -> ('a * 'c list, 'd) result) -> 'a -> ('b, 'c, (unit, 'd) result) t

  val drop : int -> ('a, 'a, unit) t
end

module Make(M : Monad) = struct
  let ( >>= ) x f = M.bind x f

  type 'a monad = 'a M.t

  type 'a thunk = unit -> 'a

  type finalizer = (unit -> unit M.t) option

  type ('i, 'o, 'r) t =
    | Has_output of 'o * ('i, 'o, 'r) t thunk * finalizer
    | Needs_input of ('i option -> ('i, 'o, 'r) t)
    | Done of 'r
    | PipeM of ('i, 'o, 'r) t M.t thunk

  type 'a source = (void, 'a, unit) t
  type 'a sink = ('a, void, unit) t

  let return x = Done x

  let rec bind
  (*    : ('i, 'o, 'a) t -> ('a -> ('i, 'o, 'b) t) -> ('i, 'o, 'b) t *)
    = fun x f ->
      match x with
      | Has_output (o, next, cleanup) ->
        Has_output (o, (fun () -> bind (next ()) f), cleanup)

      | Needs_input on_input ->
        Needs_input (fun i -> bind (on_input i) f)

      | Done r -> f r

      | PipeM m -> PipeM (fun () ->
          m () >>= fun p ->
          M.return (bind p f)
        )

  module Monad_infix = struct
    let ( >>= ) x f = bind x f
  end

  let await () =
    Needs_input (fun i -> Done i)

  let yield x =
    Has_output (x, (fun () -> Done ()),  None)

  let finalizer_compose f g =
    match f, g with
    | None, None -> None
    | Some f, None -> Some f
    | None, Some g -> Some g
    | Some f, Some g -> Some (fun () -> f () >>= g)

  let finalize = function
    | None -> M.return ()
    | Some f -> f ()

  let compose p q =
    let rec go_right final left = function
      | Has_output (o, next, clean) ->
        let next () = go_right final left (next ()) in
        let clean = finalizer_compose clean final in
        Has_output (o, next, clean)

      | Needs_input f ->
        go_left f final left

      | Done r ->
        PipeM (fun () ->
            finalize final >>= fun () ->
            M.return (Done r)
          )

      | PipeM m ->
        PipeM (fun () ->
            m () >>= fun p ->
            M.return (
              go_right final left p
            )
          )

    and go_left next_right final = function
      | Has_output (o, next, final') ->
        go_right final' (next ()) (next_right (Some o))

      | Needs_input f ->
        Needs_input (fun i -> go_left next_right final (f i))

      | Done r ->
        go_right None (Done r) (next_right None)

      | PipeM m ->
        PipeM (fun () ->
            m () >>= fun p ->
            M.return (go_left next_right final p)
          )
    in
    go_right None p q

  let ( $$ ) = compose

  let rec run = function
    | Done r -> M.return r
    | PipeM m -> m () >>= run
    | Has_output _ -> assert false
    | Needs_input _ -> assert false


  let bracket alloc free f =
    let rec add_clean_up f = function
      | Done x ->
        PipeM (fun () ->
            f () >>= fun () ->
            M.return (Done x)
          )

      | Has_output (o, next, final) ->
        Has_output (o,
                    (fun () -> add_clean_up f (next ())),
                    finalizer_compose (Some f) final)

      | Needs_input next ->
        Needs_input (fun i -> add_clean_up f (next i))

      | PipeM m ->
        PipeM (fun () ->
            m () >>= fun next ->
            M.return (add_clean_up f next)
          )
    in
    PipeM (fun () ->
        alloc () >>= fun seed ->
        M.return (add_clean_up (fun () -> free seed) (f seed))
      )

  let rec fold init f =
    Needs_input (function
        | None -> Done init
        | Some i -> fold (f i init) f
      )

  let rec map f =
    let open Monad_infix in
    await () >>= function
    | None -> return ()
    | Some x ->
      yield (f x) >>= fun () -> map f

  let mapi f =
    let open Monad_infix in
    let rec aux i =
      await () >>= function
      | None -> return ()
      | Some x ->
        yield (f i x) >>= fun () -> aux (i + 1)
    in
    aux 0

  let rec filter f =
    let open Monad_infix in
    await () >>= function
    | None -> return ()
    | Some x ->
      if f x then yield x >>= fun () -> filter f
      else filter f

  let rec filter_map f =
    let open Monad_infix in
    await () >>= function
    | None -> return ()
    | Some x ->
      match f x with
      | None -> filter_map f
      | Some y -> yield y >>= fun () -> filter_map f

  let from_list l =
    let open Monad_infix in
    let rec loop = function
      | [] -> Done ()
      | h :: t ->
        yield h >>= fun () ->
        loop t
    in
    loop l

  let to_list () =
    let open Monad_infix in
    let rec loop accu =
      await () >>= function
      | None -> Done (List.rev accu)
      | Some x -> loop (x :: accu)
    in
    loop []

  let all () =
    let open Monad_infix in
    let rec loop accu =
      await () >>= function
      | None -> Done (Ok (List.rev accu))
      | Some (Ok x) -> loop (x :: accu)
      | Some (Error _ as e) -> Done e
    in
    loop []

  let rec loop f st =
    let open Monad_infix in
    await () >>= fun i ->
    let (st', ys) = f st i in
    let rec loop' ys =
      match ys, i with
      | [], None -> Done ()
      | [], Some _ -> loop f st'
      | h :: t, _ ->
        yield h >>= fun () -> loop' t
    in
    loop' ys

  let rec loop' f st =
    let open Monad_infix in
    await () >>= fun i ->
    match f st i with
    | Ok (st', ys) ->
      let rec inner ys =
        match ys, i with
        | [], None -> Done (Ok ())
        | [], Some _ -> loop' f st'
        | h :: t, _ ->
          yield h >>= fun () -> inner t
      in
      inner ys
    | Error _ as e -> Done e

  let drop n =
    let open Monad_infix in
    let rec loop k =
      await () >>= function
      | None -> return ()
      | Some x ->
        if k <= 0 then
          yield x >>= fun () -> loop 0
        else
          loop (k - 1)
    in
    loop n
end

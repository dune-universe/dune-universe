module Make
    (Repr : Basic_intf.Lang.Empty)
    (Monad : Basic_intf.Codegen_monad with type 'a m = 'a Repr.m)
    (B : Basic_intf.Lang.Bool with type 'a m = 'a Repr.m)
    (R : Basic_intf.Lang.Ring with type 'a m = 'a Repr.m)
    (R_ord : Basic_intf.Lang.Infix_order
               with type 'a m = 'a Repr.m
                and type t = R.t)
    (Loop : Basic_intf.Lang.Loop with type 'a m = 'a Repr.m and type index = R.t)
    (P : Basic_intf.Lang.Product with type 'a m = 'a Repr.m)
    (E : Basic_intf.Lang.Exn with type 'a m = 'a Repr.m)
    (M : Basic_intf.Lang.Sequencing with type 'a m = 'a Repr.m) :
  Intf.Tensor
    with type 'a k = 'a Monad.t
     and type 'a m = 'a Repr.m
     and type pos = R.t = struct
  type 'a k = 'a Monad.t

  type 'a m = 'a Repr.m

  type pos = R.t

  module Path = struct
    type _ t = End : pos t | L : 'a t -> ('a * 'b) t | R : 'a t -> ('b * 'a) t

    let empty = End

    let l x = L x

    let r x = R x

    let left () = l empty

    let right () = r empty
  end

  type 'a t = One : R.t m -> pos t | Prod : 'a t * 'b t -> ('a * 'b) t

  let rank_one length = One length

  let rank_two l1 l2 = Prod (One l1, One l2)

  let scalar = One R.one

  let empty = One R.zero

  let tensor x y = Prod (x, y)

  let rec numel : type a. a t -> pos m =
   fun shape ->
    match shape with One dim -> dim | Prod (l, r) -> R.mul (numel l) (numel r)

  let rec proj : type a. a t -> a Path.t -> pos t =
   fun shape path ->
    match (shape, path) with
    | (One _, Path.End) -> shape
    | (Prod (l, _), L p) -> proj l p
    | (Prod (_, r), R p) -> proj r p
    | _ ->
        (* can't be refuted because of abstract types *)
        assert false

  let dim shape path =
    match proj shape path with One dim -> dim | _ -> assert false

  let rec mem : type a. a t -> a m -> bool m =
    fun (type a) (shape : a t) (pos : a m) ->
     match shape with
     | One dim -> R_ord.(B.(R.zero <= pos && pos < dim))
     | Prod (left, right) ->
         M.(
           let* lpos = P.fst pos in
           let* rpos = P.snd pos in
           let* lres = mem left lpos in
           let* rres = mem right rpos in
           B.(lres && rres))

  let rec equal : type a. a t -> a t -> bool m =
    fun (type a) (shape1 : a t) (shape2 : a t) ->
     match (shape1, shape2) with
     | (One p, One p') -> R_ord.(p = p')
     | (Prod (l1, r1), Prod (l2, r2)) ->
         M.(
           let* lres = equal l1 l2 in
           let* rres = equal r1 r2 in
           B.(lres && rres))
     | _ ->
         (* can't be refuted because of abstract types *)
         assert false

  let rec pos_equal : type a. a t -> a m -> a m -> bool m =
   fun shape pos1 pos2 ->
    match shape with
    | One _ -> R_ord.(pos1 = pos2)
    | Prod (ls, rs) ->
        let open M in
        let* lpos1 = P.fst pos1 in
        let* rpos1 = P.snd pos1 in
        let* lpos2 = P.fst pos2 in
        let* rpos2 = P.snd pos2 in
        B.(pos_equal ls lpos1 lpos2 && pos_equal rs rpos1 rpos2)

  let rec concat : type a. a t -> a t -> a Path.t -> a t k =
   fun prod1 prod2 path ->
    let open Monad.Infix in
    match path with
    | End -> (
        match (prod1, prod2) with
        | (One p, One p') -> Monad.return (One (R.add p p'))
        | _ ->
            (* can't be refuted because of abstract types *)
            assert false)
    | L path -> (
        match (prod1, prod2) with
        | (Prod (l1, r1), Prod (l2, r2)) ->
            let*! _ =
              B.dispatch (equal r1 r2) @@ function
              | false -> E.raise_ Intf.Dimensions_mismatch
              | true -> M.unit
            in
            let* lres = concat l1 l2 path in
            Monad.return (Prod (lres, r1))
        | _ ->
            (* can't be refuted because of abstract types *)
            assert false)
    | R path -> (
        match (prod1, prod2) with
        | (Prod (l1, r1), Prod (l2, r2)) ->
            let*! _ =
              B.dispatch (equal l1 l2) @@ function
              | false -> E.raise_ Intf.Dimensions_mismatch
              | true -> M.unit
            in
            let* rres = concat r1 r2 path in
            Monad.return (Prod (l1, rres))
        | _ ->
            (* can't be refuted because of abstract types *)
            assert false)

  module type Storage = Basic_intf.Lang.Storage with type 'a m = 'a m
  (* We declare this here to avoid resorting to higher-kinded encodings. *)

  type 'elt storage = (module Storage with type elt = 'elt)

  let rec iter : type a. (a m -> unit m) -> a t -> unit m =
   fun f shape ->
    match shape with
    | One dim -> Loop.for_ ~start:R.zero ~stop:R.(sub dim one) f
    | Prod (l, r) ->
        iter
          (fun l_index -> iter (fun r_index -> f (P.prod l_index r_index)) r)
          l

  let fold :
      type acc a.
      acc storage -> (a m -> acc m -> acc m) -> a t -> acc m -> acc m =
    fun (type acc) ((module Storage) : acc storage) f shape init ->
     let open M in
     let* acc = Storage.create init in
     let rec loop : type a. (a m -> unit m) -> a t -> unit m =
       fun (type a) (f : a m -> unit m) (shape : a t) ->
        match shape with
        | One dim -> Loop.for_ ~start:R.zero ~stop:R.(sub dim one) f
        | Prod (l, r) ->
            loop
              (fun l_index ->
                loop (fun r_index -> f (P.prod l_index r_index)) r)
              l
     in
     seq
       (loop (fun pos -> Storage.set acc (f pos (Storage.get acc))) shape)
       (fun () -> Storage.get acc)

  let fst : type a b. (a * b) t -> a t =
   fun shape -> match shape with One _ -> assert false | Prod (l, _) -> l

  let snd : type a b. (a * b) t -> b t =
   fun shape -> match shape with One _ -> assert false | Prod (_, r) -> r

  module Morphism = struct
    type ('a, 'b) morphism =
      { dom : 'a t; range : 'b t; pos_transform : 'a m -> 'b m }

    type 'a obj = 'a t

    type ('a, 'b) t = ('a, 'b) morphism

    let underlying : ('a, 'b) t -> 'a m -> 'b m = fun m -> m.pos_transform

    let identity shape =
      let pos_transform = Fun.id in
      { dom = shape; range = shape; pos_transform }
      [@@inline]

    let compose m1 m2 =
      let pos_transform p = m2.pos_transform (m1.pos_transform p) in
      { dom = m1.dom; range = m2.range; pos_transform }
      [@@inline]

    let domain m = m.dom

    let range m = m.range

    let tensor : type a b c d. (a, b) t -> (c, d) t -> (a * c, b * d) t =
      fun (type a b c d) (m1 : (a, b) morphism) (m2 : (c, d) morphism) ->
       let pos_transform (p : (a * c) m) =
         M.(
           let* lp = P.fst p in
           let* rp = P.snd p in
           let* lp' = m1.pos_transform lp in
           let* rp' = m2.pos_transform rp in
           P.prod lp' rp')
       in
       { dom = tensor m1.dom m2.dom;
         range = tensor m1.range m2.range;
         pos_transform
       }

    let rec pullback_at_path :
        type a. (pos, pos) t -> a Path.t -> a obj -> (a, a) t k =
      fun (type a) (m : (pos, pos) t) (path : a Path.t) (s : a obj) ->
       let open Monad.Infix in
       match (path, s) with
       | (Path.End, One _) ->
           let*! _ =
             B.dispatch (equal s m.range) @@ function
             | false -> E.raise_ Intf.Dimensions_mismatch
             | true -> M.unit
           in
           Monad.return (m : (a, a) morphism)
       | (Path.L lp, Prod (ls, rs)) ->
           let* lm = pullback_at_path m lp ls in
           Monad.return (tensor lm (identity rs))
       | (Path.R rp, Prod (ls, rs)) ->
           let* rm = pullback_at_path m rp rs in
           Monad.return (tensor (identity ls) rm)
       | _ -> assert false

    let rec pullback_pointwise : type a. (pos, pos) t -> a obj -> (a, a) t k =
      fun (type a) (m : (pos, pos) t) (s : a obj) ->
       let open Monad.Infix in
       match s with
       | One _ ->
           let*! _ =
             B.dispatch (equal s m.range) @@ function
             | false -> E.raise_ Intf.Dimensions_mismatch
             | true -> M.unit
           in
           Monad.return (m : (a, a) morphism)
       | Prod (ls, rs) ->
           let* lm = pullback_pointwise m ls in
           let* rm = pullback_pointwise m rs in
           Monad.return (tensor lm rm)

    let sub ~ofs : pos obj -> pos obj -> (pos, pos) morphism k =
     fun dom range ->
      match (dom, range) with
      | (One dom_dim, One range_dim) ->
          let open Monad.Infix in
          let*! _ =
            B.(
              dispatch
                R_ord.(
                  R.zero <= ofs && R.zero < dom_dim
                  && R.add ofs dom_dim <= range_dim)
              @@ function
              | false -> E.raise_ Intf.Dimensions_mismatch
              | true -> M.unit)
          in
          let pos_transform (p : pos m) = R.add p ofs in
          Monad.return { dom; range; pos_transform }
      | _ -> assert false
  end
end

(** Instantiate some typical schemes *)
module BL = Basic_impl.Lang

module Int =
  Make (BL.Empty) (BL.Codegen) (BL.Bool) (BL.Int) (BL.Int) (BL.Loop)
    (BL.Product)
    (BL.Exn)
    (BL.Sequencing)

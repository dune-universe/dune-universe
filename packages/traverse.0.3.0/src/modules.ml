module type TypeS = sig
  type t
end

module Monoid = struct
  module type S = sig
    type t

    val zero : t

    val ( + ) : t -> t -> t
  end

  module Addition = struct
    type t = int

    let zero = 0

    let ( + ) = ( + )
  end

  type 'a t = (module S with type t = 'a)
end

module Functor = struct
  module type S = sig
    type 'a t

    val map : ('a -> 'b) -> 'a t -> 'b t
  end
end

module Applicative = struct
  module type S = sig
    include Functor.S

    val pure : 'a -> 'a t

    val apply : ('a -> 'b) t -> (unit -> 'a t) -> 'b t
  end

  module Iter : S with type 'a t = unit = struct
    type 'a t = unit

    let map _f () = ()

    let pure _x = ()

    let apply _f x = x ()
  end

  module Map : S with type 'a t = 'a = struct
    type 'a t = 'a

    let map f x = f x

    let pure x = x

    let apply f x = f (x ())
  end

  module Reduce (Monoid : Monoid.S) : S with type 'a t = Monoid.t = struct
    type 'a t = Monoid.t

    let map _f accu =
      accu

    let pure _x =
      Monoid.zero

    let apply a b = Monoid.(a + b ())
  end

  module Env (E : TypeS) (Base : S) : S
  with type 'a t = E.t -> 'a Base.t = struct
    type 'a t = E.t -> 'a Base.t

    let map f x env =
      Base.map f (x env)

    let pure x _env =
      Base.pure x

    let apply f x env =
      Base.apply (f env) (fun () -> x () env)
  end

  module Fold (Accu : TypeS) : S with type 'a t = Accu.t -> Accu.t = struct
    type 'a t = Accu.t -> Accu.t

    let map _f x accu =
      x accu

    let pure _x accu =
      accu

    let apply f x accu =
      x () (f accu)
  end

  module Pair (U : S) (V : S) : S
  with type 'a t = 'a U.t * 'a V.t = struct
    type 'a t = 'a U.t * 'a V.t

    let map f (u, v) =
      (U.map f u, V.map f v)

    let pure x =
      (U.pure x, V.pure x)

    let apply (fu, fv) uv =
      let uv = lazy (uv ()) in
      (U.apply fu (fun () -> fst (Lazy.force uv)),
        V.apply fv (fun () -> snd (Lazy.force uv)))
  end

  module Forall : S with type 'a t = bool = struct
    type 'a t = bool

    let map _f b = b

    let pure _x = true

    let apply f x = f && x ()
  end

  module Exists : S with type 'a t = bool = struct
    type 'a t = bool

    let map _f b = b

    let pure _x = false

    let apply f x = f || x ()
  end

  module Option (Base : S) : S with type 'a t = 'a Base.t option = struct
    type 'a t = 'a Base.t option

    let map f o =
      Option.map (Base.map f) o

    let pure x =
      Some (Base.pure x)

    let apply f o =
      Option.bind f (fun f ->
        Option.map (fun v ->
          Base.apply f (fun () -> v)) (o ()))
  end

  module Result (Base : S) (Err : TypeS) : S
  with type 'a t = ('a Base.t, Err.t) result = struct
    type 'a t = ('a Base.t, Err.t) result

    let map f r =
      Result.map (Base.map f) r

    let pure x =
      Ok (Base.pure x)

    let apply f r =
      Result.bind f (fun f ->
        Result.map (fun v ->
          Base.apply f (fun () -> v)) (r ()))
  end

  module List (Base : S) : S
  with type 'a t = 'a Base.t list = struct
    type 'a t = 'a Base.t list

    let map f l =
      List.map (Base.map f) l

    let pure x =
      [Base.pure x]

    let apply f r =
      match f with
      | [] -> []
      | _ ->
          let r = r () in
          List.concat_map (fun f ->
            List.map (fun v ->
              Base.apply f (fun () -> v)) r) f
  end
end

type ('a, 'b) length =
  | Zero : (unit, unit) length
  | Succ : ('a, 'b) length -> (_ * 'a, _ * 'b) length

module type SequenceOfUnaryTypeS = sig
  type 'item x

  type 'sequence t =
    | [] : unit t
    | (::) : 'hd x * 'tl t -> ('hd * 'tl) t
end

module rec Sequence : SequenceOfUnaryTypeS with type 'a x = 'a = Sequence

module type SequenceOfBinaryTypeS = sig
  type ('a, 'b) x

  type ('a_s, 'b_s) t =
    | [] : (unit, unit) t
    | (::) : ('a, 'b) x * ('a_s, 'b_s) t -> ('a * 'a_s, 'b * 'b_s) t
end

module Arity = struct
  module type S = sig
    type ('a, 'b) t

    module ArrowSequence : SequenceOfBinaryTypeS with
    type ('a, 'b) x = ('a, 'b) t -> 'b

    val destruct :
        ('a, 'b) length ->
        ('c -> 'a Sequence.t) ->
        (('a, 'b) ArrowSequence.t -> 'd) ->
        ('c, 'd) t
  end

  module type NonNullS = sig
    module Pred : S

    include S with type ('a, 'b) t = 'a -> ('a, 'b) Pred.t
  end

  module O : S with type ('a, 'b) t = 'b = struct
    type ('a, 'b) t = 'b

    module rec ArrowSequence : SequenceOfBinaryTypeS with
    type ('a, 'b) x = ('a, 'b) t -> 'b = ArrowSequence

    let rec id_arrow_sequence :
      type a b . (a, b) length -> (a, b) ArrowSequence.t =
    fun l ->
      match l with
      | Zero -> []
      | Succ l' ->
          ArrowSequence.(Fun.id :: id_arrow_sequence l')

    let destruct l _p k =
      k (id_arrow_sequence l)
  end

  module S (Pred : S) : NonNullS with module Pred = Pred = struct
    module Pred = Pred

    type ('a, 'b) t = 'a -> ('a, 'b) Pred.t

    module rec ArrowSequence : SequenceOfBinaryTypeS with
    type ('a, 'b) x = ('a, 'b) t -> 'b = ArrowSequence

    let rec cons_arrow_sequence :
      type a b .
      a Sequence.t ->
      ((a, b) ArrowSequence.t -> 'c) ->
      (a, b) Pred.ArrowSequence.t -> 'c =
    fun s k a ->
      let open Sequence in
      match s with
      | [] ->
          let open Pred.ArrowSequence in
          let [] = a in
          k []
      | hd :: tl ->
          let open Pred.ArrowSequence in
          let hd' :: tl' = a in
          cons_arrow_sequence tl
            (fun s -> k ((fun f -> hd' (f hd)) :: s)) tl'

    let destruct (l : ('a, 'b) length)
        (p : ('c -> 'a Sequence.t))
        (k : (('a, 'b) ArrowSequence.t -> 'd))
        (x : 'c) : ('c, 'd) Pred.t =
      Pred.destruct l p (cons_arrow_sequence (p x) k)
  end

  module A1 = S (O)

  module A2 = S (A1)

  module A3 = S (A2)

  module A4 = S (A3)

  module A5 = S (A4)

  module A6 = S (A5)

  module A7 = S (A6)

  module A8 = S (A7)

  module A9 = S (A8)
end

exception StructuralMismatch

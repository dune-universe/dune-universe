module type TypeS = sig
  type t
end

module Monoid = struct
  module type S = sig
    type t

    val zero : t

    val ( + ) : t -> t -> t
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

    let apply _f _ = ()
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

module type UnaryTypeS = sig
  type 'a t
end

module Arity (T : UnaryTypeS) = struct
  type ('a, 'a_t, 'f, 'result, 'is_empty) t =
    | O : ('a, 'a_t, 'a, 'a_t, [`Empty]) t
    | S : ('a, 'a_t, 'f, 'result, _) t ->
        ('a, 'a_t, 'x -> 'f, 'x T.t -> 'result, [`Not_empty]) t
end

module type SequenceSpecS = sig
  type 'a t

  type 'a desc =
    | Nil
    | Cons of 'a * 'a t

  val destruct : 'a t -> 'a desc

  val construct : 'a desc -> 'a t
end

module type SequenceS = sig
  type 'a s

  module Arity : sig
    type ('a, 'a_t, 'f, 'result, 'is_empty) t =
      | O : ('a, 'a_t, 'a, 'a_t, [`Empty]) t
      | S : ('a, 'a_t, 'f, 'result, _) t ->
          ('a, 'a_t, 'x -> 'f, 'x s -> 'result, [`Not_empty]) t
  end

  module Make (Applicative : Applicative.S) : sig
    val traverse :
        ('a Applicative.t, 'a s Applicative.t, 'f, 'result,
         [`Not_empty]) Arity.t -> 'f -> 'result
  end
end

module Sequence (S : SequenceSpecS) : SequenceS with type 'a s = 'a S.t = struct
  type 'a s = 'a S.t

  module Arity = Arity (S)

  let cons hd tl =
    S.construct (Cons (hd, tl))

  module Make (Applicative : Applicative.S) = struct
    open Applicative

    let rec traverse : type a f result .
          (a Applicative.t, a S.t Applicative.t, f, result,
            [`Not_empty]) Arity.t -> f -> result =
    fun arity f ->
      let rec empty : type a f result w .
        (a Applicative.t, a S.t Applicative.t, f, result, w) Arity.t ->
          result =
      fun arity ->
        match arity with
        | Arity.O -> pure (S.construct Nil)
        | Arity.S arity' ->
            fun l ->
              match S.destruct l with
              | Nil -> empty arity'
              | Cons _ -> invalid_arg "traverse" in
      let rec non_empty : type a f result w .
        (a Applicative.t, a S.t Applicative.t, f, result, w) Arity.t -> f ->
          (unit -> result) -> result =
      fun arity f k ->
        match arity with
        | Arity.O -> apply (map cons f) k
        | Arity.S arity ->
            fun l ->
              match S.destruct l with
              | Nil -> invalid_arg "traverse"
              | Cons (hd, tl) ->
                  non_empty arity (f hd) (fun () -> k () tl) in
      let Arity.S arity' = arity in
      fun a ->
      match S.destruct a with
      | Nil -> empty arity'
      | Cons (hd, tl) -> non_empty arity' (f hd) (fun () -> traverse arity f tl)
  end
end

module List = Sequence (struct
  type 'a t = 'a list

  type 'a desc =
    | Nil
    | Cons of 'a * 'a t

  let destruct l =
    match l with
    | [] -> Nil
    | hd :: tl -> Cons (hd, tl)

  let construct d =
    match d with
    | Nil -> []
    | Cons (hd, tl) -> hd :: tl
end)

module Seq = Sequence (struct
  type 'a t = 'a Seq.t

  type 'a desc = 'a Seq.node =
    | Nil
    | Cons of 'a * 'a t

  let destruct l = l ()

  let construct d () = d
end)

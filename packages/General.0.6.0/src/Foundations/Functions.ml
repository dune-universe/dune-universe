module Function1 = struct
  type ('a, 'z) t = 'a -> 'z

  let identity x =
    x

  let apply f x =
    f x

  let rev_apply x f =
    f x

  let compose f g =
    fun x -> f (g x)

  module O = struct
    let (@@) = apply
    let (|>) = rev_apply
    let (%) = compose
  end
end

module Function2 = struct
  type ('a, 'b, 'z) t = 'a -> 'b -> 'z

  let flip f =
    fun x y ->
      f y x

  let curry f =
    fun x y ->
      f (x, y)

  let uncurry f =
    fun (x, y) ->
      f x y
end

module Function3 = struct
  type ('a, 'b, 'c, 'z) t = 'a -> 'b -> 'c -> 'z

  let flip f =
    fun x y z ->
      f z y x

  let curry f =
    fun x y z ->
      f (x, y, z)

  let uncurry f =
    fun (x, y, z) ->
      f x y z
end

module Function4 = struct
  type ('a, 'b, 'c, 'd, 'z) t = 'a -> 'b -> 'c -> 'd -> 'z

  let flip f =
    fun x y z u ->
      f u z y x

  let curry f =
    fun x y z u ->
      f (x, y, z, u)

  let uncurry f =
    fun (x, y, z, u) ->
      f x y z u
end

module Function5 = struct
  type ('a, 'b, 'c, 'd, 'e, 'z) t = 'a -> 'b -> 'c -> 'd -> 'e -> 'z

  let flip f =
    fun x y z u v ->
      f v u z y x

  let curry f =
    fun x y z u v ->
      f (x, y, z, u, v)

  let uncurry f =
    fun (x, y, z, u, v) ->
      f x y z u v
end

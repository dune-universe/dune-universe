module type PT = sig
  type 'a t

  val map : f:('a -> 'b) -> 'a t -> 'b t
  val map2 : f:('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val iter : f:('a -> unit) -> 'a t -> unit
  val iter2 : f:('a -> 'b -> unit) -> 'a t -> 'b t -> unit
end

module Single = struct
  type 'a t = S of 'a

  let unpack : 'a t -> 'a = fun (S x) -> x
  let pack : 'a -> 'a t = fun x -> S x
  let map ~f a = pack (f (unpack a))
  let map2 ~f a b = pack (f (unpack a) (unpack b))
  let iter ~f a = f (unpack a)
  let iter2 ~f a b = f (unpack a) (unpack b)
end

module Pair = struct
  type 'a t = P of 'a * 'a

  let unpack : 'a t -> 'a * 'a = fun (P (x, y)) -> x, y
  let pack : 'a * 'a -> 'a t = fun (x, y) -> P (x, y)

  let map ~f a =
    let x, y = unpack a in
    pack (f x, f y)


  let map2 ~f a b =
    let xa, ya = unpack a in
    let xb, yb = unpack b in
    pack (f xa xb, f ya yb)


  let iter ~f a =
    let x, y = unpack a in
    f x;
    f y


  let iter2 ~f a b =
    let xa, ya = unpack a in
    let xb, yb = unpack b in
    f xa xb;
    f ya yb
end

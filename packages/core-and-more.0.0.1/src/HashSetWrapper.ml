open Util
open Core

module type ImperativeSet =
sig
  type elt
  type t

  val create : int -> t
  val empty : unit -> t
  val singleton : elt -> t
  val add : elt -> t -> unit
  val remove : elt -> t -> unit
  val size : t -> int
  val is_empty : t -> bool
  val contains : elt -> t -> bool
  val fold : (elt -> 'b -> 'b) -> t -> 'b -> 'b
  val fold2 : (elt -> elt -> 'a -> 'a) -> t -> t -> 'a -> 'a
  val as_list : t -> elt list
  val iter : (elt -> unit) -> t -> unit
  val union : t -> t -> t
  val pp : (Format.formatter -> elt -> unit) -> Format.formatter -> t -> unit
  val update : (elt option -> unit) -> elt -> t -> unit
  val copy : t -> t
end

module HSWrapper(D : Data) =
struct
  module D = struct
    type t = D.t
    [@@deriving ord, show, hash]

    let sexp_of_t _ = failwith "cant call"
  end

  type elt = D.t
  type t = elt Hash_set.t

  let create size =
    Hash_set.create ~size (module D)

  let empty _ =
    Hash_set.create (module D)

  let singleton v =
    Hash_set.of_list (module D) [v]

  let singleton_sized size v =
    Hash_set.of_list ~size (module D) [v]

  let add
      (elt:D.t)
      (s:t)
    : unit =
    Hash_set.add s elt

  let remove
      (elt:D.t)
      (s:t)
    : unit =
    Hash_set.remove s elt

  let size
      (s:t)
    : int =
    Hash_set.length s

  let is_empty
      (s:t)
    : bool =
    Hash_set.is_empty s

  let contains
      (elt:D.t)
      (s:t)
    : bool =
    Hash_set.mem s elt

  let fold
      (type b)
      (f:elt -> b -> b)
      (s:t)
      (init:b)
    : b =
    Hash_set.fold
      ~f:(fun acc e -> f e acc)
      ~init
      s

  let fold2 f a b x =
    let fold2' ea x = fold (f ea) b x in
    fold (fold2') a x

  let exists f s =
    Hash_set.exists
      ~f
      s

  let as_list s =
    Hash_set.to_list
      s

  let iter f s =
    Hash_set.iter ~f s

  let union s1 s2 =
    Hash_set.union s1 s2

  let pp k_pp f s =
    Format.fprintf
      f
      "[";
    iter
      (fun k -> k_pp f k)
      s;
    Format.fprintf
      f
      "]"

  let update
      (f:elt option -> unit)
      (e:elt)
      (s:t)
    : unit =
    if contains e s then
      (f (Some e))
    else
      (f None; add e s)

  let copy
      (x:t)
    : t =
    Hash_set.copy x
end

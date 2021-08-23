(** Direct implementation of the [Intf_lang] signatures. *)

module Empty : Intf_lang.Empty with type 'a m = 'a = struct
  type 'a m = 'a
end

module Sequencing : Intf_lang.Sequencing with type 'a m = 'a = struct
  include Empty

  let seq x y = y x [@@inline]

  let ( let* ) m f = f m [@@inline]

  let unit = ()
end

module Lambda : Intf_lang.Lambda with type 'a m = 'a = struct
  include Empty

  let lam f = f [@@inline]

  let app f x = f x [@@inline]
end

module Product : Intf_lang.Product with type 'a m = 'a = struct
  include Empty

  let prod x y = (x, y) [@@inline]

  let fst (x, _) = x [@@inline]

  let snd (_, y) = y [@@inline]
end

module Bool : Intf_lang.Bool with type 'a m = 'a = struct
  include Empty

  let true_ = true

  let false_ = false

  let ( || ) = ( || )

  let ( && ) = ( && )

  let dispatch cond k = k cond [@@inline]
end

module Int = struct
  include Std.Int
  include Empty

  let ( = ) (x : int) (y : int) = x = y

  let compare (x : int) (y : int) = if x < y then -1 else if x > y then 1 else 0

  let ( <> ) (x : int) (y : int) = x <> y

  let ( < ) (x : int) (y : int) = x < y

  let ( <= ) (x : int) (y : int) = x <= y

  let ( > ) (x : int) (y : int) = x > y

  let ( >= ) (x : int) (y : int) = x >= y

  let of_int x = x
end

module Float = struct
  include Std.Float
  include Empty

  let ( = ) = Float.equal

  let ( <> ) (x : float) (y : float) = x <> y

  let ( < ) (x : float) (y : float) = x < y

  let ( <= ) (x : float) (y : float) = x <= y

  let ( > ) (x : float) (y : float) = x > y

  let ( >= ) (x : float) (y : float) = x >= y
end

module Complex = struct
  include Std.Complex
  include Empty

  let of_int (x : int) = Complex.{ re = float_of_int x; im = 0.0 }
end

module Rational = struct
  include Std.Q
  include Empty
end

module String = struct
  include Std.String
  include Empty
end

module Exn = struct
  include Empty

  let const exn = exn

  let raise_ exn = raise exn
end

module Loop = struct
  include Empty

  type index = int

  let for_ ~start ~stop f =
    for i = start to stop do
      f i
    done

  let while_ ~cond f =
    while cond () do
      f ()
    done
end

module Make_enum (X : sig
  type t

  val enum : t -> int

  val all : t array
end) =
struct
  include Empty

  type t = X.t

  let enum = X.enum

  let all = X.all

  let const x = x

  let dispatch x f = f x
end

module Make_storage (X : sig
  type t
end) =
struct
  include Empty

  type t = X.t ref

  type elt = X.t

  let create x = ref x

  let set x e = x := e

  let get x = !x
end

module Make_array (X : sig
  type t
end) =
struct
  type 'a m = 'a

  type t = X.t array

  type elt = X.t

  type index = int

  let make = Array.make

  let copy = Array.copy

  let sub = Array.sub

  let length = Array.length

  let blit = Array.blit

  let get = Array.get

  let unsafe_get = Array.unsafe_get

  let set = Array.set

  let unsafe_set = Array.unsafe_set
end

module Float_storage = struct
  include Empty

  type t = float ref

  type elt = float

  let create (x : float) : t = ref x

  let set x (e : float) = x := e

  let get (x : t) = !x
end

module Make_const (X : sig
  type t
end) =
struct
  include Empty

  type t = X.t

  let const x = x
end

module Codegen = Impl_monad.Codegen_identity (Empty) (Sequencing)

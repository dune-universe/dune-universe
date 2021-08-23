(** Extension of basic modules making them compatible with the [Std] module type *)

open Intf_std

module Bool = struct
  include Bool

  let hash = function false -> 0 | true -> 1

  let pp = Format.pp_print_bool
end

module _ : Std with type t = bool = Bool

module Int = struct
  include Int

  let hash (i : int) = i

  let pp = Format.pp_print_int
end

module _ : Std with type t = int = Int

module Float = struct
  include Float

  let pp = Format.pp_print_float
end

module _ : Std with type t = float = Float

module Complex = struct
  include Complex

  let pp fmtr { Complex.re; im } =
    Format.fprintf fmtr "@[{ re = %f; im = %f }@]" re im

  let hash { Complex.re; im } = Hashtbl.hash (re, im) [@@inline]

  let equal (x : Complex.t) (y : Complex.t) = x = y [@@inline]

  let compare (x : Complex.t) (y : Complex.t) =
    let c = Float.compare x.re y.re in
    if c <> 0 then c else Float.compare x.im y.im
    [@@inline]
end

module _ : Std with type t = float = Float

module String = struct
  include String

  let hash = Hashtbl.hash

  let pp = Format.pp_print_string
end

module _ : Std with type t = string = String

module Z = struct
  include Z

  let pp = pp_print
end

module _ : Std with type t = Z.t = Z

module Q = struct
  include Q

  let hash = Hashtbl.hash

  let pp = pp_print
end

module _ : Std with type t = Q.t = Q

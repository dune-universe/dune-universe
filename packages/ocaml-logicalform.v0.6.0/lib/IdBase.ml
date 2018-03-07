open Base

module type BASE =
  sig
    type t [@@deriving sexp]

    include Comparable.With_zero with type t := t

    include Intable.S with type t := t

    include Stringable.S with type t := t
  end

module type I =
  sig
    include BASE

    include Properties.Negatable with type t := t
  end

module Make (B : BASE) : I with type t = B.t =
  struct
    include B

    let not_ (i : t) : t = of_int_exn (- (to_int_exn i))

    let to_string (i : t) : string = Printf.sprintf "{%s}" (B.to_string i)

    let of_string (str : string) : t =
      B.of_string (String.sub str ~pos:1 ~len:((String.length str) - 2))
  end
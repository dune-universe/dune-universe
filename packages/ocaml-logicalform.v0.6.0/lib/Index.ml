open Base

type nonzero_sign = Pos | Neg [@@deriving sexp]

module type I =
  sig
    module T : IdBase.I

    type t = private Valid of (nonzero_sign * T.t) [@@deriving sexp]

    val id : T.t -> t

    val to_nonzero_int_exn : t -> int

    val of_nonzero_int_exn : int -> t

    include Stringable.S with type t := t

    include Properties.Negatable with type t := t
  end

module Make (T : IdBase.BASE) : I with module T = IdBase.Make(T) =
  struct
    module T = IdBase.Make(T)

    type t = Valid of (nonzero_sign * T.t)

    let id (i : T.t) : t =
      match T.sign i with
      | Pos -> Valid (Pos, i)
      | Neg -> Valid (Neg, T.of_int_exn (- (T.to_int_exn i)))
      | _ -> raise (Exceptions.Invalid_literal (T.to_string i))

    let not_ (Valid (sgn, i) : t) : t = match sgn with
                                        | Pos -> Valid (Neg, i)
                                        | Neg -> Valid (Pos, i)

    let to_nonzero_int_exn (Valid (sgn, i) : t) : int =
      match sgn with
      | Pos -> T.to_int_exn i
      | Neg -> - (T.to_int_exn i)

    let of_nonzero_int_exn (i : int) : t = id (T.of_int_exn i)

    let to_string (Valid (sgn, i) : t) : string =
      match sgn with
      | Pos -> T.to_string i
      | Neg -> T.to_string (T.not_ i)

    let of_string (str : string) : t = id (T.of_string str)

    let t_of_sexp (sexp : Sexp.t) : t =
      match sexp with
      | Atom str -> of_string str
      | _ -> raise (Exceptions.invalid_sexp_exception ~ctx:"index" sexp)

    let sexp_of_t (id : t) : Sexp.t = Atom (to_string id)
  end
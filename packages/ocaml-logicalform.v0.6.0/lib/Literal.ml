open Base

module type I =
  sig
    open Properties

    module Id : Index.I

    type nonrec t = [ `True
                    | `False
                    | `Pos of Id.T.t
                    | `Neg of Id.T.t
                    | `L of Id.t ]
                    [@@deriving sexp]

    include Negatable with type t := t

    include Base.Stringable.S with type t := t

    include PrettyPrint.I with type t := t

    include NeedsValidation with type t := t

    include Executable with type t := t
  end

module Make (Id : Index.I) : I with module Id = Id =
  struct
    open PrettyPrint

    module Id = Id

    type nonrec t = [ `True
                    | `False
                    | `Pos of Id.T.t
                    | `Neg of Id.T.t
                    | `L of Id.t ]
                    [@@deriving sexp]

    let validate (lit : t) : t =
      match lit with
      | `Pos i -> `L (Id.id i)
      | `Neg i -> `L (Id.not_ (Id.id i))
      | _ -> lit

    let of_string (str : string) : t =
      let ( === ) = String.equal
      in if str === Style.Infix.default.true_string then `True
         else if str === Style.Infix.default.false_string then `False
         else `L (Id.of_string str)

    let to_string (lit : t) : string =
      match lit with
      | `True -> Style.Infix.default.true_string
      | `False -> Style.Infix.default.false_string
      | `Pos i -> Id.T.to_string i
      | `Neg i -> Id.T.to_string (Id.T.not_ i)
      | `L id -> Id.to_string id

    let to_pretty_string ?(style = Style.Infix.default) (lit : t) : string =
      let sfmt = Printf.sprintf
      in match lit with
         | `True -> style.true_string
         | `False -> style.false_string
         | `Pos i -> sfmt "%s%d" style.var_prefix (Id.T.to_int_exn i)
         | `Neg i
           -> sfmt "%s%s%d" style._not_ style.var_prefix (Id.T.to_int_exn i)
         | `L (Valid (sgn, id))
           -> let i = Id.T.to_int_exn id
              in match sgn with
                 | Pos -> sfmt "%s%d" style.var_prefix i
                 | Neg -> sfmt "%s%s%d" style._not_ style.var_prefix i

    let of_pretty_sexp ?(style = Style.Prefix.default) (sexp_lit : Sexp.t) : t =
      let open String
      in let open Exceptions
      in let plen = length style.var_prefix
      in match sexp_lit with
         | Atom a
           -> if equal a style.true_string then `True
              else if equal a style.false_string then `False
              else if equal (subo a ~len:plen) style.var_prefix
                   then `L (Id.id (Id.T.of_int_exn
                              (Int.of_string (subo a ~pos:plen))))
                   else raise (invalid_sexp_exception ~ctx:"literal" sexp_lit)
         | List [ Atom neg ; Atom a ]
           -> if equal neg style.not_
              then if equal (subo a ~len:plen) style.var_prefix
                   then `L (Id.not_ (Id.id (Id.T.of_int_exn
                              (Int.of_string (subo a ~pos:plen)))))
                   else raise (invalid_sexp_exception ~ctx:"literal" (Atom a))
              else raise (invalid_sexp_exception ~ctx:"literal" sexp_lit)
         | _ -> raise (invalid_sexp_exception ~ctx:"literal" sexp_lit)

    let to_pretty_sexp ?(style = Style.Prefix.default) (lit : t) : Sexp.t =
      let sfmt = Printf.sprintf
      in match lit with
         | `True -> Atom style.true_string
         | `False -> Atom style.false_string
         | `Pos i -> Atom (sfmt "%s%d" style.var_prefix (Id.T.to_int_exn i))
         | `Neg i
           -> List [ Atom style.not_
                   ; Atom (sfmt "%s%d" style.var_prefix (Id.T.to_int_exn i)) ]
         | `L (Valid (sgn, id))
           -> let i = Id.T.to_int_exn id
              in match sgn with
                 | Pos -> Atom (sfmt "%s%d" style.var_prefix i)
                 | Neg -> List [ Atom style.not_
                               ; Atom (sfmt "%s%d" style.var_prefix i) ]

    let not_ (lit : t) : t =
      match lit with
      | `True -> `False
      | `False -> `True
      | `Pos i
      | `Neg i -> `L (Id.id (Id.T.not_ i))
      | `L id -> `L (Id.not_ id)

    let eval (lit : t) (bools : bool array) : bool option =
      match lit with
      | `True -> Some true
      | `False -> Some false
      | `Pos i -> (try Some bools.((Id.T.to_int_exn i) - 1) with _ -> None)
      | `Neg i -> (try Some (not bools.((Id.T.to_int_exn i) - 1)) with _ -> None)
      | `L (Valid (sgn, id))
         -> let i = (Id.T.to_int_exn id) - 1
            in match sgn with Pos -> (try Some bools.(i) with _ -> None)
                            | Neg -> (try Some (not bools.(i)) with _ -> None)
  end
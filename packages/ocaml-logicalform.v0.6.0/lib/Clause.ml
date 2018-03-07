open Base

module type I =
  sig
    open Properties

    module Literal : Literal.I

    type nonrec t = [ Literal.t
                    | `Or of Literal.t list ] [@@deriving sexp]

    include Disjunctable with type t := t

    include NeedsValidation with type t := t

    include Executable with type t := t

    include PrettyPrint.I with type t := t
  end

module Make (L : Literal.I) : I with module Literal = L =
  struct
    open PrettyPrint

    module Literal = L

    type literal = Literal.t [@@deriving sexp_poly]

    type nonrec t = [ literal
                    | `Or of literal list ] [@@deriving sexp]

    let validate (clause : t) : t =
      match clause with
      | #literal as l -> (Literal.validate l :> t)
      | `Or [] -> `False
      | `Or [ lit ] -> (Literal.validate lit :> t)
      | `Or lits -> `Or (List.map ~f:Literal.validate lits)

    let to_pretty_string ?(style = Style.Infix.default) (clause : t) : string =
      match clause with
      | #literal as l -> Literal.to_pretty_string ~style l
      | `Or [] -> Literal.to_pretty_string ~style `False
      | `Or lits
        -> let lit_strs = (List.map lits ~f:(Literal.to_pretty_string ~style))
           in Printf.sprintf "(%s)" (String.concat ~sep:style._or_ lit_strs)

    let to_pretty_sexp ?(style = Style.Prefix.default) (clause : t) : Sexp.t =
      match clause with
      | #literal as lit -> Literal.to_pretty_sexp ~style lit
      | `Or [] -> Literal.to_pretty_sexp ~style `False
      | `Or [ lit ] -> Literal.to_pretty_sexp ~style lit
      | `Or lits -> List ((Atom style.or_)
                          :: (List.map ~f:(Literal.to_pretty_sexp ~style) lits))

    let of_pretty_sexp ?(style = Style.Prefix.default) (sexp : Sexp.t) : t =
      let open Exceptions
      in let ( === ) = String.equal
      in match sexp with
         | Atom _ -> (Literal.of_pretty_sexp ~style sexp :> t)
         | List ((Atom op) :: rest)
           -> if op === style.or_
              then `Or (List.map ~f:(Literal.of_pretty_sexp ~style) rest)
              else if op === style.not_
                   then (Literal.of_pretty_sexp ~style sexp :> t)
                   else raise (invalid_sexp_exception ~ctx:"clause" sexp)
         |_ -> raise (invalid_sexp_exception ~ctx:"clause" sexp)

    let or_ (clauses : t list) : t =
      match clauses with
      | [] -> `False
      | [ clause ] -> clause
      | _ -> `Or (List.fold_left clauses ~init:[]
                                 ~f:(fun acc c -> match c with
                                                  | #literal as l -> l :: acc
                                                  | `Or lits -> lits @ acc))

    let eval (clause : t) (bools : bool array) : bool option =
      let rec helper (lits : literal list) : bool option =
        let open Option
        in match lits with
         | [] -> Some false
         | l :: ls -> Literal.eval l bools
                      >>= function true -> Some true
                                 | false -> (helper [@tailcall]) ls
      in match clause with
         | #literal as l -> Literal.eval l bools
         | `Or lits -> helper lits
  end
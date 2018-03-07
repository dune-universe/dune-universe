open Base

module type I =
  sig
    open Properties

    module Literal : Literal.I

    type nonrec t = [ Literal.t
                    | `And of Literal.t list ] [@@deriving sexp]

    include Conjunctable with type t := t

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
                    | `And of literal list ] [@@deriving sexp]

    let validate (term : t) : t =
      match term with
      | #literal as l -> (Literal.validate l :> t)
      | `And [] -> `True
      | `And [ lit ] -> (Literal.validate lit :> t)
      | `And lits -> `And (List.map ~f:Literal.validate lits)

    let to_pretty_string ?(style = Style.Infix.default) (term : t) : string =
      match term with
      | #literal as l -> Literal.to_pretty_string ~style l
      | `And [] -> Literal.to_pretty_string ~style `True
      | `And lits
        -> let lit_strs = List.map lits ~f:(Literal.to_pretty_string ~style)
           in Printf.sprintf "(%s)" (String.concat ~sep:style._and_ lit_strs)

    let to_pretty_sexp ?(style = Style.Prefix.default) (term : t) : Sexp.t =
      match term with
      | #literal as l -> Literal.to_pretty_sexp ~style l
      | `And [] -> Literal.to_pretty_sexp ~style `True
      | `And [ lit ] -> Literal.to_pretty_sexp ~style lit
      | `And lits -> List ((Atom style.and_)
                           :: (List.map ~f:(Literal.to_pretty_sexp ~style) lits))

    let of_pretty_sexp ?(style = Style.Prefix.default) (sexp : Sexp.t) : t =
      let open Exceptions
      in let ( === ) = String.equal
      in match sexp with
         | Atom _ -> (Literal.of_pretty_sexp ~style sexp :> t)
         | List ((Atom op) :: rest)
           -> if op === style.and_
              then `And (List.map ~f:(Literal.of_pretty_sexp ~style) rest)
              else if op === style.not_
                   then (Literal.of_pretty_sexp ~style sexp :> t)
                   else raise (invalid_sexp_exception ~ctx:"term" sexp)
         | _ -> raise (invalid_sexp_exception ~ctx:"term" sexp)

    let and_ (terms : t list) : t =
      match terms with
      | [] -> `True
      | [ term ] -> term
      | _ -> `And (List.fold_left terms ~init:[]
                                  ~f:(fun acc c -> match c with
                                                  | #literal as l -> l :: acc
                                                  | `And lits -> lits @ acc))

    let eval (term : t) (bools : bool array) : bool option =
      let rec helper (lits : Literal.t list) : bool option =
        let open Option
        in match lits with
           | [] -> Some true
           | l :: ls -> (Literal.eval l) bools
                        >>= (function false -> Some false
                                    | true -> (helper [@tailcall]) ls)
      in match term with
         | #literal as l -> Literal.eval l bools
         | `And lits -> helper lits
  end
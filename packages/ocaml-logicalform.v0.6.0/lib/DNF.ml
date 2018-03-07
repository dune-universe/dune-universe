open Base

module type I =
  sig
    module Term : Term.I

    type nonrec t = [ Term.t
                    | `Or of Term.t list ]

    include Form.I with module Literal := Term.Literal
                    and type t := t

    include PrettyPrint.I with type t := t
  end

module Make (T : Term.I) : I with module Term = T =
  struct
    open PrettyPrint

    module Term = T

    module Literal = Term.Literal

    type literal = Literal.t

    type term = Term.t [@@deriving sexp_poly]

    type nonrec t = [ term
                    | `Or of term list ] [@@deriving sexp]

    let validate (dnf : t) : t =
      match dnf with
      | #term as t -> (Term.validate t :> t)
      | `Or [] -> `False
      | `Or [ term ] -> (Term.validate term :> t)
      | `Or terms -> `Or (List.map ~f:Term.validate terms)

    let to_pretty_string ?(style = Style.Infix.default) (dnf : t) : string =
      match dnf with
      | #term as t -> Term.to_pretty_string ~style t
      | `Or [] -> Literal.to_pretty_string ~style `False
      | `Or terms
        -> let t_strs = List.map terms ~f:(Term.to_pretty_string ~style)
           in Printf.sprintf "(%s)" (String.concat ~sep:style._or_ t_strs)

    let to_pretty_sexp ?(style = Style.Prefix.default) (dnf : t) : Sexp.t =
      match dnf with
      | #term as t -> Term.to_pretty_sexp ~style t
      | `Or [] -> Literal.to_pretty_sexp ~style `False
      | `Or [ term ] -> Term.to_pretty_sexp ~style term
      | `Or terms -> List ((Atom style.or_)
                           :: (List.map ~f:(Term.to_pretty_sexp ~style) terms))

    let of_pretty_sexp ?(style = Style.Prefix.default) (sexp : Sexp.t) : t =
      let open Exceptions
      in let ( === ) = String.equal
      in match sexp with
         | Atom _ -> (Literal.of_pretty_sexp ~style sexp :> t)
         | List ((Atom op) :: rest)
           -> if op === style.or_
              then `Or (List.map ~f:(Term.of_pretty_sexp ~style) rest)
              else if op === style.and_
                   then `Or [ Term.of_pretty_sexp ~style sexp ]
                   else if op === style.not_
                        then (Literal.of_pretty_sexp ~style sexp :> t)
                        else raise (invalid_sexp_exception ~ctx:"DNF" sexp)
         | _ -> raise (invalid_sexp_exception ~ctx:"DNF" sexp)

    let or_ (dnfs : t list) : t =
      `Or (List.fold_left dnfs ~init:[]
                          ~f:(fun acc dnf
                              -> match dnf with #term as t -> t :: acc
                                              | `Or terms -> terms @ acc))

    let rec and_ (dnfs : t list) : t =
      match dnfs with
      | [] -> `True
      | [ dnf ] -> dnf
      | dnf1 :: dnf2 :: t
        -> let terms1 = match dnf1 with #term as t -> [ t ] | `Or ts -> ts
           and terms2 = match dnf2 with #term as t -> [ t ] | `Or ts -> ts
           in let cross_terms =
                List.(concat (map terms1
                                  ~f:(fun a
                                      -> map terms2
                                             ~f:(fun b -> Term.and_ [a ; b]))))
              in (and_ [@tailcall]) ((`Or cross_terms) :: t)

    let not_term (term : term) : t =
      match term with
      | #literal as l -> (Literal.not_ l :> t)
      | `And literals -> `Or (List.map literals
                                       ~f:(fun l -> (Literal.not_ l :> term)))
    let not_ (dnf : t) : t =
      match dnf with #term as t -> not_term t
                   | `Or terms -> and_ (List.map terms ~f:not_term)

    let eval (dnf : t) (bools : bool array) : bool option =
      let rec helper (terms : Term.t list) : bool option =
        let open Option
        in match terms with
          | [] -> Some false
          | t :: ts -> Term.eval t bools
                       >>= (function true -> Some true
                                   | false -> (helper [@tailcall]) ts)
      in match dnf with
         | #term as t -> Term.eval t bools
         | `Or terms -> helper terms
  end
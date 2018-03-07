open Base

module type I =
  sig
    module Clause : Clause.I

    type nonrec t = [ Clause.t
                    | `And of Clause.t list ]

    include Form.I with module Literal := Clause.Literal
                    and type t := t

    include PrettyPrint.I with type t := t
  end

module Make (C : Clause.I) : I with module Clause = C =
  struct
    open PrettyPrint

    module Clause = C

    module Literal = Clause.Literal

    type literal = Literal.t

    type clause = Clause.t [@@deriving sexp_poly]

    type nonrec t = [ clause
                    | `And of clause list ] [@@deriving sexp]

    let validate (cnf : t) : t =
      match cnf with
      | #clause as c -> (Clause.validate c :> t)
      | `And [] -> `True
      | `And [ clause ] -> (Clause.validate clause :> t)
      | `And clauses -> `And (List.map ~f:Clause.validate clauses)

    let to_pretty_string ?(style = Style.Infix.default) (cnf : t) : string =
      match cnf with
      | #clause as c -> Clause.to_pretty_string ~style c
      | `And [] -> Literal.to_pretty_string ~style `True
      | `And clauses
        -> let c_strs = List.map clauses ~f:(Clause.to_pretty_string ~style)
           in Printf.sprintf "(%s)" (String.concat ~sep:style._and_ c_strs)

    let to_pretty_sexp ?(style = Style.Prefix.default) (cnf : t) : Sexp.t =
      match cnf with
      | #clause as c -> Clause.to_pretty_sexp ~style c
      | `And [] -> Literal.to_pretty_sexp ~style `True
      | `And [ clause ] -> Clause.to_pretty_sexp ~style clause
      | `And clauses
        -> List ((Atom style.and_)
                 :: (List.map ~f:(Clause.to_pretty_sexp ~style) clauses))

    let of_pretty_sexp ?(style = Style.Prefix.default) (sexp : Sexp.t) : t =
      let open Exceptions
      in let ( === ) = String.equal
      in match sexp with
         | Atom _ -> (Literal.of_pretty_sexp ~style sexp :> t)
         | List ((Atom op) :: rest)
           -> if op === style.and_
              then `And (List.map ~f:(Clause.of_pretty_sexp ~style) rest)
              else if op === style.or_
                   then `And [ Clause.of_pretty_sexp ~style sexp ]
                   else if op === style.not_
                        then (Literal.of_pretty_sexp ~style sexp :> t)
                        else raise (invalid_sexp_exception ~ctx:"CNF" sexp)
         | _ -> raise (invalid_sexp_exception ~ctx:"CNF" sexp)

    (* TODO: Binary split and merge *)
    let rec or_ (cnfs : t list) : t =
      match cnfs with
      | [] -> `False
      | [ cnf ] -> cnf
      | cnf1 :: cnf2 :: t
        -> let clauses1 = match cnf1 with #clause as c -> [ c ] | `And cs -> cs
           and clauses2 = match cnf2 with #clause as c -> [ c ] | `And cs -> cs
           in let cross_clauses =
                List.(concat (map clauses1
                                  ~f:(fun a
                                      -> map clauses2
                                             ~f:(fun b -> Clause.or_ [a ; b]))))
              in (or_ [@tailcall]) ((`And cross_clauses) :: t)

    let and_ (cnfs : t list) : t =
      `And (List.fold_left cnfs ~init:[]
                           ~f:(fun acc cnf
                               -> match cnf with #clause as c -> c :: acc
                                               | `And clauses -> clauses @ acc))

    let not_clause (clause : clause) : t =
      match clause with
      | #literal as l -> (Literal.not_ l :> t)
      | `Or literals -> `And (List.map literals
                                       ~f:(fun l -> (Literal.not_ l :> clause)))

    let not_ (cnf : t) : t =
      match cnf with #clause as c -> not_clause c
                   | `And clauses -> or_ (List.map clauses ~f:not_clause)

    let eval (cnf : t) (bools : bool array) : bool option =
      let rec helper (clauses : Clause.t list) : bool option =
        let open Option
        in match clauses with
           | [] -> Some true
           | c :: cs -> Clause.eval c bools
                        >>= (function false -> Some false
                                    | true -> (helper [@tailcall]) cs)
      in match cnf with
         | #clause as c -> Clause.eval c bools
         | `And clauses -> helper clauses
  end
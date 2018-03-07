open Base

module type I =
  sig
    module Literal : Literal.I

    type t = [ Literal.t
             | `Or of t list
             | `And of t list ]

    include Form.I with module Literal := Literal
                    and type t := t

    include PrettyPrint.I with type t := t
  end

module Make (L : Literal.I) : I with module Literal = L =
  struct
    open PrettyPrint

    module Literal = L

    type literal = Literal.t [@@deriving sexp_poly]

    type t = [ literal
             | `Or of t list
             | `And of t list ]
             [@@deriving sexp]

    (* TODO: Tail recursion *)
    let rec validate (nnf : t) : t =
      match nnf with
      | #literal as l -> (Literal.validate l :> t)
      | `Or [] -> `False
      | `Or [ nnf ] -> validate nnf
      | `Or nnfs -> `Or (List.map ~f:validate nnfs)
      | `And [] -> `True
      | `And [ nnf ] -> validate nnf
      | `And nnfs -> `And (List.map ~f:validate nnfs)

    (* TODO: Tail recursion *)
    let rec to_pretty_string ?(style = Style.Infix.default) (nnf : t) : string =
      match nnf with
      | #literal as lit -> Literal.to_pretty_string ~style lit
      | `Or [] -> Literal.to_pretty_string ~style `False
      | `And [] -> Literal.to_pretty_string ~style `True
      | `And nnfs
        -> let ns = List.map nnfs ~f:(to_pretty_string ~style)
           in Printf.sprintf "(%s)" (String.concat ~sep:style._and_ ns)
      | `Or nnfs
        -> let ns = List.map nnfs ~f:(to_pretty_string ~style)
           in Printf.sprintf "(%s)" (String.concat ~sep:style._or_ ns)

    (* TODO: Tail recursion *)
    let rec to_pretty_sexp ?(style = Style.Prefix.default) (nnf : t) : Sexp.t =
      match nnf with
      | #literal as lit -> Literal.to_pretty_sexp ~style lit
      | `Or [] -> Literal.to_pretty_sexp ~style `False
      | `Or nnfs -> List ((Atom style.or_)
                            :: (List.map ~f:(to_pretty_sexp ~style) nnfs))
      | `And [] -> Literal.to_pretty_sexp ~style `True
      | `And nnfs -> List ((Atom style.and_)
                            :: (List.map ~f:(to_pretty_sexp ~style) nnfs))

    (* TODO: Tail recursion *)
    let rec of_pretty_sexp ?(style = Style.Prefix.default) (sexp : Sexp.t) : t =
      let open Exceptions
      in let ( === ) = String.equal
      in match sexp with
         | Atom _ -> (Literal.of_pretty_sexp ~style sexp :> t)
         | List [Atom op ; Atom _ ] when op === style.not_
           -> (Literal.of_pretty_sexp ~style sexp :> t)
         | List ((Atom op) :: rest)
           -> if op === style.or_
              then `Or (List.map ~f:(of_pretty_sexp ~style) rest)
              else if op === style.and_
                   then `And (List.map ~f:(of_pretty_sexp ~style) rest)
                   else raise (invalid_sexp_exception ~ctx:"NNF" sexp)
         | _ -> raise (invalid_sexp_exception ~ctx:"NNF" sexp)

    let or_ (nnfs : t list) : t = `Or nnfs

    let and_ (nnfs : t list) : t = `And nnfs

    (* TODO: Tail recursion *)
    let rec not_ (nnf : t) : t =
      match nnf with #literal as l -> (Literal.not_ l :> t)
                   | `Or nnfs -> `And (List.map ~f:not_ nnfs)
                   | `And nnfs -> `Or (List.map ~f:not_ nnfs)

    let rec eval (nnf : t) (bools : bool array) : bool option =
      let open Option
      in match nnf with
         | #literal as lit -> Literal.eval lit bools
         | `Or [] -> Some false
         | `Or (h :: t)
           -> eval h bools >>= (function true -> Some true
                                       | false -> eval (`Or t) bools)
         | `And [] -> Some true
         | `And (h :: t)
           -> eval h bools >>= (function false -> Some false
                                       | true -> eval (`And t) bools)
  end
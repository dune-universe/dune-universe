open Base

module type I =
  sig
    module Literal : Literal.I

    type t = [ Literal.t
             | `Not of t
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
             | `Not of t
             | `Or of t list
             | `And of t list ]
             [@@deriving sexp]

    (* TODO: Tail recursion *)
    let rec validate (unnf : t) : t =
      match unnf with
      | #literal as l -> (Literal.validate l :> t)
      | `Not u -> validate u
      | `Or [] -> `False
      | `Or [ unnf ] -> validate unnf
      | `Or unnfs -> `Or (List.map ~f:validate unnfs)
      | `And [] -> `True
      | `And [ unnf ] -> validate unnf
      | `And unnfs -> `And (List.map ~f:validate unnfs)

    (* TODO: Tail recursion *)
    let rec to_pretty_string ?(style = Style.Infix.default) (unnf : t) : string =
      match unnf with
      | #literal as l -> Literal.to_pretty_string ~style l
      | `Not unnf -> Printf.sprintf "%s(%s)" style._not_
                                    (to_pretty_string ~style unnf)
      | `Or [] -> Literal.to_pretty_string ~style `False
      | `And [] -> Literal.to_pretty_string ~style `True
      | `And unnfs
        -> let ns = List.map unnfs ~f:(to_pretty_string ~style)
           in Printf.sprintf "(%s)" (String.concat ~sep:style._and_ ns)
      | `Or unnfs
        -> let ns = List.map unnfs ~f:(to_pretty_string ~style)
           in Printf.sprintf "(%s)" (String.concat ~sep:style._or_ ns)

    (* TODO: Tail recursion *)
    let rec to_pretty_sexp ?(style = Style.Prefix.default) (unnf : t) : Sexp.t =
      match unnf with
      | #literal as l -> Literal.to_pretty_sexp ~style l
      | `Not unnf -> List [ (Atom style.not_)
                          ; (to_pretty_sexp ~style unnf) ]
      | `Or [] -> Literal.to_pretty_sexp ~style `False
      | `Or unnfs -> List ((Atom style.or_)
                          :: (List.map ~f:(to_pretty_sexp ~style) unnfs))
      | `And [] -> Literal.to_pretty_sexp ~style `True
      | `And unnfs -> List ((Atom style.and_)
                           :: (List.map ~f:(to_pretty_sexp ~style) unnfs))

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
                   else if op === style.not_ && (List.length rest) = 1
                        then `Not (of_pretty_sexp ~style (List.hd_exn rest))
                        else raise (invalid_sexp_exception ~ctx:"UnNF" sexp)
         | _ -> raise (invalid_sexp_exception ~ctx:"UnNF" sexp)

    let or_ (unnfs : t list) : t = `Or unnfs

    let and_ (unnfs : t list) : t = `And unnfs

    let not_ (unnf : t) : t =
      match unnf with #literal as l -> (Literal.not_ l :> t)
                    | `Not unnf -> unnf
                    | _ -> `Not unnf

    let rec eval (unnf : t) (bools : bool array) : bool option =
      let open Option
      in match unnf with
         | #literal as l -> Literal.eval l bools
         | `Not unnf -> eval unnf bools >>= fun res -> Some (not res)
         | `Or [] -> Some false
         | `Or (h :: t)
           -> eval h bools >>= (function true -> Some true
                                       | false -> eval (`Or t) bools)
         | `And [] -> Some true
         | `And (h :: t)
           -> eval h bools >>= (function false -> Some false
                                       | true -> eval (`And t) bools)
  end
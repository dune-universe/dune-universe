open Properties

module type I =
  sig
    module Literal : Literal.I

    (* TODO: Remove private, but extend Literal.t *)
    type t = private [> Literal.t] [@@deriving sexp]

    include Conjunctable with type t := t

    include Disjunctable with type t := t

    include Negatable with type t := t

    include NeedsValidation with type t := t

    include Executable with type t := t
  end
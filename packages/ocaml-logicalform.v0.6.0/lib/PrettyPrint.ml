module Style =
  struct
    module Infix =
      struct
        type nonrec t =
          { var_prefix : string
          ; true_string : string
          ; false_string : string
          ; _not_ : string
          ; _and_ : string
          ; _or_ : string
          }

        let default =
          { var_prefix = "x"
          ; true_string = "true"
          ; false_string = "false"
          ; _not_ = "~"
          ; _and_ = " & "
          ; _or_ = " | "
          }
      end

    module Prefix =
      struct
        type nonrec t =
          { var_prefix : string
          ; true_string : string
          ; false_string : string
          ; not_ : string
          ; and_ : string
          ; or_ : string
          }

        let default =
          { var_prefix = "x"
          ; true_string = "true"
          ; false_string = "false"
          ; not_ = "not"
          ; and_ = "and"
          ; or_ = "or"
          }
      end
  end

module type Inspectable =
  sig
    type t

    val to_pretty_string : ?style:Style.Infix.t -> t -> string
  end

module type Sexpable =
  sig
    type t

    val to_pretty_sexp : ?style:Style.Prefix.t -> t -> Base.Sexp.t
    val of_pretty_sexp : ?style:Style.Prefix.t -> Base.Sexp.t -> t
  end

module type I =
  sig
    type t

    include Inspectable with type t := t

    include Sexpable with type t := t
  end
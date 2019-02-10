(** Csv support, including generic optional and list types. *)

module Optional = struct
  module type ARGS = sig
    type t [@@deriving sexp, yojson]
    include Csvfields.Csv.Stringable with type t := t

    val null : string
  end

  module Default_args
      (Args : sig
         type t [@@deriving sexp, yojson]
         include Csvfields.Csv.Stringable with type t := t
       end) : ARGS with type t = Args.t =
  struct
    let null = ""
    include Args
  end

  module type S =
  sig
    type elt [@@deriving sexp, yojson]
    type t = elt option [@@deriving sexp, yojson]
    include Csvfields.Csv.Csvable with type t := t
  end

  module Make(C:ARGS) : S with type elt = C.t =
  struct

    type elt = C.t [@@deriving sexp, yojson]
    module T = struct
      type t = elt option [@@deriving sexp, yojson]
      let to_string t =
        Option.value_map ~default:C.null t ~f:C.to_string
      let of_string s =
        if C.null = s then None else Some (C.of_string s)
    end
    include T
    include (Csvfields.Csv.Atom(T) : Csvfields.Csv.Csvable with type t := t)
  end

  module Make_default
      (Args : sig
         type t [@@deriving sexp, yojson]
         include Csvfields.Csv.Stringable with type t := t
       end) =
   Make(Default_args(Args))


  module String =
    Make_default(
    struct
      type t = string [@@deriving yojson]
      include (String : module type of String with type t := t)
    end
    )
end


module List = struct
  module type ARGS = sig
    type t [@@deriving sexp, yojson]
    include Csvfields.Csv.Stringable with type t := t
    val sep : char
  end

  module Default_args
      (Args : sig
         type t [@@deriving sexp, yojson]
         include Csvfields.Csv.Stringable with type t := t
       end) =
  struct
    let sep = ' '
    include Args
  end

  module type S =
  sig
    type elt [@@deriving sexp, yojson]
    type t = elt list [@@deriving sexp, yojson]
    include Csvfields.Csv.Csvable with type t := t
  end

  module Make(C:ARGS) : S with type elt = C.t =
  struct

    type elt = C.t [@@deriving sexp, yojson]
    module T = struct
      type t = elt list [@@deriving sexp, yojson]
      let to_string t =
        List.map ~f:C.to_string t |> String.concat ~sep:(Char.to_string C.sep)
      let of_string s =
        String.split ~on:C.sep s |> List.map ~f:C.of_string
    end
    include T
    include (Csvfields.Csv.Atom(T) : Csvfields.Csv.Csvable with type t := t)
  end

  module Make_default
      (Args : sig
         type t [@@deriving sexp, yojson]
         include Csvfields.Csv.Stringable with type t := t
       end) =
   Make(Default_args(Args))

   module String =
    Make
      (Default_args(
        struct
          type t = string [@@deriving yojson]
          include (String : module type of String with type t := t)
        end
        )
      )
end

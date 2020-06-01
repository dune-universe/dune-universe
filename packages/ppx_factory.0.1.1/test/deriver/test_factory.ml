module A = struct
  module B = struct
    type t = char
    let default = 'c'
  end
end

module Types : sig
  type simple_record =
    { int_field : int
    ; string_field : string
    ; other_field : A.B.t
    }
  [@@deriving factory]

  type simple_variant =
    | A
    | B of int
    | C of int * string
    | D of {int_field : int; string_field : string}
  [@@deriving factory]

  type record_with_options =
    { non_optional : int
    ; optional : int option
    ; nested : int option option
    }
  [@@deriving factory]

  type 'a parametrized =
    { param : 'a option
    ; non_paramed : string
    }
  [@@deriving factory]

  type copied = simple_record =
    { int_field : int
    ; string_field : string
    ; other_field : A.B.t
    }
  [@@deriving factory]
end = struct
  type simple_record =
    { int_field : int
    ; string_field : string
    ; other_field : A.B.t
    }
  [@@deriving factory]

  type simple_variant =
    | A
    | B of int
    | C of int * string
    | D of {int_field : int; string_field : string}
  [@@deriving factory]

  type record_with_options =
    { non_optional : int
    ; optional : int option
    ; nested : int option option
    }
  [@@deriving factory]

  type 'a parametrized =
    { param : 'a option
    ; non_paramed : string
    }
  [@@deriving factory]

  type copied = simple_record =
    { int_field : int
    ; string_field : string
    ; other_field : A.B.t
    }
  [@@deriving factory]
end

type json =
    [
    | `Null
    | `Bool of bool
    | `Int of int
    | `Intlit of string
    | `Float of float
    | `Floatlit of string
    | `String of string
    | `Stringlit of string
    | `Assoc of (string * json) list
    | `List of json list
    | `Tuple of json list
    | `Variant of (string * json option)
    ]

type t = json

module Extended = struct
  type nonrec json = json
  type t = json
end

module Basic = struct
  type json =
      [
      | `Null
      | `Bool of bool
      | `Int of int
      | `Float of float
      | `String of string
      | `Assoc of (string * json) list
      | `List of json list
      ]
  type t = json
end

module Strict = struct
  type json =
      [
      | `Null
      | `Bool of bool
      | `Float of float
      | `String of string
      | `Assoc of (string * json) list
      | `List of json list
      ]
  type t = json
end

open! Base
open! Import

module Simple_record = struct
  type t =
    { foo : int
    ; bar : string
    ; baz : float
    }
  [@@deriving accessors]
end

module Simple_inline_record = struct
  type t =
    | T of
        { foo : int
        ; bar : string
        ; baz : float
        }
  [@@deriving accessors]
end

module Record_with_polymorphism = struct
  type ('a, 'b) t =
    { foo : int
    ; bar : 'a
    ; baz : 'b
    }
  [@@deriving accessors]
end

module Singleton_record = struct
  type t = { foo : int } [@@deriving accessors]
end

module Singleton_inline_record = struct
  type t = T of { foo : int } [@@deriving accessors]
end

module Singleton_record_with_polymorphism = struct
  type 'a t = { foo : 'a option } [@@deriving accessors]
end

module Recursive_record = struct
  type t =
    { label : string
    ; children : t list
    }
  [@@deriving accessors]
end

module Recursive_records = struct
  type tree =
    { children : node list
    ; next : tree
    }

  and node =
    { host : tree
    ; id : int
    }
  [@@deriving accessors]
end

module Basic_variant_type = struct
  type t =
    | A of int
    | B of string * bool
    | C
    | D of (float * float)
    | E of
        { x : float
        ; y : float
        }
    | F of { z : float }
  [@@deriving accessors]
end

module Singleton_variant = struct
  type t = T of int [@@deriving accessors]
end

module Singleton_variant_with_polymorphism = struct
  type 'a t = T of 'a [@@deriving accessors]
end

module Basic_variant_type_with_polymorphism = struct
  type 'a t =
    | A of 'a
    | B
  [@@deriving accessors]
end

module Recursive_variant = struct
  type 'a t =
    | Nil
    | Cons of 'a * 'a t
  [@@deriving accessors]
end

module Basic_polymorphic_variant = struct
  type t =
    [ `Foo
    | `Bar of int
    | `Baz of string * float
    ]
  [@@deriving accessors]
end

module Polymorphic_variant_with_inherit = struct
  type 'a t =
    [ `Wibble
    | `Wobble of 'a
    | Basic_polymorphic_variant.t
    ]
  [@@deriving accessors]
end

module Singleton_polymorphic_variant = struct
  type t = [ `Foo of int ] [@@deriving accessors]
end

module Polymorphization = struct
  let swapped =
    let swap (a, b) = b, a in
    [%accessor Accessor.isomorphism ~get:swap ~construct:swap]
  ;;
end

module Gadt_syntax = struct
  type 'a t = T : 'x * string -> 'x t [@@deriving accessors]
end

module Simple_record_with_submodule = struct
  type t =
    { foo : int
    ; bar : string
    ; baz : float
    }
  [@@deriving accessors ~submodule:Foo]
end

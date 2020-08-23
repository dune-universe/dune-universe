open! Base
open! Import

module Simple_record : sig
  type t =
    { foo : int
    ; bar : string
    ; baz : float
    }
  [@@deriving accessors]
end

module Simple_inline_record : sig
  type t =
    | T of
        { foo : int
        ; bar : string
        ; baz : float
        }
  [@@deriving accessors]
end

module Record_with_polymorphism : sig
  type ('a, 'b) t =
    { foo : int
    ; bar : 'a
    ; baz : 'b
    }
  [@@deriving accessors]
end

module Singleton_record : sig
  type t = { foo : int } [@@deriving accessors]
end

module Singleton_inline_record : sig
  type t = T of { foo : int } [@@deriving accessors]
end

module Singleton_record_with_polymorphism : sig
  type 'a t = { foo : 'a option } [@@deriving accessors]
end

module Recursive_record : sig
  type t =
    { label : string
    ; children : t list
    }
  [@@deriving accessors]
end

module Recursive_records : sig
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

module Basic_variant_type : sig
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

module Singleton_variant : sig
  type t = T of int [@@deriving accessors]
end

module Singleton_variant_with_polymorphism : sig
  type 'a t = T of 'a [@@deriving accessors]
end

module Basic_variant_type_with_polymorphism : sig
  type 'a t =
    | A of 'a
    | B
  [@@deriving accessors]
end

module Recursive_variant : sig
  type 'a t =
    | Nil
    | Cons of 'a * 'a t
  [@@deriving accessors]
end

module Basic_polymorphic_variant : sig
  type t =
    [ `Foo
    | `Bar of int
    | `Baz of string * float
    ]
  [@@deriving accessors]
end

module Polymorphic_variant_with_inherit : sig
  type 'a t =
    [ `Wibble
    | `Wobble of 'a
    | Basic_polymorphic_variant.t
    ]
  [@@deriving accessors]
end

module Singleton_polymorphic_variant : sig
  type t = [ `Foo of int ] [@@deriving accessors]
end

module Polymorphization : sig
  val swapped
    : ('i -> 'a * 'b -> 'c * 'd, 'i -> 'b * 'a -> 'd * 'c, [< isomorphism ]) Accessor.t
end

module Gadt_syntax : sig
  (* This demonstrates that the ppx can handle types defined using GADT syntax as long as
     it's not using existential types or type indices, even if the syntax is being used to
     rename parameters. *)

  type 'a t = T : 'x * string -> 'x t [@@deriving accessors]
end

module Simple_record_with_submodule : sig
  type t =
    { foo : int
    ; bar : string
    ; baz : float
    }
  [@@deriving accessors ~submodule:Foo]
end

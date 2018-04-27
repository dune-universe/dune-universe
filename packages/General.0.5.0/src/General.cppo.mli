(* Basics *)

(** Some doc for :mod:`General` *)

module Reset: sig
  #include "Reset/CommonHeader.ml"
  #include "Reset/SignatureHeader.ml"

  module ResetPervasives: sig
    #include "Reset/ResetPervasives.ml"
  end

  module ResetStandardLibrary: sig
    #include "Reset/ResetStandardLibrary.ml"
  end

  #include "Reset/Footer.ml"
end [@@autodoc.hide]

(** Some doc for :mod:`General.Pervasives` *)
module Pervasives: sig
  include module type of Reset.ResetPervasives [@@autodoc.hide]
  include module type of Reset.ResetStandardLibrary [@@autodoc.hide]

  (** This module overrides all elements from the standard
  `pervasives <https://caml.inria.fr/pub/docs/manual-ocaml/libref/Pervasives.html>`_
  with unusable but guiding values like:

  .. val:: raise
    :noindex:
    :type: [ `Please_use_General__Exception__raise ]

    The types of these values point at what replaces them in :mod:`General`.
    (In that case, :val:`General.Exception.raise`).

  It then brings back a small set of ubiquitous values: *)

  (** **Boolean operators** *)

  (** Negation. Alias of :val:`General.Bool.O.not`. *)
  val not: bool -> bool

  (** Conjunction. Lazy. Alias of :val:`General.Bool.O.(&&)` *)
  val (&&): bool -> bool -> bool

  (** Disjunction. Lazy. Alias of :val:`General.Bool.O.(||)` *)
  val (||): bool -> bool -> bool

  (** **Integer operators** *)

  val (~-): int -> int
  val (~+): int -> int
  val (+): int -> int -> int
  val (-): int -> int -> int
  val ( * ): int -> int -> int
  val (/): int -> int -> int
  val (mod): int -> int -> int

  (** **Floating point operators** *)

  val (~-.): float -> float
  val (~+.): float -> float
  val (+.): float -> float -> float
  val (-.): float -> float -> float
  val ( *. ): float -> float -> float
  val (/.): float -> float -> float
  val ( ** ): float -> float -> float

  (** **Function composition and application** *)

  val (@@): ('a -> 'b) -> 'a -> 'b
  val (|>): 'a -> ('a -> 'b) -> 'b
  val (%): ('a -> 'b) -> ('c -> 'a) -> ('c -> 'b)

  (** **References** *)

  val ref: 'a -> 'a OCamlStandard.Pervasives.ref
  val (:=): 'a OCamlStandard.Pervasives.ref -> 'a -> unit
  val (!): 'a OCamlStandard.Pervasives.ref -> 'a

  (** **Polymorphic comparison** *)

  val (=): 'a -> 'a -> bool
  val (<>): 'a -> 'a -> bool

  val (<): 'a -> 'a -> bool
  val (<=): 'a -> 'a -> bool
  val (>=): 'a -> 'a -> bool
  val (>): 'a -> 'a -> bool

  (** **Ubiquitous functions** *)

  val ignore: 'a -> unit

  val identity: 'a -> 'a

  (** **Miscelaneous operators** *)

  val (@): 'a list -> 'a list -> 'a list

  val (^): string -> string -> string
end

module Shorten: sig
  (** Return type for functions used in short-circuit iterations over collections.
  (i.e: :val:`General.Traits.Foldable.Short.S0:fold_short`) *)
  type t =
    | GoOn (** Used to indicate iteration should proceed to next item *)
    | ShortCircuit (** Used to indicate iteration should stop after this item *)
end

module Compare: sig
  type t = LT | EQ | GT

  module Poly: sig
    val compare: 'a -> 'a -> t

    val less_than: 'a -> 'a -> bool
    val less_or_equal: 'a -> 'a -> bool
    val greater_than: 'a -> 'a -> bool
    val greater_or_equal: 'a -> 'a -> bool

    val between: 'a -> low:'a -> high:'a -> bool
    val between_or_equal: 'a -> low:'a -> high:'a -> bool

    val min: 'a -> 'a -> 'a
    val max: 'a -> 'a -> 'a
    val min_max: 'a -> 'a -> 'a * 'a

    module O: sig
      val (<): 'a -> 'a -> bool
      val (<=): 'a -> 'a -> bool
      val (>): 'a -> 'a -> bool
      val (>=): 'a -> 'a -> bool
    end
  end
end

module Equate: sig
  module Poly: sig
    (** Polymorphic structural equality *)
    val equal: 'a -> 'a -> bool

    (** Polymorphic inequality *)
    val different: 'a -> 'a -> bool

    module O: sig
      val (=): 'a -> 'a -> bool
      val (<>): 'a -> 'a -> bool
    end
  end

  module Phys: sig
    (** Physical identity (address equality) *)
    val equal: 'a -> 'a -> bool

    (** Physical inequality *)
    val different: 'a -> 'a -> bool
  end
end

(** Traits are isolated capabilities associated with a type. *)
module Traits: sig
  (* @feature Traits.Hashable with val hash: t -> int, Poly using Hashtbl.hash *)
  (* @feature Traits for head and tail (Headable.Left?), and init and last (Headable.Right?) *)
  (* @feature Publish helper functors (Specialize, Ringoid.Exponentiate.Make, Tests.Make, etc.) *)

  (** A *representation* is a string representing a value for a software developer audience.
  When possible, it should a valid OCaml expression for the value. *)
  module Representable: sig
    #include "Traits/Representable.signatures.ml"
  end

  module Displayable: sig
    #include "Traits/Displayable.signatures.ml"
  end

  module Parsable: sig
    #include "Traits/Parsable.signatures.ml"
  end

  module Equatable: sig
    module Basic: sig
      #include "Traits/Equatable.signatures.Basic.ml"
    end

    module Operators: sig
      #include "Traits/Equatable.signatures.Operators.ml"
    end

    #include "Traits/Equatable.signatures.ml"
  end

  module Comparable: sig
    module Basic: sig
      #include "Traits/Comparable.signatures.Basic.ml"
    end

    module Operators: sig
      #include "Traits/Comparable.signatures.Operators.ml"
    end

    #include "Traits/Comparable.signatures.ml"
  end

  module Ringoid: sig
    module Basic: sig
      #include "Traits/Ringoid.signatures.Basic.ml"
    end

    module Operators: sig
      #include "Traits/Ringoid.signatures.Operators.ml"
    end

    #include "Traits/Ringoid.signatures.ml"
  end

  module PredSucc: sig
    #include "Traits/PredSucc.signatures.ml"
  end

  module FilterMapable: sig
    #include "Traits/FilterMapable.signatures.ml"

    module ToContainer(C: sig type 'a t end): sig
      #include "Traits/FilterMapable.signatures.ToContainer.ml"
    end

    module ToList: module type of ToContainer(struct type 'a t = 'a list end)

    module ToArray: module type of ToContainer(struct type 'a t = 'a array end)
  end

  module Foldable: sig
    module Basic: sig
      #include "Traits/Foldable.signatures.Basic.ml"
    end

    #include "Traits/Foldable.signatures.ml"

    module Right: sig
      module Basic: sig
        #include "Traits/Foldable.signatures.Right.Basic.ml"
      end

      #include "Traits/Foldable.signatures.Right.ml"
    end

    module Short: sig
      module Basic: sig
        #include "Traits/Foldable.signatures.Short.Basic.ml"
      end

      #include "Traits/Foldable.signatures.Short.ml"

      module Right: sig
        module Basic: sig
          #include "Traits/Foldable.signatures.Short.Right.Basic.ml"
        end

        #include "Traits/Foldable.signatures.Short.Right.ml"
      end
    end
  end

  module Scanable: sig
    #include "Traits/Scanable.signatures.ml"

    module ToContainer(C: sig type 'a t end): sig
      #include "Traits/Scanable.signatures.ToContainer.ml"
    end

    module ToList: module type of ToContainer(struct type 'a t = 'a list end)

    module ToArray: module type of ToContainer(struct type 'a t = 'a array end)

    module Right: sig
      #include "Traits/Scanable.signatures.Right.ml"

      module ToContainer(C: sig type 'a t end): sig
        #include "Traits/Scanable.signatures.Right.ToContainer.ml"
      end

      module ToList: module type of ToContainer(struct type 'a t = 'a list end)

      module ToArray: module type of ToContainer(struct type 'a t = 'a array end)
    end

    module Short: sig
      #include "Traits/Scanable.signatures.Short.ml"

      module ToContainer(C: sig type 'a t end): sig
        #include "Traits/Scanable.signatures.Short.ToContainer.ml"
      end

      module ToList: module type of ToContainer(struct type 'a t = 'a list end)

      module ToArray: module type of ToContainer(struct type 'a t = 'a array end)

      module Right: sig
        #include "Traits/Scanable.signatures.Short.Right.ml"

        module ToContainer(C: sig type 'a t end): sig
          #include "Traits/Scanable.signatures.Short.Right.ToContainer.ml"
        end

        module ToList: module type of ToContainer(struct type 'a t = 'a list end)

        module ToArray: module type of ToContainer(struct type 'a t = 'a array end)
      end
    end
  end
end

(* Typology of iterations other collection:
- is there an accumulator (fold vs map)
- is it short circuit (exists/fold_short/find vs fold/map)
- does it produce a list (map) or a scalar (find/fold) or nothing (iter)
- from a list (map) or from a scalar (unfold/until_none/until_fixed)? (until_none i: [f i; f f i; f f f i; ... until f returns None]; until_fixed: until f f f i = f f i)
- other criteria?
and we need a generic function in each category. *)

module Concepts: sig
  (* @feature Concepts for iterables and collections. Something like Collection, Container, MonoBag, MultiBag, LinearContainer *)
  (* @feature Concepts.Stringable including Parsable and Displayable *)

  module Identifiable: sig
    #include "Concepts/Identifiable.signatures.ml"
  end

  module Able: sig
    module Operators: sig
      #include "Concepts/Able.signatures.Operators.ml"
    end

    #include "Concepts/Able.signatures.ml"
  end

  module Number: sig
    module Operators: sig
      #include "Concepts/Number.signatures.Operators.ml"
    end

    #include "Concepts/Number.signatures.ml"
  end

  module RealNumber: sig
    module Operators: sig
      #include "Concepts/RealNumber.signatures.Operators.ml"
    end

    #include "Concepts/RealNumber.signatures.ml"
  end

  module Integer: sig
    #include "Concepts/Integer.signatures.ml"
  end
end

(* Technical, utility modules *)

module CallStack: sig
  type t = Pervasives.OCamlStandard.Printexc.raw_backtrace

  include Traits.Displayable.S0 with type t := t
  include Traits.Representable.S0 with type t := t

  val current: ?max_size:int -> unit -> t

  module Location: sig
    type t = Pervasives.OCamlStandard.Printexc.location = {
      filename: string;
      line_number: int;
      start_char: int;
      end_char: int;
    }

    include Concepts.Able.S0 with type t := t
  end

  module Frame: sig
    type t = Pervasives.OCamlStandard.Printexc.backtrace_slot

    val is_raise: t -> bool
    (* @feature val is_inline: t -> bool *)

    val location: t -> Location.t option

    val format: int -> t -> string option
  end

  (* @feature? val size: t -> int *)
  (* @feature? val frame: t -> int -> Frame.t *)
  val frames: t -> Frame.t list
end

module Exception: sig
  type t = exn

  include Concepts.Identifiable.S0 with type t := t
  include Traits.Displayable.S0 with type t := t

  val register_printer: (t -> string option) -> unit

  val record_backtraces: bool -> unit
  val recording_backtraces: unit -> bool
  (* There is no way to get the call stack of a specific exception.
  It's just possible to get the call stack of the most recent exception. *)
  val most_recent_backtrace: unit -> CallStack.t option

  (* Aliases for all predefined exceptions
  https://caml.inria.fr/pub/docs/manual-ocaml-4.05/core.html#sec527 *)
  exception MatchFailure of (string * int * int)
  exception AssertFailure of (string * int * int)
  exception InvalidArgument of string
  exception Failure of string
  exception NotFound

  (** Raised when the system could not allocate memory *)
  exception OutOfMemory

  exception StackOverflow
  exception SysError of string
  exception EndOfFile
  exception DivisionByZero
  exception SysBlockedIO
  exception UndefinedRecursiveModule of (string * int * int)

  (* Aliases for all exceptions in Pervasives
  https://caml.inria.fr/pub/docs/manual-ocaml-4.05/libref/Pervasives.html *)
  exception Exit

  val raise: t -> 'a

  val raise_without_backtrace: t -> 'a

  val invalid_argument: ('a, unit, string, string, string, 'b) CamlinternalFormatBasics.format6 -> 'a

  val failure: ('a, unit, string, string, string, 'b) CamlinternalFormatBasics.format6 -> 'a

  val failure_if: bool -> ('a, unit, string, string, string, unit) CamlinternalFormatBasics.format6 -> 'a
  val failure_unless: bool -> ('a, unit, string, string, string, unit) CamlinternalFormatBasics.format6 -> 'a

  val name: exn -> string

  val or_none: 'a lazy_t -> 'a option
end

module Exit: sig
  type t =
    | Success
    | Failure of int

  (* @feature Able *)

  val of_int: int -> t

  val exit: t -> 'a

  val at_exit: (unit -> unit) -> unit
end

(* Functions *)

module Function1: sig
  type ('a, 'z) t = 'a -> 'z

  val identity: ('a, 'a) t

  val apply: ('a, 'z) t -> 'a -> 'z
  val rev_apply: 'a -> ('a, 'z) t -> 'z
  val compose: ('a, 'b) t -> ('c, 'a) t -> ('c, 'b) t

  module O: sig
    val (@@): ('a, 'z) t -> 'a -> 'z
    val (|>): 'a -> ('a, 'z) t -> 'z
    val (%): ('a, 'b) t -> ('c, 'a) t -> ('c, 'b) t
  end
end

module Function2: sig
  type ('a, 'b, 'z) t = 'a -> 'b -> 'z

  val flip: ('a, 'b, 'z) t -> ('b, 'a, 'z) t

  val curry: ('a * 'b, 'z) Function1.t -> ('a, 'b, 'z) t
  val uncurry: ('a, 'b, 'z) t -> ('a * 'b, 'z) Function1.t
end

module Function3: sig
  type ('a, 'b, 'c, 'z) t = 'a -> 'b -> 'c -> 'z

  val flip: ('a, 'b, 'c, 'z) t -> ('c, 'b, 'a, 'z) t

  val curry: ('a * 'b * 'c, 'z) Function1.t -> ('a, 'b, 'c, 'z) t
  val uncurry: ('a, 'b, 'c, 'z) t -> ('a * 'b * 'c, 'z) Function1.t
end

module Function4: sig
  type ('a, 'b, 'c, 'd, 'z) t = 'a -> 'b -> 'c -> 'd -> 'z

  val flip: ('a, 'b, 'c, 'd, 'z) t -> ('d, 'c, 'b, 'a, 'z) t

  val curry: ('a * 'b * 'c * 'd, 'z) Function1.t -> ('a, 'b, 'c, 'd, 'z) t
  val uncurry: ('a, 'b, 'c, 'd, 'z) t -> ('a * 'b * 'c * 'd, 'z) Function1.t
end

module Function5: sig
  type ('a, 'b, 'c, 'd, 'e, 'z) t = 'a -> 'b -> 'c -> 'd -> 'e -> 'z

  val flip: ('a, 'b, 'c, 'd, 'e, 'z) t -> ('e, 'd, 'c, 'b, 'a, 'z) t

  val curry: ('a * 'b * 'c * 'd * 'e, 'z) Function1.t -> ('a, 'b, 'c, 'd, 'e, 'z) t
  val uncurry: ('a, 'b, 'c, 'd, 'e, 'z) t -> ('a * 'b * 'c * 'd * 'e, 'z) Function1.t
end

(* @feature Predicate1,2,3,etc. (or BoolFunction1,2,3, more consistent with other specializations) with composition of predicates (not, and, or, xor) *)

(* Atomic values *)

module Unit: sig
  type t = unit

  (* @feature Able *)

  val ignore: 'a -> t
end

module Bool: sig
  type t = bool

  module O: sig
    include Concepts.Able.Operators.S0 with type t := t

    val not: t -> t
    val (&&): t -> t -> t (* Lazy *)
    val (||): t -> t -> t (* Lazy *)
  end

  include Concepts.Able.S0 with type t := t and module O := O
  include Traits.Displayable.S0 with type t := t
  include Traits.Parsable.S0 with type t := t

  val not: t -> t
  val and_: t -> t -> t (* Not lazy *)
  val or_: t -> t -> t (* Not lazy *)
  val xor: t -> t -> t
end

module Char: sig
  type t = char

  (* @feature Integer, smallest, greatest *)
  include Traits.Comparable.S0 with type t := t

  val of_int: int -> t
  val to_int: t -> int

  val to_string: t -> string
  val repeat: t -> len:int -> string
end

module Int: sig
  type t = int

  include Concepts.Integer.S0 with type t := t

  (* @feature Traits.Bounded? Concept.FixedWidthInteger? *)
  val smallest: t
  val greatest: t

  module Bitwise: sig
    val logical_and: t -> t -> t
    val logical_or: t -> t -> t
    val logical_xor: t -> t -> t
    val logical_not: t -> t
    val logical_shift_left: t -> shift:t -> t
    val logical_shift_right: t -> shift:t -> t
    val arithmetic_shift_right: t -> shift:t -> t
  end
end

module Int32: sig
  type t = int32

  include Concepts.Integer.S0 with type t := t

  val smallest: t
  val greatest: t
end

module Int64: sig
  type t = int64

  include Concepts.Integer.S0 with type t := t

  val smallest: t
  val greatest: t
end

module NativeInt: sig
  type t = nativeint

  include Concepts.Integer.S0 with type t := t

  val smallest: t
  val greatest: t
end

module BigInt: sig
  type t = Pervasives.OCamlStandard.Big_int.big_int

  include Concepts.Integer.S0 with type t := t
end

module Float: sig
  type t = float

  include Concepts.RealNumber.S0 with type t := t

  val approx_equal: ?precision:t -> t -> t -> bool

  val epsilon: t
  val smallest: t
  val greatest: t
  val infinity: t
  val negative_infinity: t
  val not_a_number: t
  val pi: float
  val e: float

  val of_parts: significand:float -> exponent:int -> t
  val to_parts: t -> float * int
  val to_fractional_and_integral: t -> float * float

  val sqrt: float -> float

  val exp: float -> float
  val log: float -> float
  val log10: float -> float
  val expm1: float -> float
  val log1p: float -> float

  val cos: float -> float
  val sin: float -> float
  val tan: float -> float
  val acos: float -> float
  val asin: float -> float
  val atan: float -> float
  val atan2: y:float -> x:float -> float
  val hypot: float -> float -> float
  val cosh: float -> float
  val sinh: float -> float
  val tanh: float -> float

  val ceil: float -> float
  val floor: float -> float
  val copy_sign: t -> sign:t -> t

  module Class: sig
    type t =
      | Normal
      | SubNormal
      | Zero
      | Infinite
      | NotANumber

    include Traits.Representable.S0 with type t := t

    val of_float: float -> t
  end
end

module String: sig
  type t = string

  val of_char: char -> t
  val of_list: char list -> t
  val to_list: t -> char list

  val size: t -> int
  val get: t -> int -> char
  val set: bytes -> int -> char -> unit

  val of_bytes: bytes -> t
  val to_bytes: t -> bytes

  module O: sig
    include Concepts.Able.Operators.S0 with type t := t
    val (^): t -> t -> t
  end

  include Traits.Displayable.S0 with type t := t
  include Traits.Parsable.S0 with type t := t
  include Concepts.Able.S0 with type t := t and module O := O

  val concat: t -> t -> t

  (* @feature val try_substring: t -> pos:int -> len:int -> t option *)
  val substring: t -> pos:int -> len:int -> t
  (* @feature val try_prefix: t -> len:int -> t option *)
  val prefix: t -> len:int -> t
  (* @feature val try_suffix: t -> len:int -> t option *)
  val suffix: t -> len:int -> t

  val has_prefix: t -> pre:t -> bool
  val try_drop_prefix: t -> pre:t -> t option
  val drop_prefix: t -> pre:t -> t
  val drop_prefix': t -> len:int -> t
  val has_suffix: t -> suf:t -> bool
  val try_drop_suffix: t -> suf:t -> t option
  val drop_suffix: t -> suf:t -> t
  val drop_suffix': t -> len:int -> t

  val split: t -> sep:t -> t list
  val split': t -> seps:char list -> t list

  (* @feature Traits *)
  val fold: init:'a -> t -> f:('a -> char -> 'a) -> 'a
  val filter: t -> f:(char -> bool) -> t
end

module Bytes: sig
  type t = bytes

  val size: t -> int

  val of_string: string -> t
  val to_string: t -> string

  val get: t -> int -> char
  val set: t -> int -> char -> unit

  val empty: t
  val make: len:int -> t
end

(* @feature Rational, Complex, Quaternion, Matrix *)

(* Fixed-size containers *)

module Option: sig
  type 'a t = 'a option

  include Concepts.Able.S1 with type 'a t := 'a t

  (* @feature coalesce[_def] (with an (|||) operator? The operator *has* to be lazy like (&&) and (||)) *)

  val none: 'a t
  val some: 'a -> 'a t

  val some_if: bool -> 'a lazy_t -> 'a t
  val some_if': bool -> 'a -> 'a t

  val is_some: 'a t -> bool
  val is_none: 'a t -> bool

  val value_def: 'a t -> def:'a -> 'a
  val value: ?exc:exn -> 'a t -> 'a
  val or_failure: ('a, unit, string, string, string, 'b t -> 'b) CamlinternalFormatBasics.format6 -> 'a

  val map: 'a t -> f:('a -> 'b) -> 'b t
  val iter: 'a t -> f:('a -> unit) -> unit
  val filter: 'a t -> f:('a -> bool) -> 'a t
  val filter_map: 'a t -> f:('a -> 'b option) -> 'b t

  val value_map: 'a t -> def:'b -> f:('a -> 'b) -> 'b

  module Specialize(A: sig type t end): sig
    type t = A.t option

    val some_if: bool -> A.t lazy_t -> t
    val some_if': bool -> A.t -> t

    val is_some: t -> bool
    val is_none: t -> bool

    val value_def: t -> def:A.t -> A.t
    val value: ?exc:exn -> t -> A.t
    val or_failure: ('a, unit, string, string, string, t -> A.t) CamlinternalFormatBasics.format6 -> 'a

    val map: t -> f:(A.t -> 'a) -> 'a option
    val iter: t -> f:(A.t -> unit) -> unit
    val filter: t -> f:(A.t -> bool) -> t
    val filter_map: t -> f:(A.t -> 'a option) -> 'a option

    val value_map: t -> def:'a -> f:(A.t -> 'a) -> 'a
  end
end

module Lazy: sig
  type 'a t = 'a lazy_t

  val is_value: 'a t -> bool

  val value: 'a t -> 'a

  val map: 'a t -> f:('a -> 'b) -> 'b t
end

module Reference: sig
  type 'a t = 'a Pervasives.OCamlStandard.Pervasives.ref = {mutable contents: 'a}

  (* @feature Concept.Able *)

  val of_contents: 'a -> 'a t
  val contents: 'a t -> 'a
  val assign: 'a t -> 'a -> unit

  module O: sig
    val ref: 'a -> 'a t
    val (!): 'a t -> 'a
    val (:=): 'a t -> 'a -> unit
  end

  module SpecializeOperators(A: sig type t end): sig
    type nonrec t = A.t t

    val ref: A.t -> t
    val (!): t -> A.t
    val (:=): t -> A.t -> unit
  end

  module Specialize(A: sig type t end): sig
    type nonrec t = A.t t

    val of_contents: A.t -> t
    val contents: t -> A.t
    val assign: t -> A.t -> unit

    module O: module type of SpecializeOperators(A) with type t := t
  end

  (* @feature SpecializeComparable *)
  (* @feature SpecializeEquatable *)
  (* @feature SpecializeRepresentable *)
  (* @feature SpecializeAble (merge of three previous) *)

  module SpecializePredSucc(A: Traits.PredSucc.S0): sig
    type nonrec t = A.t t

    val increment: t -> unit
    val decrement: t -> unit
  end

  module SpecializeRingoidOperators(A: Traits.Ringoid.Basic.S0): sig
    type nonrec t = A.t t

    val (=+): t -> A.t -> unit
    val (=-): t -> A.t -> unit
    val (=*): t -> A.t -> unit
    val (=/): t -> A.t -> unit
  end

  module SpecializeRingoid(A: Traits.Ringoid.Basic.S0): sig
    type nonrec t = A.t t

    module O: module type of SpecializeRingoidOperators(A) with type t := t
  end
end

module Tuple2: sig
  type ('a, 'b) t = 'a * 'b

  include Concepts.Able.S2 with type ('a, 'b) t := ('a, 'b) t

  val make: 'a -> 'b -> ('a, 'b) t

  val get_0: ('a, _) t -> 'a
  val get_1: (_, 'b) t -> 'b

  val flip: ('a, 'b) t -> ('b, 'a) t
end

module Tuple3: sig
  type ('a, 'b, 'c) t = 'a * 'b * 'c

  include Concepts.Able.S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) t

  val make: 'a -> 'b -> 'c -> ('a, 'b, 'c) t

  val get_0: ('a, _, _) t -> 'a
  val get_1: (_, 'b, _) t -> 'b
  val get_2: (_, _, 'c) t -> 'c

  val flip: ('a, 'b, 'c) t -> ('c, 'b, 'a) t
end

module Tuple4: sig
  type ('a, 'b, 'c, 'd) t = 'a * 'b * 'c * 'd

  include Concepts.Able.S4 with type ('a, 'b, 'c, 'd) t := ('a, 'b, 'c, 'd) t

  val make: 'a -> 'b -> 'c -> 'd -> ('a, 'b, 'c, 'd) t

  val get_0: ('a, _, _, _) t -> 'a
  val get_1: (_, 'b, _, _) t -> 'b
  val get_2: (_, _, 'c, _) t -> 'c
  val get_3: (_, _, _, 'd) t -> 'd

  val flip: ('a, 'b, 'c, 'd) t -> ('d, 'c, 'b, 'a) t
end

module Tuple5: sig
  type ('a, 'b, 'c, 'd, 'e) t = 'a * 'b * 'c * 'd * 'e

  include Concepts.Able.S5 with type ('a, 'b, 'c, 'd, 'e) t := ('a, 'b, 'c, 'd, 'e) t

  val make: 'a -> 'b -> 'c -> 'd -> 'e -> ('a, 'b, 'c, 'd, 'e) t

  val get_0: ('a, _, _, _, _) t -> 'a
  val get_1: (_, 'b, _, _, _) t -> 'b
  val get_2: (_, _, 'c, _, _) t -> 'c
  val get_3: (_, _, _, 'd, _) t -> 'd
  val get_4: (_, _, _, _, 'e) t -> 'e

  val flip: ('a, 'b, 'c, 'd, 'e) t -> ('e, 'd, 'c, 'b, 'a) t
end

(* Specializations of fixed-size containers *)

module IntOption: sig
  include module type of Option.Specialize(Int)
end

module FloatOption: sig
  include module type of Option.Specialize(Float)
end

module StringOption: sig
  include module type of Option.Specialize(String)
end

(* @feature BoolOption, with tri-bool operations (None == "unknown") as functions and as operators *)

module IntReference: sig
  type t = int Reference.t

  module O: sig
    include module type of Reference.SpecializeOperators(Int) with type t := t
    include module type of Reference.SpecializeRingoidOperators(Int) with type t := t
  end

  include module type of Reference.Specialize(Int) with type t := t and module O := O
  include module type of Reference.SpecializePredSucc(Int) with type t := t
  include module type of Reference.SpecializeRingoid(Int) with type t := t and module O := O
end

module FloatReference: sig
  type t = float Reference.t

  module O: sig
    include module type of Reference.SpecializeOperators(Float) with type t := t
    include module type of Reference.SpecializeRingoidOperators(Float) with type t := t
  end

  include module type of Reference.Specialize(Float) with type t := t and module O := O
  include module type of Reference.SpecializeRingoid(Float) with type t := t and module O := O
end

module StringReference: sig
  type t = string Reference.t

  module O: sig
    include module type of Reference.SpecializeOperators(String) with type t := t
    val (=^): t -> string -> unit
  end

  include module type of Reference.Specialize(String) with type t := t and module O := O
end

(* @feature BoolReference with set and reset *)
(* @feature OptionReference with := x setting to Some x and reset setting to None *)
(* @feature OptionPair.to_pair_option (Some a, Some b) -> Some (a, b) | _ -> None *)
(* @feature LazyPair.to_pair_lazy *)

(* Collection containers *)

module List: sig
  type 'a t = 'a list

  module O: sig
    val (@): 'a t -> 'a t -> 'a t
  end

  val empty: 'a t
  val singleton: 'a -> 'a t
  val of_list: 'a list -> 'a t
  val to_list: 'a t -> 'a list
  val of_array: 'a array -> 'a t
  val to_array: 'a t -> 'a array

  val size: 'a t -> int
  val is_empty: 'a t -> bool
  val head: 'a t -> 'a
  val tail: 'a t -> 'a t
  val try_head: 'a t -> 'a option
  val try_tail: 'a t -> 'a t option

  val contains: 'a t -> 'a -> equal_a:('a -> 'a -> bool) -> bool

  module Poly: sig
    val contains: 'a t -> 'a -> bool
  end

  val prepend: 'a -> 'a t -> 'a t
  val reverse: 'a t -> 'a t
  val concat: 'a t -> 'a t -> 'a t

  include Traits.FilterMapable.S1 with type 'a t := 'a t
  include Traits.Foldable.S1 with type 'a t := 'a t
  include Traits.Foldable.Short.S1 with type 'a t := 'a t
  (* include Traits.Foldable.Right.S1 with type 'a t := 'a t *)
  (* include Traits.Foldable.Short.Right.S1 with type 'a t := 'a t *)
  include Traits.Scanable.S1 with type 'a t := 'a t
  include Traits.Scanable.Short.S1 with type 'a t := 'a t
  (* include Traits.Scanable.Right.S1 with type 'a t := 'a t *)
  (* include Traits.Scanable.Short.Right.S1 with type 'a t := 'a t *)

  module Two: sig
    val to_pair_list: 'a t -> 'b t -> ('a * 'b) t
    val to_pair_list_short: 'a t -> 'b t -> ('a * 'b) t
  end

  module Specialize(A: sig type t end): sig
    type t = A.t list

    module O: sig
      val (@): t -> t -> t
    end

    val empty: t
    val singleton: A.t -> t
    val of_list: A.t list -> t
    val to_list: t -> A.t list
    val of_array: A.t array -> t
    val to_array: t -> A.t array

    val size: t -> int
    val is_empty: t -> bool
    val head: t -> A.t
    val tail: t -> t
    val try_head: t -> A.t option
    val try_tail: t -> t option

    val prepend: A.t -> t -> t
    val reverse: t -> t
    val concat: t -> t -> t

    include Traits.FilterMapable.S0 with type elt := A.t and type t := t
    include Traits.Foldable.S0 with type elt := A.t and type t := t
    include Traits.Foldable.Short.S0 with type elt := A.t and type t := t
    (* include Traits.Foldable.Right.S0 with type elt := A.t and type t := t *)
    (* include Traits.Foldable.Short.Right.S0 with type elt := A.t and type t := t *)
    include Traits.Scanable.S0 with type elt := A.t and type t := t
    include Traits.Scanable.Short.S0 with type elt := A.t and type t := t
    (* include Traits.Scanable.Right.S0 with type elt := A.t and type t := t *)
    (* include Traits.Scanable.Short.Right.S0 with type elt := A.t and type t := t *)

    module ToList: sig
      include Traits.FilterMapable.ToList.S0 with type elt := A.t and type t := t
      include Traits.Scanable.ToList.S0 with type elt := A.t and type t := t
      include Traits.Scanable.Short.ToList.S0 with type elt := A.t and type t := t
      (* include Traits.Scanable.Right.ToList.S0 with type elt := A.t and type t := t *)
      (* include Traits.Scanable.Short.Right.ToList.S0 with type elt := A.t and type t := t *)
    end
  end

  module SpecializeEquatable(A: Traits.Equatable.Basic.S0): sig
    type t = A.t list

    val contains: t -> A.t -> bool
  end
end

module Array: sig
  type 'a t = 'a array

  (* @todo Implement *)

  val size: 'a t -> int
  val get: 'a t -> int -> 'a
  val set: 'a t -> int -> 'a -> unit
end

module Stream: sig
  type 'a t = 'a Pervasives.OCamlStandard.Stream.t

  val empty: 'a t
  val singleton: 'a -> 'a t

  val to_list: 'a t -> 'a list
  val of_list: 'a list -> 'a t

  val prepend: 'a -> 'a t -> 'a t
  val concat: 'a t -> 'a t -> 'a t

  include Traits.FilterMapable.S1 with type 'a t := 'a t
  (* @feature Other iteration traits: Foldable and Scanable *)

  module ToList: sig
    include Traits.FilterMapable.ToList.S1 with type 'a t := 'a t
  end

  (* @feature module Specialize (with ToList and ToStream) *)
end

module SortedSet: sig
  module Poly: sig
    type 'a t

    val empty: 'a t

    val of_list: 'a list -> 'a t
    val to_list: 'a t -> 'a list

    val is_empty: 'a t -> bool
    val size: 'a t -> int

    val add: 'a t -> v:'a -> bool * 'a t
    val replace: 'a t -> v:'a -> 'a t
    val remove: 'a t -> v:'a -> bool * 'a t

    val contains: 'a t -> v:'a -> bool

    (* @feature Traits *)
  end

  module Make(E: Traits.Comparable.Basic.S0): sig
    type t

    val empty: t

    val of_list: E.t list -> t
    val to_list: t -> E.t list

    val is_empty: t -> bool
    val size: t -> int

    val add: t -> v:E.t -> bool * t
    val replace: t -> v:E.t -> t
    val remove: t -> v:E.t -> bool * t

    val contains: t -> v:E.t -> bool

    (* @feature Traits *)
  end
end

module SortedMap: sig
  module Poly: sig
    type ('a, 'b) t

    val empty: ('a, 'b) t

    (* val of_list_unique: ('a * 'b) list -> ('a, 'b) t *)
    (* val try_of_list_unique: ('a * 'b) list -> ('a, 'b) t option *)
    val of_list_first: ('a * 'b) list -> ('a, 'b) t
    val of_list_last: ('a * 'b) list -> ('a, 'b) t
    (* val of_list_reduce: ('a * 'b) list -> f:('b -> 'b -> 'b) -> ('a, 'b) t *)
    val to_list: ('a, 'b) t -> ('a * 'b) list

    val is_empty: ('a, 'b) t -> bool
    val size: ('a, 'b) t -> int

    val add: ('a, 'b) t -> k:'a -> v:'b -> bool * ('a, 'b) t
    val replace: ('a, 'b) t -> k:'a -> v:'b -> ('a, 'b) t
    val remove: ('a, 'b) t -> k:'a -> bool * ('a, 'b) t

    val try_get: ('a, 'b) t -> k:'a -> 'b option
    val get: ('a, 'b) t -> k:'a -> 'b

    (* @feature Traits *)
  end

  module Make(K: Traits.Comparable.Basic.S0): sig
    type 'a t

    val empty: 'a t

    (* @feature [try_]of_list_unique, of_list_reduce *)
    val of_list_first: (K.t * 'a) list -> 'a t
    val of_list_last: (K.t * 'a) list -> 'a t
    val to_list: 'a t -> (K.t * 'a) list

    val is_empty: 'a t -> bool
    val size: 'a t -> int

    val add: 'a t -> k:K.t -> v:'a -> bool * 'a t
    val replace: 'a t -> k:K.t -> v:'a -> 'a t
    val remove: 'a t -> k:K.t -> bool * 'a t

    val try_get: 'a t -> k:K.t -> 'a option
    val get: 'a t -> k:K.t -> 'a

    (* @feature Traits *)
  end
end

module Heap: sig
  module Poly: sig
    type 'a t

    val empty: 'a t

    (* val of_list: 'a list -> 'a t *)
    (* val to_list: 'a t -> 'a list *)

    (* val is_empty: 'a t -> bool *)
    (* val size: 'a t -> int *)

    val add: 'a t -> v:'a -> 'a t
    val pop_max: 'a t -> 'a t
    val max: 'a t -> 'a
  end

  module Make(E: Traits.Comparable.Basic.S0): sig
    type t

    val empty: t

    val add: t -> v:E.t -> t
    val pop_max: t -> t
    val max: t -> E.t
  end
end

module PriorityQueue: sig
  module Poly: sig
    type ('a, 'b) t

    val empty: ('a, 'b) t

    val add: ('a, 'b) t -> k:'a -> v:'b -> ('a, 'b) t
    val pop_max: ('a, 'b) t -> ('a, 'b) t
    val max: ('a, 'b) t -> 'a * 'b
  end

  module Make(K: Traits.Comparable.Basic.S0): sig
    type 'a t

    val empty: 'a t

    val add: 'a t -> k:K.t -> v:'a -> 'a t
    val pop_max: 'a t -> 'a t
    val max: 'a t -> K.t * 'a
  end
end

(* @feature SortedList, UniqueList? *)
(* @feature Double-ended queue *)
(* @feature HashSet, HashMap *)
(* @feature SortedMultiSet, SortedMultiMap, HashMultiSet, HashMultiMap *)
(* [Sorted|Hash][Multi]Index (a Map where the keys are computed from the values (think "vertex_by_name")) *)

(* Specializations of collection containers *)

module IntRange: sig
  type t

  include Concepts.Identifiable.S0 with type t := t
  (* @feature Add Comparable to make it Able
  Warning: compare r1 r2 should always be equal to List.compare (to_list r1) (to_list r2), so Compare.Poly will not work. *)

  val empty: t
  val make: ?start:int -> ?step:int -> int -> t

  val to_list: t -> int list
  val to_array: t -> int array

  include Traits.Foldable.S0 with type elt := int and type t := t
  include Traits.Foldable.Short.S0 with type elt := int and type t := t

  module ToList: sig
    include Traits.FilterMapable.ToList.S0 with type elt := int and type t := t
    include Traits.Scanable.ToList.S0 with type elt := int and type t := t
    include Traits.Scanable.Short.ToList.S0 with type elt := int and type t := t
  end
end

module IntList: sig
  include module type of List.Specialize(Int)
  include module type of List.SpecializeEquatable(Int) with type t := t
end

module FloatList: sig
  include module type of List.Specialize(Float)
  include module type of List.SpecializeEquatable(Float) with type t := t
end

module StringList: sig
  include module type of List.Specialize(String)
  include module type of List.SpecializeEquatable(String) with type t := t

  val join: ?sep:string -> t -> string
end

module IntSortedSet: sig
  include module type of SortedSet.Make(Int)
end

module FloatSortedSet: sig
  include module type of SortedSet.Make(Float)
end

module StringSortedSet: sig
  include module type of SortedSet.Make(String)
end

module CharSortedSet: sig
  include module type of SortedSet.Make(Char)
end

module IntSortedMap: sig
  include module type of SortedMap.Make(Int)
end

module FloatSortedMap: sig
  include module type of SortedMap.Make(Float)
end

module StringSortedMap: sig
  include module type of SortedMap.Make(String)
end

module CharSortedMap: sig
  include module type of SortedMap.Make(Char)
end

(* @feature XxxList when Xxx is a Ringoid: add sum, product *)
(* @feature BoolList (with all, exists, etc.) *)
(* @feature OptionList (with first_some, values (ie filter_some)) *)
(* @feature ListList, ArrayArray, StreamStream with val flatten: 'a t t -> 'a t *)
(* @feature LazyList.to_list_lazy *)

(* Input/output *)

module Format: sig
  type ('a, 'b, 'c, 'd, 'e, 'f) t = ('a, 'b, 'c, 'd, 'e, 'f) CamlinternalFormatBasics.format6

  (* From Pervasives:
  type ('a, 'b, 'c, 'd) format4 = ('a, 'b, 'c, 'c, 'c, 'd) format6
  type ('a, 'b, 'c) format = ('a, 'b, 'c, 'c, 'c, 'c) format6 *)

  val with_result: ('b, unit, string, string, string, 'a) t -> f:(string -> 'a) -> 'b

  val apply: ('a, unit, string, string, string, string) t -> 'a

  val to_string: ('a, 'b, 'c, 'd, 'e, 'f) t -> string

  val of_string: ('a, 'b, 'c, 'd, 'e, 'f) t -> ('a, 'b, 'c, 'd, 'e, 'f) t

  val concat: ('a, 'b, 'c, 'd, 'e, 'f) t -> ('f, 'b, 'c, 'e, 'g, 'h) t -> ('a, 'b, 'c, 'd, 'g, 'h) t

  val with_scan_result: ('a, Pervasives.OCamlStandard.Scanf.Scanning.scanbuf, 'b, 'c -> 'd, 'a -> 'e, 'e) t -> f:'c -> string -> 'd
end

module InChannel: sig
  type t = Pervasives.OCamlStandard.Pervasives.in_channel

  (* @feature val lines: string Stream.t *)
end

module InFile: sig
  type t

  (* @todo Open modes *)
  val with_file: string -> f:(t -> 'a) -> 'a
  val with_channel: string -> f:(InChannel.t -> 'a) -> 'a

  val channel: t -> InChannel.t

  val seek: t -> pos:int64 -> unit
  val pos: t -> int64
  val size: t -> int64
end

module OutChannel: sig
  type t = Pervasives.OCamlStandard.Pervasives.out_channel

  val print: ?flush:bool -> t -> ('a, t, unit, unit, unit, unit) Format.t -> 'a
  val output: t -> bytes -> unit
  val flush: t -> unit
end

module OutFile: sig
  type t

  (* @todo Open modes *)
  val with_file: string -> f:(t -> 'a) -> 'a
  val with_channel: string -> f:(OutChannel.t -> 'a) -> 'a

  val channel: t -> OutChannel.t

  val seek: t -> pos:int64 -> unit
  val pos: t -> int64
  val size: t -> int64
end

module StdIn: sig
  val channel: InChannel.t
end

module StdOut: sig
  val channel: OutChannel.t
  val print: ?flush:bool -> ('a, OutChannel.t, unit, unit, unit, unit) Format.t -> 'a
  val output: bytes -> unit
  val flush: unit -> unit
end

module StdErr: sig
  val channel: OutChannel.t
  val print: ?flush:bool -> ('a, OutChannel.t, unit, unit, unit, unit) Format.t -> 'a
  val output: bytes -> unit
  val flush: unit -> unit
end

(* Testing *)

module Testing: sig
  module Result: sig
    module Status: sig
      type failure

      type t =
        | Success
        | Failure of failure
        | Error of exn * CallStack.t option

      val to_string: t -> string
    end

    type single = {
      label: string;
      status: Status.t;
    }

    module Counts: sig
      type t = {
        successes: int;
        failures: int;
        errors: int;
      }
    end

    type group = {
      name: string;
      children: t list;
      counts: Counts.t;
    }

    and t =
      | Single of single
      | Group of group
  end

  module Test: sig
    type t

    val run: ?record_backtrace:bool -> t -> Result.t
  end

  val command_line_main: argv:string list -> Test.t -> Exit.t

  val (>::): string -> Test.t list -> Test.t

  val (>:): string -> unit lazy_t -> Test.t

  val (~:): ('a, unit, string, string, string, unit lazy_t -> Test.t) CamlinternalFormatBasics.format6 -> 'a

  val (~::): ('a, unit, string, string, string, Test.t list -> Test.t) CamlinternalFormatBasics.format6 -> 'a

  val fail: ('a, unit, string, string, string, 'b) CamlinternalFormatBasics.format6 -> 'a

  val expect_exception: expected:exn -> 'a lazy_t -> unit

  val expect_exception_named: expected:string -> 'a lazy_t -> unit

  val check: repr:('a -> string) -> equal:('a -> 'a -> bool) -> expected:'a -> 'a -> unit

  val check_poly: repr:('a -> string) -> expected:'a -> 'a -> unit

  val check_string: expected:string -> string -> unit

  val check_bool: expected:bool -> bool -> unit

  val check_true: bool -> unit

  val check_false: bool -> unit

  val check_int: expected:int -> int -> unit

  val check_float: ?precision:float -> expected:float -> float -> unit

  val check_float_in: low:float -> high:float -> float -> unit

  val check_float_exact: expected:float -> float -> unit

  val check_option: repr:('a -> string) -> equal:('a -> 'a -> bool) -> expected:'a option -> 'a option -> unit

  val check_option_poly: repr:('a -> string) -> expected:'a option -> 'a option -> unit

  val check_some: repr:('a -> string) -> equal:('a -> 'a -> bool) -> expected:'a -> 'a option -> unit

  val check_none: repr:('a -> string) -> equal:('a -> 'a -> bool) -> 'a option -> unit

  val check_some_poly: repr:('a -> string) -> expected:'a -> 'a option -> unit

  val check_none_poly: repr:('a -> string) -> 'a option -> unit

  val check_int_option: expected:int option -> int option -> unit

  val check_some_int: expected:int -> int option -> unit

  val check_none_int: int option -> unit

  val check_string_option: expected:string option -> string option -> unit

  val check_some_string: expected:string -> string option -> unit

  val check_none_string: string option -> unit

  val check_list: repr:('a -> string) -> equal:('a -> 'a -> bool) -> expected:'a list -> 'a list -> unit

  val check_list_poly: repr:('a -> string) -> expected:'a list -> 'a list -> unit

  val check_string_list: expected:string list -> string list -> unit

  val check_int_list: expected:int list -> int list -> unit
end

(** Modules to be opened *)

module Standard: sig
  (** This modules defines aliases for all standard modules in :mod:`General`: *)

  module Testing = Testing

  module Array = Array
  module BigInt = BigInt
  module Bool = Bool
  module Bytes = Bytes
  module CallStack = CallStack
  module Char = Char
  module Exception = Exception
  module Exit = Exit
  module Float = Float
  module Format = Format
  module Function1 = Function1
  module Function2 = Function2
  module Function3 = Function3
  module Function4 = Function4
  module Function5 = Function5
  module Heap = Heap
  module InChannel = InChannel
  module InFile = InFile
  module Int = Int
  module Int32 = Int32
  module Int64 = Int64
  module Lazy = Lazy
  module List = List
  module NativeInt = NativeInt
  module Option = Option
  module OutChannel = OutChannel
  module OutFile = OutFile
  module PriorityQueue = PriorityQueue
  module Reference = Reference
  module SortedMap = SortedMap
  module SortedSet = SortedSet
  module StdErr = StdErr
  module StdIn = StdIn
  module StdOut = StdOut
  module Stream = Stream
  module String = String
  module Tuple2 = Tuple2
  module Tuple3 = Tuple3
  module Tuple4 = Tuple4
  module Tuple5 = Tuple5
  module Unit = Unit

  module IntRange = IntRange

  module FloatOption = FloatOption
  module IntOption = IntOption
  module StringOption = StringOption

  module FloatReference = FloatReference
  module IntReference = IntReference
  module StringReference = StringReference

  module FloatList = FloatList
  module IntList = IntList
  module StringList = StringList

  module CharSortedSet = CharSortedSet
  module FloatSortedSet = FloatSortedSet
  module IntSortedSet = IntSortedSet
  module StringSortedSet = StringSortedSet

  module CharSortedMap = CharSortedMap
  module FloatSortedMap = FloatSortedMap
  module IntSortedMap = IntSortedMap
  module StringSortedMap = StringSortedMap

  (** It also includes :mod:`General.Pervasives`. *)

  include module type of Pervasives
  with module Array := Array
  and module Bytes := Bytes
  and module Char := Char
  and module Format := Format
  and module Int32 := Int32
  and module Int64 := Int64
  and module Lazy := Lazy
  and module List := List
  and module Stream := Stream
  and module String := String
  [@@autodoc.hide]
end

module Abbr: sig
  (** This modules defines abbreviated aliases for all standard modules in :mod:`General`: *)

  module Tst = Testing

  module Ar = Array
  module BigInt = BigInt
  module Bo = Bool
  module By = Bytes
  module CallStack = CallStack
  module Ch = Char
  module Exit = Exit
  module Exn = Exception
  module Fl = Float
  module Frmt = Format
  module Fun1 = Function1
  module Fun2 = Function2
  module Fun3 = Function3
  module Fun4 = Function4
  module Fun5 = Function5
  module Heap = Heap
  module InCh = InChannel
  module InFile = InFile
  module Int = Int
  module Int32 = Int32
  module Int64 = Int64
  module Laz = Lazy
  module Li = List
  module NativeInt = NativeInt
  module Opt = Option
  module OutCh = OutChannel
  module OutFile = OutFile
  module PriQu = PriorityQueue
  module Ref = Reference
  module SoMap = SortedMap
  module SoSet = SortedSet
  module StdErr = StdErr
  module StdIn = StdIn
  module StdOut = StdOut
  module Str = String
  module Strm = Stream
  module Tu2 = Tuple2
  module Tu3 = Tuple3
  module Tu4 = Tuple4
  module Tu5 = Tuple5
  module Unit = Unit

  module IntRa = IntRange

  module FlOpt = FloatOption
  module IntOpt = IntOption
  module StrOpt = StringOption

  module FlRef = FloatReference
  module IntRef = IntReference
  module StrRef = StringReference

  module FlLi = FloatList
  module IntLi = IntList
  module StrLi = StringList

  module ChSoSet = CharSortedSet
  module FlSoSet = FloatSortedSet
  module IntSoSet = IntSortedSet
  module StrSoSet = StringSortedSet

  module ChSoMap = CharSortedMap
  module FlSoMap = FloatSortedMap
  module IntSoMap = IntSortedMap
  module StrSoMap = StringSortedMap

  (** It also includes :mod:`General.Pervasives`. *)

  include module type of Pervasives
  with module Int32 := Int32
  and module Int64 := Int64
  [@@autodoc.hide]
end

(* @todo Remove from interface, or make a functor, to avoid linking tests in client applications *)
module Tests: sig
  val test: Testing.Test.t
end [@@autodoc.hide]

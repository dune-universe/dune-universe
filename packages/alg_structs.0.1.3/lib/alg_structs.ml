(** {1:structures Structures} *)

(** {2 Triviality} *)

module Triv = Triv

(** {2 Functors} *)

module Functor = Functor
module Applicative = Applicative

(** {2 Algebras} *)

module Semigroup = Semigroup
module Monoid = Monoid

(** {2 Folding}*)

module Foldable = Foldable

(** {1 Utilities }*)

module NonEmptyList = NonEmptyList

(* Don't need to document this since it's included *)
(**/**)
module Util = Util
(**/**)

include Util

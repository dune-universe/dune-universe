(** {1 Supported Structures} *)

(** QCheck tests of {{!module:Alg_structs.Functor.Law} Functor Laws} *)
module Functor = Functor

(** QCheck tests of {{!module:Alg_structs.Applicative.Law} Applicative Laws} *)
module Applicative = Applicative

(** QCheck tests of {{!module:Alg_structs.Semigroup.Law} Semigroup Laws} *)
module Semigroup = Semigroup

(** QCheck tests of {{!module:Alg_structs.Monoid.Law} Monoid Laws} *)
module Monoid = Monoid

(** QCheck tests of {{!module:Alg_structs.Foldable.Law} Foldable Laws} *)
module Foldable = Foldable


(** {1 Utilities} *)

module Utils = Utils

(**/**)
(* Exported for use in testing of this package *)
module Alg_structs = Alg_structs
(**/**)

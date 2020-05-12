(** Modular interface. *)
module Modules = Modules

(** First-class module interface. *)
module Values = Values

include (
  module type of struct include Modules end with
  module Applicative := Modules.Applicative)

include (
  module type of struct include Values end with
  module Applicative := Values.Applicative)

module Applicative : sig
  include module type of Modules.Applicative

  include module type of Values.Applicative
end

module Primitives = Primitives

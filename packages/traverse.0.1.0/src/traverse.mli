(** Modular interface. *)
module Modules = Traverse_modules

(** First-class module interface. *)
module Values = Traverse_values

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

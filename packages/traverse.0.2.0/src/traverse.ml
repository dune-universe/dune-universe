module Modules = Modules

module Values = Values

include (Modules :
  module type of struct include Modules end with
  module Applicative := Modules.Applicative)

include (Values :
  module type of struct include Values end with
  module Applicative := Values.Applicative)

module Applicative = struct
  include Modules.Applicative

  include Values.Applicative
end

module Primitives = Primitives

module Operators = Traits.Equatable.Operators

module type S0 = sig
  type t

  include Traits.Equatable.S0 with type t := t
  include Traits.Representable.S0 with type t := t
end

module type S1 = sig
  type 'a t

  include Traits.Equatable.S1 with type 'a t := 'a t
  include Traits.Representable.S1 with type 'a t := 'a t
end

module type S2 = sig
  type ('a, 'b) t

  include Traits.Equatable.S2 with type ('a, 'b) t := ('a, 'b) t
  include Traits.Representable.S2 with type ('a, 'b) t := ('a, 'b) t
end

module type S3 = sig
  type ('a, 'b, 'c) t

  include Traits.Equatable.S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
  include Traits.Representable.S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
end

module type S4 = sig
  type ('a, 'b, 'c, 'd) t

  include Traits.Equatable.S4 with type ('a, 'b, 'c, 'd) t := ('a, 'b, 'c, 'd) t
  include Traits.Representable.S4 with type ('a, 'b, 'c, 'd) t := ('a, 'b, 'c, 'd) t
end

module type S5 = sig
  type ('a, 'b, 'c, 'd, 'e) t

  include Traits.Equatable.S5 with type ('a, 'b, 'c, 'd, 'e) t := ('a, 'b, 'c, 'd, 'e) t
  include Traits.Representable.S5 with type ('a, 'b, 'c, 'd, 'e) t := ('a, 'b, 'c, 'd, 'e) t
end

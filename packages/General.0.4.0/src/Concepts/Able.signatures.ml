module type S0 = sig
  type t

  module O: Operators.S0 with type t := t

  include Identifiable.S0 with type t := t and module O := O
  include Traits.Comparable.S0 with type t := t and module O := O
end

module type S1 = sig
  type 'a t

  include Identifiable.S1 with type 'a t := 'a t
  include Traits.Comparable.S1 with type 'a t := 'a t
end

module type S2 = sig
  type ('a, 'b) t

  include Identifiable.S2 with type ('a, 'b) t := ('a, 'b) t
  include Traits.Comparable.S2 with type ('a, 'b) t := ('a, 'b) t
end

module type S3 = sig
  type ('a, 'b, 'c) t

  include Identifiable.S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
  include Traits.Comparable.S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
end

module type S4 = sig
  type ('a, 'b, 'c, 'd) t

  include Identifiable.S4 with type ('a, 'b, 'c, 'd) t := ('a, 'b, 'c, 'd) t
  include Traits.Comparable.S4 with type ('a, 'b, 'c, 'd) t := ('a, 'b, 'c, 'd) t
end

module type S5 = sig
  type ('a, 'b, 'c, 'd, 'e) t

  include Identifiable.S5 with type ('a, 'b, 'c, 'd, 'e) t := ('a, 'b, 'c, 'd, 'e) t
  include Traits.Comparable.S5 with type ('a, 'b, 'c, 'd, 'e) t := ('a, 'b, 'c, 'd, 'e) t
end

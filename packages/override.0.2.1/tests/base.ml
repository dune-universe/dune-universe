type t = int

type u = A | B

let x = 1

module M = struct
  type 'a t = 'a option

  let x = 2
end

module type S = sig
  type t
end

module F (X : S) = struct
  type t
end

module N = struct
  type t
end

module O = struct
  type t = int

  type u = bool

  type 'a v = 'a option
end

module P = struct
  type t = A of u

  type u = bool

  type 'a v = 'a option
end

module G (X : S) = struct
  type t = X.t
end

module Rec_group = struct
  type a = A of b and b = B of a

  type c = C of d and d = D of c
end

module Module_type = struct
  type first

  module type S = sig
    type t = first
  end

  type last = (module S)
end

module Redefine_module_type = struct
  let x = ()

  module type S = sig
    type t
  end
end

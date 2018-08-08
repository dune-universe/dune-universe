module A = struct
  type t = (* T => *) T (* <= T *)
end

module F(X: sig end) = struct
  include A
end

module FX =  F(struct end)

module B = struct
  include FX
  let x = T
end

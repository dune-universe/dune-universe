module F(X: sig end) = struct
  type t = (* T => *) T (* <= T *)
end

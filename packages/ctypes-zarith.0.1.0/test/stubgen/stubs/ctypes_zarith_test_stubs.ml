open Ctypes
open Ctypes_zarith

module Stubs (F : Ctypes.FOREIGN) = struct
  open F

  let z_add =
    foreign "mpz_add"
      (MPZ.t_ptr @-> MPZ.zarith @-> MPZ.zarith @-> returning void)

  let q_add =
    foreign "mpq_add"
      (MPQ.t_ptr @-> MPQ.zarith @-> MPQ.zarith @-> returning void)
end

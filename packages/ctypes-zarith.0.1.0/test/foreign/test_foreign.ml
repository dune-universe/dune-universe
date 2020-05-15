open Ctypes
open Ctypes_zarith

let z_add =
  Foreign.foreign "__gmpz_add"
    (MPZ.t_ptr @-> MPZ.zarith @-> MPZ.zarith @-> returning void)

let q_add =
  Foreign.foreign "__gmpq_add"
    (MPQ.t_ptr @-> MPQ.zarith @-> MPQ.zarith @-> returning void)

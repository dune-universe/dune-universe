let%c () = header {|
#include <gmp.h>
|}

external z_add :
  Ctypes_zarith.MPZ.t_ptr ->
  Ctypes_zarith.MPZ.zarith ->
  Ctypes_zarith.MPZ.zarith ->
  void = "mpz_add"
  [@@noalloc]

external q_add :
  Ctypes_zarith.MPQ.t_ptr ->
  Ctypes_zarith.MPQ.zarith ->
  Ctypes_zarith.MPQ.zarith ->
  void = "mpq_add"
  [@@noalloc]

module CI = Cstubs_internals

external _1_Hacl_Impl_Curve25519_Field51_fadd
  : _ CI.fatptr -> _ CI.fatptr -> _ CI.fatptr -> unit
  = "_1_Hacl_Impl_Curve25519_Field51_fadd" 

external _2_Hacl_Impl_Curve25519_Field51_fsub
  : _ CI.fatptr -> _ CI.fatptr -> _ CI.fatptr -> unit
  = "_2_Hacl_Impl_Curve25519_Field51_fsub" 

external _3_Hacl_Impl_Curve25519_Field51_fmul1
  : _ CI.fatptr -> _ CI.fatptr -> Unsigned.uint64 -> unit
  = "_3_Hacl_Impl_Curve25519_Field51_fmul1" 

external _4_Hacl_Curve25519_51_scalarmult
  : bytes CI.ocaml -> bytes CI.ocaml -> bytes CI.ocaml -> unit
  = "_4_Hacl_Curve25519_51_scalarmult" 

external _5_Hacl_Curve25519_51_secret_to_public
  : bytes CI.ocaml -> bytes CI.ocaml -> unit
  = "_5_Hacl_Curve25519_51_secret_to_public" 

external _6_Hacl_Curve25519_51_ecdh
  : bytes CI.ocaml -> bytes CI.ocaml -> bytes CI.ocaml -> bool
  = "_6_Hacl_Curve25519_51_ecdh" 

type 'a result = 'a
type 'a return = 'a
type 'a fn =
 | Returns  : 'a CI.typ   -> 'a return fn
 | Function : 'a CI.typ * 'b fn  -> ('a -> 'b) fn
let map_result f x = f x
let returning t = Returns t
let (@->) f p = Function (f, p)
let foreign : type a b. string -> (a -> b) fn -> (a -> b) =
  fun name t -> match t, name with
| Function
    (CI.OCaml CI.Bytes,
     Function
       (CI.OCaml CI.Bytes,
        Function (CI.OCaml CI.Bytes, Returns (CI.Primitive CI.Bool)))),
  "Hacl_Curve25519_51_ecdh" -> _6_Hacl_Curve25519_51_ecdh
| Function (CI.OCaml CI.Bytes, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "Hacl_Curve25519_51_secret_to_public" ->
  _5_Hacl_Curve25519_51_secret_to_public
| Function
    (CI.OCaml CI.Bytes,
     Function
       (CI.OCaml CI.Bytes, Function (CI.OCaml CI.Bytes, Returns CI.Void))),
  "Hacl_Curve25519_51_scalarmult" -> _4_Hacl_Curve25519_51_scalarmult
| Function
    (CI.Pointer _,
     Function
       (CI.Pointer _, Function (CI.Primitive CI.Uint64_t, Returns CI.Void))),
  "Hacl_Impl_Curve25519_Field51_fmul1" ->
  (fun x9 x11 x13 ->
    let CI.CPointer x12 = x11 in
    let CI.CPointer x10 = x9 in
    _3_Hacl_Impl_Curve25519_Field51_fmul1 x10 x12 x13)
| Function
    (CI.Pointer _,
     Function (CI.Pointer _, Function (CI.Pointer _, Returns CI.Void))),
  "Hacl_Impl_Curve25519_Field51_fsub" ->
  (fun x14 x16 x18 ->
    let CI.CPointer x19 = x18 in
    let CI.CPointer x17 = x16 in
    let CI.CPointer x15 = x14 in
    _2_Hacl_Impl_Curve25519_Field51_fsub x15 x17 x19)
| Function
    (CI.Pointer _,
     Function (CI.Pointer _, Function (CI.Pointer _, Returns CI.Void))),
  "Hacl_Impl_Curve25519_Field51_fadd" ->
  (fun x20 x22 x24 ->
    let CI.CPointer x25 = x24 in
    let CI.CPointer x23 = x22 in
    let CI.CPointer x21 = x20 in
    _1_Hacl_Impl_Curve25519_Field51_fadd x21 x23 x25)
| _, s ->  Printf.ksprintf failwith "No match for %s" s


let foreign_value : type a. string -> a Ctypes.typ -> a Ctypes.ptr =
  fun name t -> match t, name with
| _, s ->  Printf.ksprintf failwith "No match for %s" s

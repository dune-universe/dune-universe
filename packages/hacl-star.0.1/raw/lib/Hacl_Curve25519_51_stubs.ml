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
  : Bytes.t CI.ocaml -> Bytes.t CI.ocaml -> Bytes.t CI.ocaml -> unit
  = "_4_Hacl_Curve25519_51_scalarmult" 

external _5_Hacl_Curve25519_51_secret_to_public
  : Bytes.t CI.ocaml -> Bytes.t CI.ocaml -> unit
  = "_5_Hacl_Curve25519_51_secret_to_public" 

external _6_Hacl_Curve25519_51_ecdh
  : Bytes.t CI.ocaml -> Bytes.t CI.ocaml -> Bytes.t CI.ocaml -> bool
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
  (fun x9 x10 x11 ->
    _3_Hacl_Impl_Curve25519_Field51_fmul1 (CI.cptr x9) (CI.cptr x10) x11)
| Function
    (CI.Pointer _,
     Function (CI.Pointer _, Function (CI.Pointer _, Returns CI.Void))),
  "Hacl_Impl_Curve25519_Field51_fsub" ->
  (fun x12 x13 x14 ->
    _2_Hacl_Impl_Curve25519_Field51_fsub (CI.cptr x12) (CI.cptr x13)
    (CI.cptr x14))
| Function
    (CI.Pointer _,
     Function (CI.Pointer _, Function (CI.Pointer _, Returns CI.Void))),
  "Hacl_Impl_Curve25519_Field51_fadd" ->
  (fun x15 x16 x17 ->
    _1_Hacl_Impl_Curve25519_Field51_fadd (CI.cptr x15) (CI.cptr x16)
    (CI.cptr x17))
| _, s ->  Printf.ksprintf failwith "No match for %s" s


let foreign_value : type a. string -> a Ctypes.typ -> a Ctypes.ptr =
  fun name t -> match t, name with
| _, s ->  Printf.ksprintf failwith "No match for %s" s

module CI = Cstubs_internals

external _1_Hacl_Bignum25519_reduce_513 : _ CI.fatptr -> unit
  = "_1_Hacl_Bignum25519_reduce_513" 

external _2_Hacl_Bignum25519_inverse : _ CI.fatptr -> _ CI.fatptr -> unit
  = "_2_Hacl_Bignum25519_inverse" 

external _3_Hacl_Bignum25519_load_51 : _ CI.fatptr -> bytes CI.ocaml -> unit
  = "_3_Hacl_Bignum25519_load_51" 

external _4_Hacl_Bignum25519_store_51 : bytes CI.ocaml -> _ CI.fatptr -> unit
  = "_4_Hacl_Bignum25519_store_51" 

external _5_Hacl_Impl_Ed25519_PointAdd_point_add
  : _ CI.fatptr -> _ CI.fatptr -> _ CI.fatptr -> unit
  = "_5_Hacl_Impl_Ed25519_PointAdd_point_add" 

external _6_Hacl_Impl_Ed25519_Ladder_point_mul
  : _ CI.fatptr -> bytes CI.ocaml -> _ CI.fatptr -> unit
  = "_6_Hacl_Impl_Ed25519_Ladder_point_mul" 

external _7_Hacl_Impl_Ed25519_PointCompress_point_compress
  : bytes CI.ocaml -> _ CI.fatptr -> unit
  = "_7_Hacl_Impl_Ed25519_PointCompress_point_compress" 

external _8_Hacl_Impl_Ed25519_PointDecompress_point_decompress
  : _ CI.fatptr -> bytes CI.ocaml -> bool
  = "_8_Hacl_Impl_Ed25519_PointDecompress_point_decompress" 

external _9_Hacl_Impl_Ed25519_PointEqual_point_equal
  : _ CI.fatptr -> _ CI.fatptr -> bool
  = "_9_Hacl_Impl_Ed25519_PointEqual_point_equal" 

external _10_Hacl_Impl_Ed25519_PointNegate_point_negate
  : _ CI.fatptr -> _ CI.fatptr -> unit
  = "_10_Hacl_Impl_Ed25519_PointNegate_point_negate" 

external _11_Hacl_Ed25519_sign
  : bytes CI.ocaml -> bytes CI.ocaml -> Unsigned.uint32 -> bytes CI.ocaml ->
    unit = "_11_Hacl_Ed25519_sign" 

external _12_Hacl_Ed25519_verify
  : bytes CI.ocaml -> Unsigned.uint32 -> bytes CI.ocaml -> bytes CI.ocaml ->
    bool = "_12_Hacl_Ed25519_verify" 

external _13_Hacl_Ed25519_secret_to_public
  : bytes CI.ocaml -> bytes CI.ocaml -> unit
  = "_13_Hacl_Ed25519_secret_to_public" 

external _14_Hacl_Ed25519_expand_keys
  : bytes CI.ocaml -> bytes CI.ocaml -> unit = "_14_Hacl_Ed25519_expand_keys" 

external _15_Hacl_Ed25519_sign_expanded
  : bytes CI.ocaml -> bytes CI.ocaml -> Unsigned.uint32 -> bytes CI.ocaml ->
    unit = "_15_Hacl_Ed25519_sign_expanded" 

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
        Function
          (CI.Primitive CI.Uint32_t,
           Function (CI.OCaml CI.Bytes, Returns CI.Void)))),
  "Hacl_Ed25519_sign_expanded" -> _15_Hacl_Ed25519_sign_expanded
| Function (CI.OCaml CI.Bytes, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "Hacl_Ed25519_expand_keys" -> _14_Hacl_Ed25519_expand_keys
| Function (CI.OCaml CI.Bytes, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "Hacl_Ed25519_secret_to_public" -> _13_Hacl_Ed25519_secret_to_public
| Function
    (CI.OCaml CI.Bytes,
     Function
       (CI.Primitive CI.Uint32_t,
        Function
          (CI.OCaml CI.Bytes,
           Function (CI.OCaml CI.Bytes, Returns (CI.Primitive CI.Bool))))),
  "Hacl_Ed25519_verify" -> _12_Hacl_Ed25519_verify
| Function
    (CI.OCaml CI.Bytes,
     Function
       (CI.OCaml CI.Bytes,
        Function
          (CI.Primitive CI.Uint32_t,
           Function (CI.OCaml CI.Bytes, Returns CI.Void)))),
  "Hacl_Ed25519_sign" -> _11_Hacl_Ed25519_sign
| Function (CI.Pointer _, Function (CI.Pointer _, Returns CI.Void)),
  "Hacl_Impl_Ed25519_PointNegate_point_negate" ->
  (fun x17 x19 ->
    let CI.CPointer x20 = x19 in
    let CI.CPointer x18 = x17 in
    _10_Hacl_Impl_Ed25519_PointNegate_point_negate x18 x20)
| Function
    (CI.Pointer _, Function (CI.Pointer _, Returns (CI.Primitive CI.Bool))),
  "Hacl_Impl_Ed25519_PointEqual_point_equal" ->
  (fun x21 x23 ->
    let CI.CPointer x24 = x23 in
    let CI.CPointer x22 = x21 in
    _9_Hacl_Impl_Ed25519_PointEqual_point_equal x22 x24)
| Function
    (CI.Pointer _,
     Function (CI.OCaml CI.Bytes, Returns (CI.Primitive CI.Bool))),
  "Hacl_Impl_Ed25519_PointDecompress_point_decompress" ->
  (fun x25 x27 ->
    let CI.CPointer x26 = x25 in
    _8_Hacl_Impl_Ed25519_PointDecompress_point_decompress x26 x27)
| Function (CI.OCaml CI.Bytes, Function (CI.Pointer _, Returns CI.Void)),
  "Hacl_Impl_Ed25519_PointCompress_point_compress" ->
  (fun x28 x29 ->
    let CI.CPointer x30 = x29 in
    _7_Hacl_Impl_Ed25519_PointCompress_point_compress x28 x30)
| Function
    (CI.Pointer _,
     Function (CI.OCaml CI.Bytes, Function (CI.Pointer _, Returns CI.Void))),
  "Hacl_Impl_Ed25519_Ladder_point_mul" ->
  (fun x31 x33 x34 ->
    let CI.CPointer x35 = x34 in
    let CI.CPointer x32 = x31 in
    _6_Hacl_Impl_Ed25519_Ladder_point_mul x32 x33 x35)
| Function
    (CI.Pointer _,
     Function (CI.Pointer _, Function (CI.Pointer _, Returns CI.Void))),
  "Hacl_Impl_Ed25519_PointAdd_point_add" ->
  (fun x36 x38 x40 ->
    let CI.CPointer x41 = x40 in
    let CI.CPointer x39 = x38 in
    let CI.CPointer x37 = x36 in
    _5_Hacl_Impl_Ed25519_PointAdd_point_add x37 x39 x41)
| Function (CI.OCaml CI.Bytes, Function (CI.Pointer _, Returns CI.Void)),
  "Hacl_Bignum25519_store_51" ->
  (fun x42 x43 ->
    let CI.CPointer x44 = x43 in _4_Hacl_Bignum25519_store_51 x42 x44)
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "Hacl_Bignum25519_load_51" ->
  (fun x45 x47 ->
    let CI.CPointer x46 = x45 in _3_Hacl_Bignum25519_load_51 x46 x47)
| Function (CI.Pointer _, Function (CI.Pointer _, Returns CI.Void)),
  "Hacl_Bignum25519_inverse" ->
  (fun x48 x50 ->
    let CI.CPointer x51 = x50 in
    let CI.CPointer x49 = x48 in _2_Hacl_Bignum25519_inverse x49 x51)
| Function (CI.Pointer _, Returns CI.Void), "Hacl_Bignum25519_reduce_513" ->
  (fun x52 ->
    let CI.CPointer x53 = x52 in _1_Hacl_Bignum25519_reduce_513 x53)
| _, s ->  Printf.ksprintf failwith "No match for %s" s


let foreign_value : type a. string -> a Ctypes.typ -> a Ctypes.ptr =
  fun name t -> match t, name with
| _, s ->  Printf.ksprintf failwith "No match for %s" s

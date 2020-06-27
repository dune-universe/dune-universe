module CI = Cstubs_internals

external _1_Hacl_Impl_P256_LowLevel_toUint8
  : _ CI.fatptr -> bytes CI.ocaml -> unit
  = "_1_Hacl_Impl_P256_LowLevel_toUint8" 

external _2_Hacl_Impl_P256_LowLevel_changeEndian : _ CI.fatptr -> unit
  = "_2_Hacl_Impl_P256_LowLevel_changeEndian" 

external _3_Hacl_Impl_P256_LowLevel_toUint64ChangeEndian
  : bytes CI.ocaml -> _ CI.fatptr -> unit
  = "_3_Hacl_Impl_P256_LowLevel_toUint64ChangeEndian" 

external _4_Hacl_Impl_P256_Core_isPointAtInfinityPrivate
  : _ CI.fatptr -> Unsigned.uint64
  = "_4_Hacl_Impl_P256_Core_isPointAtInfinityPrivate" 

external _5_Hacl_Impl_P256_Core_secretToPublic
  : _ CI.fatptr -> bytes CI.ocaml -> _ CI.fatptr -> unit
  = "_5_Hacl_Impl_P256_Core_secretToPublic" 

external _6_Hacl_Impl_P256_DH__ecp256dh_r
  : _ CI.fatptr -> _ CI.fatptr -> bytes CI.ocaml -> Unsigned.uint64
  = "_6_Hacl_Impl_P256_DH__ecp256dh_r" 

external _7_Hacl_P256_ecdsa_sign_p256_sha2
  : bytes CI.ocaml -> Unsigned.uint32 -> bytes CI.ocaml -> bytes CI.ocaml ->
    bytes CI.ocaml -> Unsigned.uint64 = "_7_Hacl_P256_ecdsa_sign_p256_sha2" 

external _8_Hacl_P256_ecdsa_sign_p256_sha384
  : bytes CI.ocaml -> Unsigned.uint32 -> bytes CI.ocaml -> bytes CI.ocaml ->
    bytes CI.ocaml -> Unsigned.uint64 = "_8_Hacl_P256_ecdsa_sign_p256_sha384" 

external _9_Hacl_P256_ecdsa_sign_p256_sha512
  : bytes CI.ocaml -> Unsigned.uint32 -> bytes CI.ocaml -> bytes CI.ocaml ->
    bytes CI.ocaml -> Unsigned.uint64 = "_9_Hacl_P256_ecdsa_sign_p256_sha512" 

external _10_Hacl_P256_ecdsa_sign_p256_without_hash
  : bytes CI.ocaml -> Unsigned.uint32 -> bytes CI.ocaml -> bytes CI.ocaml ->
    bytes CI.ocaml -> Unsigned.uint64
  = "_10_Hacl_P256_ecdsa_sign_p256_without_hash" 

external _11_Hacl_P256_ecdsa_verif_p256_sha2
  : Unsigned.uint32 -> bytes CI.ocaml -> bytes CI.ocaml -> bytes CI.ocaml ->
    bytes CI.ocaml -> bool = "_11_Hacl_P256_ecdsa_verif_p256_sha2" 

external _12_Hacl_P256_ecdsa_verif_p256_sha384
  : Unsigned.uint32 -> bytes CI.ocaml -> bytes CI.ocaml -> bytes CI.ocaml ->
    bytes CI.ocaml -> bool = "_12_Hacl_P256_ecdsa_verif_p256_sha384" 

external _13_Hacl_P256_ecdsa_verif_p256_sha512
  : Unsigned.uint32 -> bytes CI.ocaml -> bytes CI.ocaml -> bytes CI.ocaml ->
    bytes CI.ocaml -> bool = "_13_Hacl_P256_ecdsa_verif_p256_sha512" 

external _14_Hacl_P256_ecdsa_verif_without_hash
  : Unsigned.uint32 -> bytes CI.ocaml -> bytes CI.ocaml -> bytes CI.ocaml ->
    bytes CI.ocaml -> bool = "_14_Hacl_P256_ecdsa_verif_without_hash" 

external _15_Hacl_P256_verify_q : bytes CI.ocaml -> bool
  = "_15_Hacl_P256_verify_q" 

external _16_Hacl_P256_decompression_not_compressed_form
  : bytes CI.ocaml -> bytes CI.ocaml -> bool
  = "_16_Hacl_P256_decompression_not_compressed_form" 

external _17_Hacl_P256_decompression_compressed_form
  : bytes CI.ocaml -> bytes CI.ocaml -> bool
  = "_17_Hacl_P256_decompression_compressed_form" 

external _18_Hacl_P256_compression_not_compressed_form
  : bytes CI.ocaml -> bytes CI.ocaml -> unit
  = "_18_Hacl_P256_compression_not_compressed_form" 

external _19_Hacl_P256_compression_compressed_form
  : bytes CI.ocaml -> bytes CI.ocaml -> unit
  = "_19_Hacl_P256_compression_compressed_form" 

external _20_Hacl_P256_reduction_8_32
  : bytes CI.ocaml -> bytes CI.ocaml -> unit = "_20_Hacl_P256_reduction_8_32" 

external _21_Hacl_P256_ecp256dh_i
  : bytes CI.ocaml -> bytes CI.ocaml -> Unsigned.uint64
  = "_21_Hacl_P256_ecp256dh_i" 

external _22_Hacl_P256_ecp256dh_r
  : bytes CI.ocaml -> bytes CI.ocaml -> bytes CI.ocaml -> Unsigned.uint64
  = "_22_Hacl_P256_ecp256dh_r" 

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
        Function (CI.OCaml CI.Bytes, Returns (CI.Primitive CI.Uint64_t)))),
  "Hacl_P256_ecp256dh_r" -> _22_Hacl_P256_ecp256dh_r
| Function
    (CI.OCaml CI.Bytes,
     Function (CI.OCaml CI.Bytes, Returns (CI.Primitive CI.Uint64_t))),
  "Hacl_P256_ecp256dh_i" -> _21_Hacl_P256_ecp256dh_i
| Function (CI.OCaml CI.Bytes, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "Hacl_P256_reduction_8_32" -> _20_Hacl_P256_reduction_8_32
| Function (CI.OCaml CI.Bytes, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "Hacl_P256_compression_compressed_form" ->
  _19_Hacl_P256_compression_compressed_form
| Function (CI.OCaml CI.Bytes, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "Hacl_P256_compression_not_compressed_form" ->
  _18_Hacl_P256_compression_not_compressed_form
| Function
    (CI.OCaml CI.Bytes,
     Function (CI.OCaml CI.Bytes, Returns (CI.Primitive CI.Bool))),
  "Hacl_P256_decompression_compressed_form" ->
  _17_Hacl_P256_decompression_compressed_form
| Function
    (CI.OCaml CI.Bytes,
     Function (CI.OCaml CI.Bytes, Returns (CI.Primitive CI.Bool))),
  "Hacl_P256_decompression_not_compressed_form" ->
  _16_Hacl_P256_decompression_not_compressed_form
| Function (CI.OCaml CI.Bytes, Returns (CI.Primitive CI.Bool)),
  "Hacl_P256_verify_q" -> _15_Hacl_P256_verify_q
| Function
    (CI.Primitive CI.Uint32_t,
     Function
       (CI.OCaml CI.Bytes,
        Function
          (CI.OCaml CI.Bytes,
           Function
             (CI.OCaml CI.Bytes,
              Function (CI.OCaml CI.Bytes, Returns (CI.Primitive CI.Bool)))))),
  "Hacl_P256_ecdsa_verif_without_hash" ->
  _14_Hacl_P256_ecdsa_verif_without_hash
| Function
    (CI.Primitive CI.Uint32_t,
     Function
       (CI.OCaml CI.Bytes,
        Function
          (CI.OCaml CI.Bytes,
           Function
             (CI.OCaml CI.Bytes,
              Function (CI.OCaml CI.Bytes, Returns (CI.Primitive CI.Bool)))))),
  "Hacl_P256_ecdsa_verif_p256_sha512" ->
  _13_Hacl_P256_ecdsa_verif_p256_sha512
| Function
    (CI.Primitive CI.Uint32_t,
     Function
       (CI.OCaml CI.Bytes,
        Function
          (CI.OCaml CI.Bytes,
           Function
             (CI.OCaml CI.Bytes,
              Function (CI.OCaml CI.Bytes, Returns (CI.Primitive CI.Bool)))))),
  "Hacl_P256_ecdsa_verif_p256_sha384" ->
  _12_Hacl_P256_ecdsa_verif_p256_sha384
| Function
    (CI.Primitive CI.Uint32_t,
     Function
       (CI.OCaml CI.Bytes,
        Function
          (CI.OCaml CI.Bytes,
           Function
             (CI.OCaml CI.Bytes,
              Function (CI.OCaml CI.Bytes, Returns (CI.Primitive CI.Bool)))))),
  "Hacl_P256_ecdsa_verif_p256_sha2" -> _11_Hacl_P256_ecdsa_verif_p256_sha2
| Function
    (CI.OCaml CI.Bytes,
     Function
       (CI.Primitive CI.Uint32_t,
        Function
          (CI.OCaml CI.Bytes,
           Function
             (CI.OCaml CI.Bytes,
              Function
                (CI.OCaml CI.Bytes, Returns (CI.Primitive CI.Uint64_t)))))),
  "Hacl_P256_ecdsa_sign_p256_without_hash" ->
  _10_Hacl_P256_ecdsa_sign_p256_without_hash
| Function
    (CI.OCaml CI.Bytes,
     Function
       (CI.Primitive CI.Uint32_t,
        Function
          (CI.OCaml CI.Bytes,
           Function
             (CI.OCaml CI.Bytes,
              Function
                (CI.OCaml CI.Bytes, Returns (CI.Primitive CI.Uint64_t)))))),
  "Hacl_P256_ecdsa_sign_p256_sha512" -> _9_Hacl_P256_ecdsa_sign_p256_sha512
| Function
    (CI.OCaml CI.Bytes,
     Function
       (CI.Primitive CI.Uint32_t,
        Function
          (CI.OCaml CI.Bytes,
           Function
             (CI.OCaml CI.Bytes,
              Function
                (CI.OCaml CI.Bytes, Returns (CI.Primitive CI.Uint64_t)))))),
  "Hacl_P256_ecdsa_sign_p256_sha384" -> _8_Hacl_P256_ecdsa_sign_p256_sha384
| Function
    (CI.OCaml CI.Bytes,
     Function
       (CI.Primitive CI.Uint32_t,
        Function
          (CI.OCaml CI.Bytes,
           Function
             (CI.OCaml CI.Bytes,
              Function
                (CI.OCaml CI.Bytes, Returns (CI.Primitive CI.Uint64_t)))))),
  "Hacl_P256_ecdsa_sign_p256_sha2" -> _7_Hacl_P256_ecdsa_sign_p256_sha2
| Function
    (CI.Pointer _,
     Function
       (CI.Pointer _,
        Function (CI.OCaml CI.Bytes, Returns (CI.Primitive CI.Uint64_t)))),
  "Hacl_Impl_P256_DH__ecp256dh_r" ->
  (fun x57 x59 x61 ->
    let CI.CPointer x60 = x59 in
    let CI.CPointer x58 = x57 in _6_Hacl_Impl_P256_DH__ecp256dh_r x58 x60 x61)
| Function
    (CI.Pointer _,
     Function (CI.OCaml CI.Bytes, Function (CI.Pointer _, Returns CI.Void))),
  "Hacl_Impl_P256_Core_secretToPublic" ->
  (fun x62 x64 x65 ->
    let CI.CPointer x66 = x65 in
    let CI.CPointer x63 = x62 in
    _5_Hacl_Impl_P256_Core_secretToPublic x63 x64 x66)
| Function (CI.Pointer _, Returns (CI.Primitive CI.Uint64_t)),
  "Hacl_Impl_P256_Core_isPointAtInfinityPrivate" ->
  (fun x67 ->
    let CI.CPointer x68 = x67 in
    _4_Hacl_Impl_P256_Core_isPointAtInfinityPrivate x68)
| Function (CI.OCaml CI.Bytes, Function (CI.Pointer _, Returns CI.Void)),
  "Hacl_Impl_P256_LowLevel_toUint64ChangeEndian" ->
  (fun x69 x70 ->
    let CI.CPointer x71 = x70 in
    _3_Hacl_Impl_P256_LowLevel_toUint64ChangeEndian x69 x71)
| Function (CI.Pointer _, Returns CI.Void),
  "Hacl_Impl_P256_LowLevel_changeEndian" ->
  (fun x72 ->
    let CI.CPointer x73 = x72 in _2_Hacl_Impl_P256_LowLevel_changeEndian x73)
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "Hacl_Impl_P256_LowLevel_toUint8" ->
  (fun x74 x76 ->
    let CI.CPointer x75 = x74 in _1_Hacl_Impl_P256_LowLevel_toUint8 x75 x76)
| _, s ->  Printf.ksprintf failwith "No match for %s" s


let foreign_value : type a. string -> a Ctypes.typ -> a Ctypes.ptr =
  fun name t -> match t, name with
| _, s ->  Printf.ksprintf failwith "No match for %s" s

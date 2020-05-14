module CI = Cstubs_internals

external _1_Hacl_Impl_P256_DH_ecp256dh_i
  : Bytes.t CI.ocaml -> Bytes.t CI.ocaml -> Unsigned.uint64
  = "_1_Hacl_Impl_P256_DH_ecp256dh_i" 

external _2_Hacl_Impl_P256_DH_ecp256dh_r
  : Bytes.t CI.ocaml -> Bytes.t CI.ocaml -> Bytes.t CI.ocaml ->
    Unsigned.uint64 = "_2_Hacl_Impl_P256_DH_ecp256dh_r" 

external _3_Hacl_Impl_ECDSA_ecdsa_p256_sha2_sign
  : Bytes.t CI.ocaml -> Unsigned.uint32 -> Bytes.t CI.ocaml ->
    Bytes.t CI.ocaml -> Bytes.t CI.ocaml -> Unsigned.uint64
  = "_3_Hacl_Impl_ECDSA_ecdsa_p256_sha2_sign" 

external _4_Hacl_Impl_ECDSA_ecdsa_p256_sha2_verify
  : Unsigned.uint32 -> Bytes.t CI.ocaml -> Bytes.t CI.ocaml ->
    Bytes.t CI.ocaml -> Bytes.t CI.ocaml -> bool
  = "_4_Hacl_Impl_ECDSA_ecdsa_p256_sha2_verify" 

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
    (CI.Primitive CI.Uint32_t,
     Function
       (CI.OCaml CI.Bytes,
        Function
          (CI.OCaml CI.Bytes,
           Function
             (CI.OCaml CI.Bytes,
              Function (CI.OCaml CI.Bytes, Returns (CI.Primitive CI.Bool)))))),
  "Hacl_Impl_ECDSA_ecdsa_p256_sha2_verify" ->
  _4_Hacl_Impl_ECDSA_ecdsa_p256_sha2_verify
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
  "Hacl_Impl_ECDSA_ecdsa_p256_sha2_sign" ->
  _3_Hacl_Impl_ECDSA_ecdsa_p256_sha2_sign
| Function
    (CI.OCaml CI.Bytes,
     Function
       (CI.OCaml CI.Bytes,
        Function (CI.OCaml CI.Bytes, Returns (CI.Primitive CI.Uint64_t)))),
  "Hacl_Impl_P256_DH_ecp256dh_r" -> _2_Hacl_Impl_P256_DH_ecp256dh_r
| Function
    (CI.OCaml CI.Bytes,
     Function (CI.OCaml CI.Bytes, Returns (CI.Primitive CI.Uint64_t))),
  "Hacl_Impl_P256_DH_ecp256dh_i" -> _1_Hacl_Impl_P256_DH_ecp256dh_i
| _, s ->  Printf.ksprintf failwith "No match for %s" s


let foreign_value : type a. string -> a Ctypes.typ -> a Ctypes.ptr =
  fun name t -> match t, name with
| _, s ->  Printf.ksprintf failwith "No match for %s" s

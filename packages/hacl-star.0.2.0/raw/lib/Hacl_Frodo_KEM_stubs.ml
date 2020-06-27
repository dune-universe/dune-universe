module CI = Cstubs_internals

external _1_Hacl_Frodo_KEM_crypto_kem_keypair
  : bytes CI.ocaml -> bytes CI.ocaml -> Unsigned.uint32
  = "_1_Hacl_Frodo_KEM_crypto_kem_keypair" 

external _2_Hacl_Frodo_KEM_crypto_kem_enc
  : bytes CI.ocaml -> bytes CI.ocaml -> bytes CI.ocaml -> Unsigned.uint32
  = "_2_Hacl_Frodo_KEM_crypto_kem_enc" 

external _3_Hacl_Frodo_KEM_crypto_kem_dec
  : bytes CI.ocaml -> bytes CI.ocaml -> bytes CI.ocaml -> Unsigned.uint32
  = "_3_Hacl_Frodo_KEM_crypto_kem_dec" 

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
        Function (CI.OCaml CI.Bytes, Returns (CI.Primitive CI.Uint32_t)))),
  "Hacl_Frodo_KEM_crypto_kem_dec" -> _3_Hacl_Frodo_KEM_crypto_kem_dec
| Function
    (CI.OCaml CI.Bytes,
     Function
       (CI.OCaml CI.Bytes,
        Function (CI.OCaml CI.Bytes, Returns (CI.Primitive CI.Uint32_t)))),
  "Hacl_Frodo_KEM_crypto_kem_enc" -> _2_Hacl_Frodo_KEM_crypto_kem_enc
| Function
    (CI.OCaml CI.Bytes,
     Function (CI.OCaml CI.Bytes, Returns (CI.Primitive CI.Uint32_t))),
  "Hacl_Frodo_KEM_crypto_kem_keypair" -> _1_Hacl_Frodo_KEM_crypto_kem_keypair
| _, s ->  Printf.ksprintf failwith "No match for %s" s


let foreign_value : type a. string -> a Ctypes.typ -> a Ctypes.ptr =
  fun name t -> match t, name with
| _, s ->  Printf.ksprintf failwith "No match for %s" s

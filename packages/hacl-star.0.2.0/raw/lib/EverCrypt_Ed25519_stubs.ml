module CI = Cstubs_internals

external _1_EverCrypt_Ed25519_sign
  : bytes CI.ocaml -> bytes CI.ocaml -> Unsigned.uint32 -> bytes CI.ocaml ->
    unit = "_1_EverCrypt_Ed25519_sign" 

external _2_EverCrypt_Ed25519_verify
  : bytes CI.ocaml -> Unsigned.uint32 -> bytes CI.ocaml -> bytes CI.ocaml ->
    bool = "_2_EverCrypt_Ed25519_verify" 

external _3_EverCrypt_Ed25519_secret_to_public
  : bytes CI.ocaml -> bytes CI.ocaml -> unit
  = "_3_EverCrypt_Ed25519_secret_to_public" 

external _4_EverCrypt_Ed25519_expand_keys
  : bytes CI.ocaml -> bytes CI.ocaml -> unit
  = "_4_EverCrypt_Ed25519_expand_keys" 

external _5_EverCrypt_Ed25519_sign_expanded
  : bytes CI.ocaml -> bytes CI.ocaml -> Unsigned.uint32 -> bytes CI.ocaml ->
    unit = "_5_EverCrypt_Ed25519_sign_expanded" 

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
  "EverCrypt_Ed25519_sign_expanded" -> _5_EverCrypt_Ed25519_sign_expanded
| Function (CI.OCaml CI.Bytes, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "EverCrypt_Ed25519_expand_keys" -> _4_EverCrypt_Ed25519_expand_keys
| Function (CI.OCaml CI.Bytes, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "EverCrypt_Ed25519_secret_to_public" ->
  _3_EverCrypt_Ed25519_secret_to_public
| Function
    (CI.OCaml CI.Bytes,
     Function
       (CI.Primitive CI.Uint32_t,
        Function
          (CI.OCaml CI.Bytes,
           Function (CI.OCaml CI.Bytes, Returns (CI.Primitive CI.Bool))))),
  "EverCrypt_Ed25519_verify" -> _2_EverCrypt_Ed25519_verify
| Function
    (CI.OCaml CI.Bytes,
     Function
       (CI.OCaml CI.Bytes,
        Function
          (CI.Primitive CI.Uint32_t,
           Function (CI.OCaml CI.Bytes, Returns CI.Void)))),
  "EverCrypt_Ed25519_sign" -> _1_EverCrypt_Ed25519_sign
| _, s ->  Printf.ksprintf failwith "No match for %s" s


let foreign_value : type a. string -> a Ctypes.typ -> a Ctypes.ptr =
  fun name t -> match t, name with
| _, s ->  Printf.ksprintf failwith "No match for %s" s

module CI = Cstubs_internals

external _1_Hacl_HPKE_Curve64_CP128_SHA512_setupBaseI
  : bytes CI.ocaml -> bytes CI.ocaml -> bytes CI.ocaml -> bytes CI.ocaml ->
    bytes CI.ocaml -> Unsigned.uint32 -> bytes CI.ocaml -> Unsigned.uint32
  =
  "_1_Hacl_HPKE_Curve64_CP128_SHA512_setupBaseI_byte7" "_1_Hacl_HPKE_Curve64_CP128_SHA512_setupBaseI"
  

external _2_Hacl_HPKE_Curve64_CP128_SHA512_setupBaseR
  : bytes CI.ocaml -> bytes CI.ocaml -> bytes CI.ocaml -> bytes CI.ocaml ->
    Unsigned.uint32 -> bytes CI.ocaml -> Unsigned.uint32
  =
  "_2_Hacl_HPKE_Curve64_CP128_SHA512_setupBaseR_byte6" "_2_Hacl_HPKE_Curve64_CP128_SHA512_setupBaseR"
  

external _3_Hacl_HPKE_Curve64_CP128_SHA512_sealBase
  : bytes CI.ocaml -> bytes CI.ocaml -> Unsigned.uint32 -> bytes CI.ocaml ->
    Unsigned.uint32 -> bytes CI.ocaml -> bytes CI.ocaml -> Unsigned.uint32
  =
  "_3_Hacl_HPKE_Curve64_CP128_SHA512_sealBase_byte7" "_3_Hacl_HPKE_Curve64_CP128_SHA512_sealBase"
  

external _4_Hacl_HPKE_Curve64_CP128_SHA512_openBase
  : bytes CI.ocaml -> bytes CI.ocaml -> Unsigned.uint32 -> bytes CI.ocaml ->
    Unsigned.uint32 -> bytes CI.ocaml -> bytes CI.ocaml -> Unsigned.uint32
  =
  "_4_Hacl_HPKE_Curve64_CP128_SHA512_openBase_byte7" "_4_Hacl_HPKE_Curve64_CP128_SHA512_openBase"
  

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
           Function
             (CI.OCaml CI.Bytes,
              Function
                (CI.Primitive CI.Uint32_t,
                 Function
                   (CI.OCaml CI.Bytes,
                    Function
                      (CI.OCaml CI.Bytes, Returns (CI.Primitive CI.Uint32_t)))))))),
  "Hacl_HPKE_Curve64_CP128_SHA512_openBase" ->
  _4_Hacl_HPKE_Curve64_CP128_SHA512_openBase
| Function
    (CI.OCaml CI.Bytes,
     Function
       (CI.OCaml CI.Bytes,
        Function
          (CI.Primitive CI.Uint32_t,
           Function
             (CI.OCaml CI.Bytes,
              Function
                (CI.Primitive CI.Uint32_t,
                 Function
                   (CI.OCaml CI.Bytes,
                    Function
                      (CI.OCaml CI.Bytes, Returns (CI.Primitive CI.Uint32_t)))))))),
  "Hacl_HPKE_Curve64_CP128_SHA512_sealBase" ->
  _3_Hacl_HPKE_Curve64_CP128_SHA512_sealBase
| Function
    (CI.OCaml CI.Bytes,
     Function
       (CI.OCaml CI.Bytes,
        Function
          (CI.OCaml CI.Bytes,
           Function
             (CI.OCaml CI.Bytes,
              Function
                (CI.Primitive CI.Uint32_t,
                 Function
                   (CI.OCaml CI.Bytes, Returns (CI.Primitive CI.Uint32_t))))))),
  "Hacl_HPKE_Curve64_CP128_SHA512_setupBaseR" ->
  _2_Hacl_HPKE_Curve64_CP128_SHA512_setupBaseR
| Function
    (CI.OCaml CI.Bytes,
     Function
       (CI.OCaml CI.Bytes,
        Function
          (CI.OCaml CI.Bytes,
           Function
             (CI.OCaml CI.Bytes,
              Function
                (CI.OCaml CI.Bytes,
                 Function
                   (CI.Primitive CI.Uint32_t,
                    Function
                      (CI.OCaml CI.Bytes, Returns (CI.Primitive CI.Uint32_t)))))))),
  "Hacl_HPKE_Curve64_CP128_SHA512_setupBaseI" ->
  _1_Hacl_HPKE_Curve64_CP128_SHA512_setupBaseI
| _, s ->  Printf.ksprintf failwith "No match for %s" s


let foreign_value : type a. string -> a Ctypes.typ -> a Ctypes.ptr =
  fun name t -> match t, name with
| _, s ->  Printf.ksprintf failwith "No match for %s" s

module CI = Cstubs_internals

external _1_Hacl_RSAPSS2048_SHA256_rsapss_sign
  : Unsigned.uint32 -> Unsigned.uint32 -> _ CI.fatptr -> Unsigned.uint32 ->
    bytes CI.ocaml -> Unsigned.uint32 -> bytes CI.ocaml -> bytes CI.ocaml ->
    bool
  =
  "_1_Hacl_RSAPSS2048_SHA256_rsapss_sign_byte8" "_1_Hacl_RSAPSS2048_SHA256_rsapss_sign"
  

external _2_Hacl_RSAPSS2048_SHA256_rsapss_verify
  : Unsigned.uint32 -> _ CI.fatptr -> Unsigned.uint32 -> Unsigned.uint32 ->
    bytes CI.ocaml -> Unsigned.uint32 -> bytes CI.ocaml -> bool
  =
  "_2_Hacl_RSAPSS2048_SHA256_rsapss_verify_byte7" "_2_Hacl_RSAPSS2048_SHA256_rsapss_verify"
  

external _3_Hacl_RSAPSS2048_SHA256_new_rsapss_load_pkey
  : Unsigned.uint32 -> bytes CI.ocaml -> bytes CI.ocaml -> CI.voidp
  = "_3_Hacl_RSAPSS2048_SHA256_new_rsapss_load_pkey" 

external _4_Hacl_RSAPSS2048_SHA256_new_rsapss_load_skey
  : Unsigned.uint32 -> Unsigned.uint32 -> bytes CI.ocaml -> bytes CI.ocaml ->
    bytes CI.ocaml -> CI.voidp
  = "_4_Hacl_RSAPSS2048_SHA256_new_rsapss_load_skey" 

external _5_Hacl_RSAPSS2048_SHA256_rsapss_skey_sign
  : Unsigned.uint32 -> Unsigned.uint32 -> bytes CI.ocaml -> bytes CI.ocaml ->
    bytes CI.ocaml -> Unsigned.uint32 -> bytes CI.ocaml -> Unsigned.uint32 ->
    bytes CI.ocaml -> bytes CI.ocaml -> bool
  =
  "_5_Hacl_RSAPSS2048_SHA256_rsapss_skey_sign_byte10" "_5_Hacl_RSAPSS2048_SHA256_rsapss_skey_sign"
  

external _6_Hacl_RSAPSS2048_SHA256_rsapss_pkey_verify
  : Unsigned.uint32 -> bytes CI.ocaml -> bytes CI.ocaml -> Unsigned.uint32 ->
    Unsigned.uint32 -> bytes CI.ocaml -> Unsigned.uint32 -> bytes CI.ocaml ->
    bool
  =
  "_6_Hacl_RSAPSS2048_SHA256_rsapss_pkey_verify_byte8" "_6_Hacl_RSAPSS2048_SHA256_rsapss_pkey_verify"
  

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
             (CI.Primitive CI.Uint32_t,
              Function
                (CI.Primitive CI.Uint32_t,
                 Function
                   (CI.OCaml CI.Bytes,
                    Function
                      (CI.Primitive CI.Uint32_t,
                       Function
                         (CI.OCaml CI.Bytes, Returns (CI.Primitive CI.Bool))))))))),
  "Hacl_RSAPSS2048_SHA256_rsapss_pkey_verify" ->
  _6_Hacl_RSAPSS2048_SHA256_rsapss_pkey_verify
| Function
    (CI.Primitive CI.Uint32_t,
     Function
       (CI.Primitive CI.Uint32_t,
        Function
          (CI.OCaml CI.Bytes,
           Function
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
                               (CI.OCaml CI.Bytes,
                                Returns (CI.Primitive CI.Bool))))))))))),
  "Hacl_RSAPSS2048_SHA256_rsapss_skey_sign" ->
  _5_Hacl_RSAPSS2048_SHA256_rsapss_skey_sign
| Function
    (CI.Primitive CI.Uint32_t,
     Function
       (CI.Primitive CI.Uint32_t,
        Function
          (CI.OCaml CI.Bytes,
           Function
             (CI.OCaml CI.Bytes,
              Function (CI.OCaml CI.Bytes, Returns (CI.Pointer x24)))))),
  "Hacl_RSAPSS2048_SHA256_new_rsapss_load_skey" ->
  (fun x19 x20 x21 x22 x23 ->
    CI.make_ptr x24
      (_4_Hacl_RSAPSS2048_SHA256_new_rsapss_load_skey x19 x20 x21 x22 x23))
| Function
    (CI.Primitive CI.Uint32_t,
     Function
       (CI.OCaml CI.Bytes,
        Function (CI.OCaml CI.Bytes, Returns (CI.Pointer x28)))),
  "Hacl_RSAPSS2048_SHA256_new_rsapss_load_pkey" ->
  (fun x25 x26 x27 ->
    CI.make_ptr x28
      (_3_Hacl_RSAPSS2048_SHA256_new_rsapss_load_pkey x25 x26 x27))
| Function
    (CI.Primitive CI.Uint32_t,
     Function
       (CI.Pointer _,
        Function
          (CI.Primitive CI.Uint32_t,
           Function
             (CI.Primitive CI.Uint32_t,
              Function
                (CI.OCaml CI.Bytes,
                 Function
                   (CI.Primitive CI.Uint32_t,
                    Function
                      (CI.OCaml CI.Bytes, Returns (CI.Primitive CI.Bool)))))))),
  "Hacl_RSAPSS2048_SHA256_rsapss_verify" ->
  (fun x29 x30 x32 x33 x34 x35 x36 ->
    let CI.CPointer x31 = x30 in
    _2_Hacl_RSAPSS2048_SHA256_rsapss_verify x29 x31 x32 x33 x34 x35 x36)
| Function
    (CI.Primitive CI.Uint32_t,
     Function
       (CI.Primitive CI.Uint32_t,
        Function
          (CI.Pointer _,
           Function
             (CI.Primitive CI.Uint32_t,
              Function
                (CI.OCaml CI.Bytes,
                 Function
                   (CI.Primitive CI.Uint32_t,
                    Function
                      (CI.OCaml CI.Bytes,
                       Function
                         (CI.OCaml CI.Bytes, Returns (CI.Primitive CI.Bool))))))))),
  "Hacl_RSAPSS2048_SHA256_rsapss_sign" ->
  (fun x37 x38 x39 x41 x42 x43 x44 x45 ->
    let CI.CPointer x40 = x39 in
    _1_Hacl_RSAPSS2048_SHA256_rsapss_sign x37 x38 x40 x41 x42 x43 x44 x45)
| _, s ->  Printf.ksprintf failwith "No match for %s" s


let foreign_value : type a. string -> a Ctypes.typ -> a Ctypes.ptr =
  fun name t -> match t, name with
| _, s ->  Printf.ksprintf failwith "No match for %s" s

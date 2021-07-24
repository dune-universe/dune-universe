module CI = Cstubs_internals

external _1_Hacl_RSAPSS_rsapss_sign
  : Unsigned.uint8 -> Unsigned.uint32 -> Unsigned.uint32 ->
    Unsigned.uint32 -> _ CI.fatptr -> Unsigned.uint32 -> bytes CI.ocaml ->
    Unsigned.uint32 -> bytes CI.ocaml -> bytes CI.ocaml -> bool
  = "_1_Hacl_RSAPSS_rsapss_sign_byte10" "_1_Hacl_RSAPSS_rsapss_sign" 

external _2_Hacl_RSAPSS_rsapss_verify
  : Unsigned.uint8 -> Unsigned.uint32 -> Unsigned.uint32 -> _ CI.fatptr ->
    Unsigned.uint32 -> Unsigned.uint32 -> bytes CI.ocaml ->
    Unsigned.uint32 -> bytes CI.ocaml -> bool
  = "_2_Hacl_RSAPSS_rsapss_verify_byte9" "_2_Hacl_RSAPSS_rsapss_verify" 

external _3_Hacl_RSAPSS_new_rsapss_load_pkey
  : Unsigned.uint32 -> Unsigned.uint32 -> bytes CI.ocaml -> bytes CI.ocaml ->
    CI.voidp = "_3_Hacl_RSAPSS_new_rsapss_load_pkey" 

external _4_Hacl_RSAPSS_new_rsapss_load_skey
  : Unsigned.uint32 -> Unsigned.uint32 -> Unsigned.uint32 ->
    bytes CI.ocaml -> bytes CI.ocaml -> bytes CI.ocaml -> CI.voidp
  =
  "_4_Hacl_RSAPSS_new_rsapss_load_skey_byte6" "_4_Hacl_RSAPSS_new_rsapss_load_skey"
  

external _5_Hacl_RSAPSS_rsapss_skey_sign
  : Unsigned.uint8 -> Unsigned.uint32 -> Unsigned.uint32 ->
    Unsigned.uint32 -> bytes CI.ocaml -> bytes CI.ocaml -> bytes CI.ocaml ->
    Unsigned.uint32 -> bytes CI.ocaml -> Unsigned.uint32 -> bytes CI.ocaml ->
    bytes CI.ocaml -> bool
  =
  "_5_Hacl_RSAPSS_rsapss_skey_sign_byte12" "_5_Hacl_RSAPSS_rsapss_skey_sign" 

external _6_Hacl_RSAPSS_rsapss_pkey_verify
  : Unsigned.uint8 -> Unsigned.uint32 -> Unsigned.uint32 -> bytes CI.ocaml ->
    bytes CI.ocaml -> Unsigned.uint32 -> Unsigned.uint32 -> bytes CI.ocaml ->
    Unsigned.uint32 -> bytes CI.ocaml -> bool
  =
  "_6_Hacl_RSAPSS_rsapss_pkey_verify_byte10" "_6_Hacl_RSAPSS_rsapss_pkey_verify"
  

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
    (CI.View {CI.ty = CI.Primitive CI.Uint8_t; write = x2; _},
     Function
       (CI.Primitive CI.Uint32_t,
        Function
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
                               (CI.OCaml CI.Bytes,
                                Returns (CI.Primitive CI.Bool))))))))))),
  "Hacl_RSAPSS_rsapss_pkey_verify" ->
  (fun x1 x4 x5 x6 x7 x8 x9 x10 x11 x12 ->
    let x3 = x2 x1 in
    _6_Hacl_RSAPSS_rsapss_pkey_verify x3 x4 x5 x6 x7 x8 x9 x10 x11 x12)
| Function
    (CI.View {CI.ty = CI.Primitive CI.Uint8_t; write = x14; _},
     Function
       (CI.Primitive CI.Uint32_t,
        Function
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
                                      Returns (CI.Primitive CI.Bool))))))))))))),
  "Hacl_RSAPSS_rsapss_skey_sign" ->
  (fun x13 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 ->
    let x15 = x14 x13 in
    _5_Hacl_RSAPSS_rsapss_skey_sign x15 x16 x17 x18 x19 x20 x21 x22 x23 x24
    x25 x26)
| Function
    (CI.Primitive CI.Uint32_t,
     Function
       (CI.Primitive CI.Uint32_t,
        Function
          (CI.Primitive CI.Uint32_t,
           Function
             (CI.OCaml CI.Bytes,
              Function
                (CI.OCaml CI.Bytes,
                 Function (CI.OCaml CI.Bytes, Returns (CI.Pointer x33))))))),
  "Hacl_RSAPSS_new_rsapss_load_skey" ->
  (fun x27 x28 x29 x30 x31 x32 ->
    CI.make_ptr x33
      (_4_Hacl_RSAPSS_new_rsapss_load_skey x27 x28 x29 x30 x31 x32))
| Function
    (CI.Primitive CI.Uint32_t,
     Function
       (CI.Primitive CI.Uint32_t,
        Function
          (CI.OCaml CI.Bytes,
           Function (CI.OCaml CI.Bytes, Returns (CI.Pointer x38))))),
  "Hacl_RSAPSS_new_rsapss_load_pkey" ->
  (fun x34 x35 x36 x37 ->
    CI.make_ptr x38 (_3_Hacl_RSAPSS_new_rsapss_load_pkey x34 x35 x36 x37))
| Function
    (CI.View {CI.ty = CI.Primitive CI.Uint8_t; write = x40; _},
     Function
       (CI.Primitive CI.Uint32_t,
        Function
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
                            (CI.OCaml CI.Bytes,
                             Returns (CI.Primitive CI.Bool)))))))))),
  "Hacl_RSAPSS_rsapss_verify" ->
  (fun x39 x42 x43 x44 x46 x47 x48 x49 x50 ->
    let CI.CPointer x45 = x44 in
    let x41 = x40 x39 in
    _2_Hacl_RSAPSS_rsapss_verify x41 x42 x43 x45 x46 x47 x48 x49 x50)
| Function
    (CI.View {CI.ty = CI.Primitive CI.Uint8_t; write = x52; _},
     Function
       (CI.Primitive CI.Uint32_t,
        Function
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
                               (CI.OCaml CI.Bytes,
                                Returns (CI.Primitive CI.Bool))))))))))),
  "Hacl_RSAPSS_rsapss_sign" ->
  (fun x51 x54 x55 x56 x57 x59 x60 x61 x62 x63 ->
    let CI.CPointer x58 = x57 in
    let x53 = x52 x51 in
    _1_Hacl_RSAPSS_rsapss_sign x53 x54 x55 x56 x58 x59 x60 x61 x62 x63)
| _, s ->  Printf.ksprintf failwith "No match for %s" s


let foreign_value : type a. string -> a Ctypes.typ -> a Ctypes.ptr =
  fun name t -> match t, name with
| _, s ->  Printf.ksprintf failwith "No match for %s" s

module CI = Cstubs_internals

external _1_EverCrypt_AEAD_alg_of_state : _ CI.fatptr -> Unsigned.uint8
  = "_1_EverCrypt_AEAD_alg_of_state" 

external _2_EverCrypt_AEAD_create_in
  : Unsigned.uint8 -> _ CI.fatptr -> bytes CI.ocaml -> Unsigned.uint8
  = "_2_EverCrypt_AEAD_create_in" 

external _3_EverCrypt_AEAD_encrypt
  : _ CI.fatptr -> bytes CI.ocaml -> Unsigned.uint32 -> bytes CI.ocaml ->
    Unsigned.uint32 -> bytes CI.ocaml -> Unsigned.uint32 -> bytes CI.ocaml ->
    bytes CI.ocaml -> Unsigned.uint8
  = "_3_EverCrypt_AEAD_encrypt_byte9" "_3_EverCrypt_AEAD_encrypt" 

external _4_EverCrypt_AEAD_encrypt_expand_aes128_gcm
  : bytes CI.ocaml -> bytes CI.ocaml -> Unsigned.uint32 -> bytes CI.ocaml ->
    Unsigned.uint32 -> bytes CI.ocaml -> Unsigned.uint32 -> bytes CI.ocaml ->
    bytes CI.ocaml -> Unsigned.uint8
  =
  "_4_EverCrypt_AEAD_encrypt_expand_aes128_gcm_byte9" "_4_EverCrypt_AEAD_encrypt_expand_aes128_gcm"
  

external _5_EverCrypt_AEAD_encrypt_expand_aes256_gcm
  : bytes CI.ocaml -> bytes CI.ocaml -> Unsigned.uint32 -> bytes CI.ocaml ->
    Unsigned.uint32 -> bytes CI.ocaml -> Unsigned.uint32 -> bytes CI.ocaml ->
    bytes CI.ocaml -> Unsigned.uint8
  =
  "_5_EverCrypt_AEAD_encrypt_expand_aes256_gcm_byte9" "_5_EverCrypt_AEAD_encrypt_expand_aes256_gcm"
  

external _6_EverCrypt_AEAD_encrypt_expand_chacha20_poly1305
  : bytes CI.ocaml -> bytes CI.ocaml -> Unsigned.uint32 -> bytes CI.ocaml ->
    Unsigned.uint32 -> bytes CI.ocaml -> Unsigned.uint32 -> bytes CI.ocaml ->
    bytes CI.ocaml -> Unsigned.uint8
  =
  "_6_EverCrypt_AEAD_encrypt_expand_chacha20_poly1305_byte9" "_6_EverCrypt_AEAD_encrypt_expand_chacha20_poly1305"
  

external _7_EverCrypt_AEAD_encrypt_expand
  : Unsigned.uint8 -> bytes CI.ocaml -> bytes CI.ocaml -> Unsigned.uint32 ->
    bytes CI.ocaml -> Unsigned.uint32 -> bytes CI.ocaml -> Unsigned.uint32 ->
    bytes CI.ocaml -> bytes CI.ocaml -> Unsigned.uint8
  =
  "_7_EverCrypt_AEAD_encrypt_expand_byte10" "_7_EverCrypt_AEAD_encrypt_expand"
  

external _8_EverCrypt_AEAD_decrypt
  : _ CI.fatptr -> bytes CI.ocaml -> Unsigned.uint32 -> bytes CI.ocaml ->
    Unsigned.uint32 -> bytes CI.ocaml -> Unsigned.uint32 -> bytes CI.ocaml ->
    bytes CI.ocaml -> Unsigned.uint8
  = "_8_EverCrypt_AEAD_decrypt_byte9" "_8_EverCrypt_AEAD_decrypt" 

external _9_EverCrypt_AEAD_decrypt_expand_aes128_gcm
  : bytes CI.ocaml -> bytes CI.ocaml -> Unsigned.uint32 -> bytes CI.ocaml ->
    Unsigned.uint32 -> bytes CI.ocaml -> Unsigned.uint32 -> bytes CI.ocaml ->
    bytes CI.ocaml -> Unsigned.uint8
  =
  "_9_EverCrypt_AEAD_decrypt_expand_aes128_gcm_byte9" "_9_EverCrypt_AEAD_decrypt_expand_aes128_gcm"
  

external _10_EverCrypt_AEAD_decrypt_expand_aes256_gcm
  : bytes CI.ocaml -> bytes CI.ocaml -> Unsigned.uint32 -> bytes CI.ocaml ->
    Unsigned.uint32 -> bytes CI.ocaml -> Unsigned.uint32 -> bytes CI.ocaml ->
    bytes CI.ocaml -> Unsigned.uint8
  =
  "_10_EverCrypt_AEAD_decrypt_expand_aes256_gcm_byte9" "_10_EverCrypt_AEAD_decrypt_expand_aes256_gcm"
  

external _11_EverCrypt_AEAD_decrypt_expand_chacha20_poly1305
  : bytes CI.ocaml -> bytes CI.ocaml -> Unsigned.uint32 -> bytes CI.ocaml ->
    Unsigned.uint32 -> bytes CI.ocaml -> Unsigned.uint32 -> bytes CI.ocaml ->
    bytes CI.ocaml -> Unsigned.uint8
  =
  "_11_EverCrypt_AEAD_decrypt_expand_chacha20_poly1305_byte9" "_11_EverCrypt_AEAD_decrypt_expand_chacha20_poly1305"
  

external _12_EverCrypt_AEAD_decrypt_expand
  : Unsigned.uint8 -> bytes CI.ocaml -> bytes CI.ocaml -> Unsigned.uint32 ->
    bytes CI.ocaml -> Unsigned.uint32 -> bytes CI.ocaml -> Unsigned.uint32 ->
    bytes CI.ocaml -> bytes CI.ocaml -> Unsigned.uint8
  =
  "_12_EverCrypt_AEAD_decrypt_expand_byte10" "_12_EverCrypt_AEAD_decrypt_expand"
  

external _13_EverCrypt_AEAD_free : _ CI.fatptr -> unit
  = "_13_EverCrypt_AEAD_free" 

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
| Function (CI.Pointer _, Returns CI.Void), "EverCrypt_AEAD_free" ->
  (fun x1 -> let CI.CPointer x2 = x1 in _13_EverCrypt_AEAD_free x2)
| Function
    (CI.View {CI.ty = CI.Primitive CI.Uint8_t; write = x4; _},
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
                         (CI.Primitive CI.Uint32_t,
                          Function
                            (CI.OCaml CI.Bytes,
                             Function
                               (CI.OCaml CI.Bytes,
                                Returns
                                  (CI.View
                                     {CI.ty = CI.Primitive CI.Uint8_t;
                                      read = x15; _}))))))))))),
  "EverCrypt_AEAD_decrypt_expand" ->
  (fun x3 x6 x7 x8 x9 x10 x11 x12 x13 x14 ->
    let x5 = x4 x3 in
    x15
    (_12_EverCrypt_AEAD_decrypt_expand x5 x6 x7 x8 x9 x10 x11 x12 x13 x14))
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
                      (CI.Primitive CI.Uint32_t,
                       Function
                         (CI.OCaml CI.Bytes,
                          Function
                            (CI.OCaml CI.Bytes,
                             Returns
                               (CI.View
                                  {CI.ty = CI.Primitive CI.Uint8_t;
                                   read = x25; _})))))))))),
  "EverCrypt_AEAD_decrypt_expand_chacha20_poly1305" ->
  (fun x16 x17 x18 x19 x20 x21 x22 x23 x24 ->
    x25
    (_11_EverCrypt_AEAD_decrypt_expand_chacha20_poly1305 x16 x17 x18 x19 x20
     x21 x22 x23 x24))
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
                      (CI.Primitive CI.Uint32_t,
                       Function
                         (CI.OCaml CI.Bytes,
                          Function
                            (CI.OCaml CI.Bytes,
                             Returns
                               (CI.View
                                  {CI.ty = CI.Primitive CI.Uint8_t;
                                   read = x35; _})))))))))),
  "EverCrypt_AEAD_decrypt_expand_aes256_gcm" ->
  (fun x26 x27 x28 x29 x30 x31 x32 x33 x34 ->
    x35
    (_10_EverCrypt_AEAD_decrypt_expand_aes256_gcm x26 x27 x28 x29 x30 x31 x32
     x33 x34))
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
                      (CI.Primitive CI.Uint32_t,
                       Function
                         (CI.OCaml CI.Bytes,
                          Function
                            (CI.OCaml CI.Bytes,
                             Returns
                               (CI.View
                                  {CI.ty = CI.Primitive CI.Uint8_t;
                                   read = x45; _})))))))))),
  "EverCrypt_AEAD_decrypt_expand_aes128_gcm" ->
  (fun x36 x37 x38 x39 x40 x41 x42 x43 x44 ->
    x45
    (_9_EverCrypt_AEAD_decrypt_expand_aes128_gcm x36 x37 x38 x39 x40 x41 x42
     x43 x44))
| Function
    (CI.Pointer _,
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
                      (CI.Primitive CI.Uint32_t,
                       Function
                         (CI.OCaml CI.Bytes,
                          Function
                            (CI.OCaml CI.Bytes,
                             Returns
                               (CI.View
                                  {CI.ty = CI.Primitive CI.Uint8_t;
                                   read = x56; _})))))))))),
  "EverCrypt_AEAD_decrypt" ->
  (fun x46 x48 x49 x50 x51 x52 x53 x54 x55 ->
    let CI.CPointer x47 = x46 in
    x56 (_8_EverCrypt_AEAD_decrypt x47 x48 x49 x50 x51 x52 x53 x54 x55))
| Function
    (CI.View {CI.ty = CI.Primitive CI.Uint8_t; write = x58; _},
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
                         (CI.Primitive CI.Uint32_t,
                          Function
                            (CI.OCaml CI.Bytes,
                             Function
                               (CI.OCaml CI.Bytes,
                                Returns
                                  (CI.View
                                     {CI.ty = CI.Primitive CI.Uint8_t;
                                      read = x69; _}))))))))))),
  "EverCrypt_AEAD_encrypt_expand" ->
  (fun x57 x60 x61 x62 x63 x64 x65 x66 x67 x68 ->
    let x59 = x58 x57 in
    x69
    (_7_EverCrypt_AEAD_encrypt_expand x59 x60 x61 x62 x63 x64 x65 x66 x67
      x68))
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
                      (CI.Primitive CI.Uint32_t,
                       Function
                         (CI.OCaml CI.Bytes,
                          Function
                            (CI.OCaml CI.Bytes,
                             Returns
                               (CI.View
                                  {CI.ty = CI.Primitive CI.Uint8_t;
                                   read = x79; _})))))))))),
  "EverCrypt_AEAD_encrypt_expand_chacha20_poly1305" ->
  (fun x70 x71 x72 x73 x74 x75 x76 x77 x78 ->
    x79
    (_6_EverCrypt_AEAD_encrypt_expand_chacha20_poly1305 x70 x71 x72 x73 x74
     x75 x76 x77 x78))
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
                      (CI.Primitive CI.Uint32_t,
                       Function
                         (CI.OCaml CI.Bytes,
                          Function
                            (CI.OCaml CI.Bytes,
                             Returns
                               (CI.View
                                  {CI.ty = CI.Primitive CI.Uint8_t;
                                   read = x89; _})))))))))),
  "EverCrypt_AEAD_encrypt_expand_aes256_gcm" ->
  (fun x80 x81 x82 x83 x84 x85 x86 x87 x88 ->
    x89
    (_5_EverCrypt_AEAD_encrypt_expand_aes256_gcm x80 x81 x82 x83 x84 x85 x86
     x87 x88))
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
                      (CI.Primitive CI.Uint32_t,
                       Function
                         (CI.OCaml CI.Bytes,
                          Function
                            (CI.OCaml CI.Bytes,
                             Returns
                               (CI.View
                                  {CI.ty = CI.Primitive CI.Uint8_t;
                                   read = x99; _})))))))))),
  "EverCrypt_AEAD_encrypt_expand_aes128_gcm" ->
  (fun x90 x91 x92 x93 x94 x95 x96 x97 x98 ->
    x99
    (_4_EverCrypt_AEAD_encrypt_expand_aes128_gcm x90 x91 x92 x93 x94 x95 x96
     x97 x98))
| Function
    (CI.Pointer _,
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
                      (CI.Primitive CI.Uint32_t,
                       Function
                         (CI.OCaml CI.Bytes,
                          Function
                            (CI.OCaml CI.Bytes,
                             Returns
                               (CI.View
                                  {CI.ty = CI.Primitive CI.Uint8_t;
                                   read = x110; _})))))))))),
  "EverCrypt_AEAD_encrypt" ->
  (fun x100 x102 x103 x104 x105 x106 x107 x108 x109 ->
    let CI.CPointer x101 = x100 in
    x110
    (_3_EverCrypt_AEAD_encrypt x101 x102 x103 x104 x105 x106 x107 x108 x109))
| Function
    (CI.View {CI.ty = CI.Primitive CI.Uint8_t; write = x112; _},
     Function
       (CI.Pointer _,
        Function
          (CI.OCaml CI.Bytes,
           Returns
             (CI.View {CI.ty = CI.Primitive CI.Uint8_t; read = x117; _})))),
  "EverCrypt_AEAD_create_in" ->
  (fun x111 x114 x116 ->
    let CI.CPointer x115 = x114 in
    let x113 = x112 x111 in x117 (_2_EverCrypt_AEAD_create_in x113 x115 x116))
| Function
    (CI.Pointer _,
     Returns (CI.View {CI.ty = CI.Primitive CI.Uint8_t; read = x120; _})),
  "EverCrypt_AEAD_alg_of_state" ->
  (fun x118 ->
    let CI.CPointer x119 = x118 in x120 (_1_EverCrypt_AEAD_alg_of_state x119))
| _, s ->  Printf.ksprintf failwith "No match for %s" s


let foreign_value : type a. string -> a Ctypes.typ -> a Ctypes.ptr =
  fun name t -> match t, name with
| _, s ->  Printf.ksprintf failwith "No match for %s" s

module CI = Cstubs_internals

external _1_EverCrypt_AEAD_alg_of_state : _ CI.fatptr -> Unsigned.uint8
  = "_1_EverCrypt_AEAD_alg_of_state" 

external _2_EverCrypt_AEAD_create_in
  : Unsigned.uint8 -> _ CI.fatptr -> Bytes.t CI.ocaml -> Unsigned.uint8
  = "_2_EverCrypt_AEAD_create_in" 

external _3_EverCrypt_AEAD_encrypt
  : _ CI.fatptr -> Bytes.t CI.ocaml -> Unsigned.uint32 -> Bytes.t CI.ocaml ->
    Unsigned.uint32 -> Bytes.t CI.ocaml -> Unsigned.uint32 ->
    Bytes.t CI.ocaml -> Bytes.t CI.ocaml -> Unsigned.uint8
  = "_3_EverCrypt_AEAD_encrypt_byte9" "_3_EverCrypt_AEAD_encrypt" 

external _4_EverCrypt_AEAD_decrypt
  : _ CI.fatptr -> Bytes.t CI.ocaml -> Unsigned.uint32 -> Bytes.t CI.ocaml ->
    Unsigned.uint32 -> Bytes.t CI.ocaml -> Unsigned.uint32 ->
    Bytes.t CI.ocaml -> Bytes.t CI.ocaml -> Unsigned.uint8
  = "_4_EverCrypt_AEAD_decrypt_byte9" "_4_EverCrypt_AEAD_decrypt" 

external _5_EverCrypt_AEAD_free : _ CI.fatptr -> unit
  = "_5_EverCrypt_AEAD_free" 

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
  (fun x1 -> _5_EverCrypt_AEAD_free (CI.cptr x1))
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
                                   read = x11; _})))))))))),
  "EverCrypt_AEAD_decrypt" ->
  (fun x2 x3 x4 x5 x6 x7 x8 x9 x10 ->
    x11 (_4_EverCrypt_AEAD_decrypt (CI.cptr x2) x3 x4 x5 x6 x7 x8 x9 x10))
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
                                   read = x21; _})))))))))),
  "EverCrypt_AEAD_encrypt" ->
  (fun x12 x13 x14 x15 x16 x17 x18 x19 x20 ->
    x21
    (_3_EverCrypt_AEAD_encrypt (CI.cptr x12) x13 x14 x15 x16 x17 x18 x19 x20))
| Function
    (CI.View {CI.ty = CI.Primitive CI.Uint8_t; write = x23; _},
     Function
       (CI.Pointer _,
        Function
          (CI.OCaml CI.Bytes,
           Returns (CI.View {CI.ty = CI.Primitive CI.Uint8_t; read = x27; _})))),
  "EverCrypt_AEAD_create_in" ->
  (fun x22 x25 x26 ->
    let x24 = x23 x22 in
    x27 (_2_EverCrypt_AEAD_create_in x24 (CI.cptr x25) x26))
| Function
    (CI.Pointer _,
     Returns (CI.View {CI.ty = CI.Primitive CI.Uint8_t; read = x29; _})),
  "EverCrypt_AEAD_alg_of_state" ->
  (fun x28 -> x29 (_1_EverCrypt_AEAD_alg_of_state (CI.cptr x28)))
| _, s ->  Printf.ksprintf failwith "No match for %s" s


let foreign_value : type a. string -> a Ctypes.typ -> a Ctypes.ptr =
  fun name t -> match t, name with
| _, s ->  Printf.ksprintf failwith "No match for %s" s

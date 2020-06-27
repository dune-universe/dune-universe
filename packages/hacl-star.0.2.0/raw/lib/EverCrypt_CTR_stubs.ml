module CI = Cstubs_internals

external _1_EverCrypt_CTR_xor8
  : Unsigned.uint8 -> Unsigned.uint8 -> Unsigned.uint8
  = "_1_EverCrypt_CTR_xor8" 

external _2_EverCrypt_CTR_alg_of_state : _ CI.fatptr -> Unsigned.uint8
  = "_2_EverCrypt_CTR_alg_of_state" 

external _3_EverCrypt_CTR_create_in
  : Unsigned.uint8 -> _ CI.fatptr -> bytes CI.ocaml -> bytes CI.ocaml ->
    Unsigned.uint32 -> Unsigned.uint32 -> Unsigned.uint8
  = "_3_EverCrypt_CTR_create_in_byte6" "_3_EverCrypt_CTR_create_in" 

external _4_EverCrypt_CTR_init
  : _ CI.fatptr -> bytes CI.ocaml -> bytes CI.ocaml -> Unsigned.uint32 ->
    Unsigned.uint32 -> unit = "_4_EverCrypt_CTR_init" 

external _5_EverCrypt_CTR_update_block
  : _ CI.fatptr -> bytes CI.ocaml -> bytes CI.ocaml -> unit
  = "_5_EverCrypt_CTR_update_block" 

external _6_EverCrypt_CTR_free : _ CI.fatptr -> unit
  = "_6_EverCrypt_CTR_free" 

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
| Function (CI.Pointer _, Returns CI.Void), "EverCrypt_CTR_free" ->
  (fun x1 -> let CI.CPointer x2 = x1 in _6_EverCrypt_CTR_free x2)
| Function
    (CI.Pointer _,
     Function
       (CI.OCaml CI.Bytes, Function (CI.OCaml CI.Bytes, Returns CI.Void))),
  "EverCrypt_CTR_update_block" ->
  (fun x3 x5 x6 ->
    let CI.CPointer x4 = x3 in _5_EverCrypt_CTR_update_block x4 x5 x6)
| Function
    (CI.Pointer _,
     Function
       (CI.OCaml CI.Bytes,
        Function
          (CI.OCaml CI.Bytes,
           Function
             (CI.Primitive CI.Uint32_t,
              Function (CI.Primitive CI.Uint32_t, Returns CI.Void))))),
  "EverCrypt_CTR_init" ->
  (fun x7 x9 x10 x11 x12 ->
    let CI.CPointer x8 = x7 in _4_EverCrypt_CTR_init x8 x9 x10 x11 x12)
| Function
    (CI.View {CI.ty = CI.Primitive CI.Uint8_t; write = x14; _},
     Function
       (CI.Pointer _,
        Function
          (CI.OCaml CI.Bytes,
           Function
             (CI.OCaml CI.Bytes,
              Function
                (CI.Primitive CI.Uint32_t,
                 Function
                   (CI.Primitive CI.Uint32_t,
                    Returns
                      (CI.View
                         {CI.ty = CI.Primitive CI.Uint8_t; read = x22; _}))))))),
  "EverCrypt_CTR_create_in" ->
  (fun x13 x16 x18 x19 x20 x21 ->
    let CI.CPointer x17 = x16 in
    let x15 = x14 x13 in
    x22 (_3_EverCrypt_CTR_create_in x15 x17 x18 x19 x20 x21))
| Function
    (CI.Pointer _,
     Returns (CI.View {CI.ty = CI.Primitive CI.Uint8_t; read = x25; _})),
  "EverCrypt_CTR_alg_of_state" ->
  (fun x23 ->
    let CI.CPointer x24 = x23 in x25 (_2_EverCrypt_CTR_alg_of_state x24))
| Function
    (CI.Primitive CI.Uint8_t,
     Function (CI.Primitive CI.Uint8_t, Returns (CI.Primitive CI.Uint8_t))),
  "EverCrypt_CTR_xor8" -> _1_EverCrypt_CTR_xor8
| _, s ->  Printf.ksprintf failwith "No match for %s" s


let foreign_value : type a. string -> a Ctypes.typ -> a Ctypes.ptr =
  fun name t -> match t, name with
| _, s ->  Printf.ksprintf failwith "No match for %s" s
